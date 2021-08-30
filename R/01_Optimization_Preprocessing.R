##############################################################
### Version 1:                                             ###              
### PURPOSE: Develop AMPL files for WMOST scaled-up        ###                                                       
###   optimization model screening tool                    ###                                         
### BY: Kate Munson, Yishen Li, Alyssa Le (ICF)            ###                                 
### DATE: 06/19/2020 (Updated 07/1/2020)                   ###                                                     
###                                                        ###     
### Version 2:                                             ###
### BY: Cathy Chamberlin                                   ###       
### DATE: 8/27/2021                                        ### 
###                                                        ###  
### Copyright (C) 2021  US EPA                             ###        
###                                                        ###  
### This program is free software: you can redistribute    ###                                                          
### it and/or modify it under the terms of the GNU         ###                                                      
### General Public License as published by the Free        ###                                                      
### Software Foundation, either version 3 of the           ###                                                    
### License, or(at your option) any later version.         ###                                                    
###                                                        ###           
### This program is distributed in the hope that it        ###                                                               
### will be useful, but WITHOUT ANY WARRANTY; without      ###                                                                 
### even the implied warranty of MERCHANTABILITY or        ###                                                               
### FITNESS FOR A PARTICULAR PURPOSE.  See the GNU         ###                                                               
### General Public License for more details.               ###                                                       
###                                                        ###           
### You should have received a copy of the GNU General     ###                                                                   
### Public License along with this program.  If not,       ###                                                                 
### see <https://www.gnu.org/licenses/>.                   ###
###                                                        ### 
### This code contains contributions from ICF under        ###                                     
### contract to US EPA. The Agency has unlimited rights to ###                                           
### all custom-developed code produced. The development of ###                                           
### this code was supported in part by an appointment to   ###                                         
### the U.S. Environmental Protection Agency (EPA)         ###                                   
### Research Participation Program administered by the Oak ###                                           
### Ridge Institute for Science and Education (ORISE)      ###                                       
### through an interagency agreement between the U.S.      ###                                       
### Department of Energy (DOE) and the U.S. Environmental  ###                                           
### Protection Agency. ORISE is managed by ORAU under DOE  ###                                           
### contract number DE-SC0014664. All comments expressed   ###                                         
### in this code file are the author's and do not          ###                                   
### necessarily reflect the policies and views of US EPA,  ###                                           
### DOE, or ORAU/ORISE.                                    ###       
##############################################################

# -------------General notes-------------

# 1. User must specify which BMPs will be considered in the optimization by specifying "X" in the BMP_Selection field of "UserSpecs_BMPs.csv"
# 2. Current input data refers to Northeastern Regional SPARROW loading data and land use data specific to Vermont and New Hampshire
# 3. If more advanced R users would like to assess different locations/data, they would need to search for and update lines of code that end in "#*#" 


# -------------Install and open packages-------------

# Note to user: Must install packages before running in R. Remove "#" symbols for "install.packages" lines of code before running code.
# install.packages("dplyr")
# install.packages("reshape2")
# install.packages("tidyr")
# install.packages("data.table")
# install.packages("stringr")
# install.packages("tidyverse")

packages <- c("tidyverse", "reshape2", "data.table", "stringr", "foreach")
invisible(
  suppressPackageStartupMessages(
    lapply(packages, library, character.only = TRUE)
  )
)

# This option prevents numeric data from being printed in scientific notation
options(scipen = 999)

# source helper functions
source("./R/Optimization_HelperFunctions.R")

##############################################################################################
# PART 1: USER SPECIFICATIONS
##############################################################################################

# -------------Read in user specifications-------------

### BMPS specifications: If BMP_Selection field = X, retain BMP for optimization. Otherwise ignore. 

user_specs_BMPs <- fread(
  paste(InPath,"01_UserSpecs_BMPs.csv",sep=""), data.table = FALSE
) 

### Loading targets specification: If TermFlag_X = X, this comid is the most downstream loading target
### If OutofNetworkFlag_X = X, then the only contributing watershed is itself.

user_specs_loadingtargets <- fread(
  paste0(InPath, "01_UserSpecs_loadingtargets.csv")
)


# -------------Establish load reduction goal-------------

### Specify TMDL percent reduction. This percent reduction will be applied to total baseline loadings at specified pore point.
# Note to user: If TMDL is associated with a particular load value, calculate percent reduction required to meet target at pore point.
# load_perc_reduc is now specified in user_specs_loadingtargets

load_perc_reduc_tn <- (
  user_specs_loadingtargets %>% filter(TN_or_TP == "TN")
)$Percent_Reduction 

load_perc_reduc_tp <- (
  user_specs_loadingtargets %>% filter(TN_or_TP == "TP")
)$Percent_Reduction 


##############################################################################################
# PART 2: PREPROCESS DATA INPUTS
##############################################################################################

# -------------Read raw data inputs-------------

# USGS Northeastern SPARROW Model Input Data, 2020: https://www.sciencebase.gov/catalog/item/5d4192aee4b01d82ce8da477
sparrow_in <- fread(paste(InPath,"ne_sparrow_model_input.csv",sep="")) #*#
sparrow_in$HUC_12_Rev <- str_pad(sparrow_in$HUC_12, width=12, pad="0")  #*#

# USGS Northeastern SPARROW Model Output Data, 2020: https://www.sciencebase.gov/catalog/item/5d4192aee4b01d82ce8da477
sparrow_cons_out_tn <- fread(
  paste(InPath,"ne_sparrow_model_output_tn.csv",sep=""), data.table = FALSE
) #*#

names(sparrow_cons_out_tn)[13] <- "in." # The column name 'in' causes issues because it is also an operator

sparrow_cons_out_tp <- fread(
  paste(InPath,"ne_sparrow_model_output_tp.csv",sep=""), data.table = FALSE
) #*#

# StreamCat Land Use Data, 2011: https://www.epa.gov/national-aquatic-resource-surveys/streamcat 
# Percent Cropland Data (NLCD2011)
vt_cropland <- fread(paste(InPath,"VT_streamcat_2011_cropland.csv",sep="")) #*#
nh_cropland <- fread(paste(InPath,"NH_streamcat_2011_cropland.csv",sep="")) #*#

# Percent Imperviousness Data (ImperviousSurfaces2011)
vt_imperv <- fread(
  paste(InPath,"VT_streamcat_2011_imperv.csv",sep=""), data.table = FALSE
) #*#
nh_imperv <- fread(
  paste(InPath,"NH_streamcat_2011_imperv.csv",sep=""), data.table = FALSE
) #*#

# Preprocessed Riparian Data
riparian.loadings <- fread("./Preprocessing/Inputs/RiparianLoadings.csv")
riparian.existingbuffer <- fread(
  "./Preprocessing/Inputs/LengthinBuffer_2016.csv"
)
riparian.efficiencies <- fread(
  "./Preprocessing/Inputs/RiparianEfficiencies.csv"
)


# Note to user: Make sure that both the variable names and associated data match the state(s) that your watershed falls within.


# -------------Combine state-specific datasets-------------

# Combine StreamCat land use and imperviouness datasets for associated states
vt_cropland$StateAbbrev <- "VT" #*#
nh_cropland$StateAbbrev <- "NH" #*#

cropland <- rbind(vt_cropland, nh_cropland) #*#

vt_imperv$StateAbbrev <- "VT" #*#
nh_imperv$StateAbbrev <- "NH" #*#

imperv <- rbind(vt_imperv, nh_imperv) #*#


# -------------Perform data modifications for all reaches-------------

### Change all COMID columns to lower case
names(sparrow_in)[1] <- "comid"
names(cropland)[1] <- "comid"
names(imperv)[1] <- "comid"

### Specify incremental loads as four categories: Point sources, urban, agricultural, and "other"
# Note to user: if making adjustment to SPARROW region, must identify which column names that reflect agricultural, point, and urban 
# nutrient sources based on SPARROW regional model metadata. For example, we used the USGS metadata from the Northeast SPARROW model: 
# https://www.sciencebase.gov/catalog/item/5d4192aee4b01d82ce8da477 
temp_inc_tn <- sparrow_cons_out_tn %>% select(c("comid","in_poin":"in_urb")) #*#
temp_inc_tn$in_ag <- with(temp_inc_tn, in_fert+in_fix+in_manu) #*#

### Adjust 2012 SPARROW loadings for N deposition based on percentage decreases to represent present conditions
ndep_change <- fread(paste(InPath, "NdepChange_2012_2019.csv", sep = ""))

temp_inc_tn_rev <- merge(
  temp_inc_tn,
  ndep_change[, c("comid", "Change_2012_2019")],
  by = "comid",
  all.x = TRUE
)

temp_inc_tn_rev[is.na(temp_inc_tn_rev)] <- 0

temp_inc_tn_rev[c("in_atmo")] <-  temp_inc_tn_rev[c("in_atmo")] -
  temp_inc_tn_rev[c("in_atmo")] * temp_inc_tn_rev[["Change_2012_2019"]] #*#

temp_inc_tn_rev[,ncol(temp_inc_tn_rev)] <- NULL

temp_inc_tn_rev$in_other <- with(temp_inc_tn_rev, in_sept + in_atmo) #*#

inc_tn <- temp_inc_tn_rev %>% 
  select(c("comid", "in_poin", "in_urb", "in_ag", "in_other")) #*#

temp_inc_tp <- sparrow_cons_out_tp %>% select(c("comid","ip_poin":"ip_rock")) #*#
temp_inc_tp$ip_ag <- with(temp_inc_tp, ip_fert+ip_manu) #*#
temp_inc_tp$ip_other <- with(temp_inc_tp, ip_rock) #*#

inc_tp <- temp_inc_tp %>% 
  select(c("comid", "ip_poin", "ip_urb", "ip_ag", "ip_other")) #*#

### Adjust 2012 SPARROW loadings for WWTPs based on percentage decreases to represent present conditions
## This information is only available for TN
wwtp_rem <- fread(paste(InPath,"WWTP_COMIDs_BslnRemoval.csv",sep="")) #*#

inc_tn_rev <- merge(
  inc_tn, 
  wwtp_rem[, c("comid","Rem_2014_2018_load_ch")], 
  by = "comid", 
  all.x = TRUE
) #*#

inc_tn_rev[is.na(inc_tn_rev)] <- 0

inc_tn_rev[c("in_poin")] <-  
  inc_tn_rev[c("in_poin")] -
  inc_tn_rev[c("in_poin")] * inc_tn_rev[["Rem_2014_2018_load_ch"]] #*#

inc_tn_rev[,ncol(inc_tn_rev)] <- NULL

### Specify urban and agricultural areas, change units from km2 to acres

# Select urban area and incremental area from SPARROW input data
temp_sparrow_area <- sparrow_in %>% select(c("comid","IncAreaKm2","urban_km2"))

# Select percentage of incremental area that is cropland
streamcat_crop <- cropland %>% select(c("comid","PctCrop2011Cat")) #*#

km2_to_ac <- 1/247.105

temp_sparrow_area$urban_ac <- temp_sparrow_area$urban_km2/km2_to_ac #*#
temp_sparrow_area$inc_ac <- temp_sparrow_area$IncAreaKm2/km2_to_ac #*#

temp_area <- merge(temp_sparrow_area, streamcat_crop,by=c("comid"),all.y=TRUE) # limits dataframe to the COMIDs available in state-specific streamcat datasets
temp_area$ag_ac <- with(temp_area,(PctCrop2011Cat/100)*inc_ac) #*#

area <- temp_area %>% select(c("comid","urban_ac","ag_ac"))
area <- distinct(area) # Removes duplicates

### Specify the length of streambank already buffered
RiparianBuffer_BMPs <- with(
  user_specs_BMPs, BMP[which(BMP_Category == "ripbuf"& BMP_Selection == "X")]
) 

RiparianBuffer_Widths <- with(
  user_specs_BMPs, UserSpec_RD_in[
    which(BMP_Category == "ripbuf"& BMP_Selection == "X")
  ]
) 

UserSpecs_bufferwidth_nearest <- custom.round(
  x = RiparianBuffer_Widths, breaks = c(20, 40, 60, 80, 100)
)

if(length(RiparianBuffer_BMPs) > 0) {
  riparian_buffer_maximp <- foreach(
    i = 1:length(RiparianBuffer_BMPs), .combine = "merge"
  ) %do% {
    RiparianBuffer_BMPs_tmp <- RiparianBuffer_BMPs[i]
    
    riparian.existingbuffer %>%
      select(
        comid, 
        totalbanklength_ft, 
        contains(as.character(UserSpecs_bufferwidth_nearest[i]))
      ) %>%
      rename_with(
        .fn = ~gsub(
          paste0("_", UserSpecs_bufferwidth_nearest[i], "ft_ft"), '', .
        ), 
        .col = contains(as.character(UserSpecs_bufferwidth_nearest[i]))
      ) %>%
      mutate(
        maximp = case_when(
          RiparianBuffer_BMPs_tmp == "Forested_Buffer" ~ 
            totalbanklength_ft - Forest_buffer,
          RiparianBuffer_BMPs_tmp == "Grassed_Buffer" ~ 
            totalbanklength_ft - Forest_buffer - Grass_buffer
        )
      ) %>%
      mutate(maximp = case_when(maximp < 0 ~ 0, maximp >= 0 ~ maximp)) %>%
      select(comid, totalbanklength_ft, maximp) %>%
      rename_with(.fn = ~paste(RiparianBuffer_BMPs_tmp), .cols = "maximp")
  }
}

### Calculate runoff coefficient for urban area

# Specify column for impervious dataset
temp_runoffcoeff <- imperv %>% select(c("comid","PctImp2011Cat")) #*#
temp_runoffcoeff$runoffcoeff <- with(
  temp_runoffcoeff, 0.05 + 0.009 * PctImp2011Cat
) #*#

runoffcoeff <- temp_runoffcoeff %>% select(c("comid", "runoffcoeff"))
runoffcoeff <- distinct(runoffcoeff) # Removes duplicates


### Remove temporary datasets
rm(list = ls(pattern = "^temp"))

# -------------Subset data to conceptual model flowpaths-------------

### Specify pour point
# Note: Upper Connecticut River Basin defined by HUC6 = 010801, which ends at MA-VT-NH border: https://water.usgs.gov/lookup/getwatershed?01080101
# Note: The pour point (pore_pt) of this basin is comid = 9332552

# Specify target watershed
watershed_tn <- with(
  user_specs_loadingtargets %>% filter(TN_or_TP == "TN"), 
  str_pad(
    string = as.character(Watershed_HUC), 
    width = nchar(Watershed_HUC) + nchar(Watershed_HUC) %% 2, 
    side = "left", 
    pad = "0"
  )
)#*#

watershed_tp <- with(
  user_specs_loadingtargets %>% filter(TN_or_TP == "TP"), 
  str_pad(
    string = as.character(Watershed_HUC), 
    width = nchar(Watershed_HUC) + nchar(Watershed_HUC) %% 2, 
    side = "left", 
    pad = "0"
  )
)#*#

# Specify comids of loading targets
pore_pt_tn <- with(
  user_specs_loadingtargets %>% filter(TN_or_TP == "TN"),
  as.character(ComID)
) #*#

pore_pt_tp <- with(
  user_specs_loadingtargets %>% filter(TN_or_TP == "TP"),
  as.character(ComID)
) #*#

# Get list of upstream.comids
# get_upstream_ComIDs source code is in Optimization_HelperFunctions.R

outofnetworkflags_tn <- with(
  user_specs_loadingtargets %>% filter(TN_or_TP == "TN"), 
  OutofNetworkFlag_X
)

upstream.comids_tn <- foreach(
  i = min(
    c(1, length(outofnetworkflags_tn))
  ):length(outofnetworkflags_tn)
) %do% {
  if(i > 0) {
    if(
      !is.na(outofnetworkflags_tn[i]) & outofnetworkflags_tn[i] == "X"
    ) {
      
      upstream.comids_tmp_tn <- c(pore_pt_tn[i])
      
    } else {
      
      upstream.comids_tmp_tn <- get_upstream_ComIDs(
        sparrow_in = sparrow_in, target.comid = pore_pt_tn[i]
      )
      
    }
    upstream.comids_tmp_tn
  } 
  
}

outofnetworkflags_tp <- with(
  user_specs_loadingtargets %>% filter(TN_or_TP == "TP"), 
  OutofNetworkFlag_X
)

upstream.comids_tp <- foreach(
  i = min(
    c(1, length(outofnetworkflags_tp))
  ):length(outofnetworkflags_tp)
) %do% {
  if(i > 0) {
    if(
      !is.na(outofnetworkflags_tp[i]) & outofnetworkflags_tp[i] == "X"
    ) {
      
      upstream.comids_tmp_tp <- c(pore_pt_tp[i])
      
    } else {
      
      upstream.comids_tmp_tp <- get_upstream_ComIDs(
        sparrow_in = sparrow_in, target.comid = pore_pt_tp[i]
      )
      
    }
    upstream.comids_tmp_tp
  } 
  
}


# Limit dataframe to the COMIDs within the specified watershed
reaches_all <- sparrow_in %>% 
  filter(
    comid %in% c(
      unlist(upstream.comids_tn, use.names = FALSE), 
      unlist(upstream.comids_tp, use.names = FALSE)
    )
  ) %>%
  mutate(HUC_6 = left(HUC_12_Rev, 6))

### Message regarding SPARROW/StreamCat mismatch

streamcat_subset_all <- as.data.frame(
  runoffcoeff[
    runoffcoeff$comid %in% 
      unique(unlist(c(upstream.comids_tn, upstream.comids_tp))), 
  ]
) %>%
  select(comid) %>%
  arrange(comid)

streamcat_subset_tn <- foreach(i = 1:length(upstream.comids_tn)) %do% {
  
  streamcat_subset_tn_tmp <- as.data.frame(
    runoffcoeff[runoffcoeff$comid %in% upstream.comids_tn[[i]], ]
  ) %>%
    select(comid) %>%
    arrange(comid)
  
  streamcat_subset_tn_tmp
  
}

streamcat_subset_tp <- foreach(i = 1:length(upstream.comids_tp)) %do% {
  
  streamcat_subset_tp_tmp <- as.data.frame(
    runoffcoeff[runoffcoeff$comid %in% upstream.comids_tp[[i]], ]
  ) %>%
    select(comid) %>%
    arrange(comid)
  
  streamcat_subset_tp_tmp
  
}

Message.tn <- foreach(i = 1:length(upstream.comids_tn)) %do% {
  if(
    length(unique(streamcat_subset_tn[[i]]$comid)) > 
    length(unique(upstream.comids_tn[[i]]))
  ) {
    Message.tmp <- paste(
      "Note: There are more reaches in the Vermont and New Hampshire StreamCat",
      "datasets than are included in SPARROW.", 
      "Only the reaches that are included in both datasets will be available",
      "for BMP optimization.Loads the remaining reaches will be included in", 
      "the 'other_loads' parameter.", 
      sep = "\n"
    )
  } else if(
    length(unique(streamcat_subset_tn[[i]]$comid)) < 
    length(unique(upstream.comids_tn[[i]]))
  ) {
    Message.tmp <- paste(
      "Note: There are fewer reaches in the Vermont and New Hampshire",
      "StreamCat datasets than are included in SPARROW.", 
      "Only the reaches that are included in both datasets will be available",
      "for BMP optimization. Loads the remaining reaches will be included in",
      "the 'other_loads' parameter.", 
      sep = "\n"
    )
  } else {
    Message.tmp <- paste(
      "Note: There are the same number of reaches in SPARROW and Streamcat",
      "subsetted datasets.", 
      "All reaches available for BMP optimization", 
      sep = "\n"
    )
  }
  
  Message.tmp
}

invisible(lapply(Message.tn, cat, sep = "\n"))

Message.tp <- foreach(i = 1:length(upstream.comids_tp)) %do% {
  if(
    length(unique(streamcat_subset_tp[[i]]$comid)) > 
    length(unique(upstream.comids_tp[[i]]))
  ) {
    Message.tmp <- paste(
      "Note: There are more reaches in the Vermont and New Hampshire StreamCat",
      "datasets than are included in SPARROW.", 
      "Only the reaches that are included in both datasets will be available",
      "for BMP optimization.Loads the remaining reaches will be included in",
      "the 'other_loads' parameter.", 
      sep = "\n"
    )
  } else if(
    length(unique(streamcat_subset_tp[[i]]$comid)) < 
    length(unique(upstream.comids_tp[[i]]))
  ) {
    Message.tmp <- paste(
      "Note: There are fewer reaches in the Vermont and New Hampshire",
      "StreamCat datasets than are included in SPARROW.", 
      "Only the reaches that are included in both datasets will be available",
      "for BMP optimization. Loads the remaining reaches will be included in",
      "the 'other_loads' parameter.", 
      sep = "\n"
    )
  } else {
    Message.tmp <- paste(
      "Note: There are the same number of reaches in SPARROW and Streamcat",
      "subsetted datasets.", 
      "All reaches available for BMP optimization", 
      sep = "\n"
    )
  }
  
  Message.tmp
}

invisible(lapply(Message.tp, cat, sep = "\n"))

### Determine Riparian Loadings

riparian.loadings_tn <- foreach(i = 1:length(upstream.comids_tn)) %do% {
  riparian.loadings %>% 
    filter(comid %in% upstream.comids_tn[[i]]) %>%
    select(comid, N_riparian_kgyr) %>%
    right_join(
      ., 
      sparrow_cons_out_tn %>% 
        filter(comid %in% upstream.comids_tn[[i]]) %>% 
        select(comid, in.), 
      by = "comid"
    ) %>%
    mutate(
      N_riparian_kgyr = case_when(
        N_riparian_kgyr > in. ~ in., 
        is.na(N_riparian_kgyr) ~ 0, 
        TRUE ~ N_riparian_kgyr
      )
    ) %>%
    select(-in.)
}

riparian.loadings_tp <- foreach(i = 1:length(upstream.comids_tp)) %do% {
  riparian.loadings %>% 
    filter(comid %in% upstream.comids_tp[[i]]) %>%
    select(comid, P_riparian_kgyr) %>%
    right_join(
      ., 
      sparrow_cons_out_tp %>% 
        filter(comid %in% upstream.comids_tp[[i]]) %>% 
        select(comid, ip), 
      by = "comid"
    ) %>%
    mutate(
      P_riparian_kgyr = case_when(
        P_riparian_kgyr > ip ~ ip, 
        is.na(P_riparian_kgyr) ~ 0, 
        TRUE ~ P_riparian_kgyr
      )
    )  %>%
    select(-ip)
}

### Adjust delivery fraction to pore point

max_delfrac_tn <- unlist(
  foreach(i = 1:length(pore_pt_tn)) %do% {
    
    sparrow_cons_out_tn$DEL_FRAC[sparrow_cons_out_tn$comid == pore_pt_tn[i]]
    
  }
)

temp_delfrac_rev_tn <- foreach(i = 1:length(upstream.comids_tn)) %do% {
  
  sparrow_cons_out_tn %>% 
    filter(comid %in% upstream.comids_tn[[i]])  %>% 
    select(c("comid", "DEL_FRAC"))
  
}

invisible(
  foreach(i = 1:length(temp_delfrac_rev_tn)) %do% {
    
    temp_delfrac_rev_tn[[i]]$delfrac_rev <- with(
      temp_delfrac_rev_tn[[i]], DEL_FRAC / max_delfrac_tn[i]
    )
    
  }
)

delfrac_rev_tn <- foreach(i = 1:length(temp_delfrac_rev_tn)) %do% {
  
  temp_delfrac_rev_tn[[i]] %>% select(c("comid", "delfrac_rev"))
  
}

max_delfrac_tp <- unlist(
  foreach(i = 1:length(pore_pt_tp)) %do% {
    
    sparrow_cons_out_tp$DEL_FRAC[sparrow_cons_out_tp$comid == pore_pt_tp[i]]
    
  }
)

temp_delfrac_rev_tp <- foreach(i = 1:length(upstream.comids_tp)) %do% {
  
  sparrow_cons_out_tp %>% filter(comid %in% upstream.comids_tp[[i]])  %>% 
    select(c("comid", "DEL_FRAC"))
  
}

invisible(
  foreach(i = 1:length(temp_delfrac_rev_tp)) %do% {
    
    temp_delfrac_rev_tp[[i]]$delfrac_rev <- with(
      temp_delfrac_rev_tp[[i]], DEL_FRAC / max_delfrac_tp[i]
    )
    
  }
)

delfrac_rev_tp <- foreach(i = 1:length(temp_delfrac_rev_tp)) %do% {
  
  temp_delfrac_rev_tp[[i]] %>% select(c("comid", "delfrac_rev"))
  
}


# -------------Define costs-------------

ft2_to_ac <- 43560
yd2_to_ac <- 4840

temp_bmp_costs <- user_specs_BMPs[
  user_specs_BMPs$BMP_Selection=="X",
  c(
    "BMP_Category",
    "BMP",
    "capital_VT",
    "capital_NH",
    "operations_VT",
    "operations_NH", 
    "capital_units",
    "operations_units",
    "UserSpec_RD_in"
  )
] 

# Convert area-specific costs to costs per acre for ag BMPs and to costs per linear foot for Riparian BMPs

temp_bmp_costs_rev <- temp_bmp_costs %>%
  mutate(
    capital_VT_rev = case_when(
      BMP_Category == "ag" ~ case_when(
        capital_units == "ft2" ~ capital_VT * ft2_to_ac,
        capital_units == "km2" ~ capital_VT * km2_to_ac,
        capital_units == "yd2" ~ capital_VT * yd2_to_ac,
        capital_units == "ac" ~ capital_VT
      ),
      BMP_Category == "urban" ~ capital_VT,
      BMP_Category == "point" ~ capital_VT,
      BMP_Category == "ripbuf" ~ case_when(
        capital_units == "ft2" ~ capital_VT * UserSpec_RD_in,
        capital_units == "km2" ~ capital_VT * 
          km2_to_ac / ft2_to_ac * 
          UserSpec_RD_in,
        capital_units == "yd2" ~ capital_VT * 
          yd2_to_ac / ft2_to_ac * 
          UserSpec_RD_in,
        capital_units == "ac" ~ capital_VT / ft2_to_ac * UserSpec_RD_in
      )
    ), 
    capital_NH_rev = case_when(
      BMP_Category == "ag" ~ case_when(
        capital_units == "ft2" ~ capital_NH * ft2_to_ac,
        capital_units == "km2" ~ capital_NH * km2_to_ac,
        capital_units == "yd2" ~ capital_NH * yd2_to_ac,
        capital_units == "ac" ~ capital_NH
      ),
      BMP_Category == "urban" ~ capital_NH,
      BMP_Category == "point" ~ capital_NH,
      BMP_Category == "ripbuf" ~ case_when(
        capital_units == "ft2" ~ capital_NH * UserSpec_RD_in,
        capital_units == "km2" ~ capital_NH *
          km2_to_ac / ft2_to_ac * 
          UserSpec_RD_in,
        capital_units == "yd2" ~ capital_NH * 
          yd2_to_ac / ft2_to_ac * 
          UserSpec_RD_in,
        capital_units == "ac" ~ capital_NH / ft2_to_ac * UserSpec_RD_in
      )
    ), 
    operations_VT_rev = case_when(
      BMP_Category == "ag" ~ case_when(
        capital_units == "ft2" ~ operations_VT * ft2_to_ac,
        capital_units == "km2" ~ operations_VT * km2_to_ac,
        capital_units == "yd2" ~ operations_VT * yd2_to_ac,
        capital_units == "ac" ~ operations_VT
      ),
      BMP_Category == "urban" ~ operations_VT,
      BMP_Category == "point" ~ operations_VT,
      BMP_Category == "ripbuf" ~ case_when(
        capital_units == "ft2" ~ operations_VT * UserSpec_RD_in,
        capital_units == "km2" ~ operations_VT *
          km2_to_ac / ft2_to_ac * 
          UserSpec_RD_in,
        capital_units == "yd2" ~ operations_VT * 
          yd2_to_ac / ft2_to_ac * 
          UserSpec_RD_in,
        capital_units == "ac" ~ operations_VT / ft2_to_ac * UserSpec_RD_in
      )
    ), 
    operations_NH_rev = case_when(
      BMP_Category == "ag" ~ case_when(
        capital_units == "ft2" ~ operations_NH * ft2_to_ac,
        capital_units == "km2" ~ operations_NH * km2_to_ac,
        capital_units == "yd2" ~ operations_NH * yd2_to_ac,
        capital_units == "ac" ~ operations_NH
      ),
      BMP_Category == "urban" ~ operations_NH,
      BMP_Category == "point" ~ operations_NH,
      BMP_Category == "ripbuf" ~ case_when(
        capital_units == "ft2" ~ operations_NH * UserSpec_RD_in,
        capital_units == "km2" ~ operations_NH * 
          km2_to_ac / ft2_to_ac * 
          UserSpec_RD_in,
        capital_units == "yd2" ~ operations_NH * 
          yd2_to_ac / ft2_to_ac * 
          UserSpec_RD_in,
        capital_units == "ac" ~ operations_NH / ft2_to_ac * UserSpec_RD_in
      )
    )
  )

bmp_costs <- temp_bmp_costs_rev %>% 
  select(
    c(
      "BMP_Category",
      "BMP",
      "capital_VT_rev",
      "capital_NH_rev",
      "operations_VT_rev",
      "operations_NH_rev"
    )
  )
names(bmp_costs) <- c(
  "category","bmp","capital_VT","capital_NH","operations_VT","operations_NH"
)

# Annualize capital costs based on planning horizon and interest rate
bmp_costs[c(3:4)] <- bmp_costs[c(3:4)] *
  (
    interest_rate * ((1 + interest_rate) ^ horizon) / 
      (((1 + interest_rate) ^ horizon) - 1)
  )

bmp_costs[is.na(bmp_costs)] <- 0

### Specify point source costs as either capital or operations, depending on WWTP state

point_comid <- fread(paste(InPath, "WWTP_COMIDs.csv", sep="")) #*#
# Note to user: Can update this file if more WWTPs desired for analysis; must also update UserSpecs_BMPs.csv with costs
names(point_comid) <- c("State", "bmp", "NPDES_ID", "comid")

temp_bmp_costs_point <- merge(
  bmp_costs[bmp_costs$category =="point",], 
  point_comid,
  by = "bmp",
  all.x = TRUE
)
temp_bmp_costs_point$capital <- with(
  temp_bmp_costs_point, ifelse(State == "NH", capital_NH, capital_VT)
) #*#
temp_bmp_costs_point$operations <- with(
  temp_bmp_costs_point, ifelse(State == "NH", operations_NH, operations_VT)
) #*#
bmp_costs_point <- temp_bmp_costs_point %>% 
  select(c("category", "comid", "capital", "operations", "State"))

### Define the fraction of agricultural costs that reflect base payment versus actual agricultural BMP costs
agcost_frac <- 100/75

### Remove temporary datasets
rm(list = ls(pattern = "^temp"))

# ------------- Define urban cost correction coefficients
# These are based on the type of urban land. The costs given in UserSpecs are for new development. 
# Retrofitting is multiplied by a factor of 2 and difficult retrofit by a factor of 3.
# Open and low density development we assume will be new development, med density will be retrofits, and high density will be difficult retrofits.

urban_cost_coeffs <- cropland %>%
  mutate(
    total_urban = PctUrbOp2011Cat + 
      PctUrbLo2011Cat + 
      PctUrbMd2011Cat + 
      PctUrbHi2011Cat,
    urban_cost_coef = case_when(
      total_urban == 0 ~ 1,
      total_urban > 0 ~ 
        ((PctUrbOp2011Cat + PctUrbLo2011Cat) / total_urban) * 1 +
        (PctUrbMd2011Cat / total_urban) * 2 +
        (PctUrbHi2011Cat / total_urban) * 3
    )
  ) %>%
  select(comid, urban_cost_coef)



# -------------Define efficiencies-------------

### Agricultural BMPs

# Read in efficiency data for Fert_20 and Manure_Injection BMPs
temp_ag_effic_fert_man <- fread(
  paste(InPath, "AgBMPEffic_FertManure.csv", sep = "")
) #*#
temp_ag_effic_fert_man_cast_tn <- reshape2::dcast(
  temp_ag_effic_fert_man, Category ~ BMP, value.var = "N_Efficiency"
) #*#
temp_ag_effic_fert_man_cast_tp <- reshape2::dcast(
  temp_ag_effic_fert_man, Category ~ BMP, value.var = "P_Efficiency"
) #*#

# Read in efficiency data for ACRE database BMPs
temp_acre <- fread(paste0(InPath, "ACRE_HUC12_HRU_Summary.csv")) #*#
temp_acre$bmp <- with(
  temp_acre,
  ifelse(
    Scenario=="CONSERVATION",
    "Conservation",
    ifelse(
      Scenario=="Contour Farming",
      "Contour_Farming",
      ifelse(
        Scenario=="Terraces and Waterway",
        "Terrace_Waterway",
        ifelse(
          Scenario=="Terraces Only",
          "Terrace_Only",
          ifelse(
            Scenario=="Waterway Only", "Waterway_Only", as.character(Scenario)
          )
        )
      )
    )
  )
) #*#


temp_acre$Scenario <- NULL
temp_acre$HUC12_Rev <- str_pad(temp_acre$HUC12, width=12, pad="0")
temp_acre$HUC10_Rev <- str_pad(temp_acre$HUC10, width=10, pad="0")
temp_acre$HUC8_Rev <- str_pad(temp_acre$HUC8, width=8, pad="0")

temp_acre_cast_tn <- reshape2::dcast(
  temp_acre, HUC8_Rev+HUC10_Rev+HUC12_Rev ~ bmp, value.var = "MeanTN_Effic"
) %>% 
  select(-Baseline) %>%
  rename(HUC8 = HUC8_Rev, HUC10 = HUC10_Rev, HUC12 = HUC12_Rev)

temp_acre_cast_tn_HUC8 <- temp_acre_cast_tn %>%
  filter(is.na(HUC12) & is.na(HUC10))

temp_acre_cast_tn_HUC10 <- temp_acre_cast_tn %>%
  filter(is.na(HUC12) & is.na(HUC8))

temp_acre_cast_tn_HUC12 <- temp_acre_cast_tn %>%
  filter(!is.na(HUC12), !is.na(HUC10), !is.na(HUC8))


ACRE_BMPs <- names(temp_acre_cast_tn[, -c(1:3)])

reaches_huc12_tn <- sparrow_in %>% 
  filter(comid %in% unlist(upstream.comids_tn, use.names = FALSE)) %>% 
  select(comid, HUC12 = HUC_12_Rev)  %>%
  mutate(HUC10 = left(HUC12, 10), HUC8 = left(HUC12, 8))

temp_acre_reaches_HUC12_tn <- merge(
  reaches_huc12_tn[, c("comid", "HUC12")],
  temp_acre_cast_tn_HUC12[, c("HUC12", ACRE_BMPs)],
  by = "HUC12",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC12")))

temp_acre_reaches_HUC10_tn <- merge(
  reaches_huc12_tn[, c("comid", "HUC10")],
  temp_acre_cast_tn_HUC10[, c("HUC10", ACRE_BMPs)],
  by = "HUC10",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC10")))

temp_acre_reaches_HUC8_tn <- merge(
  reaches_huc12_tn[, c("comid", "HUC8")],
  temp_acre_cast_tn_HUC8[, c("HUC8", ACRE_BMPs)],
  by = "HUC8",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC8")))

temp_acre_reaches_tn <- merge(
  merge(
    merge(
      reaches_huc12_tn, 
      temp_acre_reaches_HUC12_tn, 
      by = c("comid", "HUC12"),
      all.x = TRUE
    ),
    temp_acre_reaches_HUC10_tn,
    by = c("comid", "HUC10"),
    all.x = TRUE
  ),
  temp_acre_reaches_HUC8_tn,
  by = c("comid", "HUC8"),
  all.x = TRUE
)
temp_acre_reaches_tn[ , ACRE_BMPs] <- NA

acre_reaches_tn <- temp_acre_reaches_tn %>%
  mutate(
    across(
      all_of(ACRE_BMPs), 
      ~ case_when(
        !is.na(temp_acre_reaches_tn[[paste0(cur_column(), "_HUC12")]]) ~ 
          temp_acre_reaches_tn[[paste0(cur_column(), "_HUC12")]],
        !is.na(temp_acre_reaches_tn[[paste0(cur_column(), "_HUC10")]]) ~ 
          temp_acre_reaches_tn[[paste0(cur_column(), "_HUC10")]],
        !is.na(temp_acre_reaches_tn[[paste0(cur_column(), "_HUC8")]]) ~ 
          temp_acre_reaches_tn[[paste0(cur_column(), "_HUC8")]]
      )
    )
  ) %>%
  select(comid, all_of(ACRE_BMPs))

temp_acre_cast_tp <- reshape2::dcast(
  temp_acre, HUC8_Rev+HUC10_Rev+HUC12_Rev ~ bmp, value.var = "MeanTP_Effic"
) %>% 
  select(-Baseline) %>%
  rename(HUC8 = HUC8_Rev, HUC10 = HUC10_Rev, HUC12 = HUC12_Rev)

temp_acre_cast_tp_HUC8 <- temp_acre_cast_tp %>%
  filter(is.na(HUC12) & is.na(HUC10))

temp_acre_cast_tp_HUC10 <- temp_acre_cast_tp %>%
  filter(is.na(HUC12) & is.na(HUC8))

temp_acre_cast_tp_HUC12 <- temp_acre_cast_tp %>%
  filter(!is.na(HUC12), !is.na(HUC10), !is.na(HUC8))

reaches_huc12_tp <- sparrow_in %>% 
  filter(comid %in% unlist(upstream.comids_tp, use.names = FALSE)) %>% 
  select(comid, HUC12 = HUC_12_Rev)  %>%
  mutate(HUC10 = left(HUC12, 10), HUC8 = left(HUC12, 8))

temp_acre_reaches_HUC12_tp <- merge(
  reaches_huc12_tp[, c("comid", "HUC12")],
  temp_acre_cast_tp_HUC12[, c("HUC12", ACRE_BMPs)],
  by = "HUC12",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC12")))

temp_acre_reaches_HUC10_tp <- merge(
  reaches_huc12_tp[, c("comid", "HUC10")],
  temp_acre_cast_tp_HUC10[, c("HUC10", ACRE_BMPs)],
  by = "HUC10",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC10")))

temp_acre_reaches_HUC8_tp <- merge(
  reaches_huc12_tp[, c("comid", "HUC8")],
  temp_acre_cast_tp_HUC8[, c("HUC8", ACRE_BMPs)],
  by = "HUC8",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC8")))

temp_acre_reaches_tp <- merge(
  merge(
    merge(
      reaches_huc12_tp, 
      temp_acre_reaches_HUC12_tp, 
      by = c("comid", "HUC12"),
      all.x = TRUE
    ),
    temp_acre_reaches_HUC10_tp,
    by = c("comid", "HUC10"),
    all.x = TRUE
  ),
  temp_acre_reaches_HUC8_tp,
  by = c("comid", "HUC8"),
  all.x = TRUE
)
temp_acre_reaches_tp[ , ACRE_BMPs] <- NA

acre_reaches_tp <- temp_acre_reaches_tp %>%
  mutate(
    across(
      all_of(ACRE_BMPs), 
      ~ case_when(
        !is.na(temp_acre_reaches_tp[[paste0(cur_column(), "_HUC12")]]) ~ 
          temp_acre_reaches_tp[[paste0(cur_column(), "_HUC12")]],
        !is.na(temp_acre_reaches_tp[[paste0(cur_column(), "_HUC10")]]) ~ 
          temp_acre_reaches_tp[[paste0(cur_column(), "_HUC10")]],
        !is.na(temp_acre_reaches_tp[[paste0(cur_column(), "_HUC8")]]) ~ 
          temp_acre_reaches_tp[[paste0(cur_column(), "_HUC8")]]
      )
    )
  ) %>%
  select(comid, all_of(ACRE_BMPs))

# Combine ACRE bmps with Manure_Injection and Fert_20 BMP-specific efficiencies
temp_Ag_BMPs <- user_specs_BMPs[
  user_specs_BMPs$BMP_Selection == "X" & (user_specs_BMPs$BMP_Category == "ag"),
]
Ag_BMPs <- paste0(temp_Ag_BMPs$BMP)

ag_effic_bycomid_tn <- add_column(
  acre_reaches_tn, temp_ag_effic_fert_man_cast_tn[-c(1)]
) %>% 
  select(comid, all_of(Ag_BMPs)) %>%
  mutate(across(all_of(Ag_BMPs), ~ replace_na(., -999))) # By setting unknowns to -999, the model will not implement BMPs that have missing data.

ag_effic_bycomid_tp <- add_column(
  acre_reaches_tp, temp_ag_effic_fert_man_cast_tp[-c(1)]
) %>% 
  select(comid, all_of(Ag_BMPs)) %>%
  mutate(across(all_of(Ag_BMPs), ~ replace_na(., -999))) # By setting unknowns to -999, the model will not implement BMPs that have missing data.


### Point Source BMPs

temp_point_effic <- fread(
  paste(InPath, "WWTP_RemovalEffic.csv", sep = ""), 
  col.names = c("BMP_Category", "bmp", "N_Efficiency", "P_Efficiency")
) #*#

point_effic <- merge(
  temp_point_effic[
    temp_point_effic$bmp %in% 
      user_specs_BMPs$BMP[user_specs_BMPs$BMP_Selection=="X"],
  ],
  point_comid,
  by = c("bmp"),
  all.y = TRUE
) %>%
  mutate(across(contains("Efficiency"), ~ replace_na(., -999))) # By setting unknowns to -999, the model will not implement BMPs that have missing data

point_effic_bycomid_tn <- point_effic %>% 
  select(category = BMP_Category, comid, effic = N_Efficiency)

point_effic_bycomid_tp <- point_effic %>% 
  select(category = BMP_Category, comid, effic = P_Efficiency)


### Urban BMPs
temp_Urban_BMPs <- user_specs_BMPs[
  user_specs_BMPs$BMP_Selection == "X" & 
    (user_specs_BMPs$BMP_Category == "urban"),
]
Urban_BMPs <- paste0(temp_Urban_BMPs$BMP)

urban.effic.curves <- fread(
  paste0(InPath, "/UrbanBMPPerformanceCurves.csv"), data.table = FALSE
)

comid.infiltrationrates <- fread(
  paste0(InPath, "/NHD+infiltrationrates.csv")
)

comid.infiltrationrates.matched <-comid.infiltrationrates %>%
  rename(InfiltrationRate_inperhr = infiltrationrate_inperhr) %>%
  mutate(
    InfiltrationRate_inperhr = custom.round(
      x = InfiltrationRate_inperhr, 
      breaks = c(0, 0.17, 0.27, 0.52, 1.02, 2.41, 8.27)  # NH BMPs calculated at 0.17 in/hr, 0.27 in/hr, 0.52 in/hr, 1.02 in/hr, 2.41 in/hr and 8.27 in/hr
    )
  )

temp_urban_effic <-  user_specs_BMPs[
  user_specs_BMPs$BMP_Category == "urban" & 
    user_specs_BMPs$BMP_Selection == "X",
  c("BMP_Category", "BMP", "Min_RD_in", "Max_RD_in", "UserSpec_RD_in")
] 

urban.BMPs.list <- temp_urban_effic

if(length(Urban_BMPs) > 0) {
  urban_effic.n <- merge(
    temp_urban_effic, 
    urban.effic.curves %>% filter(Pollutant == "N"), 
    by = "BMP"
  ) %>%
    mutate(
      expression = gsub(
        ' x', ' UserSpec_RD_in ', gsub('y ~ ', '', Best.Fit.Curve)
      )
    ) %>%
    rowwise() %>%
    mutate(iter = 1, effic = eval(parse(text = expression))) %>%
    filter(
      BMP %in% user_specs_BMPs$BMP[user_specs_BMPs$BMP_Selection == "X"]
      ) %>%
    select(
      category = BMP_Category, bmp = BMP, effic, InfiltrationRate_inperhr
    ) %>%
    group_by(bmp) %>%
    pivot_wider(
      id_cols = InfiltrationRate_inperhr, names_from = bmp, values_from = effic
    ) %>%
    add_row(InfiltrationRate_inperhr = 0) %>%
    mutate(
      across(
        any_of(
          c("Infiltration_Basin", 
            "Infiltration_Chamber",
            "Infiltration_Trench", 
            "Porous_Pavement_w_subsurface_infiltration")
        ), 
        ~ case_when(InfiltrationRate_inperhr == 0 ~ -999, TRUE ~ .) # Infiltration BMPs should not be used in subcatchments with very low infiltration rates (predominately HSG D). Not only is this unreasonable theoretically, we also do not have an efficiency curve to match these low infiltration rates. The -999 will force the model not to implement these BMPs in areas that have low infiltration rates. There is an additional check, that the max implementation of these BMPs in these comids will be set to 0. 
      )
    ) %>%
    fill(all_of(Urban_BMPs))
  
  
  urban_effic_bycomid_tn <- merge(
    merge(
      comid.infiltrationrates.matched  %>% select(-V1), 
      data.frame(comid = unlist(streamcat_subset_tn, use.names = FALSE)), 
      by = 'comid',
      all = TRUE
    ), 
    urban_effic.n, 
    by = "InfiltrationRate_inperhr",
    all = TRUE
  ) %>%
    select(-InfiltrationRate_inperhr)
  
  
  urban_effic.p <- merge(
    temp_urban_effic, 
    urban.effic.curves %>% filter(Pollutant == "P"),
    by = "BMP"
  ) %>%
    mutate(
      expression = gsub(
        ' x', ' UserSpec_RD_in ', gsub('y ~ ', '', Best.Fit.Curve)
      )
    ) %>%
    rowwise() %>%
    mutate(iter = 1, effic = eval(parse(text = expression))) %>%
    filter(
      BMP %in% user_specs_BMPs$BMP[user_specs_BMPs$BMP_Selection == "X"]
      ) %>%
    select(
      category = BMP_Category, bmp = BMP, effic, InfiltrationRate_inperhr
    ) %>%
    pivot_wider(
      id_cols = InfiltrationRate_inperhr, names_from = bmp, values_from = effic
    ) %>%
    add_row(InfiltrationRate_inperhr = 0) %>%
    mutate(
      across(
        any_of(
          c("Infiltration_Basin", 
            "Infiltration_Chamber",
            "Infiltration_Trench", 
            "Porous_Pavement_w_subsurface_infiltration")
        ), 
        ~ case_when(InfiltrationRate_inperhr == 0 ~ -999, TRUE ~ .) # Infiltration BMPs should not be used in subcatchments with very low infiltration rates (predominately HSG D). Not only is this unreasonable theoretically, we also do not have an efficiency curve to match these low infiltration rates. The -999 will force the model not to implement these BMPs in areas that have low infiltration rates. There is an additional check, that the max implementation of these BMPs in these comids will be set to 0. 
      )
    ) %>%
    fill(all_of(Urban_BMPs))
  
  
  urban_effic_bycomid_tp <- merge(
    merge(
      comid.infiltrationrates.matched  %>% select(-V1), 
      data.frame(comid = unlist(streamcat_subset_tp, use.names = FALSE)), 
      by = 'comid',
      all = TRUE
    ), 
    urban_effic.p, 
    by = "InfiltrationRate_inperhr",
    all = TRUE
  ) %>%
    select(-InfiltrationRate_inperhr)
}

### Remove temporary datasets
rm(list = ls(pattern = "^temp"))

#--------------Riparian Buffer Removals--------------

if(length(RiparianBuffer_BMPs) > 0) {
  riparian_buffer_efficiencies_N <- foreach(
    i = 1: length(streamcat_subset_tn)
  ) %do% {
    riparian_buffer_efficiencies_N_tmp <- foreach(
      j = 1:length(RiparianBuffer_BMPs), .combine = "merge"
    ) %do% {
      
      short.form.bmp.name <- if(RiparianBuffer_BMPs[j] == "Grassed_Buffer") {
        "Grass"
      } else if(RiparianBuffer_BMPs[j] == "Forested_Buffer") {"Forest"}
      
      riparian.efficiencies.tmp <- riparian.efficiencies %>% 
        select(
          -contains("P"), 
          -V1, 
          -!contains(short.form.bmp.name), 
          -!contains(as.character(UserSpecs_bufferwidth_nearest[j])), 
          -contains("uncertainty"),
          comid
        ) %>%
        rename_with(.fn = ~"Curve.Form", .cols = contains("N")) %>%
        mutate(
          x = RiparianBuffer_Widths[j], 
          expression = gsub("y ~ ", '', Curve.Form)
        ) %>% rowwise() %>%
        mutate(iter = 1, effic = eval(parse(text = expression))) %>%
        select(comid, effic) %>%
        rename_with(.fn = ~paste(RiparianBuffer_BMPs[j]), .cols = "effic")
    }
    
    riparian_buffer_efficiencies_N_tmp %>%
      filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
      mutate(
        across(
          .cols = any_of(RiparianBuffer_BMPs), 
          .fn = ~case_when(is.na(.) ~ -999, !is.na(.) ~ .))
        
      )
  }
}

if(length(RiparianBuffer_BMPs) > 0) {
  riparian_buffer_efficiencies_P <- foreach(
    i = 1: length(streamcat_subset_tp)
  ) %do% {
    riparian_buffer_efficiencies_P_tmp <- foreach(
      j = 1:length(RiparianBuffer_BMPs), .combine = "merge"
    ) %do% {
      
      short.form.bmp.name <- if(RiparianBuffer_BMPs[j] == "Grassed_Buffer") {
        "Grass"
      } else if(RiparianBuffer_BMPs[j] == "Forested_Buffer") {"Forest"}
      
      riparian.efficiencies.tmp <- riparian.efficiencies %>% 
        select(
          -contains("N"), 
          -V1, 
          -!contains(short.form.bmp.name), 
          -!contains(as.character(UserSpecs_bufferwidth_nearest[j])), 
          -contains("uncertainty"),
          comid
        ) %>%
        rename_with(.fn = ~"Curve.Form", .cols = contains("P")) %>%
        mutate(
          x = RiparianBuffer_Widths[j], 
          expression = gsub("y ~ ", '', Curve.Form)
        ) %>% rowwise() %>%
        mutate(iter = 1, effic = eval(parse(text = expression))) %>%
        select(comid, effic) %>%
        rename_with(.fn = ~paste(RiparianBuffer_BMPs[j]), .cols = "effic")
    }
    
    riparian_buffer_efficiencies_P_tmp %>%
      filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
      mutate(
        across(
          .cols = any_of(RiparianBuffer_BMPs), 
          .fn = ~case_when(is.na(.) ~ -999, !is.na(.) ~ .))
      )
  }
  
  riparian_buffer_removal_N <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
    left_join(
      riparian.loadings_tn[[i]], riparian.existingbuffer, by = "comid"
    ) %>%
      select(comid, N_riparian_kgyr, totalbanklength_ft) %>%
      mutate(
        loading_per_bankft_kg_ftyr = N_riparian_kgyr / totalbanklength_ft
      ) %>%
      left_join(., riparian_buffer_efficiencies_N[[i]], by = "comid") %>%
      mutate(
        across(
          .cols = any_of(RiparianBuffer_BMPs), 
          .fn = ~case_when(
            . == -999 ~ -999, . > 0 ~ (. * loading_per_bankft_kg_ftyr)
          )
        )
      ) %>%
      select(comid, any_of(RiparianBuffer_BMPs))
    
  }
  
  riparian_buffer_removal_P <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
    left_join(
      riparian.loadings_tp[[i]], riparian.existingbuffer, by = "comid"
    ) %>%
      select(comid, P_riparian_kgyr, totalbanklength_ft) %>%
      mutate(
        loading_per_bankft_kg_ftyr = P_riparian_kgyr / totalbanklength_ft
      ) %>%
      left_join(., riparian_buffer_efficiencies_P[[i]], by = "comid") %>%
      mutate(
        across(
          .cols = any_of(RiparianBuffer_BMPs), 
          .fn = ~case_when(
            . == -999 ~ -999, . > 0 ~ (. * loading_per_bankft_kg_ftyr)
          )
        )
      ) %>%
      select(comid, any_of(RiparianBuffer_BMPs))
    
  }
}

#-----------Make Dummy Variables if Modules aren't used----------------------
if(length(Urban_BMPs) == 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    urban_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      data.frame(comid = unique(streamcat_subset_tn[[i]]), none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    urban_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      data.frame(comid = unique(streamcat_subset_tp[[i]]), none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  urban_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid = paste0("'", comid, "'"))
}

if(length(RiparianBuffer_BMPs) == 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    ripbuf_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      data.frame(comid = unique(streamcat_subset_tn[[i]]), none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    ripbuf_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      data.frame(comid = unique(streamcat_subset_tp[[i]]), none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  ripbuf_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid = paste0("'", comid, "'"))
}

if(length(Ag_BMPs) == 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    ag_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      data.frame(comid = unique(streamcat_subset_tn[[i]]), none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    ag_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      data.frame(comid = unique(streamcat_subset_tp[[i]]), none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  ag_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid = paste0("'", comid, "'"))
}

##############################################################################################
# PART 3: WRITE AMPL MODEL FILES
##############################################################################################


# -------------Develop command file-------------

cat(
  "#WMOST Optimization Screening Tool AMPL command file
solve;
option display_precision 10;
option presolve_warnings -1;
display solve_result_num, solve_result;
display cost.result;
display cost;
option display_1col 10000000000;
option display_width 100000000000;
option display_transpose -10000;
option omit_zero_rows 1;
option omit_zero_cols 1;
display point_dec;
option display_1col 0;
display urban_frac;
display ag_frac;
display ripbuf_length;",
  file = paste(OutPath, "STcommand.amp", sep=""),
  sep = "\n"
)


# -------------Develop data file-------------

# Write urban and agricultural BMPs as vectors
bmp_urban <- user_specs_BMPs[
  user_specs_BMPs$BMP_Selection == "X" & 
    (user_specs_BMPs$BMP_Category == "urban"),
]
bmp_urban_vec_comma <- paste0("'", bmp_urban$BMP, "'")
bmp_urban_vec_comma[-length(bmp_urban_vec_comma)] <- paste0(
  bmp_urban_vec_comma[-length(bmp_urban_vec_comma)], ','
)

bmp_urban_vec <- as.character(gsub(",", "  ", bmp_urban_vec_comma))
bmp_urban_vec_direct <- bmp_urban$BMP

pervbmp_urban_vec_comma <- paste0(
  "'", with(bmp_urban, BMP[which(grepl("Porous_Pavement", BMP))]), "'"
  )
pervbmp_urban_vec_comma[-length(pervbmp_urban_vec_comma)] <- paste0(
  pervbmp_urban_vec_comma[-length(pervbmp_urban_vec_comma)], ','
)
pervbmp_urban_vec <- as.character(gsub(",", "  ", pervbmp_urban_vec_comma))

bmp_ag <- user_specs_BMPs[
  user_specs_BMPs$BMP_Selection == "X" & (user_specs_BMPs$BMP_Category == "ag"),
]
bmp_ag_vec_comma <- paste0("'", bmp_ag$BMP, "'")
bmp_ag_vec_comma[-length(bmp_ag_vec_comma)] <- paste0(
  bmp_ag_vec_comma[-length(bmp_ag_vec_comma)], ','
)

bmp_ag_vec <- as.character(gsub(",","  ", bmp_ag_vec_comma))
bmp_ag_vec_direct <- bmp_ag$BMP

bmp_ripbuf_vec_comma <- paste0("'", RiparianBuffer_BMPs, "'")
bmp_ripbuf_vec_comma[-length(bmp_ripbuf_vec_comma)] <- paste0(
  bmp_ripbuf_vec_comma[-length(bmp_ripbuf_vec_comma)], ','
)
bmp_ripbuf_vec <- as.character(gsub(",","  ", bmp_ripbuf_vec_comma))

### Subset dataframes to reaches in our analysis

temp_area_dat <- area[area$comid %in% reaches_all$comid,]
temp_area_dat <- temp_area_dat[order(temp_area_dat$comid),]
temp_area_dat$comid_form <- paste0("'", temp_area_dat$comid, "'")
area_dat <- temp_area_dat %>% select(c("comid_form", "urban_ac", "ag_ac"))

if(length(RiparianBuffer_BMPs) > 0) {
  streambanklength_total_dat <- riparian_buffer_maximp %>% 
    full_join(., streamcat_subset_all, by = "comid") %>%
    filter(comid %in% streamcat_subset_all$comid) %>%
    arrange(comid) %>%
    mutate(
      comid_form = paste0("'", comid, "'"), 
      totalbanklength_ft = case_when(
        is.na(totalbanklength_ft) ~ 0, 
        !is.na(totalbanklength_ft) ~ totalbanklength_ft
      )
    ) %>%
    select(comid_form, totalbanklength_ft)
  
  streambanklength_available_dat <-  riparian_buffer_maximp %>% 
    full_join(., streamcat_subset_all, by = "comid") %>%
    filter(comid %in% streamcat_subset_all$comid) %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'")) %>%
    mutate(
      across(
        .cols = any_of(RiparianBuffer_BMPs), 
        .fns = ~case_when(is.na(.) ~ 0, !is.na(.) ~ .)
      )
    ) %>%
    select(comid_form, any_of(RiparianBuffer_BMPs))
}

temp_runoffcoeff_dat <- runoffcoeff[
  runoffcoeff$comid %in% streamcat_subset_all$comid,
]
temp_runoffcoeff_dat <- temp_runoffcoeff_dat[order(temp_runoffcoeff_dat$comid),]
temp_runoffcoeff_dat$comid_form <- paste0("'", temp_runoffcoeff_dat$comid, "'")
runoffcoeff_dat <- temp_runoffcoeff_dat %>% 
  select(c("comid_form", "runoffcoeff"))

# Specify NH versus VT COMIDs
# Note to user: StreamCat datasets list catchments that are on the border of two states within both states' datasets
# Because agricultural costs are in some cases state-specific, we select VT costs for such catchments
COMID_State <- distinct(
  imperv[
    imperv$comid %in% streamcat_subset_all$comid, c("comid", "StateAbbrev")
    ]
) #*#
dups <- COMID_State[duplicated(COMID_State$comid),] #*#

if(nrow(dups) > 0) {
  # Assign state "VT" to duplicate COMIDs
  dups$StateAbbrev <- "VT" #*#
  COMID_State <- COMID_State[!(COMID_State$comid %in% dups$comid),] #*#
  COMID_State <- rbind(COMID_State, dups) #*#
}
names(COMID_State) <- c("comid", "State") #*#

### Multiply all loads by revised del_frac

temp_inc_tn_dat <- foreach(i = 1:length(upstream.comids_tn)) %do% {
  
  
  temp_inc_tn_dat_tmp <- merge(
    inc_tn_rev[inc_tn_rev$comid %in% upstream.comids_tn[[i]],],
    delfrac_rev_tn[[i]],
    by = "comid",
    all.x = TRUE
  )
  
  temp_inc_tn_dat_tmp[-c(1,6)] <- temp_inc_tn_dat_tmp[-c(1, 6)] * 
    temp_inc_tn_dat_tmp[["delfrac_rev"]]
  temp_inc_tn_dat_tmp <- temp_inc_tn_dat_tmp[
    order(temp_inc_tn_dat_tmp$comid), 
  ]
  
}

temp_inc_tp_dat <- foreach(i = 1:length(upstream.comids_tp)) %do% {
  
  
  temp_inc_tp_dat_tmp <- merge(
    inc_tp[inc_tp$comid %in% upstream.comids_tp[[i]],],
    delfrac_rev_tp[[i]],
    by = "comid",
    all.x = TRUE
  )
  
  temp_inc_tp_dat_tmp[-c(1,6)] <- temp_inc_tp_dat_tmp[-c(1, 6)] * 
    temp_inc_tp_dat_tmp[["delfrac_rev"]]
  temp_inc_tp_dat_tmp <- temp_inc_tp_dat_tmp[
    order(temp_inc_tp_dat_tmp$comid), 
  ]
  
}

if(length(RiparianBuffer_BMPs) > 0) {
  temp_riparian_tn_dat <- foreach(i = 1:length(upstream.comids_tn)) %do% {
    
    
    temp_riparian_tn_dat_tmp <- merge(
      riparian.loadings_tn[[i]],
      delfrac_rev_tn[[i]],
      by = "comid",
      all = TRUE
    ) %>%
      mutate(in_riparian = N_riparian_kgyr * delfrac_rev) %>%
      arrange(comid) %>%
      select(comid, in_riparian)
    
  }
  
  temp_riparian_tp_dat <- foreach(i = 1:length(upstream.comids_tp)) %do% {
    
    
    temp_riparian_tp_dat_tmp <- merge(
      riparian.loadings_tp[[i]],
      delfrac_rev_tp[[i]],
      by = "comid",
      all = TRUE
    ) %>%
      mutate(ip_riparian = P_riparian_kgyr * delfrac_rev) %>%
      arrange(comid) %>%
      select(comid, ip_riparian)
    
  }
  
  temp_riparian_tn_removal <- foreach(i = 1:length(upstream.comids_tn)) %do% {
    
    
    temp_riparian_tn_removal_tmp <- merge(
      riparian_buffer_removal_N[[i]],
      delfrac_rev_tn[[i]],
      by = "comid",
      all = TRUE
    ) %>%
      mutate(
        across(
          .col = any_of(RiparianBuffer_BMPs), 
          .fn =  ~case_when(is.na(.) ~ -999, !is.na(.) ~ . * delfrac_rev)
        )
      ) %>%
      arrange(comid) %>%
      select(comid, any_of(RiparianBuffer_BMPs))
    
  }
  
  temp_riparian_tp_removal <- foreach(i = 1:length(upstream.comids_tp)) %do% {
    
    
    temp_riparian_tp_removal_tmp <- merge(
      riparian_buffer_removal_P[[i]],
      delfrac_rev_tp[[i]],
      by = "comid",
      all = TRUE
    ) %>%
      mutate(
        across(
          .col = any_of(RiparianBuffer_BMPs), 
          .fn =  ~case_when(is.na(.) ~ -999, !is.na(.) ~ . * delfrac_rev)
        )
      ) %>%
      arrange(comid) %>%
      select(comid, any_of(RiparianBuffer_BMPs))
    
  }
}


# (SC for StreamCat)
temp_inc_tn_dat_SC <- foreach(i = 1:length(temp_inc_tn_dat)) %do% {
  temp_inc_tn_dat[[i]][
    temp_inc_tn_dat[[i]]$comid %in% streamcat_subset_tn[[i]]$comid, 
  ]
}

temp_inc_tn_dat_other <- foreach(i = 1:length(temp_inc_tn_dat)) %do% {
  temp_inc_tn_dat[[i]][
    !(temp_inc_tn_dat[[i]]$comid %in% streamcat_subset_tn[[i]]$comid), 
  ]
}

temp_inc_tp_dat_SC <- foreach(i = 1:length(temp_inc_tp_dat)) %do% {
  temp_inc_tp_dat[[i]][
    temp_inc_tp_dat[[i]]$comid %in% streamcat_subset_tp[[i]]$comid, 
  ]
}

temp_inc_tp_dat_other <- foreach(i = 1:length(temp_inc_tp_dat)) %do% {
  temp_inc_tp_dat[[i]][
    !(temp_inc_tp_dat[[i]]$comid %in% streamcat_subset_tp[[i]]$comid), 
  ]
}
### Calculate sums of loads for optimization

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  param_loads_lim_tn <- unlist(
    foreach(i = 1:length(temp_inc_tn_dat)) %do% {
      
      (1 - load_perc_reduc_tn[i]) * sum(
        temp_inc_tn_dat[[i]][ , c("in_poin", "in_urb", "in_ag", "in_other")]
      )
    }
  )
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  param_loads_lim_tp <- unlist(
    foreach(i = 1:length(temp_inc_tp_dat)) %do% {
      
      (1 - load_perc_reduc_tp[i]) * sum(
        temp_inc_tp_dat[[i]][ , c("ip_poin", "ip_urb", "ip_ag", "ip_other")]
      )
    }
  )
}

param_other_loads_tn <- unlist(
  foreach(i = 1:length(temp_inc_tn_dat_other)) %do% {
    sum(temp_inc_tn_dat_SC[[i]]$in_other) + sum(
      if(nrow(temp_inc_tn_dat_other[[i]]) > 0) {
        temp_inc_tn_dat_other[[i]][
          , c("in_poin", "in_urb", "in_ag", "in_other")
        ] 
      } else {0}
    )
  }
)

param_other_loads_tp <- unlist(
  foreach(i = 1:length(temp_inc_tp_dat_other)) %do% {
    sum(temp_inc_tp_dat_SC[[i]]$ip_other) + sum(
      if(nrow(temp_inc_tp_dat_other[[i]]) > 0) {
        temp_inc_tp_dat_other[[i]][
          , c("ip_poin", "ip_urb", "ip_ag", "ip_other")
        ] 
      } else {0}
    )
  }
)

### Preprocess COMIDs and BMPs to have ''

# Format loadings data

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  inc_tn_dat <- foreach(i = 1:length(temp_inc_tn_dat_SC)) %do% {
    temp_inc_tn_dat_opt <- temp_inc_tn_dat_SC[[i]]
    temp_inc_tn_dat_opt$comid_form <- paste0(
      "'", temp_inc_tn_dat_opt$comid, "'"
    )
    temp_inc_tn_dat_opt %>% 
      select(c("comid_form", "in_poin", "in_urb", "in_ag"))
  }
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  inc_tp_dat <- foreach(i = 1:length(temp_inc_tp_dat_SC)) %do% {
    temp_inc_tp_dat_opt <- temp_inc_tp_dat_SC[[i]]
    temp_inc_tp_dat_opt$comid_form <- paste0(
      "'", temp_inc_tp_dat_opt$comid, "'"
    )
    temp_inc_tp_dat_opt %>% 
      select(c("comid_form", "ip_poin", "ip_urb", "ip_ag"))
  }
}

if(length(RiparianBuffer_BMPs) > 0) {
  riparian_tn_dat <- foreach(i = 1:length(temp_riparian_tn_dat)) %do% {
    
    temp_riparian_tn_dat[[i]] %>%
      filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      select(comid_form, in_riparian)
    
  }
  
  riparian_tp_dat <- foreach(i = 1:length(temp_riparian_tp_dat)) %do% {
    
    temp_riparian_tp_dat[[i]] %>%
      filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      select(comid_form, ip_riparian)
    
  }
  
  riparian_tn_removal <- foreach(i = 1:length(temp_riparian_tn_removal)) %do% {
    
    temp_riparian_tn_removal[[i]] %>%
      filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      select(comid_form, any_of(RiparianBuffer_BMPs))
    
  }
  
  riparian_tp_removal <- foreach(i = 1:length(temp_riparian_tp_removal)) %do% {
    
    temp_riparian_tp_removal[[i]] %>%
      filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      select(comid_form, any_of(RiparianBuffer_BMPs))
    
  }
}
# # Format imperviousness data
# 
# pctimpvurb_dat <- pctimpvurb %>%
#   filter(comid %in% streamcat_subset_all$comid) %>%
#   arrange(comid) %>%
#   mutate(comid_form = paste0("'", comid, "'")) %>%
#   select(comid_form, imparea_pcturban)

# Format urban BMP costs (do not vary by state)
# Note to user: urban BMP costs do not vary by state, thus can set capital and operations costs to VT costs
temp_urban_costs_dat <- bmp_costs[
  bmp_costs$category == "urban",
  c("bmp", "capital_VT", "capital_NH", "operations_VT", "operations_NH")
] #*#
temp_urban_costs_dat$capital <- temp_urban_costs_dat$capital_VT #*#
temp_urban_costs_dat$operations <- temp_urban_costs_dat$operations_VT #*#
urban_costs_dat <-temp_urban_costs_dat %>% 
  select(c("bmp", "capital", "operations")) 
if(length(Urban_BMPs) > 0) {
  urban_costs_dat$bmp_form  <- paste0("'", urban_costs_dat$bmp, "'") 
}


# Separate parameters for ag_capital and ag_operations and format costs data

if(length(Ag_BMPs) > 0) {
  temp_bmp_costs_ag <- merge(
    bmp_costs[
      bmp_costs$category == "ag",
      c("bmp", "capital_VT", "capital_NH", "operations_VT", "operations_NH")
    ],
    COMID_State
  ) #*#
  temp_bmp_costs_ag$capital <-  with(
    temp_bmp_costs_ag ,ifelse(State == "NH" ,capital_NH, capital_VT)
  ) #*#
  temp_bmp_costs_ag$operations <-  with(
    temp_bmp_costs_ag, ifelse(State == "NH", operations_NH, operations_VT) 
  ) #*#
  temp_bmp_costs_ag <- temp_bmp_costs_ag[order(temp_bmp_costs_ag$comid),]
  temp_bmp_costs_ag$comid_form <- paste0("'", temp_bmp_costs_ag$comid, "'")
  
  bmp_costs_ag_capital <- reshape2::dcast(
    temp_bmp_costs_ag[, c("comid", "comid_form", "bmp", "capital")], 
    comid_form + comid ~ bmp,
    value.var = "capital"
  )
  bmp_costs_ag_operations <- reshape2::dcast(
    temp_bmp_costs_ag[, c("comid", "comid_form", "bmp", "operations")],
    comid_form + comid ~ bmp,
    value.var = "operations"
  )
  
  bmp_costs_ag_capital_rev <- bmp_costs_ag_capital[
    order(bmp_costs_ag_capital$comid),
  ]
  ag_costs_cap_dat <- bmp_costs_ag_capital_rev[
    ,names(bmp_costs_ag_capital_rev) != "comid"
  ]
  ag_costs_cap_dat <- ag_costs_cap_dat %>% select(comid_form, everything())
  
  bmp_costs_ag_operations_rev <- bmp_costs_ag_operations[
    order(bmp_costs_ag_operations$comid),
  ]
  ag_costs_op_dat <- bmp_costs_ag_operations_rev[ 
    ,names(bmp_costs_ag_operations_rev) != "comid"
  ]
  ag_costs_op_dat <- ag_costs_op_dat %>% select(comid_form, everything())
}


# Format riparian costs
temp_bmp_costs_ripbuf <- merge(
  bmp_costs[
    bmp_costs$category == "ripbuf",
    c("bmp", "capital_VT", "capital_NH", "operations_VT", "operations_NH")
  ],
  COMID_State
) #*#
temp_bmp_costs_ripbuf$capital <-  with(
  temp_bmp_costs_ripbuf ,ifelse(State == "NH" ,capital_NH, capital_VT)
) #*#
temp_bmp_costs_ripbuf$operations <-  with(
  temp_bmp_costs_ripbuf, ifelse(State == "NH", operations_NH, operations_VT) 
) #*#
temp_bmp_costs_ripbuf <- temp_bmp_costs_ripbuf[
  order(temp_bmp_costs_ripbuf$comid),
]
if(length(RiparianBuffer_BMPs) > 0) {
  temp_bmp_costs_ripbuf$comid_form <- paste0(
    "'", temp_bmp_costs_ripbuf$comid, "'"
  )
  
  bmp_costs_ripbuf_capital <- reshape2::dcast(
    temp_bmp_costs_ripbuf[, c("comid", "comid_form", "bmp", "capital")], 
    comid_form + comid ~ bmp,
    value.var = "capital"
  )
  
  bmp_costs_ripbuf_operations <- reshape2::dcast(
    temp_bmp_costs_ripbuf[, c("comid", "comid_form", "bmp", "operations")],
    comid_form + comid ~ bmp,
    value.var = "operations"
  )
  
  bmp_costs_ripbuf_capital_rev <- bmp_costs_ripbuf_capital[
    order(bmp_costs_ripbuf_capital$comid),
  ]
  ripbuf_costs_cap_dat <- bmp_costs_ripbuf_capital_rev[
    ,names(bmp_costs_ripbuf_capital_rev) != "comid"
  ]
  ripbuf_costs_cap_dat <- ripbuf_costs_cap_dat %>% 
    select(comid_form, everything())
  
  bmp_costs_ripbuf_operations_rev <- bmp_costs_ripbuf_operations[
    order(bmp_costs_ripbuf_operations$comid),
  ]
  ripbuf_costs_op_dat <- bmp_costs_ripbuf_operations_rev[ 
    ,names(bmp_costs_ripbuf_operations_rev) != "comid"
  ]
  ripbuf_costs_op_dat <- ripbuf_costs_op_dat %>% 
    select(comid_form, everything())
  
}


# Format point source BMP efficiency data
temp_point_effic_dat_tn <- point_effic_bycomid_tn[
  point_effic_bycomid_tn$comid %in% 
    unlist(streamcat_subset_tn, use.names = FALSE),
] %>% 
  select(c("comid", "effic"))

temp_point_effic_other_tn <- data.frame(
  comid = unlist(
    streamcat_subset_tn, use.names = FALSE
  )[
    !(
      unlist(streamcat_subset_tn, use.names = FALSE) %in% 
        temp_point_effic_dat_tn$comid
    )
  ]
)

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  temp_point_effic_other_tn$effic <- 0
  temp_point_effic_dat_rev_tn <- rbind(
    temp_point_effic_dat_tn, temp_point_effic_other_tn
  ) %>% 
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  point_effic_dat_tn <- temp_point_effic_dat_rev_tn %>% 
    select(c("comid_form", "effic"))
}

temp_point_effic_dat_tp <- point_effic_bycomid_tp[
  point_effic_bycomid_tp$comid %in% 
    unlist(streamcat_subset_tp, use.names = FALSE),
] %>% 
  select(c("comid", "effic"))

temp_point_effic_other_tp <- data.frame(
  comid = unlist(
    streamcat_subset_tp, use.names = FALSE
  )[
    !(
      unlist(streamcat_subset_tp, use.names = FALSE) %in% 
        temp_point_effic_dat_tp$comid
    )
  ]
)

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  temp_point_effic_other_tp$effic <- 0
  
  temp_point_effic_dat_rev_tp <- rbind(
    temp_point_effic_dat_tp, temp_point_effic_other_tp
  ) %>% 
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  point_effic_dat_tp <- temp_point_effic_dat_rev_tp %>% 
    select(c("comid_form", "effic"))
}

# Format agricultural BMP efficiency data
temp_ag_effic_dat_tn <- ag_effic_bycomid_tn[
  ag_effic_bycomid_tn$comid %in% unlist(streamcat_subset_tn, use.names = FALSE),
] %>%
  arrange(comid) %>%
  mutate(comid_form = paste0("'", comid, "'"))

ag_effic_dat_tn <- temp_ag_effic_dat_tn %>% 
  select(-comid) %>% 
  select(comid_form, everything())

temp_ag_effic_dat_tp <- ag_effic_bycomid_tp[
  ag_effic_bycomid_tp$comid %in% unlist(streamcat_subset_tp, use.names = FALSE),
] %>%
  arrange(comid) %>%
  mutate(comid_form = paste0("'", comid, "'"))

ag_effic_dat_tp <- temp_ag_effic_dat_tp %>% 
  select(-comid) %>% 
  select(comid_form, everything())

# Format urban BMP efficiency data
if(length(Urban_BMPs) > 0) {
  temp_urban_effic_dat_tn <- urban_effic_bycomid_tn[
    urban_effic_bycomid_tn$comid %in% 
      unlist(streamcat_subset_tn, use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  urban_effic_dat_tn <- temp_urban_effic_dat_tn %>% 
    select(-comid) %>% 
    select(comid_form, everything())
  
  temp_urban_effic_dat_tp <- urban_effic_bycomid_tp[
    urban_effic_bycomid_tp$comid %in% 
      unlist(streamcat_subset_tp, use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  urban_effic_dat_tp <- temp_urban_effic_dat_tp %>% 
    select(-comid) %>% 
    select(comid_form, everything())
}

# Format point source BMP costs data
temp_point_costs_dat <- bmp_costs_point[
  bmp_costs_point$comid %in% streamcat_subset_all$comid,
] %>% 
  select(c("comid", "State", "capital", "operations"))
temp_point_costs_other <- as.data.frame(
  streamcat_subset_all[
    !(streamcat_subset_all$comid %in% temp_point_costs_dat$comid),
    ]
)
names(temp_point_costs_other) <- "comid"
temp_point_costs_other_rev <- merge(
  temp_point_costs_other, COMID_State, by = c("comid"), all.x = TRUE
)
temp_point_costs_other_rev$capital <- 0
temp_point_costs_other_rev$operations <- 0
names(temp_point_costs_other_rev) <- c(
  "comid", "State", "capital", "operations"
)
temp_point_costs_dat_rev <- rbind(
  temp_point_costs_dat, temp_point_costs_other_rev
)
temp_point_costs_dat_rev <- temp_point_costs_dat_rev[
  order(temp_point_costs_dat_rev$comid),
]
temp_point_costs_dat_rev$comid_form <- paste0(
  "'", temp_point_costs_dat_rev$comid, "'"
)
point_costs_dat <- temp_point_costs_dat_rev %>% 
  select(c("comid_form", "capital", "operations"))

# Format user defined limits on BMP implementation

temp_bmp_imp <- user_specs_BMPs %>% 
  select(BMP_Category, BMP, BMP_Selection, frac_min, frac_max) %>%
  filter(BMP_Selection == "X") 

urban_bmp_imp <- temp_bmp_imp %>% 
  filter(BMP_Category == "urban") %>% 
  select(BMP, frac_min, frac_max)

ag_bmp_imp <- temp_bmp_imp %>% 
  filter(BMP_Category == "ag") %>% 
  select(BMP, frac_min, frac_max)

ripbuf_bmp_imp <- temp_bmp_imp %>% 
  filter(BMP_Category == "ripbuf") %>% 
  select(BMP, frac_min, frac_max)

# Format user defined design depths for urban BMPs

bmp_urban_ratingdepths <- bmp_urban %>% 
  mutate(
    ratingdepth = case_when(
      BMP != "Porous_Pavement_w_underdrain" ~ as.numeric(as.character(UserSpec_RD_in)),
      BMP == "Porous_Pavement_w_underdrain" ~ 4 * 0.2 + # Porous asphalt
        4 * 0.4 + # Chocker course
        UserSpec_RD_in * 0.25 + # filter course
        8 * 0.4 # gravel layer
    )
  ) %>%
  select(BMP, ratingdepth)

# Make matrix of implementability of each urban BMP for each comid's impervious land
# HSG D comids cannot have infiltration-based BMPs
# Porous Pavement can only be implemented on 10% of impervious land.

urban_bmp_implementationpotential_dat <- setNames(
  data.frame(
    matrix(
      ncol = length(Urban_BMPs) + 1, nrow = length(streamcat_subset_all$comid)
    )
  ), 
  c("comid", Urban_BMPs)
) %>%
  mutate(
    comid = streamcat_subset_all$comid,
    comid_form = paste0("'", comid, "'")
  ) %>%
  left_join(., comid.infiltrationrates.matched, by = "comid") %>%
  mutate(
    across(
      .cols = all_of(Urban_BMPs), 
      .fns = ~case_when(
        cur_column() %in% c(
          "Biofiltration_w_Underdrain", 
          "Bioretention_Basin", 
          "Enhanced_Biofiltration_w_ISR", 
          "Extended_Dry_Detention_Basin", 
          "Grass_Swale_w_detention", 
          "Gravel_Wetland", 
          "Sand_Filter_w_underdrain", 
          "Wet_Pond"
        ) ~ 
          1, 
        cur_column() == "Porous_Pavement_w_underdrain" ~ 0.1,
        cur_column() %in% c(
          "Infiltration_Basin", "Infiltration_Chamber", "Infiltration_Trench"
        ) ~ case_when(
          InfiltrationRate_inperhr <= 0.12 ~ 0, 
          InfiltrationRate_inperhr >= 0.12 ~ 1,
          is.na(InfiltrationRate_inperhr) ~ 0
        ),
        cur_column() == "Porous_Pavement_w_subsurface_infiltration" ~ case_when(
          InfiltrationRate_inperhr <= 0.12 ~ 0,
          InfiltrationRate_inperhr >= 0.12 ~ 0.1,
          is.na(InfiltrationRate_inperhr) ~ 0
        )
      )
    )
  ) %>%
  select(comid_form, all_of(Urban_BMPs))

# Format Urban Cost Coefficient Data
urban_cost_coeffs_dat <- urban_cost_coeffs %>%
  filter(comid %in% unlist(streamcat_subset_all)) %>%
  unique() %>%
  arrange('comid') %>%
  mutate(comid_form = paste0("'", comid, "'")) %>%
  select(comid_form, urban_cost_coef) 

### Write AMPL data file

write( "# Tier 1 Data AMPL File", file = paste(OutPath,"STdata.dat",sep = ""))

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(inc_tn_dat)) %do% {
      write(
        paste0("\nparam baseloads_N", i, " : 'point' 'urban' 'ag' :="), 
        file = paste(OutPath, "STdata.dat", sep = ""),
        append = T
      )
      write.table(
        inc_tn_dat[[i]] %>% 
          select(comid_form, point = in_poin, urban = in_urb, ag = in_ag), # renaming also forces the order incase they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = ""), 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath,"STdata.dat",sep = ""), append = T)
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(inc_tp_dat)) %do% {
      write(
        paste0("\nparam baseloads_P", i, " : 'point' 'urban' 'ag' :="), 
        file = paste(OutPath, "STdata.dat", sep = ""),
        append = T
      )
      write.table(
        inc_tp_dat[[i]] %>% 
          select(comid_form, point = ip_poin, urban = ip_urb, ag = ip_ag), # renaming also forces the order incase they are disordered in processing code above
        file = paste(OutPath, "STdata.dat", sep = ""), 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file = paste(OutPath,"STdata.dat",sep = ""), append = T)
    }
  }
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      write(
        paste0("\nparam: riparianload_N", i, " :="), 
        file = paste(OutPath, "STdata.dat", sep = ""),
        append = T
      )
      if(length(RiparianBuffer_BMPs) > 0) {
        write.table(
          riparian_tn_dat[[i]] %>% 
            select(comid_form, riparian = in_riparian), # renaming also forces the order incase they are disordered in processing code above
          file = paste(OutPath, "STdata.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
      } else {
        write.table(
          ripbuf_bmp_dummy_tn[[i]] %>% 
            select(comid, none = none), # renaming also forces the order incase they are disordered in processing code above
          file = paste(OutPath, "STdata.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
      }
      write( ";", file = paste(OutPath,"STdata.dat",sep = ""), append = T)
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      write(
        paste0("\nparam: riparianload_P", i, " :="), 
        file = paste(OutPath, "STdata.dat", sep = ""),
        append = T
      )
      if(length(RiparianBuffer_BMPs) > 0) {
        write.table(
          riparian_tp_dat[[i]] %>% 
            select(comid_form, riparian = ip_riparian), # renaming also forces the order incase they are disordered in processing code above
          file = paste(OutPath, "STdata.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
      } else {
        write.table(
          ripbuf_bmp_dummy_tp[[i]] %>% 
            select(comid, none = none), # renaming also forces the order incase they are disordered in processing code above
          file = paste(OutPath, "STdata.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
      }
      write( ";", file = paste(OutPath,"STdata.dat",sep = ""), append = T)
    }
  }
)

write( 
  "\nparam area : 'urban' 'ag' :=", 
  file = paste(OutPath,"STdata.dat",sep = ""), 
  append = T
)
write.table(
  area_dat %>% select(comid_form, urban = urban_ac, ag = ag_ac), # renaming also forces the order incase they are disordered in preprocessing code above
  file = paste(OutPath, "STdata.dat" ,sep = "") , 
  append = T,
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)
write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)

if(length(Urban_BMPs) > 0) {
  write(
    paste0(
      "\nparam urban_bmp_implementationpotential : ", 
      paste(bmp_urban_vec, collapse = "  "), 
      ":="
    ),
    file = paste0(OutPath, "STdata.dat"),
    append = TRUE
  )
  write.table(
    urban_bmp_implementationpotential_dat %>% 
      select(comid_form, any_of(Urban_BMPs)), # selection forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write(
    paste0("\nparam urban_bmp_implementationpotential: 'none' :="),
    file = paste0(OutPath, "STdata.dat"),
    append = TRUE
  )
  write.table(
    urban_bmp_dummy %>% 
      select(comid, none), # selection forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
}
write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)

if(length(RiparianBuffer_BMPs) > 0) {
  write( 
    paste0(
      "\nparam unbuffered_banklength : ",  
      paste(bmp_ripbuf_vec, collapse  = "  "), 
      " :="
      ), 
    file = paste(OutPath,"STdata.dat",sep = ""), 
    append = T
  )
  write.table(
    streambanklength_available_dat %>% 
      select(comid_form, any_of(RiparianBuffer_BMPs)), # selection forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write( 
    paste0("\nparam unbuffered_banklength: 'none' :="), 
    file = paste(OutPath,"STdata.dat",sep = ""), 
    append = T
  )
  write.table(
    ripbuf_bmp_dummy %>% 
      select(comid, none), # selection forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
}
write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)

write( 
  paste0("\nparam total_banklength :="), 
  file = paste(OutPath,"STdata.dat",sep = ""), 
  append = T
)
if(length(RiparianBuffer_BMPs) > 0) {
  write.table(
    streambanklength_total_dat %>% 
      select(comid_form, totalbanklength_ft), # selection forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write.table(
    ripbuf_bmp_dummy %>% 
      select(comid, none), # selection forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
}
write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)

if(length(Ag_BMPs) > 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam ag_effic_N : ",bmp_ag_vec," :=	", 
      file = paste(OutPath,"STdata.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
    write.table(
      ag_effic_dat_tn %>% select(comid_form, all_of(bmp_ag_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam ag_effic_P : ",bmp_ag_vec," :=	", 
      file = paste(OutPath,"STdata.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
    write.table(
      ag_effic_dat_tp %>% select(comid_form, all_of(bmp_ag_vec_direct)),  # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  }
} else {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam ag_effic_N : 'none' :=	", 
      file = paste(OutPath,"STdata.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
    write.table(
      unique(do.call("rbind", ag_bmp_dummy_tn)) %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam ag_effic_P : 'none' :=	", 
      file = paste(OutPath,"STdata.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
    write.table(
      unique(do.call("rbind", ag_bmp_dummy_tp)) %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  }
}


if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  write( 
    "\nparam point_effic_N : 'point' :=	", 
    file = paste(OutPath, "STdata.dat", sep = ""), 
    append = T
  )
  write.table(
    unique(point_effic_dat_tn %>% select(comid_form, point = effic)), # renaming also forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata.dat", sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  write( 
    "\nparam point_effic_P : 'point' :=	", 
    file = paste(OutPath, "STdata.dat", sep = ""), 
    append = T
  )
  write.table(
    unique(point_effic_dat_tp %>% select(comid_form, point = effic)), # renaming also forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata.dat", sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
}

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  if(length(Urban_BMPs) > 0) {
    cat(
      "\nparam urban_effic_N : ",bmp_urban_vec," :=", 
      file = paste(OutPath,"STdata.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
    write.table(
      unique(
        urban_effic_dat_tn %>% select(comid_form, all_of(bmp_urban_vec_direct))
        ), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    cat(
      "\nparam urban_effic_N : 'none' :=", 
      file = paste0(OutPath, "STdata.dat"), 
      sep = "", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
    write.table(
      unique(do.call("rbind", urban_bmp_dummy_tn) %>% select(comid, none)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  if(length(Urban_BMPs) > 0) {
    cat(
      "\nparam urban_effic_P : ",bmp_urban_vec," :=", 
      file = paste(OutPath,"STdata.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
    write.table(
      unique(
        urban_effic_dat_tp %>% select(comid_form, all_of(bmp_urban_vec_direct))
        ), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    cat(
      "\nparam urban_effic_P: 'none' :=", 
      file = paste0(OutPath, "STdata.dat"), 
      sep = "", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
    write.table(
      unique(do.call("rbind", urban_bmp_dummy_tp) %>% select(comid, none)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
}

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      if(length(RiparianBuffer_BMPs) > 0) {
        write(
          paste0(
            "\nparam riparianremoval_N", 
            i, 
            " : ", 
            paste(bmp_ripbuf_vec, collapse  = "  "), 
            " :="
          ), 
          file = paste(OutPath, "STdata.dat", sep = ""),
          append = T
        )
        write.table(
          riparian_tn_removal[[i]] %>% 
            select(comid_form, any_of(RiparianBuffer_BMPs)), # renaming also forces the order incase they are disordered in processing code above
          file = paste(OutPath, "STdata.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
      } else {
        write(
          paste0("\nparam riparianremoval_N", i, " : 'none' :="), 
          file = paste(OutPath, "STdata.dat", sep = ""),
          append = T
        )
        write.table(
          ripbuf_bmp_dummy_tn[[i]] %>% 
            select(comid, none), # renaming also forces the order incase they are disordered in processing code above
          file = paste(OutPath, "STdata.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
      }
      write( ";", file = paste(OutPath,"STdata.dat",sep = ""), append = T)
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      if(length(RiparianBuffer_BMPs) > 0) {
        write(
          paste0(
            "\nparam riparianremoval_P", 
            i, 
            " : ", 
            paste(bmp_ripbuf_vec, collapse  = "  "), 
            " :="
          ), 
          file = paste(OutPath, "STdata.dat", sep = ""),
          append = T
        )
        write.table(
          riparian_tp_removal[[i]] %>% 
            select(comid_form, any_of(RiparianBuffer_BMPs)), # renaming also forces the order incase they are disordered in processing code above
          file = paste(OutPath, "STdata.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
      } else {
        write(
          paste0("\nparam riparianremoval_P", i, " : 'none' :="), 
          file = paste(OutPath, "STdata.dat", sep = ""),
          append = T
        )
        write.table(
          ripbuf_bmp_dummy_tp[[i]] %>% 
            select(comid, none), # renaming also forces the order incase they are disordered in processing code above
          file = paste(OutPath, "STdata.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
      }
      write( ";", file = paste(OutPath,"STdata.dat",sep = ""), append = T)
    }
  }
)


write(  
  "\nparam runoff_coeff_urban : 'urban' :=", 
  file = paste(OutPath, "STdata.dat", sep = ""), 
  append = T
)
write.table(
  runoffcoeff_dat %>% select(comid_form, urban = runoffcoeff),  # renaming also forces the order incase they are disordered in preprocessing code above
  file = paste(OutPath, "STdata.dat", sep = "") , 
  append = T,
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)
write( ";", file = paste(OutPath, "STdata.dat", sep=""), append = T)

if(length(Ag_BMPs) > 0) {
  cat(
    "\nparam ag_costs_capital : ",bmp_ag_vec," :=	",
    file = paste(OutPath, "STdata.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    ag_costs_cap_dat %>% select(comid_form, all_of(bmp_ag_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  
  cat(
    "\nparam ag_costs_operations : ",bmp_ag_vec," :=	",
    file = paste(OutPath, "STdata.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    ag_costs_op_dat %>% select(comid_form, all_of(bmp_ag_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata.dat", sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
} else {
  cat(
    "\nparam ag_costs_capital : 'none' :=	",
    file = paste(OutPath, "STdata.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    ag_bmp_dummy %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  
  cat(
    "\nparam ag_costs_operations : 'none' :=	",
    file = paste(OutPath, "STdata.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    ag_bmp_dummy %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata.dat", sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
}


write( 
  "\nparam point_costs : 'capital' 'operations' :=	", 
  file = paste(OutPath, "STdata.dat", sep = ""), 
  append = T
)
write.table(
  point_costs_dat %>% select(comid_form, capital, operations), # selecting forces the order incase they are disordered in preprocessing code above
  file = paste(OutPath, "STdata.dat", sep = "") , 
  append = T,
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)
write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)

write( 
  "\nparam urban_costs : 'capital' 'operations' :=	", 
  file = paste(OutPath, "STdata.dat", sep = ""), 
  append = T
)
if(length(Urban_BMPs) > 0) {
  write.table(
    urban_costs_dat[, c("bmp_form", "capital", "operations")],
    file = paste(OutPath, "STdata.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write(
    "'none'      0      0",
    file = paste(OutPath, "STdata.dat", sep = ""),
    append = T
  )
}
write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)

if(length(RiparianBuffer_BMPs) > 0) {
  cat(
    "\nparam ripbuf_costs_capital : ", bmp_ripbuf_vec, " :=	",
    file = paste(OutPath, "STdata.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_costs_cap_dat %>% select(comid_form, all_of(RiparianBuffer_BMPs)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  
  cat(
    "\nparam ripbuf_costs_operations : ", bmp_ripbuf_vec," :=	",
    file = paste(OutPath, "STdata.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_costs_op_dat %>% select(comid_form, all_of(RiparianBuffer_BMPs)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata.dat", sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
} else {
  cat(
    "\nparam ripbuf_costs_capital : 'none' :=	",
    file = paste(OutPath, "STdata.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_bmp_dummy %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
  
  cat(
    "\nparam ripbuf_costs_operations : 'none' :=	",
    file = paste(OutPath, "STdata.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_bmp_dummy %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata.dat", sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)
}


invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0("\nparam loads_lim_N", i, " := "),
        file = paste(OutPath, "STdata.dat", sep = ""),
        sep = "", 
        append = T
      )
      cat(
        param_loads_lim_tn[i], 
        file = paste(OutPath, "STdata.dat", sep = ""), 
        sep = "", 
        append = T
      )
      cat(
        ";", 
        file = paste(OutPath, "STdata.dat", sep = ""), 
        sep = "",
        append = T
        )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0("\nparam loads_lim_P", i, " := "),
        file = paste(OutPath, "STdata.dat", sep = ""),
        sep = "", 
        append = T
      )
      cat(
        param_loads_lim_tp[i], 
        file = paste(OutPath, "STdata.dat", sep = ""), 
        sep = "", 
        append = T
      )
      cat(
        ";", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
        )
    }
  }
)

cat("\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T)

invisible(
  foreach(i = 1:length(param_other_loads_tn)) %do% {
    cat(
      paste0("\nparam other_loads_N", i, " := "),
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      param_other_loads_tn[i], 
      file = paste(OutPath, "STdata.dat", sep = ""), 
      sep = "", 
      append = T
    )
    cat(
      ";", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
  }
)

invisible(
  foreach(i = 1:length(param_other_loads_tp)) %do% {
    cat(
      paste0("\nparam other_loads_P", i, " := "),
      file = paste(OutPath, "STdata.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      param_other_loads_tp[i], 
      file = paste(OutPath, "STdata.dat", sep = ""), 
      sep = "", 
      append = T
    )
    cat(
      ";", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
      )
  }
)

cat("\n", file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T)

cat(
  "\nparam agcost_frac := ",
  file = paste(OutPath, "STdata.dat", sep = ""), 
  sep = "", 
  append = T
)
cat(
  agcost_frac, 
  file = paste(OutPath, "STdata.dat", sep = ""), 
  sep = "", 
  append = T
)
cat(
  ";",file = paste(OutPath, "STdata.dat", sep = ""), sep = "", append = T
)

cat(
  "
param acfttoft3 := 43559.9;
param pcp := 0.0833;
param agBMP_minarea := 0;",
  file = paste(OutPath, "STdata.dat", sep = ""), sep = "\n", append = T
)

write( 
  "\nparam: urban_frac_min urban_frac_max :=	", 
  file = paste(OutPath, "STdata.dat", sep = ""), 
  append = T
)
if(length(Urban_BMPs) > 0) {
  write.table(
    urban_bmp_imp,
    file = paste(OutPath, "STdata.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write(
    "'none'     0     0",
    file = paste(OutPath, "STdata.dat", sep = ""),
    append = T
  )
}

write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)

write( 
  "\nparam: ag_frac_min ag_frac_max :=	", 
  file = paste(OutPath, "STdata.dat", sep = ""), 
  append = T
)
if(length(Ag_BMPs) > 0) {
  write.table(
    ag_bmp_imp,
    file = paste(OutPath, "STdata.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write(
    "'none'     0     0",
    file = paste(OutPath, "STdata.dat", sep = ""),
    append = T
  )
}
write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)

write( 
  "\nparam: ripbuf_frac_min ripbuf_frac_max :=	", 
  file = paste(OutPath, "STdata.dat", sep = ""), 
  append = T
)
if(length(RiparianBuffer_BMPs) > 0) {
  write.table(
    ripbuf_bmp_imp,
    file = paste(OutPath, "STdata.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write(
    "'none'     0     0",
    file = paste(OutPath, "STdata.dat", sep = ""),
    append = T
  )
}

write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)


write( 
  "\nparam: urban_design_depth :=	", 
  file = paste(OutPath, "STdata.dat", sep = ""), 
  append = T
)
if(length(Urban_BMPs) > 0) {
  write.table(
    bmp_urban_ratingdepths,
    file = paste(OutPath, "STdata.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write(
    "'none'     0",
    file = paste(OutPath, "STdata.dat", sep = ""),
    append = T
  )
}
write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)

write( 
  "\nparam: urban_cost_adjustment_coef :=	", 
  file = paste(OutPath, "STdata.dat", sep = ""), 
  append = T
)
write.table(
  urban_cost_coeffs_dat,
  file = paste(OutPath, "STdata.dat", sep = ""), 
  append = T,
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)
write( ";", file = paste(OutPath, "STdata.dat", sep = ""), append = T)

# -------------Develop model file-------------

### Save COMIDs as vector

comid_vec_all <- paste0("'", streamcat_subset_all$comid,"'")
comid_vec_all[-length(comid_vec_all)] <- paste0(
  comid_vec_all[-length(comid_vec_all)], ','
)

comid_vec_all_N <- paste0(
  "'", unique(unlist(streamcat_subset_tn, use.names = FALSE)), "'"
)
comid_vec_all_N[-length(comid_vec_all_N)] <- paste0(
  comid_vec_all_N[-length(comid_vec_all_N)], ','
)

comid_vec_all_P <- paste0(
  "'", unique(unlist(streamcat_subset_tp, use.names = FALSE)), "'"
)
comid_vec_all_P[-length(comid_vec_all_P)] <- paste0(
  comid_vec_all_P[-length(comid_vec_all_P)], ','
)

comid_vec_N <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
  
  comid_vec_N_tmp <- paste0("'", streamcat_subset_tn[[i]]$comid, "'")
  comid_vec_N_tmp[-length(comid_vec_N_tmp)] <- paste0(
    comid_vec_N_tmp[-length(comid_vec_N_tmp)], ','
  )
  comid_vec_N_tmp
}
comid_vec_P <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
  
  comid_vec_P_tmp <- paste0("'", streamcat_subset_tp[[i]]$comid, "'")
  comid_vec_P_tmp[-length(comid_vec_P_tmp)] <- paste0(
    comid_vec_P_tmp[-length(comid_vec_P_tmp)], ','
  )
  comid_vec_P_tmp
}

### Write AMPL model file

write(
  "#STmodel.mod
      \nset comid_all :=", 
  file = paste(OutPath, "STmodel.mod", sep = "")
)
cat(
  "{",comid_vec_all,"};",
  file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
)
cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T)

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    paste0("\nset comid_all_N within comid_all := "),
    file = paste0(OutPath, "STmodel.mod"),
    sep = "",
    append = T
  )
  cat(
    "{",unique(comid_vec_all_N),"};",
    file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
  )
  cat(
    "\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
    )
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    paste0("\nset comid_all_P within comid_all := "),
    file = paste0(OutPath, "STmodel.mod"),
    sep = "",
    append = T
  )
  cat(
    "{",unique(comid_vec_all_P),"};",
    file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
  )
  cat(
    "\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T
    )
}

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(comid_vec_N)) %do% {
      
      cat(
        paste0("\nset comid_N", i, " within comid_all_N := "),
        file = paste0(OutPath, "STmodel.mod"),
        sep = "",
        append = T
      )
      cat(
        "{", comid_vec_N[[i]], "};",
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "",
        append = T
      )
      cat(
        "\n", 
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "", 
        append = T
        )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(comid_vec_P)) %do% {
      
      cat(
        paste0("\nset comid_P", i, " within comid_all_P := "),
        file = paste0(OutPath, "STmodel.mod"),
        sep = "",
        append = T
      )
      cat(
        "{", comid_vec_P[[i]], "};",
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "",
        append = T
      )
      cat(
        "\n",
        file = paste(OutPath, "STmodel.mod", sep = ""), 
        sep = "", 
        append = T
        )
    }
  }
)

cat(
  "\n\nset urban_bmp :=",
  file = paste(OutPath, "STmodel.mod", sep = ""),
  sep = "\n", 
  append = T
)
if(length(Urban_BMPs) > 0) {
  cat(
    "{",bmp_urban_vec_comma,"};",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "", 
    append = T
  ) 
} else {
  cat(
    "{'none'};",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "", 
    append = T
  ) 
}


cat(
  "\n\nset urban_pervbmp within urban_bmp :=",
  file = paste(OutPath, "STmodel.mod", sep = ""),
  sep = "\n", 
  append = T
)
if(length(Urban_BMPs) > 0) {
  if(!grepl("''", paste0(pervbmp_urban_vec_comma, collapse = ""))) {
    cat(
      "{",pervbmp_urban_vec_comma,"};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    )  
  } else {
    cat(
      "{};",
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "", 
      append = T
    )  
  }
} else {
  cat(
    "{'none'};",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "", 
    append = T
  )  
}

cat(
  "\n\nset ag_bmp :=",
  file = paste(OutPath, "STmodel.mod", sep = ""),
  sep = "\n", 
  append = T
)
if(length(Ag_BMPs) > 0) {
  cat(
    "{",bmp_ag_vec_comma,"};",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "", 
    append = T
  )
} else {
  cat(
    "{'none'};",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "", 
    append = T
  )
}


cat(
  "\n\nset ripbuf_bmp :=",
  file = paste(OutPath, "STmodel.mod", sep = ""),
  sep = "\n", 
  append = T
)
if(length(RiparianBuffer_BMPs) > 0) {
  cat(
    "{",bmp_ripbuf_vec_comma,"};",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "", 
    append = T
  ) 
} else {
  cat(
    "{'none'};",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "", 
    append = T
  ) 
}


cat(
  "\n\nset loads_all :=
{'point', 'urban', 'ag'};
  
set area_sub :=
{'urban', 'ag'};
  
set urban_imp :=
{'urban'};
  
set urban_c :=
{'urban'};
  
set point_c :=
{'point'};
  
set cost_type :=
{'capital', 'operations'};",
  file = paste(OutPath, "STmodel.mod", sep = ""),
  sep = "\n", 
  append = T
)

cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), sep = "", append = T)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(comid_vec_N)) %do% {
      cat(
        paste0("param baseloads_N", i, " {comid_N", i, ", loads_all} >= 0;"),
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(comid_vec_P)) %do% {
      cat(
        paste0("param baseloads_P", i, " {comid_P", i, ", loads_all} >= 0;"),
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(comid_vec_N)) %do% {
      cat(
        paste0("param riparianload_N", i, " {c in comid_N", i, "} >= 0;"),
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(comid_vec_P)) %do% {
      cat(
        paste0("param riparianload_P", i, " {c in comid_P", i, "} >= 0;"),
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)



cat(
  "\nparam area {comid_all,area_sub} >=0;
param urban_bmp_implementationpotential {comid_all, urban_bmp} >= 0;
param unbuffered_banklength {comid_all, ripbuf_bmp} >= 0;
param total_banklength {comid_all} >= 0;
param ag_costs_capital {comid_all,ag_bmp};
param ag_costs_operations {comid_all,ag_bmp};
param point_costs {comid_all,cost_type};
param urban_costs {urban_bmp,cost_type};
param ripbuf_costs_capital {comid_all, ripbuf_bmp};
param ripbuf_costs_operations {comid_all, ripbuf_bmp};
param runoff_coeff_urban {comid_all,urban_imp} >=0;
param urban_cost_adjustment_coef {comid_all} >= 0;\n",
  file = paste(OutPath, "STmodel.mod", sep = ""),
  sep = "\n",
  append = T
)  

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "\nparam ag_effic_N {comid_all_N ,ag_bmp};
param point_effic_N {comid_all_N, point_c};
param urban_effic_N {comid_all_N, urban_bmp};\n",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "\n", 
    append = T
  )
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "\nparam ag_effic_P {comid_all_P ,ag_bmp};
param point_effic_P {comid_all_P, point_c};
param urban_effic_P {comid_all_P, urban_bmp};\n",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "\n", 
    append = T
  )
}


invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(comid_vec_N)) %do% {
      cat(
        paste0(
          "param riparianremoval_N", 
          i, 
          " {c in comid_N", 
          i, 
          ", r in ripbuf_bmp};"
        ),
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(comid_vec_P)) %do% {
      cat(
        paste0(
          "param riparianremoval_P", 
          i, 
          " {c in comid_P", 
          i, 
          ", r in ripbuf_bmp};"
        ),
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0("param loads_lim_N", i, " >= 0;"),
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), append = T)


invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0("param loads_lim_P", i, " >= 0;"),
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), append = T)

invisible(
  foreach(i = 1:length(param_other_loads_tn)) %do% {
    cat(
      paste0("param other_loads_N", i, " >= 0;"),
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "\n",
      append = T
    )
  }
)

cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), append = T)

invisible(
  foreach(i = 1:length(param_other_loads_tp)) %do% {
    cat(
      paste0("param other_loads_P", i, " >= 0;"),
      file = paste(OutPath, "STmodel.mod", sep = ""),
      sep = "\n",
      append = T
    )
  }
)

cat("\n", file = paste(OutPath, "STmodel.mod", sep = ""), append = T)


cat(
  "param agcost_frac >=0;
param acfttoft3 >=0;
param pcp >=0;
param agBMP_minarea;

param urban_frac_min {urban_bmp} >=0, <= 1;
param urban_frac_max {u in urban_bmp} >= urban_frac_min[u], <= 1;
param ag_frac_min {ag_bmp} >= 0, <= 1;
param ag_frac_max {a in ag_bmp} >= ag_frac_min[a], <= 1;
param ripbuf_frac_min {ripbuf_bmp} >= 0, <= 1;
param ripbuf_frac_max {r in ripbuf_bmp} >= ripbuf_frac_min[r], <= 1;

param urban_design_depth {urban_bmp} >= 0;
  
param ps_coef {c in comid_all} := point_costs[c,'capital'] + 
   point_costs[c,'operations'];
param urban_coef {c in comid_all, u in urban_bmp} := acfttoft3 * pcp * 
   urban_design_depth[u] * runoff_coeff_urban[c,'urban'] * area[c,'urban'] * 
   (urban_costs[u,'capital'] + urban_costs[u,'operations']) * 
   urban_cost_adjustment_coef[c];
param ag_coef {c in comid_all, a in ag_bmp} := agcost_frac * area[c,'ag'] * 
(ag_costs_capital[c,a] + ag_costs_operations[c,a]) ;
param ripbuf_coef {c in comid_all, r in ripbuf_bmp} := 
agcost_frac * (ripbuf_costs_capital[c, r] + ripbuf_costs_operations[c, r]);
  
var agBMP_bin {comid_all, ag_bmp} binary;
var urbanBMP_bin {comid_all, urban_bmp} binary;
var point_dec {comid_all} binary :=0;
var urban_frac {c in comid_all, u in urban_bmp} 
   >= urban_frac_min[u] * urban_bmp_implementationpotential[c, u]  
   <= urban_frac_max[u] * urban_bmp_implementationpotential[c, u] :=0;
var ag_frac {comid_all, a in ag_bmp} >= ag_frac_min[a] <= ag_frac_max[a] :=0;
var ripbuf_length {c in comid_all, r in ripbuf_bmp} 
   >= 0 
   <= unbuffered_banklength[c, r] * ripbuf_frac_max[r] := 0;
  
minimize cost: sum {c in comid_all} (ps_coef[c] * point_dec[c]) + 
sum {c in comid_all, u in urban_bmp} (urban_coef[c,u] * urban_frac[c,u]) + 
sum {c in comid_all, a in ag_bmp} (ag_coef[c,a] * ag_frac[c,a]) +
sum {c in comid_all, r in ripbuf_bmp} (ripbuf_coef[c, r] * ripbuf_length[c, r]);
  \n",
  file = paste(OutPath, "STmodel.mod", sep = ""),
  sep = "\n", 
  append = T
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0(
          "subject to total_loads_N", i, ":\n",
          "other_loads_N", i, " + sum {c in comid_N", i, "} (baseloads_N", i, 
          "[c,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a])))", 
          " + \n",
          "sum {c in comid_N", i, "} (baseloads_N", i, 
          "[c,'urban'] * (1 - sum {u in urban_bmp}(urban_effic_N[c,u] *",
          " urban_frac[c,u]))) + \n",
          "sum {c in comid_N", i, "} (baseloads_N", i, "[c,'point'] * (1 -",
          " (point_effic_N[c,'point'] * point_dec[c]))) - \n",
          "sum {c in comid_N", i, ", r in ripbuf_bmp} (ripbuf_length[c, r] *",
          " riparianremoval_N", i, "[c, r]) <= loads_lim_N", i, "; \n"
        ),
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "\n", 
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0(
          "subject to total_loads_P", i, ":\n",
          "other_loads_P", i, " + sum {c in comid_P", i, "} (baseloads_P", i, 
          "[c,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_P[c,a] * ",
          "ag_frac[c,a]))) + \n",
          "sum {c in comid_P", i, "} (baseloads_P", i,
          "[c,'urban'] * (1 - sum {u in urban_bmp}(urban_effic_P[c,u] *", 
          "urban_frac[c,u]))) + \n",
          "sum {c in comid_P", i, "} (baseloads_P", i, "[c,'point'] * (1 -",
          "(point_effic_P[c,'point'] * point_dec[c])))  - \n",
          "sum {c in comid_P", i, ", r in ripbuf_bmp} (ripbuf_length[c, r] *",
          "riparianremoval_P", i, "[c, r]) <= loads_lim_P", i, "; \n"
        ),
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "\n", 
        append = T
      )
    }
  }
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0(
          "subject to riparian_loads_N", i, " {c in comid_N", i, "}:\n",
          "sum {r in ripbuf_bmp} (ripbuf_length[c, r] * riparianremoval_N", i, 
          "[c, r]) <= riparianload_N", i, "[c]; \n"
        ),
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "\n", 
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0(
          "subject to riparian_loads_P", i, " {c in comid_P", i, "}:\n",
          "sum {r in ripbuf_bmp} (ripbuf_length[c, r] * riparianremoval_P", i, 
          "[c, r]) <= riparianload_P", i, "[c]; \n"
        ),
        file = paste(OutPath, "STmodel.mod", sep = ""),
        sep = "\n", 
        append = T
      )
    }
  }
)

cat(
  "subject to ag_treat_min {c in comid_all, a in ag_bmp}:
ag_frac[c,a] * area[c,'ag'] >= agBMP_minarea * agBMP_bin[c,a];
  
subject to ag_frac_const {c in comid_all, a in ag_bmp}:
ag_frac[c,a] <= agBMP_bin[c,a];
  
subject to urban_frac_const {c in comid_all, u in urban_bmp}:
urban_frac[c,u] <= urbanBMP_bin[c,u];

subject to ag_frac_limit {c in comid_all}: 
sum {a in ag_bmp} ag_frac[c,a] <= 1;
# prevents multiple BMPs being implemented on the same area

subject to urban_frac_limit {c in comid_all}: 
sum {u in urban_bmp} urban_frac[c,u] <= 1;
# prevents multiple BMPs being implemented on the same area

subject to total_banks {c in comid_all}:
sum {r in ripbuf_bmp} ripbuf_length[c,r] <= max {r in ripbuf_bmp} 
   unbuffered_banklength[c,r];
  
",
  file = paste(OutPath, "STmodel.mod", sep = ""),
  sep = "\n", 
  append = T
)

if(!grepl("''", paste0(pervbmp_urban_vec_comma, collapse = ""))){
  cat(
    "
subject to roads_and_parkinglots {c in comid_all}:
sum{up in urban_pervbmp} urban_frac[c, up] <= max {up in urban_pervbmp} 
   urban_bmp_implementationpotential[c,up];
",
    file = paste(OutPath, "STmodel.mod", sep = ""),
    sep = "\n", 
    append = T
  )
}


print(
  paste(
    "RBEROST has finished writing AMPL scripts without uncertainty at",
    Sys.time()
  )
)
