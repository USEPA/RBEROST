##############################################################
### PURPOSE: Generate Uncertainty Estimates in the AMPL    ###                                                                 
###   Model                                                ###                   
### BY: Cathy Chamberlin                                   ###                                 
### DATE:  4/1/21                                          ###                         
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
### The development of                                     ###                                           
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
# This step generates the uncertainty estimates used in the AMPL model. It is designed to be run after the Preprocessor, and will ammend the AMPL files.


# Run Preprocessor without uncertainty ####
source("./R/01_Optimization_Preprocessing.R", local = TRUE)

print(
  paste(
    "RBEROST is now creating AMPL files with uncertainty analysis at",
    Sys.time()
  )
)

scenarioincrement <- 1 - scenariostepchange

## SPARROW N uncertainty ####

temp_inc_tn <- sparrow_cons_out_tn %>% 
  select(c("comid","in_poin":"in_urb", "sin_poin":"sin_urb")) %>%
  mutate(n = 200) %>%
  mutate(
    across(
      .cols = "in_poin":"in_urb", 
      .fns = list(
        sd = ~(sparrow_cons_out_tn[[paste0("s", cur_column())]] * sqrt(n))
      )
    )
  )

temp_inc_tn$in_ag <- with(temp_inc_tn, in_fert+in_fix+in_manu) #*#

temp_inc_tn$sin_ag <- temp_inc_tn %>%
  rowwise() %>% 
  mutate(
    sin_ag = my_propogateerror(
      vals = list(
        c(in_fert, sin_fert), c(in_fix, sin_fix), c(in_manu, sin_manu)
      ), 
      method = "addsub"
    )
  ) %>%
  ungroup() %>%
  .$sin_ag

## ndep uncertainties ####
### Adjust 2012 SPARROW loadings for N deposition based on percentage decreases to represent present conditions
ndep_change <- ndep_change %>%
  mutate(
    TDEP_TN_2012_sd = TDEP_TN_2012 * 0.43, # Corresponds to a WDUM of 2 in https://www.sciencedirect.com/science/article/pii/S0048969719329109?via%3Dihub, see Sensitivity Analysis Part 15 in Tier1OptimizationProject notebook
    TDEP_TN_2019_sd = TDEP_TN_2019 * 0.43, # Corresponds to a WDUM of 2 in https://www.sciencedirect.com/science/article/pii/S0048969719329109?via%3Dihub, see Sensitivity Analysis Part 15 in Tier1OptimizationProject notebook
    Change_2012_2019_sd = Change_2012_2019 * 
      sqrt(
        (
          sqrt(TDEP_TN_2012_sd ^ 2 + TDEP_TN_2019_sd ^ 2)/ 
            (TDEP_TN_2012 - TDEP_TN_2019)
        ) ^ 2 + 
          (TDEP_TN_2012_sd/TDEP_TN_2012) ^ 2
      ) /
      sqrt(nrow(.)) # using the standard error instead of the standard deviation keeps the distribution of resampled values similar to the original
  )

## revised in_atmo uncertainty ####
temp_inc_tn_rev <- merge(
  temp_inc_tn,
  ndep_change[, c("comid", "Change_2012_2019", "Change_2012_2019_sd")],
  by = "comid",
  all.x = TRUE
)

temp_inc_tn_rev[is.na(temp_inc_tn_rev)] <- 0

temp_inc_tn_rev[c("in_atmo")] <-  temp_inc_tn_rev[c("in_atmo")] -
  temp_inc_tn_rev[c("in_atmo")] * temp_inc_tn_rev[["Change_2012_2019"]] #*#

temp_inc_tn_rev[c("sin_atmo")] <- sqrt(
  (temp_inc_tn_rev[c("sin_atmo")]) ^ 2 + 
    (
      (temp_inc_tn_rev[c("in_atmo")] * temp_inc_tn_rev[["Change_2012_2019"]]) * 
        sqrt(
          (temp_inc_tn_rev[c("sin_atmo")] / temp_inc_tn_rev[c("in_atmo")]) ^ 2 + 
            (
              temp_inc_tn_rev[["Change_2012_2019_sd"]] / 
                temp_inc_tn_rev[["Change_2012_2019"]]
            ) ^ 2
        )
    ) ^ 2
)

## in_other uncertainty ####


temp_inc_tn_rev$in_other <- with(temp_inc_tn_rev, in_sept + in_atmo) #*#
temp_inc_tn_rev$sin_other <- with(
  temp_inc_tn_rev, sqrt(sin_sept ^ 2 + sin_atmo ^ 2)
) 

## N data ####

inc_tn <- temp_inc_tn_rev %>% 
  select(
    c(
      "comid", 
      "in_poin", 
      "in_urb", 
      "in_ag", 
      "in_other",
      "sin_poin", 
      "sin_urb",
      "sin_ag", 
      "sin_other"
    )
  ) %>%
  mutate(across(.fns = ~replace_na(., replace = 0)))

## SPARROW P uncertainty ####

temp_inc_tp <- sparrow_cons_out_tp %>% 
  select(c("comid","ip_poin":"ip_rock", "sip_poin":"sip_rock")) #*#

temp_inc_tp$ip_ag <- with(temp_inc_tp, ip_fert+ip_manu) #*#
temp_inc_tp$sip_ag <- with(temp_inc_tp, sqrt(sip_fert ^ 2 + sip_manu ^ 2)) #*#

temp_inc_tp$ip_other <- with(temp_inc_tp, ip_rock) #*#
temp_inc_tp$sip_other <- with(temp_inc_tp, sip_rock) #*#

inc_tp <- temp_inc_tp %>% 
  select(
    c(
      "comid", 
      "ip_poin",
      "ip_urb", 
      "ip_ag",
      "ip_other",
      "sip_poin",
      "sip_urb",
      "sip_ag", 
      "sip_other"
    )
  ) %>%
  mutate(across(.fns = ~replace_na(., replace = 0)))

## wwtp baseline uncertainty ####
### Adjust 2012 SPARROW loadings for WWTPs based on percentage decreases to represent present conditions
## This information is only available for TN

wwtp_rem_finegrain <- fread(
  paste0(InPath, "WWTP_BaselineRemoval_Finergrain.csv")
) %>%
  select(
    State,
    Plant_Name, 
    NPDES_ID, 
    comid, 
    contains("load_lbday"), 
    Offset_calcvals_over_ICF
  ) %>%
  pivot_longer(
    cols = contains("load_lbday"), 
    names_to = "Year",
    values_to = "load_lbday",
    names_pattern = "(\\d+)"
  ) %>%
  mutate(
    Year = as.numeric(as.character(Year)), 
    load_kgyr = load_lbday * 365 * 0.453592 / Offset_calcvals_over_ICF
  )

wwtp_rem_uncertain <- wwtp_rem %>%
  rowwise() %>%
  mutate(
    load_kgyr_2014_se = predict(
      with(
        wwtp_rem_finegrain[
          which(wwtp_rem_finegrain$Plant_Name %in% Plant_Name),
        ], 
        lm(load_kgyr ~ Year)
      ), 
      newdata = data.frame(Year = c(2014, 2018)), 
      se.fit = TRUE
    )$se.fit[1], 
    load_kgyr_2018_se = predict(
      with(
        wwtp_rem_finegrain[
          which(wwtp_rem_finegrain$Plant_Name %in% Plant_Name),
        ], 
        lm(load_kgyr ~ Year)
      ), 
      newdata = data.frame(Year = c(2014, 2018)), 
      se.fit = TRUE
    )$se.fit[2], 
    Rem_2014_2018_load_ch_se = abs(Rem_2014_2018_load_ch) * 
      sqrt(
        (
          sqrt(load_kgyr_2014_se ^ 2 + load_kgyr_2018_se ^ 2) / 
            (load_kgyr_2014 - load_kgyr_2018)
        ) ^ 2 + 
          (load_kgyr_2014_se / load_kgyr_2014) ^ 2
      )
  ) %>%
  ungroup()

## update in_poin ####


inc_tn_rev <- merge(
  inc_tn, 
  wwtp_rem_uncertain[, c("comid","Rem_2014_2018_load_ch", "Rem_2014_2018_load_ch_se")], 
  by = "comid", 
  all.x = TRUE
) #*#

inc_tn_rev[is.na(inc_tn_rev)] <- 0

inc_tn_rev[c("in_poin")] <-  
  inc_tn_rev[c("in_poin")] -
  inc_tn_rev[c("in_poin")] * inc_tn_rev[["Rem_2014_2018_load_ch"]] #*#

inc_tn_rev[c("sin_poin")] <-  sqrt(
  inc_tn_rev[c("sin_poin")] ^ 2 + 
    inc_tn_rev[c("in_poin")] * 
    inc_tn_rev[["Rem_2014_2018_load_ch"]] * 
    sqrt(
      (inc_tn_rev[c("sin_poin")] / inc_tn_rev[c("in_poin")]) ^ 2 + 
        (
          inc_tn_rev[["Rem_2014_2018_load_ch_se"]] / 
            inc_tn_rev[["Rem_2014_2018_load_ch"]]
        ) ^ 2
    )
)

inc_tn_rev[is.na(inc_tn_rev)] <- 0

inc_tn_rev[,(ncol(inc_tn_rev)-1):ncol(inc_tn_rev)] <- NULL

## propagate error in delivery fraction ####

### Adjust delivery fraction to pore point

max_delfrac_tn <- foreach(i = 1:length(pore_pt_tn)) %do% {
  
  c(
    sparrow_cons_out_tn$DEL_FRAC[sparrow_cons_out_tn$comid == pore_pt_tn[i]],
    sparrow_cons_out_tn$SE_DEL_FRAC[
      sparrow_cons_out_tn$comid == pore_pt_tn[i]
    ]
  )
  
}

temp_delfrac_rev_tn <- foreach(i = 1:length(upstream.comids_tn)) %do% {
  
  sparrow_cons_out_tn %>% 
    filter(comid %in% upstream.comids_tn[[i]])  %>% 
    select(c("comid", "DEL_FRAC", "SE_DEL_FRAC"))
  
}

invisible(
  foreach(i = 1:length(temp_delfrac_rev_tn)) %do% {
    
    temp_delfrac_rev_tn[[i]]$delfrac_rev <- with(
      temp_delfrac_rev_tn[[i]], DEL_FRAC / max_delfrac_tn[[i]][1]
    )
    temp_delfrac_rev_tn[[i]]$se_delfrac_rev <- with(
      temp_delfrac_rev_tn[[i]], 
      (DEL_FRAC / max_delfrac_tn[[i]][1]) * 
        sqrt(
          (SE_DEL_FRAC / DEL_FRAC) ^ 2 + 
            (max_delfrac_tn[[i]][2] / max_delfrac_tn[[i]][1]) ^ 2
        )
    )
    
  }
)

delfrac_rev_tn <- foreach(i = 1:length(temp_delfrac_rev_tn)) %do% {
  
  temp_delfrac_rev_tn[[i]] %>% select(c("comid", "delfrac_rev", "se_delfrac_rev"))
  
}

max_delfrac_tp <- foreach(i = 1:length(pore_pt_tp)) %do% {
  
  c(
    sparrow_cons_out_tp$DEL_FRAC[sparrow_cons_out_tp$comid == pore_pt_tp[i]],
    sparrow_cons_out_tp$SE_DEL_FRAC[
      sparrow_cons_out_tp$comid == pore_pt_tp[i]
    ]
  )
  
}


temp_delfrac_rev_tp <- foreach(i = 1:length(upstream.comids_tp)) %do% {
  
  sparrow_cons_out_tp %>% 
    filter(comid %in% upstream.comids_tp[[i]])  %>% 
    select(c("comid", "DEL_FRAC", "SE_DEL_FRAC"))
  
}

invisible(
  foreach(i = 1:length(temp_delfrac_rev_tp)) %do% {
    
    temp_delfrac_rev_tp[[i]]$delfrac_rev <- with(
      temp_delfrac_rev_tp[[i]], DEL_FRAC / max_delfrac_tp[[i]][1]
    )
    temp_delfrac_rev_tp[[i]]$se_delfrac_rev <- with(
      temp_delfrac_rev_tp[[i]], 
      (DEL_FRAC / max_delfrac_tp[[i]][1]) * 
        sqrt(
          (SE_DEL_FRAC / DEL_FRAC) ^ 2 + 
            (max_delfrac_tp[[i]][2] / max_delfrac_tp[[i]][1]) ^ 2
        )
    )
    
  }
)

delfrac_rev_tp <- foreach(i = 1:length(temp_delfrac_rev_tp)) %do% {
  
  temp_delfrac_rev_tp[[i]] %>% select(c("comid", "delfrac_rev", "se_delfrac_rev"))
  
}

## uncertainty in delivered baseloads ####

### Multiply all loads by revised del_frac

temp_inc_tn_dat <- foreach(i = 1:length(upstream.comids_tn)) %do% {
  
  
  temp_inc_tn_dat_tmp <- merge(
    inc_tn_rev[inc_tn_rev$comid %in% upstream.comids_tn[[i]],],
    delfrac_rev_tn[[i]],
    by = "comid",
    all.x = TRUE
  )
  
  temp_inc_tn_dat_tmp[2:5] <- temp_inc_tn_dat_tmp[2:5] * 
    temp_inc_tn_dat_tmp[["delfrac_rev"]]
  temp_inc_tn_dat_tmp[6:9] <- temp_inc_tn_dat_tmp[2:5] * 
    temp_inc_tn_dat_tmp[["delfrac_rev"]] *
    sqrt(
      (temp_inc_tn_dat_tmp[6:9] / temp_inc_tn_dat_tmp[2:5]) ^ 2 +
        (
          temp_inc_tn_dat_tmp[["se_delfrac_rev"]] / 
            temp_inc_tn_dat_tmp[["delfrac_rev"]]
        ) ^ 2
    )
  
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
  
  temp_inc_tp_dat_tmp[2:5] <- temp_inc_tp_dat_tmp[2:5] * 
    temp_inc_tp_dat_tmp[["delfrac_rev"]]
  temp_inc_tp_dat_tmp[6:9] <- temp_inc_tp_dat_tmp[2:5] * 
    temp_inc_tp_dat_tmp[["delfrac_rev"]] *
    sqrt(
      (temp_inc_tp_dat_tmp[6:9] / temp_inc_tp_dat_tmp[2:5]) ^ 2 +
        (
          temp_inc_tp_dat_tmp[["se_delfrac_rev"]] / 
            temp_inc_tp_dat_tmp[["delfrac_rev"]]
        ) ^ 2
    )
  
  temp_inc_tp_dat_tmp <- temp_inc_tp_dat_tmp[
    order(temp_inc_tp_dat_tmp$comid), 
  ]
  
}

## uncertainty in runoffcoeff ####

### Calculate runoff coefficient for urban area

# Specify column for impervious dataset
temp_runoffcoeff <- imperv %>% 
  select(c("comid","PctImp2011Cat")) %>%
  mutate(cv_numpixels_nlcd = 0.17) %>% # Wickham et al 2017 report overall 83% accuracy in level II data (including all categories, such as Med Development, High Development, etc.) The PctImp2011Cat came from NLCD, possibly the impervious dataset, not the larger categorical one. I'm assuming the accuracy is the same regardless. https://gaftp.epa.gov/epadatacommons/ORD/NHDPlusLandscapeAttributes/StreamCat/Documentation/Metadata/ImperviousSurfaces2016.html
  mutate(PctImp2011Cat_se = PctImp2011Cat * cv_numpixels_nlcd) %>%
  as.data.frame()


temp_runoffcoeff$runoffcoeff <- with(
  temp_runoffcoeff, 0.05 + 0.009 * PctImp2011Cat
) #*#

#This equation comes from Schueler 1987, Table A6 & Figure 1-2. 
Schueler.table <- data.frame(
  PercentImpervious = c(
    41, 38, 24, 33, 33, 19, 29, 29, 76, 20, 22, 38, 29, 50, 57, 21, 18, 37, 37, 22, 17, 27, 21, 34, 58, 81, 23, 5, 6, 69, 99, 91, 69, 21, 99, 90, 4, 1, 11, 7, 55, 34, 90, 22
  ),
  Mean = c(
    0.35, 0.18, 0.16, 0.46, 0.25, 0.19, 0.47, 0.24, 0.56, 0.24, 0.17, 0.22, 0.2, 0.37, 0.43, 0.17, 0.18, 0.24, 0.37, 0.99, 0.19, 0.11, 0.26, 0.28, 0.73, 0.82, 0.28, 0.11, 0.02, 0.65, 0.98, 0.99, 0.9, 0.21, 0.92, 0.74, 0.08, 0.12, 0.05, 0.08, 0.57, 0.47, 0.75, 0.42
  )
)

temp_runoffcoeff$runoffcoeff_se <- (predict(
  with(Schueler.table, lm(Mean ~ PercentImpervious)), 
  newdata = data.frame(PercentImpervious = temp_runoffcoeff$PctImp2011Cat), 
  se.fit = TRUE
)$se.fit) / sqrt(nrow(Schueler.table))

runoffcoeff <- temp_runoffcoeff %>% select(c("comid", "runoffcoeff", "runoffcoeff_se"))
runoffcoeff <- distinct(runoffcoeff) # Removes duplicates

## uncertainty in other loads ####

# (SC for StreamCat)
temp_inc_tn_dat_SC <- foreach(i = 1:length(temp_inc_tn_dat)) %do% {
  temp_inc_tn_dat[[i]][
    temp_inc_tn_dat[[i]]$comid %in% streamcat_subset_tn[[i]]$comid, 
  ] %>%
    mutate(across(.fns = ~ replace_na(., replace = 0)))
}

temp_inc_tn_dat_other <- foreach(i = 1:length(temp_inc_tn_dat)) %do% {
  temp_inc_tn_dat[[i]][
    !(temp_inc_tn_dat[[i]]$comid %in% streamcat_subset_tn[[i]]$comid), 
  ] %>%
    mutate(across(.fns = ~ replace_na(., replace = 0)))
}

temp_inc_tp_dat_SC <- foreach(i = 1:length(temp_inc_tp_dat)) %do% {
  temp_inc_tp_dat[[i]][
    temp_inc_tp_dat[[i]]$comid %in% streamcat_subset_tp[[i]]$comid, 
  ] %>%
    mutate(across(.fns = ~ replace_na(., replace = 0)))
}

temp_inc_tp_dat_other <- foreach(i = 1:length(temp_inc_tp_dat)) %do% {
  temp_inc_tp_dat[[i]][
    !(temp_inc_tp_dat[[i]]$comid %in% streamcat_subset_tp[[i]]$comid), 
  ] %>%
    mutate(across(.fns = ~ replace_na(., replace = 0)))
}

param_other_loads_tn <- foreach(i = 1:length(temp_inc_tn_dat_other)) %do% {
  list(
    param = sum(temp_inc_tn_dat_SC[[i]]$in_other) + 
      sum(
        if(nrow(temp_inc_tn_dat_other[[i]]) > 0) {
          temp_inc_tn_dat_other[[i]][
            , c("in_poin", "in_urb", "in_ag", "in_other")
          ]
        } else {c(0)},
        na.rm = TRUE
      ),
    se = sqrt(
      sum(temp_inc_tn_dat_SC[[i]]$sin_other ^ 2) + 
        sum(
          if(nrow(temp_inc_tn_dat_other[[i]]) > 0) {
            temp_inc_tn_dat_other[[i]][
              , c("sin_poin", "sin_urb", "sin_ag", "sin_other")
            ] ^ 2
          } else {0},
          na.rm = TRUE
        )
    )
  )
}


param_other_loads_tp <- foreach(i = 1:length(temp_inc_tp_dat_other)) %do% {
  list(
    param = sum(temp_inc_tp_dat_SC[[i]]$ip_other) + 
      sum(
        if(nrow(temp_inc_tp_dat_other[[i]]) > 0) {
          temp_inc_tp_dat_other[[i]][
            , c("ip_poin", "ip_urb", "ip_ag", "ip_other")
          ]
        } else {c(0)},
        na.rm = TRUE
      ),
    se = sqrt(
      sum(temp_inc_tp_dat_SC[[i]]$sip_other ^ 2) + 
        sum(
          if(nrow(temp_inc_tp_dat_other[[i]]) > 0) {
            temp_inc_tp_dat_other[[i]][
              , c("sip_poin", "sip_urb", "sip_ag", "sip_other")
            ] ^ 2
          } else {0},
          na.rm = TRUE
        )
    )
  )
}

## format baseline loading se ####

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  inc_tn_dat_se <- foreach(i = 1:length(temp_inc_tn_dat_SC)) %do% {
    temp_inc_tn_dat_opt <- temp_inc_tn_dat_SC[[i]]
    temp_inc_tn_dat_opt$comid_form <- paste0(
      "'", temp_inc_tn_dat_opt$comid, "'"
    )
    temp_inc_tn_dat_opt %>% 
      select(c("comid_form", "sin_poin", "sin_urb", "sin_ag")) %>%
      mutate(across(.cols = -comid_form, .fns = ~replace_na(., 0)))
  }
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  inc_tp_dat_se <- foreach(i = 1:length(temp_inc_tp_dat_SC)) %do% {
    temp_inc_tp_dat_opt <- temp_inc_tp_dat_SC[[i]]
    temp_inc_tp_dat_opt$comid_form <- paste0(
      "'", temp_inc_tp_dat_opt$comid, "'"
    )
    temp_inc_tp_dat_opt %>% 
      select(c("comid_form", "sip_poin", "sip_urb", "sip_ag")) %>%
      mutate(across(.cols = -comid_form, .fns = ~replace_na(., 0)))
  }
}

## format runoff coeff ####
temp_runoffcoeff_dat <- runoffcoeff[
  runoffcoeff$comid %in% streamcat_subset_all$comid,
]
temp_runoffcoeff_dat <- temp_runoffcoeff_dat[order(temp_runoffcoeff_dat$comid),]
temp_runoffcoeff_dat$comid_form <- paste0("'", temp_runoffcoeff_dat$comid, "'")
runoffcoeff_dat_se <- temp_runoffcoeff_dat %>% 
  select(c("comid_form", "runoffcoeff_se"))

## uncertainty in riparian loadings ####

riparian.loadings_tn <- foreach(i = 1:length(upstream.comids_tn)) %do% {
  riparian.loadings %>% 
    filter(comid %in% upstream.comids_tn[[i]]) %>%
    select(comid, N_riparian_kgyr, N_riparian_kgyr_se) %>%
    right_join(
      ., 
      sparrow_cons_out_tn %>% 
        filter(comid %in% upstream.comids_tn[[i]]) %>% 
        select(comid, in., sin), 
      by = "comid"
    ) %>%
    mutate(
      N_riparian_kgyr = case_when(
        N_riparian_kgyr > in. ~ in., 
        is.na(N_riparian_kgyr) ~ 0, 
        TRUE ~ N_riparian_kgyr
      ),
      N_riparian_kgyr_se = case_when(
        N_riparian_kgyr > in. ~ sin,
        is.na(N_riparian_kgyr_se) ~ 0,
        TRUE ~ N_riparian_kgyr_se
      )
    ) %>%
    select(-in., -sin)
}

riparian.loadings_tp <- foreach(i = 1:length(upstream.comids_tp)) %do% {
  riparian.loadings %>% 
    filter(comid %in% upstream.comids_tp[[i]]) %>%
    select(comid, P_riparian_kgyr, P_riparian_kgyr_se) %>%
    right_join(
      ., 
      sparrow_cons_out_tp %>% 
        filter(comid %in% upstream.comids_tp[[i]]) %>% 
        select(comid, ip, sip), 
      by = "comid"
    ) %>%
    mutate(
      P_riparian_kgyr = case_when(
        P_riparian_kgyr > ip ~ ip, 
        is.na(P_riparian_kgyr) ~ 0, 
        TRUE ~ P_riparian_kgyr
      ),
      P_riparian_kgyr_se = case_when(
        P_riparian_kgyr > ip ~ sip,
        is.na(P_riparian_kgyr_se) ~ 0,
        TRUE ~ P_riparian_kgyr_se
      )
    )  %>%
    select(-ip, -sip)
}

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  temp_riparian_tn_dat <- foreach(i = 1:length(upstream.comids_tn)) %do% {
    
    
    temp_riparian_tn_dat_tmp <- merge(
      riparian.loadings_tn[[i]],
      delfrac_rev_tn[[i]],
      by = "comid",
      all = TRUE
    ) %>%
      rowwise() %>%
      mutate(
        in_riparian = N_riparian_kgyr * delfrac_rev,
        sin_riparian = case_when(
          N_riparian_kgyr > 0 & delfrac_rev > 0 ~ my_propogateerror(
            vals = list(c(N_riparian_kgyr, N_riparian_kgyr_se), c(delfrac_rev, se_delfrac_rev)), 
            method = "mult"
          ),
          N_riparian_kgyr <= 0 | delfrac_rev <= 0 ~ 0
        )
      ) %>%
      arrange(comid) %>%
      select(comid, in_riparian, sin_riparian) %>%
      ungroup()
    
  }
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  temp_riparian_tp_dat <- foreach(i = 1:length(upstream.comids_tp)) %do% {
    
    
    temp_riparian_tp_dat_tmp <- merge(
      riparian.loadings_tp[[i]],
      delfrac_rev_tp[[i]],
      by = "comid",
      all = TRUE
    ) %>%
      rowwise() %>%
      mutate(
        ip_riparian = P_riparian_kgyr * delfrac_rev,
        sip_riparian = case_when(
          P_riparian_kgyr > 0 & delfrac_rev > 0 ~ my_propogateerror(
            vals = list(
              c(P_riparian_kgyr, P_riparian_kgyr_se), 
              c(delfrac_rev, se_delfrac_rev)
            ), 
            method = "mult"
          ),
          P_riparian_kgyr <= 0 | delfrac_rev <= 0 ~ 0
        )
      )%>%
      arrange(comid) %>%
      select(comid, ip_riparian, sip_riparian) %>%
      ungroup()
    
  }
}

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  riparian_tn_dat <- foreach(i = 1:length(temp_riparian_tn_dat)) %do% {
    
    temp_riparian_tn_dat[[i]] %>%
      filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      select(comid_form, in_riparian, sin_riparian)
    
  }
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  riparian_tp_dat <- foreach(i = 1:length(temp_riparian_tp_dat)) %do% {
    
    temp_riparian_tp_dat[[i]] %>%
      filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      select(comid_form, ip_riparian, sip_riparian)
    
  }
}

if(length(RiparianBuffer_BMPs) > 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
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
          filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
          select(
            -contains("P_"),
            -!contains(short.form.bmp.name),
            -!contains(as.character(UserSpecs_bufferwidth_nearest[j])),
            -!contains("uncertainty"),
            comid
          ) %>%
          rename_with(.fn = ~"Curve.Form", .cols = contains("N")) %>%
          mutate(
            x = RiparianBuffer_Widths[j],
            expression = strsplit(
              str_extract(
                mapply(
                  gsub,
                  pattern = "x", 
                  replacement = x,
                  x = mapply(
                    gsub,
                    pattern = "'", 
                    replacement = "", 
                    x = Curve.Form
                  )
                ), 
                pattern = "(?<=c[(]).*(?=[)])"
              ), 
              ","
            )
          ) %>%
          rowwise() %>%
          mutate(
            iter = 1,
            effic = list(
              lapply(X = lapply(X = expression, FUN = my_parse), FUN = eval)
            ),
            effic_mean = suppressWarnings(mean(unlist(effic))),
            effic_sd = sd(unlist(effic))
          ) %>%
          select(comid, effic_mean, effic_sd) %>%
          rename_with(
            .fn = ~paste0(RiparianBuffer_BMPs[j], "_", str_split(., "_")[[2]]),
            .cols = contains("effic")
          ) %>%
          ungroup()
      }
      
      riparian_buffer_efficiencies_N_tmp %>%
        mutate(
          across(
            .cols = contains(RiparianBuffer_BMPs),
            .fn = ~case_when(is.na(.) ~ -999, !is.na(.) ~ .))
          
        )
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
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
          filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
          select(
            -contains("N_"),
            -!contains(short.form.bmp.name),
            -!contains(as.character(UserSpecs_bufferwidth_nearest[j])),
            -!contains("uncertainty"),
            comid
          ) %>%
          rename_with(.fn = ~"Curve.Form", .cols = contains("P")) %>%
          mutate(
            x = RiparianBuffer_Widths[j],
            expression = strsplit(
              str_extract(
                mapply(
                  gsub,
                  pattern = "x", 
                  replacement = x,
                  x = mapply(
                    gsub,
                    pattern = "'", 
                    replacement = "", 
                    x = Curve.Form
                  )
                ), 
                pattern = "(?<=c[(]).*(?=[)])"
              ), 
              ","
            )
          ) %>%
          rowwise() %>%
          mutate(
            iter = 1,
            effic = list(
              lapply(X = lapply(X = expression, FUN = my_parse), FUN = eval)
            ),
            effic_mean = suppressWarnings(mean(unlist(effic))),
            effic_sd = sd(unlist(effic))
          ) %>%
          select(comid, effic_mean, effic_sd) %>%
          rename_with(
            .fn = ~paste0(RiparianBuffer_BMPs[j], "_", str_split(., "_")[[2]]),
            .cols = contains("effic")
          ) %>%
          ungroup()
      }
      
      riparian_buffer_efficiencies_P_tmp %>%
        mutate(
          across(
            .cols = contains(RiparianBuffer_BMPs),
            .fn = ~case_when(is.na(.) ~ -999, !is.na(.) ~ .))
          
        )
    }
  }
  
  invisible(
    foreach(i = 1:length(riparian_buffer_efficiencies_N)) %do% {
      riparian_buffer_efficiencies_N[[i]] <- riparian_buffer_efficiencies_N[[i]] %>%
        mutate(across(contains("sd"), list(se = ~ . / sqrt(10)))) %>%
        select(
          comid, contains(c("Grassed_Buffer", "Forested_Buffer"))
        ) %>%
        rename_with(
          .fn = ~gsub(., pattern = "sd_se", replacement = "se"), 
          .cols = contains("sd_se")
        )
    }
  )
  
  
  invisible(
    foreach(i = 1:length(riparian_buffer_efficiencies_P)) %do% {
      riparian_buffer_efficiencies_P[[i]] <- riparian_buffer_efficiencies_P[[i]] %>%
        mutate(across(contains("sd"), list(se = ~ . / sqrt(10)))) %>%
        select(
          comid, contains(c("Grassed_Buffer", "Forested_Buffer"))
        ) %>%
        rename_with(
          .fn = ~gsub(., pattern = "sd_se", replacement = "se"), 
          .cols = contains("sd_se")
        )
    }
  )
  
  ## uncertainty in loading per bankft ####
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    riparian_loadingperbankft_N <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      tmp1 <- left_join(
        riparian.loadings_tn[[i]], riparian.existingbuffer, by = "comid"
      ) %>%
        select(
          comid, 
          N_riparian_kgyr, 
          N_riparian_kgyr_se, 
          totalbanklength_ft, 
          totalbanklength_ft_se
        ) %>%
        filter(!is.na(totalbanklength_ft)) %>%
        rowwise() %>%
        mutate(
          loading_per_bankft_kg_ftyr = N_riparian_kgyr / totalbanklength_ft,
          loading_per_bankft_kg_ftyr_se = if(
            N_riparian_kgyr <= 0
          ) {0} else {
            my_propogateerror(
              vals = list(
                c(N_riparian_kgyr, N_riparian_kgyr_se), 
                c(totalbanklength_ft, totalbanklength_ft_se)
              ), 
              method = "div"
            )
          }
        ) %>%
        ungroup()
      tmp1
      
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    riparian_loadingperbankft_P <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      tmp1 <- left_join(
        riparian.loadings_tp[[i]], riparian.existingbuffer, by = "comid"
      ) %>%
        select(
          comid, 
          P_riparian_kgyr, 
          P_riparian_kgyr_se, 
          totalbanklength_ft, 
          totalbanklength_ft_se
        ) %>%
        filter(!is.na(totalbanklength_ft)) %>%
        rowwise() %>%
        mutate(
          loading_per_bankft_kg_ftyr = P_riparian_kgyr / totalbanklength_ft,
          loading_per_bankft_kg_ftyr_se = if(
            P_riparian_kgyr == 0
          ) {0} else {
            my_propogateerror(
              vals = list(
                c(P_riparian_kgyr, P_riparian_kgyr_se), 
                c(totalbanklength_ft, totalbanklength_ft_se)
              ), 
              method = "div"
            )
          }
        ) %>%
        ungroup()
      
      tmp1
      
    }
  }
  
  ## riparianbufferremoval un-revised uncertainty ####
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    riparian_buffer_removal_N_tmp <- foreach(i = 1:length(riparian_loadingperbankft_N)) %do% {
      
      tmp2 <- riparian_loadingperbankft_N[[i]]%>%
        left_join(., riparian_buffer_efficiencies_N[[i]], by = "comid") %>%
        pivot_longer(
          cols = contains(RiparianBuffer_BMPs), 
          names_to = c("RiparianBMP", NA, "ValueType"), 
          names_sep = "_",
          values_to = "Value"
        ) %>%
        pivot_wider(
          id_cols = c(
            "comid",
            "RiparianBMP", 
            'loading_per_bankft_kg_ftyr', 
            "loading_per_bankft_kg_ftyr_se"
          ), 
          names_from = "ValueType", 
          values_from = "Value"
        ) %>%
        mutate(across(where(is.numeric), ~replace_na(., -999))) %>%
        rowwise() %>%
        mutate(
          removal = case_when(
            effic == -999 ~ -999, effic >= 0 ~ (effic  * loading_per_bankft_kg_ftyr)
          ),
          removal_se = case_when(
            removal == -999 ~ 0, 
            removal >= 0 ~ if(
              loading_per_bankft_kg_ftyr == 0 | effic == 0
            ) {0} else {
              my_propogateerror(
                vals = list(
                  c(effic, se), 
                  c(loading_per_bankft_kg_ftyr, loading_per_bankft_kg_ftyr_se)
                ),
                method = "mult"
              )
            }
          )
        ) %>%
        ungroup()# %>% pivot_wider(id_cols = c("comid"), names_from = "RiparianBMP", values_from = c("removal", "sd"))
      
      tmp2 
      
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    riparian_buffer_removal_P_tmp <- foreach(i = 1:length(riparian_loadingperbankft_P)) %do% {
      
      tmp2 <- riparian_loadingperbankft_P[[i]]%>%
        left_join(., riparian_buffer_efficiencies_P[[i]], by = "comid") %>%
        pivot_longer(
          cols = contains(RiparianBuffer_BMPs), 
          names_to = c("RiparianBMP", NA, "ValueType"), 
          names_sep = "_",
          values_to = "Value"
        ) %>%
        pivot_wider(
          id_cols = c(
            "comid",
            "RiparianBMP", 
            'loading_per_bankft_kg_ftyr', 
            "loading_per_bankft_kg_ftyr_se"
          ), 
          names_from = "ValueType", 
          values_from = "Value"
        ) %>%
        mutate(across(where(is.numeric), ~replace_na(., -999))) %>%
        rowwise() %>%
        mutate(
          removal = case_when(
            effic == -999 ~ -999, effic >= 0 ~ (effic  * loading_per_bankft_kg_ftyr)
          ),
          removal_se = case_when(
            removal == -999 ~ 0, 
            removal >= 0 ~ if(
              loading_per_bankft_kg_ftyr == 0 | effic == 0
            ) {0} else {
              my_propogateerror(
                vals = list(
                  c(effic, se), 
                  c(loading_per_bankft_kg_ftyr, loading_per_bankft_kg_ftyr_se)
                ),
                method = "mult"
              )
            }
          )
        ) %>%
        ungroup() # %>% pivot_wider(id_cols = c("comid"), names_from = "RiparianBMP", values_from = c("removal", "sd"))
      
      tmp2 
      
    }
  }
  
  ## riparian buffer removal ####
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    riparian_buffer_removal_N <- foreach(
      i = 1:length(riparian_buffer_removal_N_tmp)
    ) %do% {
      riparian_buffer_removal_N_tmp[[i]] %>%
        pivot_wider(
          id_cols = c("comid"), 
          names_from = "RiparianBMP", 
          values_from = c("removal", "removal_se")
        )%>%
        rename_with(
          .fn = ~gsub(., pattern = "removal_se", replacement = "se"), 
          .cols = contains("removal_se")
        )
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    riparian_buffer_removal_P <- foreach(
      i = 1:length(riparian_buffer_removal_P_tmp)
    ) %do% {
      riparian_buffer_removal_P_tmp[[i]] %>%
        pivot_wider(
          id_cols = c("comid"), 
          names_from = "RiparianBMP", 
          values_from = c("removal", "removal_se")
        )%>%
        rename_with(
          .fn = ~gsub(., pattern = "removal_se", replacement = "se"), 
          .cols = contains("removal_se")
        )
    }
  }
  
  ## revised riparianbufferremoval uncertainty ####
  
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    temp_riparian_tn_removal <- foreach(i = 1:length(upstream.comids_tn)) %do% {
      
      
      temp_riparian_tn_removal_tmp <- merge(
        riparian_buffer_removal_N[[i]],
        delfrac_rev_tn[[i]],
        by = "comid",
        all = TRUE
      ) %>%
        pivot_longer(
          cols = -c(comid, delfrac_rev, se_delfrac_rev), 
          names_to = c("ValueType","RiparianBMP"), 
          names_sep = "_",
          values_to = "Value"
        ) %>%
        mutate(RiparianBMP = paste0(RiparianBMP, "_Buffer")) %>%
        pivot_wider(
          id_cols = c(
            "comid",
            "RiparianBMP", 
            "delfrac_rev",
            "se_delfrac_rev"
          ), 
          names_from = "ValueType", 
          values_from = "Value"
        ) %>%
        mutate(across(where(is.numeric), ~replace_na(., -999))) %>%
        rowwise() %>%
        mutate(
          removal = case_when(
            removal == -999 ~ -999, removal >= 0 ~ (removal * delfrac_rev)
          ),
          se = if(removal == 0 | delfrac_rev == 0) {0} else {
            case_when(
              removal == -999 ~ 0, 
              removal > 0 ~ my_propogateerror(
                vals = list(
                  c(removal, se), 
                  c(delfrac_rev, se_delfrac_rev)
                ),
                method = "mult"
              )
            )
          }
        ) %>%
        pivot_wider(id_cols = c("comid"), names_from = "RiparianBMP", values_from = c("removal", "se")) %>%
        arrange(comid) %>%
        rename_with(
          .fn = ~gsub(., pattern = "removal_", replacement = ""), 
          .cols = contains("removal")
        ) %>%
        select(comid, contains(RiparianBuffer_BMPs)) %>%
        ungroup()
      
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    temp_riparian_tp_removal <- foreach(i = 1:length(upstream.comids_tp)) %do% {
      
      
      temp_riparian_tp_removal_tmp <- merge(
        riparian_buffer_removal_P[[i]],
        delfrac_rev_tp[[i]],
        by = "comid",
        all = TRUE
      ) %>%
        pivot_longer(
          cols = -c(comid, delfrac_rev, se_delfrac_rev), 
          names_to = c("ValueType","RiparianBMP"), 
          names_sep = "_",
          values_to = "Value"
        ) %>%
        mutate(RiparianBMP = paste0(RiparianBMP, "_Buffer")) %>%
        pivot_wider(
          id_cols = c(
            "comid",
            "RiparianBMP", 
            "delfrac_rev",
            "se_delfrac_rev"
          ), 
          names_from = "ValueType", 
          values_from = "Value"
        ) %>%
        mutate(across(where(is.numeric), ~replace_na(., -999))) %>%
        rowwise() %>%
        mutate(
          removal = case_when(
            removal == -999 ~ -999, removal >= 0 ~ (removal * delfrac_rev)
          ),
          se = if(removal == 0 | delfrac_rev == 0) {0} else {
            case_when(
              removal == -999 ~ -999, 
              removal >= 0 ~ my_propogateerror(
                vals = list(
                  c(removal, se), 
                  c(delfrac_rev, se_delfrac_rev)
                ),
                method = "mult"
              )
            )
          }
        ) %>%
        pivot_wider(id_cols = c("comid"), names_from = "RiparianBMP", values_from = c("removal", "se")) %>%
        arrange(comid) %>%
        rename_with(
          .fn = ~gsub(., pattern = "removal_", replacement = ""), 
          .cols = contains("removal")
        ) %>%
        select(comid, contains(RiparianBuffer_BMPs)) %>%
        ungroup()
      
    }
  }
  
  riparian_tn_removal_se <- foreach(i = 1:length(temp_riparian_tn_removal)) %do% {
    
    temp_riparian_tn_removal[[i]] %>%
      filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      select(comid_form, contains("se_")) %>%
      rename_with(.fn = ~gsub("se_", "", .), .cols = -comid_form) %>%
      mutate(across(.cols = any_of(RiparianBuffer_BMPs), .fns = ~case_when(. == -999 ~ 0, . >= 0 ~ .)))
    
  }
  
  riparian_tp_removal_se <- foreach(i = 1:length(temp_riparian_tp_removal)) %do% {
    
    temp_riparian_tp_removal[[i]] %>%
      filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      select(comid_form, contains("se_")) %>%
      rename_with(.fn = ~gsub("se_", "", .), .cols = -comid_form) %>%
      mutate(across(any_of(RiparianBuffer_BMPs), ~case_when(. == -999 ~ 0, . >= 0 ~ .)))
    
  }
}
## urban area uncertainty ####

# Select urban area and incremental area from SPARROW input data
temp_sparrow_area <- sparrow_in %>%
  select(c("comid","IncAreaKm2","urban_km2")) %>%
  mutate(cv_numpixels_nlcd = 0.17) %>% # Wickham et al 2017 report overall 83% accuracy in level II data (including all categories, such as Med Development, High Development, etc.)
  mutate(cv_km2_nlcd = cv_numpixels_nlcd) %>%
  mutate(urban_km2_se = urban_km2 * cv_km2_nlcd) %>%
  mutate(uncertainty_boundary_wdb_m = 12.2) %>% #The 8-, 10- and 12-digit hydrologic units must be delineated from and georeferenced to a minimum horizontal accuracy of 1:24,000 scale in the United States, except for Alaska at 1:63,360 scale and 1:25,000 scale in the Caribbean, to meet the National Standard for Spatial Data Accuracy (Federal Geographic Data Committee, 1998b). For example, to quantify 1:24,000-scale horizontal accuracy as it applies to the delineation of WBD-compliant hydrologic units, a hydrologic unit boundary must fall within a buffer of 40 feet or 12.2 meters of a well-defined point on a 1:24,000-scale topographic map. Geospatial positioning accuracy standards are defined and stated in documents by the Federal Geographic Data Committee (1998b).<https://pubs.usgs.gov/tm/11/a3/pdf/tm11-a3_4ed.pdf>
  mutate(
    uncertainty_wdb_boundary_length_m = my_propogateerror( #  error in each direction for the start & end point
      list(
        c(1, uncertainty_boundary_wdb_m), 
        c(1, uncertainty_boundary_wdb_m),
        c(1, uncertainty_boundary_wdb_m),
        c(1, uncertainty_boundary_wdb_m)
      ), 
      method = "addsub"
    ),
    approx_wdb_boundary_length_m = sqrt(IncAreaKm2 * 1000000), # Assume a square
  ) %>%
  rowwise() %>%
  mutate(
    uncertainty_wdb_area_m2 = if(
      approx_wdb_boundary_length_m == 0
    ) {0} else { my_propogateerror(
      vals = list(
        c(approx_wdb_boundary_length_m, uncertainty_wdb_boundary_length_m),
        c(approx_wdb_boundary_length_m, uncertainty_wdb_boundary_length_m)
      ),
      method = "mult"
    )
    },
    IncAreaKm2_se = uncertainty_wdb_area_m2 / 1000000
  ) %>%
  ungroup()

km2_to_ac <- 1/247.105

temp_sparrow_area$urban_ac <- temp_sparrow_area$urban_km2/km2_to_ac #*#
temp_sparrow_area$urban_ac_se <- temp_sparrow_area$urban_km2_se/km2_to_ac #*#
temp_sparrow_area$inc_ac <- temp_sparrow_area$IncAreaKm2/km2_to_ac #*#
temp_sparrow_area$inc_ac_se <- temp_sparrow_area$IncAreaKm2_se/km2_to_ac #*#

## uncertainty cropland area ####

# Select percentage of incremental area that is cropland
streamcat_crop <- cropland %>% select(c("comid","PctCrop2011Cat")) %>%
  mutate(cv_numpixels_nlcd = 0.17) %>% # Wickham et al 2017 report overall 83% accuracy in level II data (including all categories, such as Med Development, High Development, etc.)
  mutate(PctCrop2011Cat_se = PctCrop2011Cat * cv_numpixels_nlcd)

temp_area <- merge(
  temp_sparrow_area, streamcat_crop,by=c("comid"),all.y=TRUE
) %>%
  mutate(
    ag_ac = (PctCrop2011Cat/100)*inc_ac
  ) %>%
  rowwise() %>%
  mutate(
    ag_ac_se = case_when(
      PctCrop2011Cat > 0 & inc_ac > 0 ~ my_propogateerror(
        list(
          c(PctCrop2011Cat/100, PctCrop2011Cat_se/100),
          c(inc_ac, inc_ac_se)
        ),
        method = "mult"
      ),
      PctCrop2011Cat <= 0 | inc_ac <= 0 ~ 0
    )
  ) %>%
  ungroup()

## Format area uncertainty ####
area <- temp_area %>% 
  select(c("comid","urban_ac","ag_ac", "urban_ac_se", "ag_ac_se")) %>%
  group_by(comid) %>%
  summarize(across(.fns = ~mean(.))) # Removes duplicates

temp_area_dat <- area[area$comid %in% reaches_all$comid,]
temp_area_dat <- temp_area_dat[order(temp_area_dat$comid),]
temp_area_dat$comid_form <- paste0("'", temp_area_dat$comid, "'")
area_dat <- temp_area_dat %>% 
  select(c("comid_form", "urban_ac", "ag_ac", "urban_ac_se", "ag_ac_se"))

## find standard error of AG & riparian costs over years ####

ag_costs_yearly <- read.csv(file = paste0(InPath, "EQIPcosts_overyears.csv"))

bmp_costs_se <- ag_costs_yearly %>%
  pivot_longer(
    cols = -c(BMP_Category, BMP, capital_units, operations_units), 
    names_to = c("costtype", "State", "Year"), 
    names_sep = "_", 
    values_to = "Value"
  ) %>%
  group_by(
    BMP_Category, BMP, State, costtype, capital_units, operations_units
  ) %>%
  summarize(mean = mean(Value), se = (sd(Value) / sqrt(4)), .groups = "keep") %>%
  pivot_wider(
    id_cols = c(BMP_Category, BMP, capital_units, operations_units), 
    names_from = c(costtype, State), 
    values_from = se
  ) %>%
  ungroup()

## adjust ag & riparian units ####

temp_bmp_costs <- merge(
  user_specs_BMPs %>%
    filter(BMP_Selection == "X") %>%
    select(
      BMP_Category, BMP, contains(c("capital", "operations")), UserSpec_RD_in
    ),
  bmp_costs_se,
  by = c("BMP_Category", "BMP", "capital_units", "operations_units"),
  suffixes = c("_Estimate", "_se")
)


# Convert area-specific costs to costs per acre for ag BMPs and to costs per linear foot for Riparian BMPs

temp_bmp_costs_rev <- temp_bmp_costs %>%
  mutate(
    across(
      .cols = c(contains(c("capital_", "operations_")) & !contains("units")), 
      .fns = ~replace_na(., NA_real_)
    ),
    across(
      .cols = c(contains(c("capital_", "operations_")) & !contains("units")), 
      .fns = ~case_when(
        BMP_Category == "ag" ~ case_when(
          capital_units == "ft2" ~ . * ft2_to_ac,
          capital_units == "km2" ~ . * km2_to_ac,
          capital_units == "yd2" ~ . * yd2_to_ac,
          capital_units == "ac" ~ .
        ),
        BMP_Category == "urban" ~ .,
        BMP_Category == "point" ~ .,
        BMP_Category == "ripbuf" ~ case_when(
          capital_units == "ft2" ~ . * UserSpec_RD_in,
          capital_units == "km2" ~ . * km2_to_ac / ft2_to_ac * UserSpec_RD_in,
          capital_units == "yd2" ~ . * yd2_to_ac / ft2_to_ac * UserSpec_RD_in,
          capital_units == "ac" ~ . / ft2_to_ac * UserSpec_RD_in
        )
      ), 
      .names = "{.col}_rev"
    )
  ) %>%
  rename_with(., .fn = ~gsub("_Estimate", "", .), .cols = everything())


## annualize costs ####
bmp_costs <- temp_bmp_costs_rev %>% 
  select(
    c(
      "BMP_Category",
      "BMP",
      contains(c("capital_", "operations_")) & !contains("units") & 
        contains("rev") & !contains("_se")
    )
  ) %>%
  rename(
    category = BMP_Category,
    bmp = BMP
  ) %>%
  rename_at(
    vars(contains(c("capital_", "operations_"))), list( ~ gsub("_rev", "", .))
  ) %>%
  # Annualize capital costs based on planning horizon and interest rate
  mutate(
    across(
      .cols = contains("capital_"), 
      .fns = ~(
        . * (
          interest_rate * ((1 + interest_rate) ^ horizon) / (
            ((1 + interest_rate) ^ horizon) - 1
          )
        )
      )
    )
  ) %>%
  mutate(
    across(
      .fns = ~case_when(
        category == "point" ~ replace_na(., 0), category != "point" ~ .
      )
    )
  )

bmp_costs_se <- temp_bmp_costs_rev %>% 
  select(
    c(
      "BMP_Category",
      "BMP",
      contains(c("capital_", "operations_")) & !contains("units") & 
        contains("rev") & contains("_se")
    )
  ) %>%
  rename(
    category = BMP_Category,
    bmp = BMP
  ) %>%
  rename_at(
    vars(contains(c("capital_", "operations_"))), list( ~ gsub("_se_rev", "", .))
  ) %>%
  # Annualize capital costs based on planning horizon and interest rate
  mutate(
    across(
      .cols = contains("capital_"), 
      .fns = ~(
        . * (
          interest_rate * ((1 + interest_rate) ^ horizon) / (
            ((1 + interest_rate) ^ horizon) - 1
          )
        )
      )
    )
  ) %>%
  mutate(
    across(
      .fns = ~case_when(
        category == "point" ~ replace_na(., 0), category != "point" ~ .
      )
    )
  )

## Separate parameters for ag_capital_se and ag_operations_se and format costs data ####


if(length(Ag_BMPs) > 0) {
  temp_bmp_costs_ag_se <- merge(
    bmp_costs_se %>%
      filter(category == "ag") %>%
      select(
        bmp, contains(c("capital", "operations"))
      ),
    COMID_State
  )  %>%
    mutate(
      capital = my_key_fun(., "State", ~paste0("capital_", .x)),
      operations = my_key_fun(., "State", ~paste0("operations_", .x))
    ) %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))#*#
  
  if(
    any(is.na(temp_bmp_costs_ag_se$capital)) | 
    any(is.na(temp_bmp_costs_ag_se$operations))
  ) {
    stop(
      paste0(
        "Uncertainty information for ag ",
        if(
          any(is.na(temp_bmp_costs_ag_se$capital)) & 
          any(is.na(temp_bmp_costs_ag_se$operations))
        ) {paste("capital and operations ")} else if (
          any(is.na(temp_bmp_costs_ag_se$capital))
        ) {paste("capital ")} else {paste("operations ")},
        "costs for ", 
        paste(
          temp_bmp_costs_ag_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(bmp) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " in ",
        paste(
          temp_bmp_costs_ag_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(State) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " not provided. Please ensure costs are available in `EQIPcosts_overyears.csv`"
      )
    )
  }
  bmp_costs_ag_capital_se <- reshape2::dcast(
    temp_bmp_costs_ag_se[, c("comid", "comid_form", "bmp", "capital")], 
    comid_form + comid ~ bmp,
    value.var = "capital"
  )
  bmp_costs_ag_operations_se <- reshape2::dcast(
    temp_bmp_costs_ag_se[, c("comid", "comid_form", "bmp", "operations")],
    comid_form + comid ~ bmp,
    value.var = "operations"
  )
  bmp_costs_ag_capital_rev_se <- bmp_costs_ag_capital_se[
    order(bmp_costs_ag_capital$comid),
  ]
  ag_costs_cap_dat_se <- bmp_costs_ag_capital_rev_se[
    ,names(bmp_costs_ag_capital_rev_se) != "comid"
  ]
  ag_costs_cap_dat_se <- ag_costs_cap_dat_se %>% select(comid_form, everything())
  
  bmp_costs_ag_operations_rev_se <- bmp_costs_ag_operations_se[
    order(bmp_costs_ag_operations_se$comid),
  ]
  ag_costs_op_dat_se <- bmp_costs_ag_operations_rev_se[ 
    ,names(bmp_costs_ag_operations_rev_se) != "comid"
  ]
  ag_costs_op_dat_se <- ag_costs_op_dat_se %>% select(comid_form, everything())
}


## Format riparian costs ####
if(length(RiparianBuffer_BMPs) > 0) {
  temp_bmp_costs_ripbuf_se <- merge(
    bmp_costs_se %>%
      filter(category == "ripbuf") %>%
      select(
        bmp, contains(c("capital", "operations"))
      ),
    COMID_State
  ) %>%
    mutate(
      capital = my_key_fun(., "State", ~paste0("capital_", .x)),
      operations = my_key_fun(., "State", ~paste0("operations_", .x))
    ) %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  if(
    any(is.na(temp_bmp_costs_ripbuf_se$capital)) | 
    any(is.na(temp_bmp_costs_ripbuf_se$operations))
  ) {
    stop(
      paste0(
        "Uncertainty information for Riparian Buffer ",
        if(
          any(is.na(temp_bmp_costs_ripbuf_se$capital)) & 
          any(is.na(temp_bmp_costs_ripbuf_se$operations))
        ) {paste("capital and operations ")} else if (
          any(is.na(temp_bmp_costs_ripbuf_se$capital))
        ) {paste("capital ")} else {paste("operations ")},
        "costs for ", 
        paste(
          temp_bmp_costs_ripbuf_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(bmp) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " in ",
        paste(
          temp_bmp_costs_ripbuf_se %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(State) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " not provided. Please ensure costs are available in `EQIPcosts_overyears.csv`"
      )
    )
  }
  
  bmp_costs_ripbuf_capital_se <- reshape2::dcast(
    temp_bmp_costs_ripbuf_se[, c("comid", "comid_form", "bmp", "capital")], 
    comid_form + comid ~ bmp,
    value.var = "capital"
  )
  bmp_costs_ripbuf_operations_se <- reshape2::dcast(
    temp_bmp_costs_ripbuf_se[, c("comid", "comid_form", "bmp", "operations")],
    comid_form + comid ~ bmp,
    value.var = "operations"
  )
  
  bmp_costs_ripbuf_capital_rev_se <- bmp_costs_ripbuf_capital_se[
    order(bmp_costs_ripbuf_capital_se$comid),
  ]
  ripbuf_costs_cap_dat_se <- bmp_costs_ripbuf_capital_rev_se[
    ,names(bmp_costs_ripbuf_capital_rev_se) != "comid"
  ]
  ripbuf_costs_cap_dat_se <- ripbuf_costs_cap_dat_se %>% 
    select(comid_form, everything())
  
  bmp_costs_ripbuf_operations_rev_se <- bmp_costs_ripbuf_operations_se[
    order(bmp_costs_ripbuf_operations_se$comid),
  ]
  ripbuf_costs_op_dat_se <- bmp_costs_ripbuf_operations_rev_se[ 
    ,names(bmp_costs_ripbuf_operations_rev_se) != "comid"
  ]
  ripbuf_costs_op_dat_se <- ripbuf_costs_op_dat_se %>% select(comid_form, everything())
  
}

## ACRES data - choose ####

temp_acre <- if(AgBMPcomparison == "No Practice") {
  fread(paste0(InPath, "ACRE_HUC12_HRU_Summary_compareNoPractice.csv"))
} else if(AgBMPcomparison == "Baseline") {
  fread(paste0(InPath, "ACRE_HUC12_HRU_Summary_compareBaseline.csv"))
} else {
  stop(
    'AgBMPcomparison must be set to either "No Practice" or "Baseline", quotation marks included.'
  )
}#*#

# Read in efficiency data for ACRE database BMPs
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

temp_acre_cast_tn_se <- reshape2::dcast(
  temp_acre, HUC8_Rev+HUC10_Rev+HUC12_Rev ~ bmp, value.var = "MeanTN_Effic_se"
) %>% 
  {
    if(AgBMPcomparison == "Baseline") {select(., -"No Practice")} else if(
      AgBMPcomparison == "No Practice"
    ) {select(., -"Baseline")} else {
      stop(
        'AgBMPcomparison must be set to either "No Practice" or "Baseline", quotation marks included.'
      )
    } 
  }%>%
  rename(HUC8 = HUC8_Rev, HUC10 = HUC10_Rev, HUC12 = HUC12_Rev)

temp_acre_cast_tn_HUC8_se <- temp_acre_cast_tn_se %>%
  filter(is.na(HUC12) & is.na(HUC10))

temp_acre_cast_tn_HUC10_se <- temp_acre_cast_tn_se %>%
  filter(is.na(HUC12) & is.na(HUC8))

temp_acre_cast_tn_HUC12_se <- temp_acre_cast_tn_se %>%
  filter(!is.na(HUC12), !is.na(HUC10), !is.na(HUC8))

temp_acre_reaches_HUC12_tn_se <- merge(
  reaches_huc12_tn[, c("comid", "HUC12")],
  temp_acre_cast_tn_HUC12_se[, c("HUC12", ACRE_BMPs)],
  by = "HUC12",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC12")))

temp_acre_reaches_HUC10_tn_se <- merge(
  reaches_huc12_tn[, c("comid", "HUC10")],
  temp_acre_cast_tn_HUC10_se[, c("HUC10", ACRE_BMPs)],
  by = "HUC10",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC10")))

temp_acre_reaches_HUC8_tn_se <- merge(
  reaches_huc12_tn[, c("comid", "HUC8")],
  temp_acre_cast_tn_HUC8_se[, c("HUC8", ACRE_BMPs)],
  by = "HUC8",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC8")))

temp_acre_reaches_tn_se <- merge(
  merge(
    merge(
      reaches_huc12_tn, 
      temp_acre_reaches_HUC12_tn_se, 
      by = c("comid", "HUC12"),
      all.x = TRUE
    ),
    temp_acre_reaches_HUC10_tn_se,
    by = c("comid", "HUC10"),
    all.x = TRUE
  ),
  temp_acre_reaches_HUC8_tn_se,
  by = c("comid", "HUC8"),
  all.x = TRUE
)
temp_acre_reaches_tn_se[ , ACRE_BMPs] <- NA

acre_reaches_tn_se <- temp_acre_reaches_tn_se %>%
  mutate(
    across(
      all_of(ACRE_BMPs), 
      ~ case_when(
        !is.na(temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC12")]]) ~ 
          temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC12")]],
        !is.na(temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC10")]]) ~ 
          temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC10")]],
        !is.na(temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC8")]]) ~ 
          temp_acre_reaches_tn_se[[paste0(cur_column(), "_HUC8")]]
      )
    )
  ) %>%
  select(comid, all_of(ACRE_BMPs))

temp_acre_cast_tp_se <- reshape2::dcast(
  temp_acre, HUC8_Rev+HUC10_Rev+HUC12_Rev ~ bmp, value.var = "MeanTP_Effic_se"
) %>% 
  {
    if(AgBMPcomparison == "Baseline") {select(., -"No Practice")} else if(
      AgBMPcomparison == "No Practice"
    ) {select(., -"Baseline")} else {
      stop(
        'AgBMPcomparison must be set to either "No Practice" or "Baseline", quotation marks included.'
      )
    }
  }%>%
  rename(HUC8 = HUC8_Rev, HUC10 = HUC10_Rev, HUC12 = HUC12_Rev)

temp_acre_cast_tp_HUC8_se <- temp_acre_cast_tp_se %>%
  filter(is.na(HUC12) & is.na(HUC10))

temp_acre_cast_tp_HUC10_se <- temp_acre_cast_tp_se %>%
  filter(is.na(HUC12) & is.na(HUC8))

temp_acre_cast_tp_HUC12_se <- temp_acre_cast_tp_se %>%
  filter(!is.na(HUC12), !is.na(HUC10), !is.na(HUC8))

temp_acre_reaches_HUC12_tp_se <- merge(
  reaches_huc12_tp[, c("comid", "HUC12")],
  temp_acre_cast_tp_HUC12_se[, c("HUC12", ACRE_BMPs)],
  by = "HUC12",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC12")))

temp_acre_reaches_HUC10_tp_se <- merge(
  reaches_huc12_tp[, c("comid", "HUC10")],
  temp_acre_cast_tp_HUC10_se[, c("HUC10", ACRE_BMPs)],
  by = "HUC10",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC10")))

temp_acre_reaches_HUC8_tp_se <- merge(
  reaches_huc12_tp[, c("comid", "HUC8")],
  temp_acre_cast_tp_HUC8_se[, c("HUC8", ACRE_BMPs)],
  by = "HUC8",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC8")))

temp_acre_reaches_tp_se <- merge(
  merge(
    merge(
      reaches_huc12_tp, 
      temp_acre_reaches_HUC12_tp_se, 
      by = c("comid", "HUC12"),
      all.x = TRUE
    ),
    temp_acre_reaches_HUC10_tp_se,
    by = c("comid", "HUC10"),
    all.x = TRUE
  ),
  temp_acre_reaches_HUC8_tp_se,
  by = c("comid", "HUC8"),
  all.x = TRUE
)
temp_acre_reaches_tp_se[ , ACRE_BMPs] <- NA

acre_reaches_tp_se <- temp_acre_reaches_tp_se %>%
  mutate(
    across(
      all_of(ACRE_BMPs), 
      ~ case_when(
        !is.na(temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC12")]]) ~ 
          temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC12")]],
        !is.na(temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC10")]]) ~ 
          temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC10")]],
        !is.na(temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC8")]]) ~ 
          temp_acre_reaches_tp_se[[paste0(cur_column(), "_HUC8")]]
      )
    )
  ) %>%
  select(comid, all_of(ACRE_BMPs))

## Fert20 & ManureInjection ####

# Read in efficiency data for Fert_20 and Manure_Injection BMPs
temp_ag_effic_fert_man <- fread(
  paste(InPath, "AgBMPEffic_FertManure.csv", sep = "")
) #*#

temp_ag_effic_fert_man_se <- data.frame(
  Category = temp_ag_effic_fert_man$Category,
  BMP = temp_ag_effic_fert_man$BMP, 
  N_Efficiency = c(0.14, 0.14), 
  P_Efficiency = c(NA_real_, 0.14)
)

temp_ag_effic_fert_man_cast_tn_se <- reshape2::dcast(
  temp_ag_effic_fert_man_se, Category ~ BMP, value.var = "N_Efficiency"
) #*#
temp_ag_effic_fert_man_cast_tp_se <- reshape2::dcast(
  temp_ag_effic_fert_man_se, Category ~ BMP, value.var = "P_Efficiency"
) #*#

## Combine ACRE bmps with Manure_Injection and Fert_20 BMP-specific efficiencies ####


ag_effic_bycomid_tn_se <- add_column(
  acre_reaches_tn_se, temp_ag_effic_fert_man_cast_tn_se[-c(1)]
) %>% 
  select(comid, all_of(Ag_BMPs)) %>%
  mutate(across(all_of(Ag_BMPs), ~ replace_na(., 0))) 

ag_effic_bycomid_tp_se <- add_column(
  acre_reaches_tp_se, temp_ag_effic_fert_man_cast_tp_se[-c(1)]
) %>% 
  select(comid, all_of(Ag_BMPs)) %>%
  mutate(across(all_of(Ag_BMPs), ~ replace_na(., 0))) 

## format ag effic data ####

temp_ag_effic_dat_tn_se <- ag_effic_bycomid_tn_se[
  ag_effic_bycomid_tn_se$comid %in% unlist(streamcat_subset_tn, use.names = FALSE),
] %>%
  arrange(comid) %>%
  mutate(comid_form = paste0("'", comid, "'"))

ag_effic_dat_tn_se <- temp_ag_effic_dat_tn_se %>% 
  select(-comid) %>% 
  select(comid_form, everything()) 

temp_ag_effic_dat_tp_se <- ag_effic_bycomid_tp_se[
  ag_effic_bycomid_tp_se$comid %in% unlist(streamcat_subset_tp, use.names = FALSE),
] %>%
  arrange(comid) %>%
  mutate(comid_form = paste0("'", comid, "'"))

ag_effic_dat_tp_se <- temp_ag_effic_dat_tp_se %>% 
  select(-comid) %>% 
  select(comid_form, everything())

## get urban efficiency ses ####

if(length(Urban_BMPs) > 0) { 
  temp_urban_effic <-  user_specs_BMPs[
    user_specs_BMPs$BMP_Category == "urban" & user_specs_BMPs$BMP_Selection == "X",
    c("BMP_Category", "BMP", "Min_RD_in", "Max_RD_in", "UserSpec_RD_in")
  ] 
  
  urban_effic <- merge(
    temp_urban_effic, urban.effic.curves, by = "BMP"
  ) %>%
    mutate(
      expression = gsub(
        ' x', ' UserSpec_RD_in ', gsub('y ~ ', '', Best.Fit.Curve)
      )
    ) %>%
    rowwise() %>%
    mutate(iter = 1, effic = eval(parse(text = expression))) %>%
    mutate(
      standarderror = case_when(
        expression == "Coef.1 + Coef.2 * log( UserSpec_RD_in  )" ~ 
          my_propogateerror(
            vals = list(
              c(Coef.1, Coef.1_se),
              c(
                Coef.2 * log(UserSpec_RD_in), 
                case_when(
                  Coef.2 > 0 & log(UserSpec_RD_in) > 0 ~ my_propogateerror(
                    vals = list(c(Coef.2, Coef.2_se), c(log(UserSpec_RD_in), 0)),
                    method = "mult"
                  ),
                  Coef.2 == 0 | log(UserSpec_RD_in) == 0 ~ 0
                )
              )
            ), 
            method = "addsub"
          ),
        expression == 
          "Coef.1 + Coef.2 * ( UserSpec_RD_in  ^2) + Coef.3 * UserSpec_RD_in " ~ 
          my_propogateerror(
            vals = list(
              c(Coef.1, Coef.1_se),
              c(
                Coef.2 * (UserSpec_RD_in ^ 2), 
                case_when(
                  Coef.2 != 0 & UserSpec_RD_in ^ 2 != 0 ~ my_propogateerror(
                    vals = list(
                      c(Coef.2, Coef.2_se), 
                      c(
                        (UserSpec_RD_in^2),
                        my_propogateerror(
                          vals = list(c(UserSpec_RD_in, 0), c(UserSpec_RD_in, 0)),
                          method = "mult"
                        )
                      )
                    ),
                    method = "mult"
                  ),
                  Coef.2 == 0 | UserSpec_RD_in ^ 2 == 0 ~ 0
                )
              ),
              c(
                Coef.3 * UserSpec_RD_in, 
                case_when(
                  Coef.3 != 0 & UserSpec_RD_in != 0 ~  my_propogateerror(
                    vals = list(c(Coef.3, Coef.3_se), c(UserSpec_RD_in, 0)), 
                    method = "mult"
                  ),
                  Coef.3 == 0 | UserSpec_RD_in == 0 ~ 0
                )
              )
            ),
            method = "addsub"
          ),
        expression == 
          "Coef.1 + Coef.2 * (1 - exp(Coef.3 * UserSpec_RD_in  ))" ~ my_propogateerror(
            vals = list(
              c(Coef.1, Coef.1_se), 
              c(
                Coef.2 * (1 - exp(Coef.3 * UserSpec_RD_in)), 
                case_when(
                  Coef.2 != 0 & (1 - exp(Coef.3 * UserSpec_RD_in)) != 0 ~
                    my_propogateerror(
                      vals = list(
                        c(Coef.2, Coef.2_se), 
                        c(
                          (1 - exp(Coef.3 * UserSpec_RD_in)), 
                          my_propogateerror(
                            vals = list(
                              c(1,0), 
                              c(
                                exp(Coef.3 * UserSpec_RD_in), 
                                exp(Coef.3 * UserSpec_RD_in) * UserSpec_RD_in * Coef.3_se
                              )
                            ), 
                            method = "addsub"
                          )
                        )
                      ), 
                      method = "mult"
                    ),
                  Coef.2 == 0 | (1 - exp(Coef.3 * UserSpec_RD_in)) == 0 ~ 0
                )
              )
            ),
            method = "addsub"
          ),
        expression == "Coef.1 + Coef.2 * UserSpec_RD_in " ~ my_propogateerror(
          vals = list(
            c(Coef.1, Coef.1_se), 
            c(
              Coef.2 * UserSpec_RD_in, 
              case_when(
                Coef.2 != 0 & UserSpec_RD_in != 0 ~ my_propogateerror(
                  vals = list(c(Coef.2, Coef.2_se), c(UserSpec_RD_in, 0)),
                  method = "mult"
                ),
                Coef.2 == 0 | UserSpec_RD_in == 0 ~ 0
              )
            )
          ), 
          method = "addsub"
        )
      )
    ) %>%
    filter(BMP %in% user_specs_BMPs$BMP[user_specs_BMPs$BMP_Selection == "X"]) %>%
    select(
      category = BMP_Category, Pollutant,  bmp = BMP, effic, se = standarderror, InfiltrationRate_inperhr
    ) %>%
    group_by(bmp) %>%
    pivot_wider(
      id_cols = c(InfiltrationRate_inperhr, Pollutant), names_from = bmp, values_from = c(effic, se)
    ) %>%
    add_row(InfiltrationRate_inperhr = 0, Pollutant = c("N", "P")) %>%
    mutate(
      across(
        contains(
          c("Infiltration_Basin", 
            "Infiltration_Chamber",
            "Infiltration_Trench", 
            "Porous_Pavement_w_subsurface_infiltration")
        ), 
        ~ case_when(InfiltrationRate_inperhr == 0 ~ 0, TRUE ~ .)
      )
    ) %>%
    group_by(Pollutant) %>%
    fill(contains(Urban_BMPs)) %>%
    ungroup()
  
  urban_effic.n <- urban_effic %>% filter(Pollutant == "N")
  urban_effic.p <- urban_effic %>% filter(Pollutant == "P")
  
  
  ## distribution of matched infiltration rates ####
  comid.infiltrationrates.matched <- comid.infiltrationrates %>%
    rename(
      InfiltrationRate_inperhr = infiltrationrate_inperhr,
      InfiltrationRate_inperhr_dist = infiltrationrate_inperhr_dist
    ) %>%
    mutate(
      InfiltrationRate_inperhr = custom.round(
        x = InfiltrationRate_inperhr, 
        breaks = c(0, 0.17, 0.27, 0.52, 1.02, 2.41, 8.27)  # NH BMPs calculated at 0.17 in/hr, 0.27 in/hr, 0.52 in/hr, 1.02 in/hr, 2.41 in/hr and 8.27 in/hr
      )
    ) %>%
    rowwise() %>%
    mutate(
      InfiltrationRate_inperhr_dist = list(
        custom.round(
          x = strsplit(
            str_extract(
              gsub(
                pattern = "'", 
                replacement = "", 
                x = InfiltrationRate_inperhr_dist
              ), 
              pattern = "(?<=c[(]).*(?=[)])"
            ), 
            ","
          )[[1]], 
          breaks = c(0, 0.17, 0.27, 0.52, 1.02, 2.41, 8.27)  # NH BMPs calculated at 0.17 in/hr, 0.27 in/hr, 0.52 in/hr, 1.02 in/hr, 2.41 in/hr and 8.27 in/hr
        )
      )
    ) %>%
    ungroup()
  
  ## urban effic by comid ####
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    urban_effic_bycomid_tn <- merge(
      comid.infiltrationrates.matched, 
      data.frame(comid = unlist(streamcat_subset_tn, use.names = FALSE)), 
      by = 'comid',
      all = TRUE
    ) %>%
      full_join(., urban_effic.n, by = "InfiltrationRate_inperhr") %>%
      filter(comid %in% unlist(streamcat_subset_tn, use.names = FALSE)) %>%
      rowwise() %>%
      mutate(
        across(
          .cols = any_of(
            c(
              "se_Infiltration_Basin",
              "se_Infiltration_Chamber",
              "se_Infiltration_Trench",
              "se_Porous_Pavement_w_subsurface_infiltration"
            )
          ),
          .fns = ~ (
            sd(
              unlist(
                urban_effic.n[[
                  paste0(
                    "effic_",
                    gsub(pattern = "se_", replacement = "", x = cur_column())
                  )
                ]][
                  unlist(
                    lapply(
                      X = InfiltrationRate_inperhr_dist, 
                      FUN = match,
                      table = urban_effic.n$InfiltrationRate_inperhr
                    )
                  )
                ]
              )
            ) / sqrt(10)
          )
        )
      ) %>%
      ungroup() %>%
      mutate(across(contains("se_"), .fns = ~ replace_na(., replace = mean(., na.rm = TRUE))))
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    urban_effic_bycomid_tp <- merge(
      comid.infiltrationrates.matched, 
      data.frame(comid = unlist(streamcat_subset_tp, use.names = FALSE)), 
      by = 'comid',
      all = TRUE
    ) %>%
      full_join(., urban_effic.p, by = "InfiltrationRate_inperhr") %>%
      filter(comid %in% unlist(streamcat_subset_tp, use.names = FALSE)) %>%
      rowwise() %>%
      mutate(
        across(
          .cols = any_of(
            c(
              "se_Infiltration_Basin",
              "se_Infiltration_Chamber",
              "se_Infiltration_Trench",
              "se_Porous_Pavement_w_subsurface_infiltration"
            )
          ),
          .fns = ~ (
            sd(
              unlist(
                urban_effic.p[[
                  paste0(
                    "effic_",
                    gsub(pattern = "se_", replacement = "", x = cur_column())
                  )
                ]][
                  unlist(
                    lapply(
                      X = InfiltrationRate_inperhr_dist, 
                      FUN = match,
                      table = urban_effic.p$InfiltrationRate_inperhr
                    )
                  )
                ]
              )
            ) / sqrt(10)
          )
        )
      ) %>%
      ungroup() %>%
      mutate(across(contains("se_"), .fns = ~ replace_na(., replace = mean(., na.rm = TRUE))))
  }
  
  ## Format urban BMP efficiency data ####
  
  temp_urban_effic_dat_tn_se <- urban_effic_bycomid_tn[
    urban_effic_bycomid_tn$comid %in% 
      unlist(streamcat_subset_tn, use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  urban_effic_dat_tn_se <- temp_urban_effic_dat_tn_se %>% 
    select(-comid) %>% 
    select(comid_form, contains("se_")) %>%
    rename_at(vars(contains("se_")), list( ~gsub("se_", "", .)))
  
  temp_urban_effic_dat_tp_se <- urban_effic_bycomid_tp[
    urban_effic_bycomid_tp$comid %in% 
      unlist(streamcat_subset_tp, use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  urban_effic_dat_tp_se <- temp_urban_effic_dat_tp_se %>% 
    select(-comid) %>% 
    select(comid_form, contains("se_")) %>%
    rename_at(vars(contains("se_")), list( ~gsub("se_", "", .)))
  
  ##  Define urban cost correction coefficients ####
  # These are based on the type of urban land. The costs given in UserSpecs are for new development. 
  # Retrofitting is multiplied by a factor of 2 and difficult retrofit by a factor of 3.
  # Open and low density development we assume will be new development, med density will be retrofits, and high density will be difficult retrofits.
  
  urban_cost_coeffs_se <- cropland %>%
    filter(comid %in% unlist(streamcat_subset_all)) %>%
    select(comid, PctUrbOp2011Cat, PctUrbLo2011Cat, PctUrbMd2011Cat, PctUrbHi2011Cat) %>%
    filter(!duplicated(comid)) %>% # was 'unique()', but that passed some corruption at the 15th decimal place and included extra lines in the dataset
    rowwise() %>%
    mutate(
      total_urban = PctUrbOp2011Cat + 
        PctUrbLo2011Cat + 
        PctUrbMd2011Cat + 
        PctUrbHi2011Cat,
      urban_cost_coef_se = case_when(
        total_urban == 0 ~ 0,
        total_urban > 0 ~ sd(
          c(sample(
            x = c(
              rep(
                x = 1,
                length.out = floor(
                  ((PctUrbOp2011Cat + PctUrbLo2011Cat) / total_urban) * 100
                )
              ) ,
              rep(x = 2, length.out = floor((PctUrbMd2011Cat / total_urban) * 100)) ,
              rep(x = 3, length.out = floor((PctUrbHi2011Cat / total_urban) * 100))
            ),
            size = 10,
            replace = TRUE
          )
          )
        ) / sqrt(10)
      )
    ) %>%
    select(comid, urban_cost_coef_se) %>%
    ungroup()
  
  # Format Urban Cost Coefficient Error Data
  urban_cost_coeffs_dat_se <- urban_cost_coeffs_se %>%
    arrange('comid') %>%
    mutate(comid_form = paste0("'", comid, "'")) %>%
    select(comid_form, urban_cost_coef_se)
  
}
# Write Command Script ####

cat(
  " 
#WMOST Optimization Screening Tool AMPL command file with uncertainty

option display_transpose -10000;
",
  file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
  sep = "\n"
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0("\ndisplay loads_lim_N", i, ";"),
        file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
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
        paste0("\ndisplay loads_lim_P", i, ";"),
        file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

cat(
  "
for {s in Scenarios} {

if s > 1
  then {
  ", 
  file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
  sep = "", 
  append = T
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0("\nlet loads_lim_N", i, ":= loads_lim_N", i, " * ", scenarioincrement, ";"),
        file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
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
        paste0("\nlet loads_lim_P", i, ":= loads_lim_P", i, " * ", scenarioincrement, ";"),
        file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

cat(
  "
  };
solve;

for {b in Bootstraps} {
",
  file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
  sep = "", 
  append = T
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0(
          "\nrepeat { \nlet other_loads_N", 
          i, 
          "_rev[b] := Normal(other_loads_N", 
          i, 
          ", ", 
          param_other_loads_tn[[i]]$se,
          "); \n } until other_loads_N",
          i,
          "_rev[b] >= 0;"
        ),
        file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
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
        paste0(
          "\nrepeat { \nlet other_loads_P", 
          i, 
          "_rev[b] := Normal(other_loads_P", 
          i, 
          ", ", 
          param_other_loads_tp[[i]]$se,
          "); \n } until other_loads_P",
          i,
          "_rev[b] >= 0;"
        ),    file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

cat(
  "
	let agcost_frac_rev[b] := Uniform(1,5/3);
  
  for {u in urban_bmp} {
	  for {o in cost_type} {
	      let urban_costs_rev[u,o,b] := urban_costs[u,o];
	     };
	 };
	 ",    
  file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
  sep = "", 
  append = T
)


invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0(
          "\nfor {c in comid_N", 
          i, 
          "} {\nfor {l in loads_all} {\nrepeat {\nlet  baseloads_N", 
          i, 
          "_rev[c,l,b] := Normal(baseloads_N", 
          i,
          "[c,l], baseloads_N",
          i,
          "_se[c,l]);\n} until baseloads_N",
          i,
          "_rev[c,l,b] >= 0;\n};\nfor {r in ripbuf_bmp} {\nlet riparianremoval_N",
          i,
          "_rev[c,r,b] := Normal(riparianremoval_N",
          i,
          "[c,r], riparianremoval_N",
          i,
          "_se[c,r]);\n  };\n};"
        ),
        file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "
for {c in comid_all_N} {
  for {a in ag_bmp} {
    repeat {
      let ag_effic_N_rev[c,a,b] := Normal(ag_effic_N[c,a], ag_effic_N_se[c,a]);
    } until ag_effic_N_rev[c,a,b] <= 1;
  };
  for {p in point_c} {
    let point_effic_N_rev[c,p,b] := Normal(point_effic_N[c,p], abs(point_effic_N[c,p]) * 0.2);
  };
  for {u in urban_bmp} {
    repeat {
      let urban_effic_N_rev[c,u,b] := Normal(urban_effic_N[c,u], urban_effic_N_se[c,u]);
    } until urban_effic_N_rev[c,u,b] <= 1;
  };
};
",    
    file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
    sep = "", 
    append = T
  )
}

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0(
          "\nfor {c in comid_P", 
          i, 
          "} {\nfor {l in loads_all} {\nrepeat {\nlet  baseloads_P", 
          i, 
          "_rev[c,l,b] := Normal(baseloads_P", 
          i,
          "[c,l], baseloads_P",
          i,
          "_se[c,l]);\n} until baseloads_P",
          i,
          "_rev[c,l,b] >= 0;\n};\nfor {r in ripbuf_bmp} {\nlet riparianremoval_P",
          i,
          "_rev[c,r,b] := Normal(riparianremoval_P",
          i,
          "[c,r], riparianremoval_P",
          i,
          "_se[c,r]);\n  };\n};"
        ),
        file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "
for {c in comid_all_P} {
  for {a in ag_bmp} {
    repeat {
      let ag_effic_P_rev[c,a,b] := Normal(ag_effic_P[c,a], ag_effic_P_se[c,a]);
    } until ag_effic_P_rev[c,a,b] <=1;
  };
  for {p in point_c} {
    let point_effic_P_rev[c,p,b] := Normal(point_effic_P[c,p], abs(point_effic_P[c,p]) * 0.2);
  };
  for {u in urban_bmp} {
    repeat {
      let urban_effic_P_rev[c,u,b] := Normal(urban_effic_P[c,u], urban_effic_P_se[c,u]);
    } until urban_effic_P_rev[c,u,b] <= 1;
  };
};",
    file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
    sep = "\n",
    append = TRUE
  )
}

cat(
  "
for {c in comid_all} {
  
  for {o in cost_type} {
    repeat {
      let point_costs_rev[c,o,b] := Normal(point_costs[c,o] * 1.15, point_costs[c,o] * 0.15);
    } until point_costs_rev[c,o,b] >= 0;
  };
  
  for {r in ripbuf_bmp} {
    repeat {
      let ripbuf_costs_capital_rev[c,r,b] := Normal(ripbuf_costs_capital[c,r], ripbuf_costs_capital_se[c,r]);
    } until ripbuf_costs_capital_rev[c,r,b] >= 0;
    repeat { 
      let ripbuf_costs_operations_rev[c,r,b] := Normal(ripbuf_costs_operations[c,r], ripbuf_costs_operations_se[c,r]);
    } until ripbuf_costs_operations_rev[c,r,b] >= 0;
  };
  
  for {ar in area_sub} {
    repeat {
      let area_rev[c,ar,b] := Normal(area[c,ar], area_se[c,ar]);
    } until area_rev[c,ar,b] >= 0;
  };
  
  for {a in ag_bmp} {
    repeat {
      let ag_costs_capital_rev[c,a,b] := Normal(ag_costs_capital[c,a], ag_costs_capital_se[c,a]);
    } until ag_costs_capital_rev[c,a,b] >= 0;
    repeat {
      let ag_costs_operations_rev[c,a,b] :=  Normal(ag_costs_operations[c,a], ag_costs_operations_se[c,a]);
    } until ag_costs_operations_rev[c,a,b] >= 0;
  };
  
  repeat {
    let runoff_coeff_urban_rev[c,'urban',b] := Normal(runoff_coeff_urban[c,'urban'], runoff_coeff_urban_se[c,'urban']);
  } until 1 >= runoff_coeff_urban_rev[c,'urban',b] >= 0;

  repeat {
    let urban_cost_adjustment_coef_rev[c,b] := Normal(urban_cost_adjustment_coef[c], urban_cost_adjustment_coef_se[c]);
  } until urban_cost_adjustment_coef_rev[c,b] >= 0;
  
};

};	

option display_precision 10;
display solve_result_num, solve_result;
display cost.result;
display cost;
option display_1col 10000000000;
option omit_zero_rows 1;
option omit_zero_cols 1;

display {b in Bootstraps} (sum {c in comid_all} (ps_coef_rev[c,b] * point_dec[c]) + sum {c in comid_all, u in urban_bmp} (urban_coef_rev[c,u,b] * urban_frac[c,u]) + sum {c in comid_all, a in ag_bmp} (ag_coef_rev[c,a,b] * ag_frac[c,a]) + sum {c in comid_all, r in ripbuf_bmp} (ripbuf_coef_rev[c,r,b] * ripbuf_length[c,r]));
",
  file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
  sep = "\n",
  append = TRUE
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0(
          "\ndisplay {b in Bootstraps}  (other_loads_N", 
          i, 
          "_rev[b] + sum {c in comid_N", 
          i, 
          "} (baseloads_N", 
          i,
          "_rev[c,'ag',b] * (1 - sum {a in ag_bmp}(ag_effic_N_rev[c,a,b] * ag_frac[c,a]))) + sum {c in comid_N",
          i,
          "} (baseloads_N",
          i,
          "_rev[c,'urban',b] * (1 - sum {u in urban_bmp}(urban_effic_N_rev[c,u,b] * urban_frac[c,u]))) + sum {c in comid_N",
          i,
          "} (baseloads_N",
          i,
          "_rev[c,'point',b] * (1 - (point_effic_N_rev[c,'point',b] * point_dec[c]))) - sum {c in comid_N",
          i,
          ", r in ripbuf_bmp} (ripbuf_length[c, r] * riparianremoval_N",
          i,
          "_rev[c, r,b]));"
        ),
        file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
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
        paste0(
          "\ndisplay {b in Bootstraps}  (other_loads_P", 
          i, 
          "_rev[b] + sum {c in comid_P", 
          i, 
          "} (baseloads_P", 
          i,
          "_rev[c,'ag',b] * (1 - sum {a in ag_bmp}(ag_effic_P_rev[c,a,b] * ag_frac[c,a]))) + sum {c in comid_P",
          i,
          "} (baseloads_P",
          i,
          "_rev[c,'urban',b] * (1 - sum {u in urban_bmp}(urban_effic_P_rev[c,u,b] * urban_frac[c,u]))) + sum {c in comid_P",
          i,
          "} (baseloads_P",
          i,
          "_rev[c,'point',b] * (1 - (point_effic_P_rev[c,'point',b] * point_dec[c]))) - sum {c in comid_P",
          i,
          ", r in ripbuf_bmp} (ripbuf_length[c, r] * riparianremoval_P",
          i,
          "_rev[c, r,b]));"
        ),
        file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

cat(
  "
display point_dec;

option display_1col 0;
display ripbuf_length;

option display_width 100000000000;
display urban_frac;
display ag_frac;

}

",
file = paste(OutPath, "STcommand_uncertainty.amp", sep=""),
sep = "\n",
append = TRUE
)

# Write Data Script ####

orig.dat <- read.table(
  file = paste(OutPath, "STdata.dat", sep=""), 
  header = FALSE,
  sep = "\n",
  quote = ""
)

write.table(
  orig.dat, # selecting forces the order incase they are disordered in preprocessing code above
  file = paste(OutPath, "STdata_uncertainty.dat", sep = "") , 
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(inc_tn_dat_se)) %do% {
      write(
        paste0("\nparam baseloads_N", i, "_se : 'point' 'urban' 'ag' :="), 
        file =  paste(OutPath, "STdata_uncertainty.dat", sep = ""),
        append = T
      )
      write.table(
        inc_tn_dat_se[[i]] %>% 
          select(comid_form, point = sin_poin, urban = sin_urb, ag = sin_ag), # renaming also forces the order incase they are disordered in processing code above
        file =  paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file =   paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(inc_tp_dat_se)) %do% {
      write(
        paste0("\nparam baseloads_P", i, "_se : 'point' 'urban' 'ag' :="), 
        file = paste(OutPath, "STdata_uncertainty.dat", sep = ""),
        append = T
      )
      write.table(
        inc_tp_dat_se[[i]] %>% 
          select(comid_form, point = sip_poin, urban = sip_urb, ag = sip_ag), # renaming also forces the order incase they are disordered in processing code above
        file =  paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
      write( ";", file =  paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
    }
  }
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      if(length(RiparianBuffer_BMPs) > 0) {
        write(
          paste0(
            "\nparam riparianremoval_N", 
            i, 
            "_se : ", 
            paste(bmp_ripbuf_vec, collapse  = "  "), 
            " :="
          ), 
          file = paste(OutPath, "STdata_uncertainty.dat", sep = ""),
          append = T
        )
        write.table(
          riparian_tn_removal_se[[i]] %>% 
            select(comid_form, any_of(RiparianBuffer_BMPs)), # renaming also forces the order incase they are disordered in processing code above
          file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
      } else {
        write(
          paste0(
            "\nparam riparianremoval_N", 
            i, 
            "_se : 'none' :="
          ), 
          file = paste(OutPath, "STdata_uncertainty.dat", sep = ""),
          append = T
        )
        write.table(
          ripbuf_bmp_dummy_tn[[i]] %>% 
            select(comid, none), # renaming also forces the order incase they are disordered in processing code above
          file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
      }
      write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
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
            "_se : ", 
            paste(bmp_ripbuf_vec, collapse  = "  "), 
            " :="
          ), 
          file = paste(OutPath, "STdata_uncertainty.dat", sep = ""),
          append = T
        )
        write.table(
          riparian_tp_removal_se[[i]] %>% 
            select(comid_form, any_of(RiparianBuffer_BMPs)), # renaming also forces the order incase they are disordered in processing code above
          file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
      } else {
        write(
          paste0(
            "\nparam riparianremoval_P", 
            i, 
            "_se : 'none' :="
          ), 
          file = paste(OutPath, "STdata_uncertainty.dat", sep = ""),
          append = T
        )
        write.table(
          ripbuf_bmp_dummy_tp[[i]] %>% 
            select(comid, none), # renaming also forces the order incase they are disordered in processing code above
          file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
          append = T,
          sep = "\t",
          row.names = F,
          col.names = F,
          na = "",
          quote = F
        )
      }
      write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
    }
  }
)

if(length(Ag_BMPs) > 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam ag_effic_N_se : ",bmp_ag_vec," :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="",
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      ag_effic_dat_tn_se %>% select(comid_form, all_of(bmp_ag_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam ag_effic_P_se : ",bmp_ag_vec," :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      ag_effic_dat_tp_se %>% select(comid_form, all_of(bmp_ag_vec_direct)),  # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
  }
} else {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam ag_effic_N_se : 'none' :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="",
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", ag_bmp_dummy_tn)) %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam ag_effic_P_se : 'none' :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", ag_bmp_dummy_tp)) %>% select(comid, none),  # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
  }
}

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  if(length(Urban_BMPs) > 0) {
    cat(
      "\nparam urban_effic_N_se : ",bmp_urban_vec," :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(urban_effic_dat_tn_se %>% select(comid_form, all_of(bmp_urban_vec_direct))), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    cat(
      "\nparam urban_effic_N_se : 'none' :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", urban_bmp_dummy_tn) %>% 
               select(comid, none)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  if(length(Urban_BMPs) > 0) {
    cat(
      "\nparam urban_effic_P_se : ",bmp_urban_vec," :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(urban_effic_dat_tp_se %>% select(comid_form, all_of(bmp_urban_vec_direct))), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    cat(
      "\nparam urban_effic_P_se : 'none' :=	", 
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat("\n", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), sep = "", append = T)
    write.table(
      unique(do.call("rbind", urban_bmp_dummy_tp) %>% 
               select(comid, none)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)
}

if(length(RiparianBuffer_BMPs) > 0) {
  cat(
    "\nparam ripbuf_costs_capital_se : ", bmp_ripbuf_vec, " :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_costs_cap_dat_se %>% select(comid_form, all_of(RiparianBuffer_BMPs)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam ripbuf_costs_operations_se : ", bmp_ripbuf_vec," :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_costs_op_dat_se %>% select(comid_form, all_of(RiparianBuffer_BMPs)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
} else {
  cat(
    "\nparam ripbuf_costs_capital_se : 'none' :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_bmp_dummy %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam ripbuf_costs_operations_se : 'none' :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_bmp_dummy %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
}

write( 
  "\n\nparam area_se : 'urban' 'ag' :=", 
  file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
  append = T
)
write.table(
  area_dat %>% select(comid_form, urban = urban_ac_se, ag = ag_ac_se), # renaming also forces the order incase they are disordered in preprocessing code above
  file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
  append = T,
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)
write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)

if(length(Ag_BMPs) > 0) {
  cat(
    "\nparam ag_costs_capital_se : ",bmp_ag_vec," :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ag_costs_cap_dat_se %>% select(comid_form, all_of(bmp_ag_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam ag_costs_operations_se : ",bmp_ag_vec," :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ag_costs_op_dat_se %>% select(comid_form, all_of(bmp_ag_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
} else {
  cat(
    "\nparam ag_costs_capital_se : 'none' :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ag_bmp_dummy %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
  
  cat(
    "\nparam ag_costs_operations_se : 'none' :=	",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""),
    sep = "", 
    append = T
  )
  write.table(
    ag_bmp_dummy %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_uncertainty.dat", sep=""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)
}


write(  
  "\nparam runoff_coeff_urban_se : 'urban' :=", 
  file =  paste(OutPath, "STdata_uncertainty.dat", sep=""), 
  append = T
)
write.table(
  runoffcoeff_dat_se %>% select(comid_form, urban = runoffcoeff_se),  # renaming also forces the order incase they are disordered in preprocessing code above
  file =  paste(OutPath, "STdata_uncertainty.dat", sep=""), 
  append = T,
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)
write( ";", file =  paste(OutPath, "STdata_uncertainty.dat", sep=""), append = T)

write( 
  "\nparam: urban_cost_adjustment_coef_se :=	", 
  file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
  append = T
)
if(length(Urban_BMPs) > 0) {
  write.table(
    urban_cost_coeffs_dat_se,
    file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write.table(
    urban_bmp_dummy,
    file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
}
write( ";", file = paste(OutPath, "STdata_uncertainty.dat", sep = ""), append = T)

# Write Model Script ####

#*# SE: resolves parsing error from original code (5-7-24)
orig.mod <- as.data.frame(readLines(paste(OutPath, "STmodel.mod", sep="")))

write.table(
  orig.mod, # selecting forces the order incase they are disordered in preprocessing code above
  file = paste(OutPath, "STmodel_uncertainty.mod", sep = "") , 
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0(
          "\nparam baseloads_N", 
          i, 
          "_se {comid_N", 
          i, 
          ", loads_all} >= 0;"
        ),
        file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
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
        paste0(
          "\nparam baseloads_P", 
          i, 
          "_se {comid_P", 
          i, 
          ", loads_all} >= 0;"
        ),
        file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      write(
        paste0(
          "\nparam riparianremoval_N", 
          i, 
          "_se {c in comid_N", 
          i, 
          ", r in ripbuf_bmp};"
        ), 
        file =  paste(OutPath, "STmodel_uncertainty.mod", sep=""),
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      write(
        paste0(
          "\nparam riparianremoval_P", 
          i, 
          "_se {c in comid_P", 
          i, 
          ", r in ripbuf_bmp};"
        ), 
        file =  paste(OutPath, "STmodel_uncertainty.mod", sep=""),
        append = T
      )
    }
  }
)

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "
param ag_effic_N_se {comid_all_N ,ag_bmp} >= 0;
param urban_effic_N_se {comid_all_N, urban_bmp} >= 0;",
    file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
    sep = "\n",
    append = TRUE
  )
  
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "
param ag_effic_P_se {comid_all_P ,ag_bmp} >= 0;
param urban_effic_P_se {comid_all_P, urban_bmp} >= 0;",
    file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
    sep = "\n",
    append = TRUE
  )
}
cat(
  "
param ripbuf_costs_capital_se {comid_all, ripbuf_bmp};
param ripbuf_costs_operations_se {comid_all, ripbuf_bmp};

param area_se {comid_all,area_sub} >=0;

param ag_costs_capital_se {comid_all,ag_bmp};
param ag_costs_operations_se {comid_all,ag_bmp};

param runoff_coeff_urban_se {comid_all,urban_imp} >=0;

param urban_cost_adjustment_coef_se {comid_all} >= 0;


",
  file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)

cat(
  paste0("set Scenarios := 1 .. ", n.scenarios, " by 1;"),
  file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)

cat(
  "
set Bootstraps := 1 .. 200 by 1;
",
  file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0(
          "\nparam baseloads_N", 
          i, 
          "_rev {comid_N", 
          i, 
          ", loads_all, Bootstraps};"
        ),
        file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
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
        paste0(
          "\nparam baseloads_P", 
          i, 
          "_rev {comid_P", 
          i, 
          ", loads_all, Bootstraps};"
        ),
        file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "
    param ag_effic_N_rev {comid_all_N ,ag_bmp, Bootstraps};
param point_effic_N_rev {comid_all_N, point_c, Bootstraps};
param urban_effic_N_rev {comid_all_N, urban_bmp, Bootstraps};",
    file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
    sep = "\n",
    append = TRUE
  )
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "
    param ag_effic_P_rev {comid_all_P ,ag_bmp, Bootstraps};
param point_effic_P_rev {comid_all_P, point_c, Bootstraps};
param urban_effic_P_rev {comid_all_P, urban_bmp, Bootstraps};",
    file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
    sep = "\n",
    append = TRUE
  )
}
cat(
  "
param area_rev {comid_all,area_sub, Bootstraps};
param ag_costs_capital_rev {comid_all,ag_bmp, Bootstraps};
param ag_costs_operations_rev {comid_all,ag_bmp, Bootstraps};
param point_costs_rev {comid_all,cost_type, Bootstraps};
param urban_costs_rev {urban_bmp,cost_type, Bootstraps};
param ripbuf_costs_capital_rev {comid_all, ripbuf_bmp, Bootstraps};
param ripbuf_costs_operations_rev {comid_all, ripbuf_bmp, Bootstraps};
param runoff_coeff_urban_rev {comid_all, urban_imp, Bootstraps};
param urban_cost_adjustment_coef_rev {comid_all, Bootstraps};
",
  file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      write(
        paste0(
          "\nparam riparianremoval_N", 
          i, 
          "_rev {c in comid_N", 
          i, 
          ", r in ripbuf_bmp, Bootstraps};"
        ), 
        file =  paste(OutPath, "STmodel_uncertainty.mod", sep=""),
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      write(
        paste0(
          "\nparam riparianremoval_P", 
          i, 
          "_rev {c in comid_P", 
          i, 
          ", r in ripbuf_bmp, Bootstraps};"
        ), 
        file =  paste(OutPath, "STmodel_uncertainty.mod", sep=""),
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
          "\nparam other_loads_N", 
          i, 
          "_rev {Bootstraps};"
        ),
        file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
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
        paste0(
          "\nparam other_loads_P", 
          i, 
          "_rev {Bootstraps};"
        ),   
        file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
        sep = "", 
        append = T
      )
    }
  }
)

cat(
  "
param agcost_frac_rev {Bootstraps} >=0;

param total_loads_N1_rev {b in  Bootstraps} >= 0; 
param total_loads_P1_rev {b in  Bootstraps} >= 0; 
param total_loads_P2_rev {b in  Bootstraps} >= 0; 

param ps_coef_rev {c in comid_all, b in Bootstraps} := point_costs_rev[c,'capital',b] + point_costs_rev[c,'operations',b];
param urban_coef_rev {c in comid_all, u in urban_bmp, b in Bootstraps} := acfttoft3 * pcp * urban_design_depth[u] *  runoff_coeff_urban_rev[c,'urban', b] * area_rev[c,'urban',b] * (urban_costs_rev[u,'capital',b] + urban_costs_rev[u,'operations',b]) * urban_cost_adjustment_coef_rev[c,b];
param ag_coef_rev {c in comid_all, a in ag_bmp, b in Bootstraps} := agcost_frac_rev[b] * area_rev[c,'ag',b] *  (ag_costs_capital_rev[c,a,b] + ag_costs_operations_rev[c,a,b]) ; 
param ripbuf_coef_rev {c in comid_all, r in ripbuf_bmp, b in Bootstraps} := agcost_frac_rev[b] * (ripbuf_costs_capital_rev[c, r,b] +  ripbuf_costs_operations_rev[c, r,b]);


",
  file = paste(OutPath, "STmodel_uncertainty.mod", sep=""),
  sep = "\n",
  append = TRUE
)


print(
  paste(
    "RBEROST has finished writing AMPL scripts with uncertainty at",
    Sys.time()
  )
)

