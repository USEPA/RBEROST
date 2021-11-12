###########################################################################################
# TITLE: Postprocessing Shiny App Server Functions
# PURPOSE: To allow users to input a text file and see the results of their NEOS optimization
# BY: Cathy Chamberlin, earlier versions of the code by Kate Munson and Alyssa Le (ICF)
# DATE: 9/23/2021
###########################################################################################

# This file is NOT intended to be run by itself. It is sourced through Optimization_ServerFile.R. This file contains most of the code necessary for the postprocessor to run as functions.
# Debugging is best done with browser() inserted in reactive contexts that may be causing problems.
# Lines marked #*# may be changed by the user.

# The components of the Server are roughly organized as such:

##################################################
# Table of Contents:
# 1. Upload user-supplied files
# 2. Parse NEOS data file
# 3. Combine NEOS results with other dataframes
# 4. Render Preview UI
# 5. Write Report Outputs
# 6. Render Report UI
##################################################

##################################################
# 1.Upload user-supplied files ####
##################################################

read_csvs_using_baseR <- function(path) {
  tryCatch(
    # when reading semicolon separated files, having a comma separator causes `read.csv` to error. Using read.csv2 causes no error.
    expr = {
      read.csv2(
        file = path, 
        colClasses = "character",
        row.names = NULL,
        header = FALSE,
        blank.lines.skip = FALSE,
        sep = "\n"
      )
    },
    error = function(e) {stop(safeError(error = e))} # return a safeError if a parsing error occurs
  )
}

read_csvs_using_fread <- function(path) {
  tmp <- tryCatch(
    expr = {fread(file = path, fill = TRUE)},
    error = function(e) {stop(safeError(error = e)) }
  )
  
  return(tmp)
}

read_all_streamcat_files <- function(scfiles) {
  tmp <- base::lapply(scfiles$datapath, read_csvs_using_fread)
  names(tmp) <- scfiles$name
  
  return(tmp)
}


##################################################
# 2.Parse NEOS data file ####
##################################################

get_solvestatus <- function(results_astable) {
  
  solved_txt <- "solve_result = "
  solved_rows <-
    results_astable[which(str_detect(results_astable[, 1], solved_txt)), 1]
  
  if(length(solved_rows > 0)) {
    solved_tmp <- strsplit(solved_rows, "\\s+")
    
    solved_tmp2 <- unlist(
      mapply(
        "[[", 
        solved_tmp, 
        index = mapply(
          which, 
          x = lapply(
            FUN = function(x, pattern) {x == pattern}, 
            X = solved_tmp, 
            pattern = "solve_result"
          )
        ) + 2
      ), use.names = FALSE
    )
    
    if ("solved" %in% solved_tmp2 | "infeasible" %in% solved_tmp2) {
      solved <- foreach(i = 1:length(solved_tmp2)) %do% {
        if(
          solved_tmp2[i] == "solved"
        ) {1} else if(
          solved_tmp2[i] == "infeasible"
        ) {2} else {-98}
      }
    } else if(grepl("Error", results_astable)){
      solved <- list(0)
    } else {solved <- list(-98)}
  } else if(grepl("Error", results_astable)){
    solved <- list(0)
  } else if (!grepl("solve_result", results_astable)) {
    solved <- list(-99)
  } else {solved <- list(-98)}
  
  return(solved)
}


get_user_notes <- function(results_astable) {
  notetextstart <- "%% COMMENTS %%"
  notetextend <- "%%%%%%%%%%"
  
  notetextlinestart <- which(str_detect(results_astable[, 1], notetextstart))
  notetextlineend <-  which(substr(results_astable[, 1], 1, 10) == notetextend)
  
  if(!is_empty(notetextlinestart)) {
    UserNotes <- paste(
      results_astable[(notetextlinestart):(notetextlineend),1], 
      collapse = "\n"
    )
    UserNotes2 <- gsub("%%", "", gsub("%% COMMENTS %%", "", UserNotes))
  } else {UserNotes2 <- ""}
  
  return(UserNotes2)
}


get_scen.vis <- function(input, x) {
  if(as.numeric(as.character(input)) <= x){
    tmp <- as.numeric(as.character(input))
  } else {tmp <- 1}
  return(tmp)
}

get_error_display <- function(results_astable) {
  
  display <- results_astable[
    min(
      which(
        grepl(c("Error"), results_astable[, 1]) | 
          grepl(c("error"), results_astable[, 1])
      )
    ):
      nrow(results_astable), 1
  ]
  
  return(display)
}

get_total_cost <- function(results_astable, scenarionumber) {
  
  cost_txt <- "cost = "
  cost_rows <- results_astable[
    which(str_detect(results_astable[, 1], cost_txt)), 1
  ][scenarionumber]
  cost_tmp <- strsplit(cost_rows, " ")
  cost_tmp2 <- unlist(
    suppressWarnings(as.numeric(as.character(lapply(cost_tmp, "[[", 3)))), 
    use.names = FALSE
  )
  total_cost <- cost_tmp2[which(!is.na(cost_tmp2))]
  
  return(total_cost)
}


get_cost_bootstraps <- function(
  results_astable, scenarionumber, endrows, startrows
) {
  
  display_txt <- "sum[{]c in comid_all[}]"
  coef_txt <- "coef"
  
  cost_bootstraps_begin <- which(
    grepl(results_astable[, 1], pattern = display_txt) &
      str_detect(results_astable[, 1], coef_txt)
  )[scenarionumber]
  
  if(length(cost_bootstraps_begin) > 0 & !is.na(cost_bootstraps_begin)) {
    cost_bootstraps_end <- min(endrows[which(endrows > cost_bootstraps_begin)])
    
    if(scenarionumber == 1) {
      cost_bootstraps_tablebegin <- min(
        startrows[which(startrows > cost_bootstraps_begin)]
      )
    } else {
      cost_bootstraps_tablebegin <- cost_bootstraps_begin
    }
    
    if((cost_bootstraps_tablebegin + 1) < (cost_bootstraps_end - 1)) {
      
      cost_bootstraps <- data.frame(
        str_split_fixed(
          parse_character(
            results_astable[
              (cost_bootstraps_tablebegin + 1):(cost_bootstraps_end - 1), 1
            ]
          )
          , " ", n = 2)
      ) %>%
        rename(Bootstrap = 1, Cost = 2) %>%
        mutate(across(.fns = ~as.numeric(as.character(.))))
    }
  } else { cost_bootstraps <- NULL}
  
  
  return(cost_bootstraps)
}

get_cost_uncertainty_message <- function(costs) {
  
  CI95.low <- quantile(costs$Cost, 0.025)
  CI95.high <- quantile(costs$Cost, 0.975)
  
  cost.message <- paste0(
    "Estimates of Total Annualized Cost ranged from ",
    sprintf(
      "$%s",
      formatC(CI95.low, format = "f", big.mark = ",", digits = 2)
    ),
    " to ",
    sprintf(
      "$%s",
      formatC(CI95.high, format = "f", big.mark = ",", digits = 2)
    ),
    ". RBEROST costs are usually most influenced by the amount of retrofitting necessary to install urban BMPs. More retrofitting leads to higher costs."
  )
  
  return(cost.message)
}

get_loading_targets <- function(results_astable) {
  
  loads_lims_lines <- results_astable[
    which(grepl(results_astable[, 1], pattern = "loads_lim_")),1
  ]
  
  targets <- str_extract(
    loads_lims_lines,
    pattern = "(?<=loads_lim_)(.*?)(?= = )"
  )
  
  loads_lims <- data.frame(
    Nutrient = substr(targets, 1,1),
    Number = as.numeric(as.character(substr(targets, 2, nchar(targets)))),
    Limit =  as.numeric(
      as.character(str_extract(loads_lims_lines, pattern = "(?<= = )(.*)"))
    )
  )
  
  return(loads_lims)
}


get_loading_bootstraps <- function(
  results_astable, scenarionumber, x, endrows, startrows
) {
  
  display_txt <- "sum[{]c in comid_"
  load_txt <- "other_loads"
  
  loading_bootstraps_begin_all <- which(
    grepl(results_astable[, 1], pattern = display_txt) &
      str_detect(results_astable[, 1], load_txt)
  )
  loading_bootstraps_begin <- loading_bootstraps_begin_all[
    (length(loading_bootstraps_begin_all) / x * (scenarionumber - 1) + 1):
      (length(loading_bootstraps_begin_all) / x * (scenarionumber))
  ]
  
  if(
    (length(loading_bootstraps_begin) > 0) &
    all(!unlist(lapply(loading_bootstraps_begin, is.na)))
  ) {
    loading_bootstraps <- data.frame(
      foreach(
        i = 1:length(loading_bootstraps_begin), .combine = "rbind"
      ) %do% {
        loading_bootstraps_end <- min(
          endrows[
            which(endrows > loading_bootstraps_begin[i])
          ]
        )
        
        if(scenarionumber == 1) {
          loading_bootstraps_tablebegin <- min(
            startrows[
              which(startrows > loading_bootstraps_begin[i])
            ]
          )
        } else {
          loading_bootstraps_tablebegin <- loading_bootstraps_begin[i]
        }
        
        load.designation <- str_extract(
          results_astable[loading_bootstraps_begin[i],1],
          pattern = "(?<=other_loads_)(.*?)(?=_rev)"
        )
        
        load.nutrient <- substr(load.designation, 1, 1)
        load.number <- as.numeric(
          as.character(substr(load.designation, 2, nchar(load.designation)))
        )
        
        if(
          (loading_bootstraps_tablebegin + 1) < (loading_bootstraps_end - 1)
        ) {
          
          loading_bootstraps.tmp <- data.frame(
            str_split_fixed(
              parse_character(
                results_astable[
                  (loading_bootstraps_tablebegin + 1):
                    (loading_bootstraps_end - 1),
                  1
                ]
              ), " ", n = 2
            )
          ) %>%
            rename(Bootstrap = 1, Load = 2) %>%
            mutate(
              across(.fns = ~as.numeric(as.character(.))),
              Nutrient = load.nutrient,
              Number = load.number
            )
          
        } else {loading_bootstraps.tmp <- NULL}
        
        loading_bootstraps.tmp
      }
    )
    
  } else { loading_bootstraps <- NULL}
  
  
  return(loading_bootstraps)
}

mung_UserSpecs_loadingtargets <- function(loadingtargets) {
  UserSpecs_loadingtargets_munged <- loadingtargets %>%
    mutate(Nutrient = gsub("T", "", TN_or_TP), ticker = 1) %>%
    group_by(Nutrient) %>%
    mutate(Number = cumsum(ticker)) %>%
    select(Waterbody_Name, Percent_Reduction, Nutrient, Number)
  
  return(UserSpecs_loadingtargets_munged)
}

get_loading_uncertainty_message <- function(
  loadings, loadingtargets, loadingtargetsmunged
) {
  
  CI.summary <- loadings %>%
    left_join(., loadingtargets, by = c("Nutrient", "Number")) %>%
    left_join(
      ., loadingtargetsmunged, by = c("Nutrient", "Number")
    ) %>%
    group_by(Nutrient, Number, Limit, Waterbody_Name, Percent_Reduction) %>%
    summarize(
      CI95.low = quantile(Load, 0.025),
      CI95.high = quantile(Load, 0.975),
      prob.met = sum(Load < Limit) / n(),
      .groups = "keep"
    ) %>%
    mutate(
      Target.name = Waterbody_Name,
      LoadingUncertaintyMessage = paste0(
        formatC(CI95.low, format = "f", big.mark = ",", digits = 1),
        " - ",
        formatC(CI95.high, format = "f", big.mark = ",", digits = 1),
        " kg ",
        paste(Nutrient),
        "/yr"
      ),
      Limit.display = paste0(
        signif(Percent_Reduction * 100, 2),
        "% reduction in T",
        Nutrient,
        "; or ",
        formatC(Limit, format = "f", big.mark = ",", digits = 1),
        " kg ",
        paste(Nutrient),
        "/yr"
      )
    ) %>%
    ungroup() %>%
    select(Target.name, Limit.display, LoadingUncertaintyMessage, prob.met)
  
  return(CI.summary)
}


get_point_results <-function(results_astable, scenarionumber, endrows) {
  
  point_txt <- "point_dec [*] :="
  
  point_rows_begin <- which(results_astable[, 1] ==  point_txt)[scenarionumber]
  
  point_rows_end <- min(endrows[which(endrows > point_rows_begin)])
  
  if((point_rows_begin + 1) < (point_rows_end - 1)) {
    point_rows <- results_astable[
      (point_rows_begin + 1):(point_rows_end - 1), 1
    ]
    
    point_tmp <- strsplit(point_rows, "\\s+")
    point_results <- do.call(rbind.data.frame, point_tmp)  %>%
      select(COMID = 2, point_bin = 3)
    
    # Adjust the formatting of the NEOS results
    point_results$COMID_adj <- gsub("'", "", point_results$COMID)
    point_results$COMID_int <- strtoi(point_results$COMID_adj)
    point_results <- point_results %>%
      select(COMID = COMID_int, point_bin)
  } else {
    point_results <- NULL
  }
  
  return(point_results)
}

read_postprocessor_urban_bmp_vec <-function(results_astable, scenarionumber) {
  
  urban_txt <- "urban_frac [*,*]"
  urban_bmp_tmp <- results_astable[
    (which(results_astable[, 1] == urban_txt) + 1), 1
  ][scenarionumber]
  
  if(
    grepl(
      "ag_frac|urban_frac|point_dec|ripbuf_length|NEOS|Presolve", 
      urban_bmp_tmp
    )
  ) {urban_bmp_tmp <- ""}
  
  urban_bmp <- strsplit(urban_bmp_tmp, "\\s+")
  urban_bmp_vec_tmp <- as.vector(unlist(urban_bmp))
  urban_bmp_vec_tmp1 <- urban_bmp_vec_tmp[
    !sapply(urban_bmp_vec_tmp, function(x) all(x == ""))
  ]
  urban_bmp_vec <- urban_bmp_vec_tmp1[-c(1, length(urban_bmp_vec_tmp1))]
  
  return(urban_bmp_vec)
}

parse_urban_results <- function(
  results_astable, scenarionumber, endrows, urbanbmps
) {
  
  urban_txt <- "urban_frac [*,*]"
  
  urban_rows_begin <- which(results_astable[, 1] == urban_txt)[scenarionumber]
  
  if(length(urban_rows_begin) > 0) {
    
    urban_rows_end <- min(endrows[which(endrows > urban_rows_begin)])
    
    urban_rows <- results_astable[
      (urban_rows_begin + 2):(urban_rows_end - 1), 1
    ]
    
    urban_tmp <- strsplit(urban_rows, "\\s+")
    
    urban_results <- do.call(rbind.data.frame, urban_tmp)
    
    names(urban_results) <- c("COMID", urbanbmps)
    
    # Adjust the formatting of the NEOS results
    urban_results$COMID_adj <- gsub("'", "", urban_results$COMID)
    urban_results$COMID_int <- strtoi(urban_results$COMID_adj)
    urban_results <- urban_results %>%
      select(COMID = COMID_int, all_of(urbanbmps))
  } else {urban_results <- NULL}
  
  return(urban_results)
}


read_postprocessor_ag_bmp_vec <- function(results_astable, scenarionumber) {
  
  ag_txt <- "ag_frac [*,*]"
  ag_bmp_tmp <- results_astable[
    (which(results_astable[, 1] == ag_txt) + 1), 1
  ][scenarionumber]
  
  if(
    grepl(
      "ag_frac|urban_frac|point_dec|ripbuf_length|NEOS|Presolve", ag_bmp_tmp
    )
  ) {ag_bmp_tmp <- ""}
  
  ag_bmp <- strsplit(ag_bmp_tmp, "\\s+")
  ag_bmp_vec_tmp <- as.vector(unlist(ag_bmp))
  ag_bmp_vec_tmp1 <- ag_bmp_vec_tmp[
    !sapply(ag_bmp_vec_tmp, function(x) all(x == ""))
  ]
  ag_bmp_vec <- ag_bmp_vec_tmp1[-c(1, length(ag_bmp_vec_tmp1))]
  
  return(ag_bmp_vec)
}

parse_ag_results <- function(results_astable, scenarionumber, endrows, agbmps) {
  
  ag_txt <- "ag_frac [*,*]"
  
  ag_rows_begin <- which(results_astable[, 1] == ag_txt)[scenarionumber]
  
  if(length(ag_rows_begin > 0)) {
    
    ag_rows_end <- min(endrows[which(endrows > ag_rows_begin)])
    
    ag_rows <- results_astable[(ag_rows_begin + 2):(ag_rows_end - 1), 1]
    
    ag_tmp <- strsplit(ag_rows, "\\s+")
    
    ag_results <- do.call(rbind.data.frame, ag_tmp)
    
    names(ag_results) <- c("COMID", agbmps)
    
    ag_results$COMID_adj <- gsub("'", "", ag_results$COMID)
    ag_results$COMID_int <- strtoi(ag_results$COMID_adj)
    ag_results <- ag_results %>%
      select(COMID = COMID_int, all_of(agbmps))
    
  } else {ag_results <- NULL}
  
  return(ag_results)
}


read_postprocessor_ripbuf_bmp_vec <- function(results_astable, scenarionumber) {
  
  ripbuf_txt <- "ripbuf_length [*,*]"
  ripbuf_bmp_tmp <- results_astable[
    (which(results_astable[, 1] == ripbuf_txt) + 1), 1
  ][scenarionumber]
  
  if(
    grepl(
      "ag_frac|urban_frac|point_dec|ripbuf_length|NEOS|Presolve", 
      ripbuf_bmp_tmp
    )
  ) {ripbuf_bmp_tmp <- ""}
  
  ripbuf_bmp <- strsplit(ripbuf_bmp_tmp, "\\s+")
  ripbuf_bmp_vec_tmp <- as.vector(unlist(ripbuf_bmp))
  ripbuf_bmp_vec_tmp1 <- ripbuf_bmp_vec_tmp[
    !sapply(ripbuf_bmp_vec_tmp, function(x) all(x == ""))
  ]
  ripbuf_bmp_vec <- ripbuf_bmp_vec_tmp1[-c(1, length(ripbuf_bmp_vec_tmp1))]
  
  return(ripbuf_bmp_vec)
}


parse_ripbuf_results <- function(
  results_astable, scenarionumber, endrows, riparianbmps
) {
  ripbuf_txt <- "ripbuf_length [*,*]"
  
  ripbuf_rows_begin <- which(results_astable[, 1] == ripbuf_txt)[scenarionumber]
  
  if(length(ripbuf_rows_begin) > 0 & !is.na(ripbuf_rows_begin)) {
    if(length(which(endrows > ripbuf_rows_begin)) > 0) {
      ripbuf_rows_end <- min(
        endrows[which(endrows > ripbuf_rows_begin)]
      )
    } else {ripbuf_rows_end <- ripbuf_rows_begin}
    
    ripbuf_rows <- results_astable[
      (ripbuf_rows_begin + 2):(ripbuf_rows_end - 1), 1
    ]
    
    ripbuf_tmp <- strsplit(ripbuf_rows, "\\s+")
    
    ripbuf_results <- do.call(rbind.data.frame, ripbuf_tmp)
    
    names(ripbuf_results) <- c("COMID", riparianbmps)
    
    ripbuf_results$COMID_adj <- gsub("'", "", ripbuf_results$COMID)
    ripbuf_results$COMID_int <- strtoi(ripbuf_results$COMID_adj)
    ripbuf_results <- ripbuf_results %>%
      select(COMID = COMID_int, all_of(riparianbmps))
  } else {
    ripbuf_results <- NULL
  }
  
  return(ripbuf_results)
}

##################################################
# 3. Combine NEOS results with other dataframes ####
##################################################

make_areadf <- function(sparrowinputs, km2_to_ac, streamcatinputs) {
  comid_huc12 <- sparrowinputs %>%
    rename(comid = 1) %>%
    select(c("comid", "HUC_12", "IncAreaKm2", "urban_km2")) %>%
    mutate(HUC12_Char = str_pad(HUC_12, width = 12, pad = "0")) %>%
    select(COMID = comid,
           HUC_12 = HUC12_Char,
           IncAreaKm2,
           urban_km2)
  
  cropland <- imap_dfr(
    streamcatinputs, ~{return(.x %>% mutate(StateAbbrev = str_sub(.y, 1, 2)))}
  )
  
  streamcat_crop <- cropland %>%
    select(c("COMID", "PctCrop2011Cat", "WsAreaSqKm", "StateAbbrev")) %>%
    mutate(WsAreaAc = WsAreaSqKm / km2_to_ac)
  
  # Calculate available agricultural area
  area <- merge(comid_huc12, streamcat_crop, by = "COMID")
  area <- area[!duplicated(area$COMID),] %>%
    mutate(ag_km2 = (PctCrop2011Cat / 100) * IncAreaKm2,
           COMID = as.character(COMID))
  
  return(area)
}


merge_urban_results <- function(urbanresults, areadf, urbanbmps, km2_to_ac) {
  # Merge the SPARROW and StreamCat data to the urban BMP results
  urban_results_HUCmerge <- merge(urbanresults,
                                  areadf,
                                  by = "COMID",
                                  all.x = TRUE)
  
  # Calculate BMP implementation area (acres) (note: this used to sum by HUC12 - stakeholders indicated that the ComID resolution would be more helpful)
  for (i in 1:length(urbanbmps)) {
    urban_results_HUCmerge[urbanbmps[i]] <- sapply(
      sapply(urban_results_HUCmerge[urbanbmps[i]], as.character), 
      as.numeric
    )
    colname <- paste(urbanbmps[i], "areaac", sep = "_")
    urban_results_HUCmerge[colname] <- urban_results_HUCmerge$urban_km2 *
      urban_results_HUCmerge[urbanbmps[i]] / km2_to_ac
  }
  
  foreach(i = 1:length(urbanbmps)) %do% {
    attr(urban_results_HUCmerge[, urbanbmps[i]], 'dimnames') <- NULL
  } # This removes the dimnames attributes from the dataframe, allowing it to be passed through dplyr pipes
  
  return(urban_results_HUCmerge)
}


calculate_urban_total <- function(urbanresults, urbanbmps) {
  
  urban_total <-data.frame(
    lapply(
      urbanresults[paste0(urbanbmps, "_areaac")],sum
    )
  )
  
  urban_total[paste0(urbanbmps, "_areaac")] <- format(
    urban_total[paste0(urbanbmps, "_areaac")],
    big.mark = ',',
    scientific = FALSE,
    digits = 2,
    nsmall = 1
  )
  
  names(urban_total) <- gsub("_", " ", paste0(urbanbmps, "_areaac"))
  names(urban_total) <- gsub("areaac", "(ac)", names(urban_total))
  
  return(urban_total)
}


present_urban_results_HUC <- function(urbanresults, urbanbmps) {
  
  urban_results_HUC <- data.frame(urbanresults) %>%
    rename_at(
      vars(one_of(urbanbmps)),list( ~ paste0(., "_FracImplement"))
    ) %>%
    select(
      COMID,
      HUC_12,
      IncAreaKm2,
      urban_km2,
      StateAbbrev,
      contains(urbanbmps)
    )
  
  return(urban_results_HUC)
}


merge_ag_results <- function(agresults, areadf, agbmps, km2_to_ac) {
  
  # Merge the COMID to HUC12 crosswalk to the ag BMP results
  ag_results_HUCmerge <- merge(
    agresults, areadf, by = "COMID", all.x = TRUE
  )
  # Calculate BMP implementation area (acres) by HUC12
  for (i in 1:length(agbmps)) {
    ag_results_HUCmerge[agbmps[i]] <- sapply(
      sapply(
        ag_results_HUCmerge[agbmps[i]], as.character
      ),
      as.numeric
    )
    colname <- paste(agbmps[i], "areaac", sep = "_")
    ag_results_HUCmerge[colname] <- ag_results_HUCmerge$ag_km2 *
      ag_results_HUCmerge[agbmps[i]] /
      km2_to_ac
    
  }
  
  foreach(i = 1:length(agbmps)) %do% {
    attr(ag_results_HUCmerge[, agbmps[i]], 'dimnames') <- NULL
  } # This removes the dimnames attributes from the dataframe, allowing it to be passed through dplyr pipes
  
  return(ag_results_HUCmerge)
}


present_ag_results_HUC <- function(agresults, agbmps) {
  ag_results_HUC <- data.frame(agresults) %>%
    rename_at(
      vars(one_of(agbmps)), list( ~ paste0(., "_FracImplement"))
    ) %>%
    select(
      COMID,
      HUC_12,
      IncAreaKm2,
      ag_km2,
      PctCrop2011Cat,
      StateAbbrev,
      contains(agbmps)
    )
  
  return(ag_results_HUC)
}

calculate_ag_total <- function(agresults, agbmps) {
  
  ag_total <- data.frame(
    lapply(agresults[paste0(agbmps, "_areaac")], sum)
  )
  ag_total[paste0(agbmps, "_areaac")] <- format(
    ag_total[paste0(agbmps, "_areaac")],
    big.mark = ',',
    scientific = FALSE,
    digits = 2,
    nsmall = 1
  )
  names(ag_total) <-gsub("_", " ", paste0(agbmps, "_areaac"))
  names(ag_total) <- gsub("areaac", "(ac)", names(ag_total))
  
  return(ag_total)
}


merge_ripbuf_results <- function(
  ripbufbmps, ripbufresults, areadf, sparrowinputs
) {
  if(length(ripbufbmps) > 0) {
    
    req(
      "ComID" %in% names(sparrowinputs),
      "COMID" %in% names(ripbufresults)
    )
    
    ripbuf_results_HUCmerge <- merge(
      merge(
        ripbufresults,
        sparrowinputs %>% rename(COMID = ComID),
        by = "COMID", all.x = TRUE
      ),
      areadf,
      by = c("COMID", "HUC_12", "IncAreaKm2"),
      all.x = TRUE
    )
    
    # Calculate BMP implementation area (acres) by HUC12
    for (i in 1:length(ripbufbmps)) {
      ripbuf_results_HUCmerge[ripbufbmps[i]] <- sapply(
        sapply(
          ripbuf_results_HUCmerge[ripbufbmps[i]], as.character
        ),
        as.numeric
      )
      colname <- paste(ripbufbmps[i], "banklengthft", sep = "_")
      ripbuf_results_HUCmerge[colname] <- ripbuf_results_HUCmerge[
        ripbufbmps[i]
      ]
      
    }
    
    foreach(i = 1:length(ripbufbmps)) %do% {
      attr(ripbuf_results_HUCmerge[, ripbufbmps[i]], 'dimnames') <- NULL
    } # This removes the dimnames attributes from the dataframe, allowing it to be passed through dplyr pipes
    
    ftperkm <- 3280.84
    
    ripbuf_results_HUCmerge <- ripbuf_results_HUCmerge %>%
      mutate(totalbanklength_ft = LENGTHKM * 2 * ftperkm) %>%
      select(
        COMID,
        HUC_12,
        IncAreaKm2,
        StateAbbrev,
        totalbanklength_ft,
        all_of(ripbufbmps),
        contains("_banklengthft")
      ) %>%
      arrange(COMID)
  } else {ripbuf_results_HUCmerge <- NULL}
  
  return(ripbuf_results_HUCmerge)
}


present_ripbuf_results_HUC <- function(ripbufresults, ripbufbmps) {
  
  ripbuf_results_HUC <- data.frame(ripbufresults) %>%
    mutate(riverreachlength_ft = totalbanklength_ft / 2) %>%
    rename_at(
      vars(
        all_of(
          ripbufbmps)), list( ~ paste0(., "_LengthImplemented_ft")
          )
    )
  
  return(ripbuf_results_HUC)
}


calculate_ripbuf_total <- function(ripbufresults, ripbufbmps) {
  ripbuf_total <- data.frame(
    lapply(ripbufresults[paste0(ripbufbmps, "_banklengthft")], sum)
  )
  ripbuf_total[paste0(ripbufbmps, "_banklengthft")] <- format(
    ripbuf_total[paste0(ripbufbmps, "_banklengthft")],
    big.mark = ',',
    scientific = FALSE,
    digits = 2,
    nsmall = 1
  )
  names(ripbuf_total) <-gsub("_", " ", paste0(ripbufbmps, "_banklengthft"))
  names(ripbuf_total) <- gsub(
    "banklengthft", "(ft of bank)", names(ripbuf_total)
  )
  
  return(ripbuf_total)
}


parse_point_results_wwtp <- function(pointresults, pointcomid) {
  
  point_results_wwtp <- merge(
    pointresults, pointcomid, by = "COMID"
  )
  
  return(point_results_wwtp)
}


get_point_plants <- function(pointresults) {
  
  point_plants <- data.frame(pointresults$Plant_Name)
  names(point_plants) <- c("Plant Name")
  
  return(point_plants)
}

##################################################
# 4. Render Preview UI ####
##################################################

make_streamcat_previews <- function(files, output) {
  iwalk(files, ~{
    output_name <- paste0("Preview_", .y)
    output[[output_name]] <- renderTable(.x)
  }
  )
}

render_streamcat_previews <- function(tables) {
  tabls_list <- imap(
    tables, 
    ~{
      tagList(
        h4(paste(str_sub(.y, 1, 2), "StreamCat File:")), 
        tableOutput(outputId = paste0("Preview_", .y))
      )
    }
  )
  return(tabls_list)
}

render_preview_ui <- function(file, tableobject, tabletitle, output) {
  output_name <- tableobject
  output[[output_name]] <- renderTable(head(x = file))
  tagList(
    h4(paste(tabletitle, ":")), 
    tableOutput(tableobject)
  )
}

make_postprocessor_previews <- function() {
  return(
    mainPanel(
      titlePanel("File Previews"),
      uiOutput(outputId = "preview_NEOS_results_ui"),
      uiOutput(outputId = "preview_user_loading_ui"),
      uiOutput(outputId = "preview_point_comid_ui"),
      uiOutput(outputId = "preview_sparrow_in_ui"),
      uiOutput(outputId = "streamcat_preview_ui")
    )
  )
}


#################################################
# 5. Write Report Outputs ####
#################################################

print_costdisplay <- function(totalcost) {
  return(
    paste0(
      "The total cost to reduce loads to the limit you provided is ",
      sprintf(
        "$%s",
        formatC(totalcost, format = "f", big.mark = ",", digits = 2)
      ),
      "."
    )
  )
}

render_costdisplay_uncertaintyplot <- function(costbootstraps) {
  return(
    ggplot(costbootstraps, aes(x = Cost)) +
      geom_density(aes(y = ..scaled..), fill = "darkgreen", n = 32) +
      scale_x_continuous(
        name = "Total Annualized Cost (x $1,000,000)",
        labels = function(x) {
          sprintf(
            "$%s",
            formatC(x / 1000000, format = "f", big.mark = ",", digits = 2)
          )
        },
        n.breaks = 4,
        guide = guide_axis(n.dodge = 2)
      )+
      scale_y_continuous(
        name = "Probability Density",
        labels = function(x) {paste0(signif(x, 2))}
      ) +
      theme_classic(base_size = 12)
  )
}

render_loadingdisplay_uncertainty <- function(loadinguncertaintymessage) {
  return(
    loadinguncertaintymessage %>%
      ungroup() %>%
      mutate(success.display = paste0(prob.met * 100, "%")) %>%
      select(
        "Loading Target" = Target.name,
        "Your Specified Target Load" = Limit.display,
        "95% CI of Estimated Annual Total Loading" = LoadingUncertaintyMessage,
        "Likelihood of Meeting Your Loading Target" = success.display
      )
  )
}

render_loadingdisplay_uncertaintyplot <- function(
  loadingbootstraps, loadingtargets_munged, loadingtargets
) {
  
  plotdat <- loadingbootstraps %>%
    left_join(
      ., loadingtargets_munged, by = c("Nutrient", "Number")
    ) %>%
    mutate(
      Target.name = case_when(
        Nutrient == "N" ~ paste0(
          "Total Nitrogen Loading \n to ",
          Waterbody_Name,
          " \n (kg N / yr)"
        ),
        Nutrient == "P" ~ paste0(
          "Total Phosphorus Loading \n to ",
          Waterbody_Name,
          " \n (kg P / yr)"
        )
      )
    )
  targetdat <- loadingtargets %>%
    left_join(
      ., loadingtargets_munged, by = c("Nutrient", "Number")
    ) %>%
    mutate(
      Target.name = case_when(
        Nutrient == "N" ~ paste0(
          "Total Nitrogen Loading \n to ",
          Waterbody_Name,
          " \n (kg N / yr)"
        ),
        Nutrient == "P" ~ paste0(
          "Total Phosphorus Loading \n to ",
          Waterbody_Name,
          " \n (kg P / yr)"
        )
      )
    )
  
  nplots <- length(unique(plotdat$Target.name))
  
  tmp.plot <- ggplot(
    plotdat,
    aes(x = Load, fill = Nutrient)
  ) +
    geom_density(aes(y = ..scaled..), show.legend = FALSE, n = 32) +
    scale_x_continuous(
      name = NULL,
      labels = function(x) {
        formatC(signif(x, 2), format = "f", big.mark = ",", digits = 0)
      },
      n.breaks = 4,
      guide = guide_axis(n.dodge = 2)
    ) +
    scale_y_continuous(
      name = "Probability Density",
      labels = function(x) {paste0(signif(x, 2))}
    ) +
    facet_wrap(
      "Target.name",
      scales = "free",
      strip.position = "bottom",
      ncol = 1
    ) +
    geom_vline(
      data = targetdat,
      mapping = aes(xintercept = Limit),
      lwd = 1,
      lty = "dotted",
      color = "grey80"
    ) +
    scale_fill_brewer(palette = "Set2") +
    theme(
      strip.background = element_rect(
        fill = "white", colour = NULL, size = rel(2)
      ),
      strip.placement = "outside",
      text = element_text(size = 12),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(
        fill = "white", color = NULL, size = rel(2)
      ),
      axis.line = element_line(colour = "black", size = rel(1)),
      legend.key = element_blank(),
      aspect.ratio = 1 / nplots
    )
  
  return(tmp.plot)
}

render_loadingsummarymessage <- function(loadinguncertaintymessage) {
  return(
    value = paste0(
      "This suggested plan has an estimated minimum of ",
      signif(min(loadinguncertaintymessage$prob.met * 100), 2),
      "% likelihood of meeting each of the specified loading targets.
        Likelihoods of meeting each specified loading target individually are
        listed in the table below. 'Likelihood' refers to the percentage of the probability
        density of estimated total annual load that falls below the user
        specified loading target."))
}

##################################################
# 6. Render Report UI ###########################
##################################################

# Display results ####
render_results_display <- function(loadingtargets_pulled) {
  mainPanel(
    titlePanel("Scaled-up Optimization Results"),
    em("To save this report, open the RBEROST postprocessor in a web browser and use the print to pdf functionality."),
    wellPanel(
      h4("User Notes:"),
      htmlOutput("usernotes")
    ),
    conditionalPanel(
      condition = "output.solvedstatus == 0",
      {
        fluidRow(
          wellPanel(h3("Your model submission returned an error.")),
          p(
            paste(
              "RBEROST may fail to optimize models for a variety of reasons. 
              Sometimes this may be a result of user error during specification of user inputs (e.g. in the case of nonsensical limits on BMPs) or during NEOS submission (e.g. uploading incorrect files).
              Other times, this is a result of loading targets that are too restrictive, or a result of not including enough BMPs for the model to use. 
              It is also possible to receive an error for reasons other than those listed above.
              Please check your user inputs and refer to the Model Sensitivity section of the documentation for more information.
              If problems persist a user might try running RBEROST again with less restrictive loading targets and/or including more BMPs in the optimization."
            ),
            h4("The model produced the following errors:"),
            code(verbatimTextOutput("errordisplay"))
          )
        )
      }
    ),
    conditionalPanel(
      condition = "output.solvedstatus == 2",
      {
        fluidRow(
          wellPanel(h3("Your model was unable to solve.")),
          p(
            paste(
              "RBEROST may fail to optimize models for a variety of reasons. 
              Often, this is a result of loading targets that are too restrictive, or a result of not including enough BMPs for the model to use. 
              It may be informative to run RBEROST again with less restrictive loading targets and/or including more BMPs in the optimization.
              Please check your user inputs and refer to the Model Sensitivity section of the documentation for more information."
            )
          )
        )
      }
    ),
    conditionalPanel(
      condition = "output.solvedstatus == 1",
      {
        fluidRow(
          wellPanel(
            h3("Your model has successfully solved."),
            textOutput("costdisplay"),
          ),
          conditionalPanel(
            condition = "output.costuncertaintyresultsavail",
            wellPanel(
              textOutput("costdisplay_uncertainty"),
              p(
                "Below is the probability density distribution of cost 
                 estimates for this scenario."
              ),
              plotOutput("costdisplay_uncertaintyplot")
            )
          ),
          conditionalPanel(
            condition = "output.loadinguncertaintyresultsavail",
            wellPanel(
              textOutput("loadingsummarymessage"),
              tableOutput("loadingdisplay_uncertainty"),
              p(
                "Below are the probability density distributions of 
                total annual load estimates at each of the loading 
                targets specified by the user. The vertical dotted grey
                lines denote the specified target loads at each loading 
                target. Probability densities to the left of the grey 
                line are load estimates that meet the loading target, 
                and probability densities to the right of the grey line are load 
                estimates that exceed the loading target."
              ),
              plotOutput(
                "loadingdisplay_uncertaintyplot",
                width = "100%",
                height = paste0(
                  400*nrow(loadingtargets_pulled),"px"
                )
              )
            )
          ),
          conditionalPanel(
            condition = "output.pointresultsavail",
            fluidRow(
              p(
                "The model chose to implement WWTP retrofits at the following locations:"
              ),
              tableOutput("wwtp")
            )
          ),
          conditionalPanel(
            condition = "!output.pointresultsavail",
            fluidRow(
              p( "The model did not implement any WWTP retrofits.")
            )
          ),
          tags$hr(),
          conditionalPanel(
            condition = "output.agdownloadavail",
            fluidRow(
              p(
                "The model chose to implement the total area of the following agricultural BMPs:"
              ),
              tableOutput("ag"),
              downloadLink(
                "downloadagdat", "Download Agricultural BMPs by COMID"
              )
            )
          ),
          conditionalPanel(
            condition = "!output.agdownloadavail",
            p("The model did not implement any agricultural BMPs.")
          ),
          tags$hr(),
          conditionalPanel(
            condition = "output.urbandownloadavail",
            fluidRow(
              p(
                "The model chose to implement the total area of the following urban BMPs."
              ),
              tableOutput("urban"),
              downloadLink(
                "downloadurbandat", "Download Urban BMPs by COMID"
              )
            )
          ),
          conditionalPanel(
            condition = "!output.urbandownloadavail",
            p("The model did not implement any urban BMPs.")
          ),
          tags$hr(),
          conditionalPanel(
            condition = "output.ripbufdownloadavail",
            fluidRow(
              p(
                "The model chose to implement the total length of the following riparian buffer BMPs."
              ),
              tableOutput("ripbuf"),
              downloadLink(
                "downloadripbufdat", "Download Riparian Buffer BMPs by COMID"
              )
            )
          ),
          conditionalPanel(
            condition = "!output.ripbufdownloadavail",
            p("The model did not implement any riparian buffer BMPs.")
          )
        )
      }
    )
  )
}

