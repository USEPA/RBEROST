##############################################################
### TITLE: Postprocessing Shiny App Server                 ###                                   
### PURPOSE: To allow users to input a text file and see   ###                                                 
###   the results of their NEOS optimization               ###                                     
### BY: Cathy Chamberlin, with previous version by Kate    ###                                                 
###   Munson and Alyssa Le (ICF)                           ###                         
### DATE: 2/16/2021                                        ###           
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

# This file is NOT intended to be run by itself. It is sourced through 02_Optimization_Postprocessing_RunShiny.R along with Optimization_PostprocessingUserInterfaceFile.R to run the shiny app.
# Debugging is best done with browser() inserted in reactive contexts that may be causing problems.
# Lines marked #*# may be changed by the user.

# The components of the Server are roughly organized as such:

##################################################
# Table of Contents:
# 1. Keep track of the session
# 2. Hard coded Parameters
# 3. Upload user-supplied files
# 4. Parse NEOS data file
# 5. Combine NEOS results with other dataframes
# 6. Write Preview Outputs
# 7. Render Preview UI
# 8. Write Report Outputs
# 9. Render Report UI
##################################################

server <- function(input, output, session) {
  
  ##################################################
  # 1. Keep track of the session ####
  ##################################################
  
  observeEvent(
    eventExpr = {
      input$preview
      input$ViewReport
      1
    },
    handlerExpr = {
      print(paste("Preview Button:", input$preview, "times"))
      print(paste("Report Button:", input$ViewReport, "times"))
      print(paste("Solved Status:", solved()[scen.vis()]))
    }
  )
  
  ##################################################
  # 2. Hard coded Parameters ####
  ##################################################   
  
  km2_to_ac <- 1 / 247.105
  
  ##################################################
  # 3.Upload user-supplied files ####
  ##################################################
  
  UserSpecs_loadingtargets_reactive <- reactive(
    {
      req(input$user_loading)
      
      tryCatch(
        # when reading semicolon separated files, having a comma separator causes `read.csv` to error. Using read.csv2 causes no error.
        expr = {fread(file = input$user_loading$datapath, fill = TRUE)},
        error = function(e) {stop(safeError(error = e))} # return a safeError if a parsing error occurs
      )
    }
  )
  
  NEOS_results_reactive <- reactive(
    {
      req(input$NEOS_results)
      
      tryCatch(
        # when reading semicolon separated files, having a comma separator causes `read.csv` to error. Using read.csv2 causes no error.
        expr = {
          read.csv2(
            file = input$NEOS_results$datapath, 
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
  )
  
  point_comid_reactive <- reactive(
    {
      req(input$point_comid)
      tryCatch(
        expr = {fread(file = input$point_comid$datapath, fill = TRUE)},
        error = function(e) {stop(safeError(error = e)) } 
      )
    }
  )
  
  sparrow_in_reactive <- eventReactive(
    eventExpr = input$sparrow_in,
    {
      req(input$sparrow_in)
      tryCatch(
        expr = {fread(file = input$sparrow_in$datapath, fill = TRUE)},
        error = function(e) {stop(safeError(error = e))} 
      )
    }
  )
  
  streambank_in_reactive <- eventReactive(
    eventExpr = input$streambank_in,
    {
      req(input$streambank_in)
      tryCatch(
        expr = {fread(file = input$streambank_in$datapath, fill = TRUE)},
        error = function(e) {stop(safeError(error = e))} 
      )
    }
  )
  
  streamcat_file_nh_reactive <- reactive(
    {
      req(input$streamcat_crop_file_nh)
      tryCatch(
        expr = {fread(file = input$streamcat_crop_file_nh$datapath, fill = TRUE)},
        error = function(e) {stop(safeError(error = e))} 
      )
    }
  )
  
  streamcat_file_vt_reactive <- reactive(
    {
      req(input$streamcat_crop_file_vt)
      tryCatch(
        expr = {fread(file = input$streamcat_crop_file_vt$datapath, fill = TRUE)},
        error = function(e) {stop(safeError(error = e))} 
      )
    }
  )
  
  ##################################################
  # 4.Parse NEOS data file ####
  ##################################################  
  
  NEOS_renamed <- eventReactive(
    eventExpr = {input$ViewReport},
    valueExpr = {
      req(
        nrow(NEOS_results_reactive() > 0), 
        nrow(UserSpecs_loadingtargets_reactive() > 0),
        nrow(point_comid_reactive() > 0),
        nrow(sparrow_in_reactive() > 0),
        nrow(streamcat_file_nh_reactive() > 0),
        nrow(streamcat_file_vt_reactive() > 0),
        input$NEOS_results$size < 10000000
      )
      
      NEOS_results_reactive() %>% rename(Output = 1)
      
    }
  )
  
  solved <- reactive(
    {
      req(NEOS_renamed)
      solved_txt <- "solve_result = "
      solved_rows <-
        NEOS_renamed()[which(str_detect(NEOS_renamed()[, 1], solved_txt)), 1]
      
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
      
      if (!grepl("solve_result", NEOS_renamed())) {
        solved <- list(-99)
      } else if ("solved" %in% solved_tmp2) {
        solved <- foreach(i = 1:length(solved_tmp2)) %do% {
          if(
            solved_tmp2[i] == "solved"
          ) {1} else if(
            solved_tmp2[i] == "infeasible"
          ) {2} else {-98}
        }
      } else if ("infeasible" %in% solved_tmp2) {
        solved <- foreach(i = 1:length(solved_tmp2)) %do% {
          if(
            solved_tmp2[i] == "solved"
          ) {1} else if(
            solved_tmp2[i] == "infeasible"
          ) {2} else {-98}
        }
      } else if(grepl("Error", NEOS_renamed())){
        solved <- list(0)
      } else {solved <- list(-98)}
      
      solved
    }
  )
  
  user_notes <- reactive(
    {
      req(NEOS_renamed)
      notetextstart <- "%% COMMENTS %%"
      notetextend <- "%%%%%%%%%%"
      
      notetextlinestart <- which(str_detect(NEOS_renamed()[, 1], notetextstart))
      notetextlineend <-  which(substr(NEOS_renamed()[, 1], 1, 10) == notetextend)
      
      if(!is_empty(notetextlinestart)) {
        UserNotes <- paste(
          NEOS_renamed()[(notetextlinestart):(notetextlineend),1], 
          collapse = "\n"
        )
        UserNotes2 <- gsub("%%", "", gsub("%% COMMENTS %%", "", UserNotes))
      } else {UserNotes2 <- ""}
      
      UserNotes2
    }
  )
  
  num.scen <- reactive(
    {
      req(NEOS_renamed, solved)
      
      length(solved())
    }
  )
  
  scen.vis <- reactive({
    req(input$scenario)
    if(as.numeric(as.character(input$scenario)) <= num.scen()){
      tmp <- as.numeric(as.character(input$scenario))
    } else {tmp <- 1}
    tmp
  })
  
  error_display <- reactive(
    {
      req(NEOS_renamed, solved()[scen.vis()] == 0)
      display <- NEOS_renamed()[
        which(substr(NEOS_renamed()[, 1], 1, 5) == "Error"):
          nrow(NEOS_renamed()), 1
      ]
      
      display
    }
  )
  
  total_cost <- reactive(
    {
      req(NEOS_renamed, solved()[scen.vis()] == 1)
      
      cost_txt <- "cost = "
      
      cost_rows <-
        NEOS_renamed()[which(str_detect(NEOS_renamed()[, 1], cost_txt)), 1][scen.vis()]
      
      cost_tmp <- strsplit(cost_rows, " ")
      
      cost_tmp2 <- unlist(suppressWarnings(as.numeric(as.character(lapply(
        cost_tmp, "[[", 3
      )))),
      use.names = FALSE)
      
      total_cost <- cost_tmp2[which(!is.na(cost_tmp2))]
      
      total_cost
    }
  )
  
  
  
  NEOS_end_rows <- reactive(
    {
      req(NEOS_renamed)
      which(NEOS_renamed()[, 1] == '' | NEOS_renamed()[, 1] == ';')
    }
  )
  
  Table_start_rows <- reactive(
    {
      req(NEOS_renamed)
      which(grepl(":=", NEOS_renamed()[, 1]))
    }
  )
  
  cost_bootstraps <- reactive(
    {
      req(NEOS_renamed, NEOS_end_rows, solved()[scen.vis()] == 1)
      
      display_txt <- "sum[{]c in comid_all[}]"
      coef_txt <- "coef"
      
      cost_bootstraps_begin <-
        which(grepl(NEOS_renamed()[, 1], pattern = display_txt) & str_detect(NEOS_renamed()[, 1], coef_txt))[scen.vis()]
      
      if(length(cost_bootstraps_begin) > 0 & !is.na(cost_bootstraps_begin)) {
        cost_bootstraps_end <- min(NEOS_end_rows()[which(NEOS_end_rows() > cost_bootstraps_begin)])
        
        if(scen.vis() == 1) {
          cost_bootstraps_tablebegin <- min(Table_start_rows()[which(Table_start_rows() > cost_bootstraps_begin)])
        } else {
          cost_bootstraps_tablebegin <- cost_bootstraps_begin
        }
        
        if((cost_bootstraps_tablebegin + 1) < (cost_bootstraps_end - 1)) {
          
          cost_bootstraps <- data.frame(
            str_split_fixed(
              parse_character(
                NEOS_renamed()[(cost_bootstraps_tablebegin + 1):(cost_bootstraps_end - 1), 1]
              )
              , " ", n = 2)
          ) %>%
            rename(Bootstrap = 1, Cost = 2) %>%
            mutate(across(.fns = ~as.numeric(as.character(.))))
          
        }
      } else { cost_bootstraps <- NULL}
      
      
      cost_bootstraps
    }
  )
  
  cost_uncertainty_message <- reactive({
    
    req(!is.null(cost_bootstraps), solved()[scen.vis()] == 1, nrow(cost_bootstraps()) > 3)
    CI95.low <- quantile(cost_bootstraps()$Cost, 0.025)
    CI95.high <- quantile(cost_bootstraps()$Cost, 0.975)
    
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
    
    cost.message
  })
  
  loading_targets <- reactive({
    req(NEOS_renamed, NEOS_end_rows, solved()[scen.vis()] == 1)
    loads_lims_lines <- NEOS_renamed()[which(grepl(NEOS_renamed()[, 1], pattern = "loads_lim_")),1]
    
    targets <- str_extract(
      loads_lims_lines, 
      pattern = "(?<=loads_lim_)(.*?)(?= = )"
    )
    
    loads_lims <- data.frame(
      Nutrient = substr(targets, 1,1),
      Number = as.numeric(as.character(substr(targets, 2,nchar(targets)))),
      Limit =  as.numeric(
        as.character(str_extract(loads_lims_lines, pattern = "(?<= = )(.*)"))
      )
    )
    
    loads_lims
  })
  
  
  loading_bootstraps <- reactive(
    {
      req(NEOS_renamed, NEOS_end_rows, solved()[scen.vis()] == 1)
      
      display_txt <- "sum[{]c in comid_"
      load_txt <- "other_loads"
      
      loading_bootstraps_begin_all <- which(
        grepl(NEOS_renamed()[, 1], pattern = display_txt) & 
          str_detect(NEOS_renamed()[, 1], load_txt)
      ) 
      loading_bootstraps_begin <- loading_bootstraps_begin_all[
        (length(loading_bootstraps_begin_all) / num.scen() * (scen.vis() - 1) + 1):
          (length(loading_bootstraps_begin_all) / num.scen() * (scen.vis()))
      ]
      
      if((length(loading_bootstraps_begin) > 0) & all(!unlist(lapply(loading_bootstraps_begin, is.na)))) {
        loading_bootstraps <- data.frame(
          foreach(
            i = 1:length(loading_bootstraps_begin), .combine = "rbind"
          ) %do% {
            loading_bootstraps_end <- min(
              NEOS_end_rows()[
                which(NEOS_end_rows() > loading_bootstraps_begin[i])
              ]
            )
            
            if(scen.vis() == 1) {
              loading_bootstraps_tablebegin <- min(
                Table_start_rows()[
                  which(Table_start_rows() > loading_bootstraps_begin[i])
                ]
              )
            } else {
              loading_bootstraps_tablebegin <- loading_bootstraps_begin[i]
            }
            
            load.designation <- str_extract(
              NEOS_renamed()[loading_bootstraps_begin[i],1], 
              pattern = "(?<=other_loads_)(.*?)(?=_rev)"
            )
            
            load.nutrient <- substr(load.designation, 1, 1)
            load.number <- as.numeric(
              as.character(
                substr(load.designation, 2, nchar(load.designation))
              )
            )
            
            if(
              (loading_bootstraps_tablebegin + 1) < (loading_bootstraps_end - 1)
            ) {
              
              loading_bootstraps.tmp <- data.frame(
                str_split_fixed(
                  parse_character(
                    NEOS_renamed()[(loading_bootstraps_tablebegin + 1):(loading_bootstraps_end - 1), 1]
                  )
                  , " ", n = 2)
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
      
      
      loading_bootstraps
    }
  )
  
  UserSpecs_loadingtargets_munged <- eventReactive(
    eventExpr = input$ViewReport, 
    {      
      req(
        nrow(UserSpecs_loadingtargets_reactive()) > 0, 
        solved()[scen.vis()] == 1,
        NEOS_renamed,
        "TN_or_TP" %in% names(UserSpecs_loadingtargets_reactive())
      )
      
      UserSpecs_loadingtargets_munged <- UserSpecs_loadingtargets_reactive() %>%
        mutate(Nutrient = gsub("T", "", TN_or_TP), ticker = 1) %>%
        group_by(Nutrient) %>%
        mutate(Number = cumsum(ticker)) %>%
        select(Waterbody_Name, Percent_Reduction, Nutrient, Number)
      
      UserSpecs_loadingtargets_munged
    })
  
  loading_uncertainty_message <- reactive({
    req(
      !is.null(loading_bootstraps), 
      solved()[scen.vis()] == 1,
      nrow(loading_bootstraps()) > 3,
      loading_targets,
      UserSpecs_loadingtargets_munged
    )
    
    CI.summary <- loading_bootstraps() %>%
      left_join(., loading_targets(), by = c("Nutrient", "Number")) %>%
      left_join(
        ., UserSpecs_loadingtargets_munged(), by = c("Nutrient", "Number")
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
    
    CI.summary
  })
  
  
  point_results <- reactive(
    {
      req(NEOS_renamed, NEOS_end_rows, solved()[scen.vis()] == 1)
      point_txt <- "point_dec [*] :="
      
      point_rows_begin <-
        which(NEOS_renamed()[, 1] ==  point_txt)[scen.vis()]
      
      point_rows_end <-
        min(NEOS_end_rows()[which(NEOS_end_rows() > point_rows_begin)])
      
      if((point_rows_begin + 1) < (point_rows_end - 1)) {
        point_rows <- NEOS_renamed()[
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
      
      point_results
    }
  )
  
  urban_bmp_vec <- reactive(
    {
      req(NEOS_renamed, solved()[scen.vis()] == 1)
      urban_txt <- "urban_frac [*,*]"
      urban_bmp_tmp <- NEOS_renamed()[
        (which(NEOS_renamed()[, 1] == urban_txt) + 1), 1
      ][scen.vis()]
      
      if(
        grepl(
          "ag_frac|urban_frac|point_dec|ripbuf_length|NEOS|Presolve", 
          urban_bmp_tmp
        )
      ) {urban_bmp_tmp <- ""}
      
      urban_bmp <- strsplit(urban_bmp_tmp, "\\s+")
      urban_bmp_vec_tmp <- as.vector(unlist(urban_bmp))
      urban_bmp_vec_tmp1 <-
        urban_bmp_vec_tmp[!sapply(urban_bmp_vec_tmp, function(x)
          all(x == ""))]
      urban_bmp_vec <-
        urban_bmp_vec_tmp1[-c(1, length(urban_bmp_vec_tmp1))]
      
      urban_bmp_vec
    }
  )
  
  urban_results <- reactive(
    {
      
      req(NEOS_renamed, NEOS_end_rows, length(urban_bmp_vec()) > 0, solved()[scen.vis()] == 1)
      urban_txt <- "urban_frac [*,*]"
      
      urban_rows_begin <-
        which(NEOS_renamed()[, 1] == urban_txt)[scen.vis()]
      
      if(length(urban_rows_begin) > 0) {
        
        urban_rows_end <-
          min(NEOS_end_rows()[which(NEOS_end_rows() > urban_rows_begin)])
        
        urban_rows <-
          NEOS_renamed()[(urban_rows_begin + 2):(urban_rows_end - 1), 1]
        
        urban_tmp <- strsplit(urban_rows, "\\s+")
        
        urban_results <- do.call(rbind.data.frame, urban_tmp)
        
        names(urban_results) <- c("COMID", urban_bmp_vec())
        
        # Adjust the formatting of the NEOS results
        urban_results$COMID_adj <- gsub("'", "", urban_results$COMID)
        urban_results$COMID_int <- strtoi(urban_results$COMID_adj)
        urban_results <- urban_results %>%
          select(COMID = COMID_int, all_of(urban_bmp_vec()))
      } else {urban_results <- NULL}
      
      urban_results
    }
  )
  
  ag_bmp_vec <- reactive(
    {
      req(NEOS_renamed, solved()[scen.vis()] == 1)
      
      ag_txt <- "ag_frac [*,*]"
      ag_bmp_tmp <- NEOS_renamed()[
        (which(NEOS_renamed()[, 1] == ag_txt) + 1), 1
      ][scen.vis()]
      
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
      
      ag_bmp_vec
    }
  )
  
  ag_results <- reactive(
    {
      req(NEOS_renamed, NEOS_end_rows, length(ag_bmp_vec()) > 0, solved()[scen.vis()] == 1)
      
      ag_txt <- "ag_frac [*,*]"
      
      ag_rows_begin <-
        which(NEOS_renamed()[, 1] == ag_txt)[scen.vis()]
      
      if(length(ag_rows_begin > 0)) {
        
        ag_rows_end <-
          min(NEOS_end_rows()[which(NEOS_end_rows() > ag_rows_begin)])
        
        ag_rows <-
          NEOS_renamed()[(ag_rows_begin + 2):(ag_rows_end - 1), 1]
        
        ag_tmp <- strsplit(ag_rows, "\\s+")
        
        ag_results <- do.call(rbind.data.frame, ag_tmp)
        
        names(ag_results) <- c("COMID", ag_bmp_vec())
        
        ag_results$COMID_adj <- gsub("'", "", ag_results$COMID)
        ag_results$COMID_int <- strtoi(ag_results$COMID_adj)
        ag_results <- ag_results %>%
          select(COMID = COMID_int, all_of(ag_bmp_vec()))
        
      } else {ag_results <- NULL}
      
      ag_results
    }
  )
  
  ripbuf_bmp_vec <- reactive(
    {
      req(NEOS_renamed, solved()[scen.vis()] == 1)
      
      ripbuf_txt <- "ripbuf_length [*,*]"
      ripbuf_bmp_tmp <- NEOS_renamed()[
        (which(NEOS_renamed()[, 1] == ripbuf_txt) + 1), 1
      ][scen.vis()]
      
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
      
      ripbuf_bmp_vec
    }
  )
  
  ripbuf_results <- reactive(
    {
      req(NEOS_renamed, NEOS_end_rows, length(ripbuf_bmp_vec()) > 0, solved()[scen.vis()] == 1)
      ripbuf_txt <- "ripbuf_length [*,*]"
      
      ripbuf_rows_begin <-
        which(NEOS_renamed()[, 1] == ripbuf_txt)[scen.vis()]
      
      if(length(ripbuf_rows_begin) > 0 & !is.na(ripbuf_rows_begin)) {
        if(length(which(NEOS_end_rows() > ripbuf_rows_begin)) > 0) {
          ripbuf_rows_end <- min(
            NEOS_end_rows()[which(NEOS_end_rows() > ripbuf_rows_begin)]
          )
        } else {ripbuf_rows_end <- ripbuf_rows_begin}
        
        ripbuf_rows <-
          NEOS_renamed()[(ripbuf_rows_begin + 2):(ripbuf_rows_end - 1), 1]
        
        ripbuf_tmp <- strsplit(ripbuf_rows, "\\s+")
        
        ripbuf_results <- do.call(rbind.data.frame, ripbuf_tmp)
        
        names(ripbuf_results) <- c("COMID", ripbuf_bmp_vec())
        
        ripbuf_results$COMID_adj <- gsub("'", "", ripbuf_results$COMID)
        ripbuf_results$COMID_int <- strtoi(ripbuf_results$COMID_adj)
        ripbuf_results <- ripbuf_results %>%
          select(COMID = COMID_int, all_of(ripbuf_bmp_vec()))
      } else {
        ripbuf_results <- NULL
      }
      
      ripbuf_results
    }
  )
  
  ##################################################
  # 5. Combine NEOS results with other dataframes ####
  ##################################################  
  
  area <- eventReactive(
    eventExpr = input$ViewReport,
    {
      req(
        sparrow_in_reactive, 
        streamcat_file_nh_reactive, 
        streamcat_file_vt_reactive, 
        solved()[scen.vis()] == 1,
        "HUC_12" %in% names(sparrow_in_reactive()),
        ncol(streamcat_file_nh_reactive()) == ncol(streamcat_file_vt_reactive())
      )
      
      comid_huc12 <- sparrow_in_reactive() %>%
        rename(comid = 1) %>%
        select(c("comid", "HUC_12", "IncAreaKm2", "urban_km2")) %>%
        mutate(HUC12_Char = str_pad(HUC_12, width = 12, pad = "0")) %>%
        select(COMID = comid,
               HUC_12 = HUC12_Char,
               IncAreaKm2,
               urban_km2)
      
      cropland <- rbind(
        streamcat_file_nh_reactive() %>% mutate(StateAbbrev = "NH"),
        streamcat_file_vt_reactive() %>% mutate(StateAbbrev = "VT")
      )
      
      streamcat_crop <- cropland %>%
        select(c("COMID", "PctCrop2011Cat", "WsAreaSqKm", "StateAbbrev")) %>%
        mutate(WsAreaAc = WsAreaSqKm / km2_to_ac)
      
      # Calculate available agricultural area
      area <- merge(comid_huc12, streamcat_crop, by = "COMID")
      area <- area[!duplicated(area$COMID),] %>%
        mutate(ag_km2 = (PctCrop2011Cat / 100) * IncAreaKm2,
               COMID = as.character(COMID))
      
      area
    }
  )
  
  urban_results_HUCmerge <- reactive(
    {
      req(!is.null(urban_results), urban_bmp_vec, area, solved()[scen.vis()] == 1)
      # Merge the SPARROW and StreamCat data to the urban BMP results
      urban_results_HUCmerge <- merge(urban_results(),
                                      area(),
                                      by = "COMID",
                                      all.x = TRUE)
      
      # Calculate BMP implementation area (acres) (note: this used to sum by HUC12 - stakeholders indicated that the ComID resolution would be more helpful)
      for (i in 1:length(urban_bmp_vec())) {
        urban_results_HUCmerge[urban_bmp_vec()[i]] <- sapply(
          sapply(urban_results_HUCmerge[urban_bmp_vec()[i]], as.character), 
          as.numeric
        )
        colname <- paste(urban_bmp_vec()[i], "areaac", sep = "_")
        urban_results_HUCmerge[colname] <-
          urban_results_HUCmerge$urban_km2 *
          urban_results_HUCmerge[urban_bmp_vec()[i]] / km2_to_ac
      }
      
      foreach(i = 1:length(urban_bmp_vec())) %do% {
        attr(urban_results_HUCmerge[, urban_bmp_vec()[i]], 'dimnames') <- NULL
      } # This removes the dimnames attributes from the dataframe, allowing it to be passed through dplyr pipes
      
      urban_results_HUCmerge
    }
  )
  
  urban_total <- reactive(
    {
      req(urban_results_HUCmerge, urban_bmp_vec, solved()[scen.vis()] == 1)
      
      urban_total <-data.frame(
        lapply(
          urban_results_HUCmerge()[paste0(urban_bmp_vec(), "_areaac")],sum
        )
      )
      
      urban_total[paste0(urban_bmp_vec(), "_areaac")] <- format(
        urban_total[paste0(urban_bmp_vec(), "_areaac")],
        big.mark = ',',
        scientific = FALSE,
        digits = 2,
        nsmall = 1
      )
      
      names(urban_total) <- gsub("_", " ", paste0(urban_bmp_vec(), "_areaac"))
      names(urban_total) <- gsub("areaac", "(ac)", names(urban_total))
      
      urban_total
    }
  )
  
  urban_results_HUC <- reactive(
    {
      req(urban_results_HUCmerge, length(urban_bmp_vec()) > 0, solved()[scen.vis()] == 1)
      
      urban_results_HUC <- data.frame(urban_results_HUCmerge()) %>%
        rename_at(
          vars(one_of(urban_bmp_vec())),list( ~ paste0(., "_FracImplement"))
        ) %>%
        select(
          COMID,
          HUC_12,
          IncAreaKm2,
          urban_km2,
          StateAbbrev,
          contains(urban_bmp_vec())
        )
      
      urban_results_HUC
    }
  )
  
  ag_results_HUCmerge <- reactive(
    {
      req(!is.null(ag_results), area, ag_bmp_vec, solved()[scen.vis()] == 1)
      # Merge the COMID to HUC12 crosswalk to the ag BMP results
      ag_results_HUCmerge <- merge(
        ag_results(), area(), by = "COMID", all.x = TRUE
      )
      # Calculate BMP implementation area (acres) by HUC12
      for (i in 1:length(ag_bmp_vec())) {
        ag_results_HUCmerge[ag_bmp_vec()[i]] <- sapply(
          sapply(
            ag_results_HUCmerge[ag_bmp_vec()[i]], as.character
          ),
          as.numeric
        )
        colname <- paste(ag_bmp_vec()[i], "areaac", sep = "_")
        ag_results_HUCmerge[colname] <- ag_results_HUCmerge$ag_km2 *
          ag_results_HUCmerge[ag_bmp_vec()[i]] / 
          km2_to_ac
        
      }
      
      foreach(i = 1:length(ag_bmp_vec)) %do% {
        attr(ag_results_HUCmerge[, ag_bmp_vec()[i]], 'dimnames') <- NULL
      } # This removes the dimnames attributes from the dataframe, allowing it to be passed through dplyr pipes
      
      ag_results_HUCmerge
    }
  )
  
  ag_results_HUC <- reactive(
    {
      req(ag_results_HUCmerge, ag_bmp_vec, solved()[scen.vis()] == 1)
      ag_results_HUC <- data.frame(ag_results_HUCmerge()) %>%
        rename_at(
          vars(one_of(ag_bmp_vec())), list( ~ paste0(., "_FracImplement"))
        ) %>%
        select(
          COMID,
          HUC_12,
          IncAreaKm2,
          ag_km2,
          PctCrop2011Cat,
          StateAbbrev,
          contains(ag_bmp_vec())
        )
      
      ag_results_HUC
    }
  )
  
  ag_total <- reactive(
    {
      req(ag_results_HUCmerge, ag_bmp_vec, solved()[scen.vis()] == 1)
      ag_total <- data.frame(
        lapply(ag_results_HUCmerge()[paste0(ag_bmp_vec(), "_areaac")], sum)
      )
      ag_total[paste0(ag_bmp_vec(), "_areaac")] <- format(
        ag_total[paste0(ag_bmp_vec(), "_areaac")],
        big.mark = ',',
        scientific = FALSE,
        digits = 2,
        nsmall = 1
      )
      names(ag_total) <-gsub("_", " ", paste0(ag_bmp_vec(), "_areaac"))
      names(ag_total) <- gsub("areaac", "(ac)", names(ag_total))
      
      ag_total
    }
  )
  
  ripbuf_results_HUCmerge <- reactive(
    {
      req(
        streambank_in_reactive, 
        ripbuf_bmp_vec, 
        solved()[scen.vis()] == 1, 
        !is.null(ripbuf_results())
      )
      
      if(length(ripbuf_bmp_vec()) > 0) {
        
        req(
          "comid" %in% names(streambank_in_reactive()),
          "COMID" %in% names(ripbuf_results())
        )
        # Merge the COMID to HUC12 crosswalk to the ag BMP results
        ripbuf_results_HUCmerge <- merge(
          merge(
            ripbuf_results(), 
            streambank_in_reactive() %>% rename(COMID = comid), 
            by = "COMID", all.x = TRUE
          ),
          area(),
          by = "COMID",
          all.x = TRUE
        )
        
        # Calculate BMP implementation area (acres) by HUC12
        for (i in 1:length(ripbuf_bmp_vec())) {
          ripbuf_results_HUCmerge[ripbuf_bmp_vec()[i]] <- sapply(
            sapply(
              ripbuf_results_HUCmerge[ripbuf_bmp_vec()[i]], as.character
            ),
            as.numeric
          )
          colname <- paste(ripbuf_bmp_vec()[i], "banklengthft", sep = "_")
          ripbuf_results_HUCmerge[colname] <- ripbuf_results_HUCmerge[
            ripbuf_bmp_vec()[i]
          ]
          
        }
        
        foreach(i = 1:length(ripbuf_bmp_vec)) %do% {
          attr(ripbuf_results_HUCmerge[, ripbuf_bmp_vec()[i]], 'dimnames') <- NULL
        } # This removes the dimnames attributes from the dataframe, allowing it to be passed through dplyr pipes
      } else {ripbuf_results_HUCmerge <- NULL}
      
      ripbuf_results_HUCmerge
    }
  )
  
  ripbuf_results_HUC <- reactive(
    {
      req(ripbuf_results_HUCmerge, length(ripbuf_bmp_vec()) > 0, solved()[scen.vis()] == 1)
      
      ripbuf_results_HUC <- data.frame(ripbuf_results_HUCmerge()) %>%
        select(
          COMID,
          HUC_12,
          IncAreaKm2,
          StateAbbrev,
          totalbanklength_ft,
          all_of(ripbuf_bmp_vec())
        ) %>%
        mutate(riverreachlength_ft = totalbanklength_ft / 2) %>%
        rename_at(
          vars(
            all_of(
              ripbuf_bmp_vec())), list( ~ paste0(., "_LengthImplemented_ft")
              )
        )
      
      ripbuf_results_HUC
    }
  )
  
  ripbuf_total <- reactive(
    {
      req(ripbuf_results_HUCmerge, ripbuf_bmp_vec, solved()[scen.vis()] == 1)
      
      ripbuf_total <- data.frame(
        lapply(ripbuf_results_HUCmerge()[paste0(ripbuf_bmp_vec(), "_banklengthft")], sum)
      )
      ripbuf_total[paste0(ripbuf_bmp_vec(), "_banklengthft")] <- format(
        ripbuf_total[paste0(ripbuf_bmp_vec(), "_banklengthft")],
        big.mark = ',',
        scientific = FALSE,
        digits = 2,
        nsmall = 1
      )
      names(ripbuf_total) <-gsub("_", " ", paste0(ripbuf_bmp_vec(), "_banklengthft"))
      names(ripbuf_total) <- gsub("banklengthft", "(ft of bank)", names(ripbuf_total))
      
      ripbuf_total
    }
  )
  
  point_results_wwtp <- reactive(
    {
      req(
        !is.null(point_results()), 
        point_comid_reactive, solved()[scen.vis()] == 1, 
        "COMID" %in% names(point_results()), 
        "COMID" %in% names(point_comid_reactive())
      )
      
      point_results_wwtp <- merge(
        point_results(), point_comid_reactive(), by = "COMID"
      )
      
      point_results_wwtp
    }
  )
  
  point_plants <- reactive(
    {
      req(
        point_results_wwtp, 
        solved()[scen.vis()] == 1, 
        "Plant_Name" %in% names(point_results_wwtp())
      )
      point_plants <- data.frame(point_results_wwtp()$Plant_Name)
      names(point_plants) <- c("Plant Name")
      
      point_plants
    }
  )
  
  ##################################################
  # 6. Write Preview Outputs ####
  ##################################################  
  
  output$preview_user_loading <- renderTable(
    expr = {
      req(input$preview)
      isolate(validate(need(input$user_loading != "", label = "User Specified Loading Targets file")))
      isolate(return(value = head(x = UserSpecs_loadingtargets_reactive())))
    }
  )
  
  output$preview_NEOS_results <- renderTable(
    expr = {
      req(input$preview)
      isolate(validate(need(input$NEOS_results != "", label = "Results file")))
      isolate(return(value = head(x = NEOS_results_reactive())))
    }
  )
  
  output$preview_point_comid <- renderTable(
    expr = {
      req(input$preview)
      isolate(validate(need(input$point_comid != "", label = "WWTP file")))
      isolate(return(value = head(x = point_comid_reactive())))
    }
  )
  
  output$preview_sparrow_in <- renderTable(
    expr = {
      req(input$preview)
      isolate(validate(need(input$sparrow_in != "", label = "SPARROW file")))
      isolate(return(value = head(x = sparrow_in_reactive())))
    }
  )
  
  output$preview_streambank_in <- renderTable(
    expr = {
      req(input$preview)
      isolate(validate(need(input$streambank_in != "", label = "Streambank file")))
      isolate(return(value = head(x = streambank_in_reactive())))
    }
  )
  
  output$preview_streamcat_crop_file_nh <- renderTable(
    expr = {
      req(input$preview)
      isolate(
        validate(
          need(
            input$streamcat_crop_file_nh != "", 
            label = "New Hampshire StreamCat Cropland file"
          )
        )
      )
      isolate(return(value = head(x = streamcat_file_nh_reactive())))
    }
  )
  
  output$preview_streamcat_crop_file_vt <- renderTable(
    expr = {
      req(input$preview)
      isolate(
        validate(
          need(
            input$streamcat_crop_file_vt != "", 
            label = "Vermont StreamCat Cropland file"
          )
        )
      )
      isolate(return(value = head(x = streamcat_file_vt_reactive())))
    }
  )
  
  ##################################################
  # 7. Render Preview UI ####
  ##################################################  
  output$previews <- renderUI(
    {
      req(input$preview)
      
      isolate(
        {
          mainPanel(
            titlePanel("File Previews"),
            tags$h4("User Specified Loading Targets File:"),
            tableOutput("preview_user_loading"),
            tags$h4("NEOS Results File:"),
            tableOutput("preview_NEOS_results"),
            tags$h4("WWTP File:"),
            tableOutput("preview_point_comid"),
            tags$h4("Sparrow Inputs File:"),
            tableOutput("preview_sparrow_in"),
            tags$h4("Streambank Buffered Length File:"),
            tableOutput("preview_streambank_in"),
            tags$h4("VT StreamCat File:"),
            tableOutput("preview_streamcat_crop_file_vt"),
            tags$h4("NH StreamCat File:"),
            tableOutput("preview_streamcat_crop_file_nh")
          )
        }
      )
    }
  )
  
  ##################################################
  # 8. Write Report Outputs ####
  ################################################## 
  observeEvent(
    eventExpr = {
      input$ViewReport
    },
    handlerExpr = {
      updateSelectInput(
        session, "scenario", choices = c(1:num.scen()), selected = 1
      )
    }
  )  
  
  output$errordisplay <- renderPrint(
    {
      req(error_display)
      return(paste(gsub("\t", "", error_display())))
    }
  )
  
  output$usernotes <- renderText(
    expr = {
      req(user_notes)
      return(
        value = user_notes()
      )
    }
  )
  
  output$costdisplay <- renderText(
    expr = {
      req(total_cost)
      return(
        value = paste0(
          "The total cost to reduce loads to the limit you provided is ",
          sprintf(
            "$%s",
            formatC(total_cost(), format = "f", big.mark = ",", digits = 2)
          ),
          "."
        )
      )
    }
  )
  ## Display Uncertainty ####
  output$costdisplay_uncertainty <- renderText(
    expr = {
      req(!is.null(cost_uncertainty_message()))
      return(
        value = cost_uncertainty_message()
      )
    }
  )
  
  output$costdisplay_uncertaintyplot <- renderPlot(
    expr = {
      req(!is.null(cost_uncertainty_message()))
      
      return(
        ggplot(cost_bootstraps(), aes(x = Cost)) + 
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
    },
    res = 150
  )
  
  output$costuncertaintyresultsavail <- reactive(
    {
      req(!is.null(cost_uncertainty_message()))
    }
  )
  outputOptions(output, "costuncertaintyresultsavail", suspendWhenHidden = FALSE)
  
  output$loadingdisplay_uncertainty <- renderTable(
    expr = {
      req(!is.null(loading_uncertainty_message()))
      return(
        value = loading_uncertainty_message() %>%
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
  )
  
  output$loadingdisplay_uncertaintyplot <- renderPlot(
    expr = {
      req(
        !is.null(loading_uncertainty_message()), 
        !is.null(UserSpecs_loadingtargets_munged), 
        UserSpecs_loadingtargets_munged
      )
      
      return(
        ggplot(
          loading_bootstraps() %>%
            left_join(
              ., UserSpecs_loadingtargets_munged(), by = c("Nutrient", "Number")
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
            ),
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
          )+
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
            data = loading_targets() %>%
              left_join(
                ., UserSpecs_loadingtargets_munged(), by = c("Nutrient", "Number")
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
              ), 
            mapping = aes(xintercept = Limit), 
            lwd = 1, 
            lty = "dotted",
            color = "grey80"
          ) +
          scale_fill_brewer(palette = "Set2") +
          theme(
            strip.background = element_rect(fill = "white", colour = NULL, size = rel(2)),
            strip.placement = "outside", 
            text = element_text(size = 12),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color = NULL, size = rel(2)),
            axis.line = element_line(colour = "black", size = rel(1)),
            legend.key = element_blank()
          ) 
      )
    },
    res = 150,
    height = "auto"
    # height = reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*1/3,0))
  )
  
  output$loadingsummarymessage <- renderText({
    req(!is.null(loading_uncertainty_message()))
    return(
      value = paste0(
        "This suggested plan has an estimated minimum of ", 
        signif(min(loading_uncertainty_message()$prob.met * 100), 2), 
        "% likelihood of meeting each of the specified loading targets. 
        Likelihoods of meeting each specified loading target individually are 
        listed in the table below. 'Likelihood' refers to the percentage of the probability 
        density of estimated total annual load that falls below the user 
        specified loading target."))
  })
  
  output$loadinguncertaintyresultsavail <- reactive(
    {
      req(!is.null(loading_uncertainty_message()))
    }
  )
  
  outputOptions(output, "loadinguncertaintyresultsavail", suspendWhenHidden = FALSE)
  ## Urban Output ####
  output$urban <- renderTable(
    expr = {
      req(urban_total, length(urban_bmp_vec()) > 0)
      return(value = urban_total())
    }
  )
  
  output$urbandownloadavail <- reactive(
    {
      req(urban_results_HUC)
      nrow(urban_results_HUC()) > 0
    }
  )
  outputOptions(output, "urbandownloadavail", suspendWhenHidden = FALSE)
  
  output$downloadurbandat <- downloadHandler(
    filename = reactive({paste0("Urban_by_ComID_Scenario", scen.vis(), ".csv")}),
    content = function(file) {
      write.csv(urban_results_HUC(), file)
    }
  )
  
  ## Ag Output
  output$ag <- renderTable(
    expr = {
      req(ag_total, length(ag_bmp_vec()) > 0)
      return(value = ag_total())
    }
  )
  
  output$agdownloadavail <-reactive(
    {
      req(ag_results_HUC)
      nrow(ag_results_HUC()) > 0
    }
  )
  outputOptions(output, "agdownloadavail", suspendWhenHidden = FALSE)
  
  output$downloadagdat <- downloadHandler(
    filename = reactive({paste0("AgBMP_by_ComID_Scenario", scen.vis(), ".csv")}),
    content = function(file) {
      write.csv(ag_results_HUC(), file)
    }
  )
  
  ## Riparian Output
  output$ripbuf <- renderTable(
    expr = {
      req(ripbuf_total, length(ripbuf_bmp_vec()) > 0)
      return(value = ripbuf_total())
    }
  )
  
  output$ripbufdownloadavail <-reactive(
    {
      req(ripbuf_results_HUC)
      nrow(ripbuf_results_HUC()) > 0
    }
  )
  outputOptions(output, "ripbufdownloadavail", suspendWhenHidden = FALSE)
  
  output$downloadripbufdat <- downloadHandler(
    filename = reactive({paste0("RipBufBMP_by_ComID_Scenario", scen.vis(), ".csv")}),
    content = function(file) {
      write.csv(ripbuf_results_HUC(), file)
    }
  )
  
  ## WWTP outputs
  output$wwtp <- renderTable(
    expr = {
      req(nrow(point_plants()) > 0)
      return(value = point_plants())
    }
  )
  
  output$pointresultsavail <- reactive(
    {
      req(point_plants)
      nrow(point_plants()) > 0
    }
  )
  outputOptions(output, "pointresultsavail", suspendWhenHidden = FALSE)
  
  output$solvedstatus <- reactive(
    {
      req(solved)
      solved()[scen.vis()]
    }
  )
  
  outputOptions(output, "solvedstatus", suspendWhenHidden = FALSE)
  
  ##################################################
  # 10. Render Report UI ###########################
  ################################################## 
  
  output$report <- renderUI(
    {
      input$viewReport
      req(input$ViewReport)
      ## Checks ####
      isolate(
        validate(
          need(input$NEOS_results != "", label = "Results file"), 
          need(input$user_loading != "", label = "User specified loading targets file"),
          need(input$point_comid != "", label = "WWTP file"),
          need(input$sparrow_in != "", label = "SPARROW file"),
          need(input$streambank_in != "", label = "Streambank file"),
          need(
            input$streamcat_crop_file_nh != "", 
            label = "New Hampshire StreamCat Cropland file"
          ),
          need(
            input$streamcat_crop_file_vt != "", 
            label = "Vermont StreamCat Cropland file"
          )
        )
      )
      ## Warnings ####
      isolate(
        validate(
          need(
            input$NEOS_results$size < 10000000,
            "NEOS Results file size detected as unusually large. Please double check inputs."
          ),
          need(
            input$user_loading$size < 2000000,
            "User specified loading target file size detected as unusually large. Please double check inputs."
          ),
          need(
            input$streambank_in$size < 2000000,
            "Streambank file size detected as unusually large. Please double check inputs."
          ),
          need(
            input$point_comid$size < 10000000,
            "WWTP file size detected as unusually large. Please double check inputs."
          ),
          need(
            input$streamcat_crop_file_vt$size < 10000000,
            "Vermont StreamCat file size detected as unusually large. Please double check inputs."
          ),
          need(
            input$streamcat_crop_file_nh$size < 10000000,
            "New Hampshire StreamCat file size detected as unusually large. Please double check inputs."
          )
        )
      )
      ## Check formattings ####
      isolate(
        validate(
          need(
            scen.vis() <= num.scen(), 
            "Selected scenario does not exist in this result file. Please change 'View available scenarios' to 1 and press 'View NEOS Results' again."
          ),
          need(
            solved()[scen.vis()] != -99, 
            "There is a problem with the provided NEOS results file. Please ensure the entire output has been copied and pasted into the .txt document."
          ),
          need(
            all(
              c("State", "Plant_Name", "NPDES_ID", "COMID") %in% 
                names(point_comid_reactive())
            ),
            "Unexpected formatting of WWTP file. Please check inputs."
          ),
          need(
            all(
              c("Waterbody_Name", "ComID", "Percent_Reduction", "TN_or_TP") %in% 
                names(UserSpecs_loadingtargets_reactive())
            ),
            "Unexpected formatting of User specified loading target file. Please check inputs."
          ),
          need(
            all(
              c("ComID", "TermFlag", "IncAreaKm2", "DivFrac", "HUC_12") %in% 
                names(sparrow_in_reactive())
            ),
            "Unexpected formatting of SPARROW inputs file. Please check inputs."
          ),
          need(
            all(
              c("comid", "totalbanklength_ft") %in% names(streambank_in_reactive())
            ),
            "Unexpected formatting of streambank length file. Please check inputs."
          ),
          need(
            ncol(streamcat_file_nh_reactive()) == 
              ncol(streamcat_file_vt_reactive()), 
            "Number of columns in state Streamcat files differ. Please check inputs and formatting."
          ),
          need(
            grepl("VT|vt|Vt|Ver|VER|ver", input$streamcat_crop_file_vt$name), 
            "For validation purposes, this postprocessor requires that Streamcat files include a recognizable state identifier (such as a standard 2 letter abbreviation) in the file name. Your Vermont file is either missing such an identifier, or has an identifier that matches a different state."
          ),
          need(
            grepl("NH|nh|Nh|Hamp|hamp|HAMP", input$streamcat_crop_file_nh$name), 
            "For validation purposes, this postprocessor requires that Streamcat files include a recognizable state identifier (such as a standard 2 letter abbreviation) in the file name. Your New Hampshire file is either missing such an identifier, or has an identifier that matches a different state."
          ),
          if(solved()[scen.vis()] == 1 & !is.null(loading_bootstraps())) {
            need(
              all(!is.na(loading_uncertainty_message()$Target.name)),
              "The User Specified Loading Target file provided does not seem to match the result file you have selected. Please check inputs."
            )
          },
          if(solved()[scen.vis()] == 1 & !is.null(loading_bootstraps())) {
            if(all(!is.na(loading_uncertainty_message()$Target.name))) {
              need(
                nrow(loading_uncertainty_message()) == 
                  nrow(UserSpecs_loadingtargets_munged()),
                "The User Specified Loading Target file provided does not seem to match the result file you have selected. Please check inputs."
              )
            }
          }
        )
      )
      ## Display results ####
      isolate(
        {
          mainPanel(
            titlePanel("Scaled-up Optimization Results"),
            em("To save this report, open the RBEROST postprocessor in a web browser and use the print to pdf functionality."),
            wellPanel(
              h4("User Notes:"),
              textOutput("usernotes")
            ),
            conditionalPanel(
              condition = "output.solvedstatus == 0",
              {
                fluidRow(
                  wellPanel(h3("Your model was unable to solve.")),
                  p(
                    paste(
                      "RBEROST may fail to optimize models for a variety of reasons. 
                      Often, this is a result of loading targets that are too restrictive, or a result of not including enough BMPs for the model to use. 
                      It may be informative to run RBEROST again with less restrictive loading targets and/or including more BMPs in the optimization.
                      Please check your user inputs and refer to the Model Sensitivity section of the documentation for more information."
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
                          400*nrow(UserSpecs_loadingtargets_reactive()),"px"
                        )
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "output.pointresultsavail",
                    fluidRow(
                      p(
                        "The model chose to implement WWTP retrofits at the following locations."
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
                        "The model chose to implement the total area of the following agricultural BMPs."
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
      )
    }
  )
}
