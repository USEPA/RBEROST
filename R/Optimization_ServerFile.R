##############################################################
### TITLE: Shiny App Server                                ###                                   
### PURPOSE: To allow users to run the postprocessor and   ###
### eventually the preprocessor through Shiny              ###                                     
### BY: Cathy Chamberlin, with previous version by Kate    ###                                                 
###   Munson and Alyssa Le (ICF)                           ###                         
### DATE: 9/22/2021                                        ###           
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
# 4. Parse NEOS data file (postprocessor)
# 5. Combine NEOS results with other dataframes (post)
# 6. Write Preview Outputs
# 7. Render Preview UI
# 8. Write Report Outputs
# 9. Render Report UI
##################################################

source("./R/Optimization_ServerFunctions_Postprocessor.R")

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
      read_csvs_using_fread(input$user_loading$datapath)
    }
  )
  
  NEOS_results_reactive <- reactive(
    {
      req(input$NEOS_results)
      read_csvs_using_baseR(input$NEOS_results$datapath)
    }
  )
  
  point_comid_reactive <- reactive(
    {req(input$point_comid); read_csvs_using_fread(input$point_comid$datapath)}
  )
  
  sparrow_in_reactive <- eventReactive(
    eventExpr = input$sparrow_in,
    {
      req(input$sparrow_in)
      read_csvs_using_fread(input$sparrow_in$datapath)
    }
  )
  
  streamcat_files_all_reactive <- reactive(
    {
      req(input$streamcat_crop_files_all)
      read_all_streamcat_files(input$streamcat_crop_files_all)
    }
  )
  ##################################################
  # 4.Parse NEOS data file ####
  ##################################################  
  
  NEOS_renamed <- eventReactive(
    eventExpr = {input$ViewReport},
    valueExpr = {
      req(
        nrow(NEOS_results_reactive()) > 0, 
        nrow(UserSpecs_loadingtargets_reactive()) > 0,
        nrow(point_comid_reactive()) > 0,
        nrow(sparrow_in_reactive()) > 0,
        all(lapply(streamcat_files_all_reactive(), nrow) > 0),
        input$NEOS_results$size < 10000000
      )
      NEOS_results_reactive() %>% rename(Output = 1)
    }
  )
  
  solved <- reactive({req(NEOS_renamed); get_solvestatus(NEOS_renamed())})
  
  user_notes <- reactive({req(NEOS_renamed); get_user_notes(NEOS_renamed())})
  
  num.scen <- reactive({req(NEOS_renamed, solved); length(solved())})
  
  scen.vis <- reactive(
    {req(input$scenario); get_scen.vis(input$scenario, num.scen())}
  )
  
  error_display <- reactive(
    {
      req(NEOS_renamed, solved()[scen.vis()] == 0)
      get_error_display(NEOS_renamed())
    }
  )
  
  total_cost <- reactive(
    {
      req(NEOS_renamed, solved()[scen.vis()] == 1)
      get_total_cost(NEOS_renamed(), scen.vis())
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
      get_cost_bootstraps(
        NEOS_renamed(), scen.vis(), NEOS_end_rows(), Table_start_rows()
      )
    }
  )
  
  cost_uncertainty_message <- reactive({
    req(
      !is.null(cost_bootstraps), 
      solved()[scen.vis()] == 1, 
      nrow(cost_bootstraps()) > 3
    )
    get_cost_uncertainty_message(cost_bootstraps())
  })
  
  loading_targets <- reactive({
    req(NEOS_renamed, NEOS_end_rows, solved()[scen.vis()] == 1)
    get_loading_targets(NEOS_renamed())
  })
  
  
  loading_bootstraps <- reactive({
    req(NEOS_renamed, NEOS_end_rows, solved()[scen.vis()] == 1)
    get_loading_bootstraps(
      NEOS_renamed(), 
      scen.vis(), 
      num.scen(), 
      NEOS_end_rows(), 
      Table_start_rows()
    ) 
  })
  
  UserSpecs_loadingtargets_munged <- eventReactive(
    eventExpr = input$ViewReport, 
    {      
      req(
        nrow(UserSpecs_loadingtargets_reactive()) > 0, 
        solved()[scen.vis()] == 1,
        NEOS_renamed,
        "TN_or_TP" %in% names(UserSpecs_loadingtargets_reactive())
      )
      mung_UserSpecs_loadingtargets(UserSpecs_loadingtargets_reactive())
    })
  
  loading_uncertainty_message <- reactive({
    req(
      !is.null(loading_bootstraps), 
      solved()[scen.vis()] == 1,
      nrow(loading_bootstraps()) > 3,
      loading_targets,
      UserSpecs_loadingtargets_munged
    )
    
    get_loading_uncertainty_message(
      loading_bootstraps(), loading_targets(), UserSpecs_loadingtargets_munged()
    ) 
  })
  
  
  point_results <- reactive(
    {
      req(NEOS_renamed, NEOS_end_rows, solved()[scen.vis()] == 1)
      get_point_results(NEOS_renamed(), scen.vis(), NEOS_end_rows()) 
    }
  )
  
  urban_bmp_vec <- reactive(
    {
      req(NEOS_renamed, solved()[scen.vis()] == 1)
      read_postprocessor_urban_bmp_vec(NEOS_renamed(), scen.vis()) 
    }
  )
  
  urban_results <- reactive(
    {
      req(
        NEOS_renamed,
        NEOS_end_rows,
        length(urban_bmp_vec()) > 0,
        solved()[scen.vis()] == 1
      )
      parse_urban_results(
        NEOS_renamed(), scen.vis(), NEOS_end_rows(), urban_bmp_vec()
      )
    }
  )
  
  ag_bmp_vec <- reactive(
    {
      req(NEOS_renamed, solved()[scen.vis()] == 1)
      read_postprocessor_ag_bmp_vec(NEOS_renamed(), scen.vis()) 
    }
  )
  
  ag_results <- reactive(
    {
      req(
        NEOS_renamed, 
        NEOS_end_rows, 
        length(ag_bmp_vec()) > 0, 
        solved()[scen.vis()] == 1
      )
      parse_ag_results(
        NEOS_renamed(), scen.vis(), NEOS_end_rows(), ag_bmp_vec()
      )
    }
  )
  
  ripbuf_bmp_vec <- reactive(
    {
      req(NEOS_renamed, solved()[scen.vis()] == 1)
      read_postprocessor_ripbuf_bmp_vec(NEOS_renamed(), scen.vis()) 
    }
  )
  
  ripbuf_results <- reactive(
    {
      req(
        NEOS_renamed,
        NEOS_end_rows, 
        length(ripbuf_bmp_vec()) > 0, 
        solved()[scen.vis()] == 1
      )
      parse_ripbuf_results(
        NEOS_renamed(), scen.vis(), NEOS_end_rows(), ripbuf_bmp_vec()
      )
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
        streamcat_files_all_reactive,
        solved()[scen.vis()] == 1,
        "HUC_12" %in% names(sparrow_in_reactive()),
        all(
          diff(
            unlist(
              lapply(streamcat_files_all_reactive(), ncol), 
              use.names = FALSE
            )
          ) == 0
        )
      )
      make_areadf(
        sparrow_in_reactive(), km2_to_ac, streamcat_files_all_reactive()
      )
    }
  )
  
  urban_results_HUCmerge <- reactive(
    {
      req(
        !is.null(urban_results), urban_bmp_vec, area, solved()[scen.vis()] == 1
      )
      merge_urban_results(urban_results(), area(), urban_bmp_vec(), km2_to_ac) 
    }
  )
  
  urban_total <- reactive(
    {
      req(urban_results_HUCmerge, urban_bmp_vec, solved()[scen.vis()] == 1)
      calculate_urban_total(urban_results_HUCmerge(), urban_bmp_vec())
    }
  )
  
  urban_results_HUC <- reactive(
    {
      req(
        urban_results_HUCmerge, 
        length(urban_bmp_vec()) > 0, 
        solved()[scen.vis()] == 1
      )
      present_urban_results_HUC(urban_results_HUCmerge(), urban_bmp_vec())
    }
  )
  
  ag_results_HUCmerge <- reactive(
    {
      req(!is.null(ag_results), area, ag_bmp_vec, solved()[scen.vis()] == 1)
      merge_ag_results(ag_results(), area(), ag_bmp_vec(), km2_to_ac)
    }
  )
  
  ag_results_HUC <- reactive(
    {
      req(ag_results_HUCmerge, ag_bmp_vec, solved()[scen.vis()] == 1)
      present_ag_results_HUC(ag_results_HUCmerge(), ag_bmp_vec()) 
    }
  )
  
  ag_total <- reactive(
    {
      req(ag_results_HUCmerge, ag_bmp_vec, solved()[scen.vis()] == 1)
      calculate_ag_total(ag_results_HUCmerge(), ag_bmp_vec()) 
    }
  )
  
  ripbuf_results_HUCmerge <- reactive(
    {
      req(
        sparrow_in_reactive, 
        ripbuf_bmp_vec, 
        solved()[scen.vis()] == 1, 
        !is.null(ripbuf_results())
      )
      merge_ripbuf_results(
        ripbuf_bmp_vec(), ripbuf_results(), area(), sparrow_in_reactive()
      )      
    }
  )
  
  ripbuf_results_HUC <- reactive(
    {
      req(
        ripbuf_results_HUCmerge, 
        length(ripbuf_bmp_vec()) > 0,
        solved()[scen.vis()] == 1
      )
      present_ripbuf_results_HUC(ripbuf_results_HUCmerge(), ripbuf_bmp_vec())
    }
  )
  
  ripbuf_total <- reactive(
    {
      req(ripbuf_results_HUCmerge, ripbuf_bmp_vec, solved()[scen.vis()] == 1)
      calculate_ripbuf_total(ripbuf_results_HUCmerge(), ripbuf_bmp_vec()) 
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
      parse_point_results_wwtp(point_results(), point_comid_reactive())
    }
  )
  
  point_plants <- reactive(
    {
      req(
        point_results_wwtp, 
        solved()[scen.vis()] == 1, 
        "Plant_Name" %in% names(point_results_wwtp())
      )
      get_point_plants(point_results_wwtp()) 
    }
  )
  
  ##################################################
  # 6. Write Preview Outputs ####
  ##################################################  
  
  output$preview_NEOS_results_ui <- renderUI(
    {    
      req(NEOS_results_reactive, input$preview)
      input$preview
      isolate(validate(need(input$NEOS_results != "", label = "Results file")))
      render_preview_ui(
        NEOS_results_reactive(), 
        "preview_NEOS_results",
        "NEOS Results File", 
        output
      )
    }
  )
  
  output$preview_user_loading_ui <- renderUI(
    {    
      req(UserSpecs_loadingtargets_reactive, input$preview)
      input$preview
      isolate(
        validate(
          need(
            input$user_loading != "", 
            label = "User Specified Loading Targets file"
          )
        )
      )
      render_preview_ui(
        UserSpecs_loadingtargets_reactive(), 
        "preview_user_loading", 
        "User Inputs File (loading targets)", 
        output
      )
    }
  )
  
  output$preview_point_comid_ui <- renderUI(
    {    
      req(point_comid_reactive, input$preview)
      input$preview
      isolate(validate(need(input$point_comid != "", label = "WWTP file")))
      render_preview_ui(
        point_comid_reactive(), "preview_point_comid", "WWTP File", output
      )
    }
  )
  
  output$preview_sparrow_in_ui <- renderUI(
    {    
      req(sparrow_in_reactive, input$preview)
      input$preview
      isolate(validate(need(input$sparrow_in != "", label = "SPARROW file")))
      render_preview_ui(
        sparrow_in_reactive(), 
        "preview_sparrow_in", 
        "SPARROW Inputs File",
        output
      )
    }
  )
  
  preview_tables_streamcat_crop_files_all <- reactive(
    {
      req(input$preview)
      req(streamcat_files_all_reactive)
      isolate(
        validate(
          need(
            input$streamcat_crop_files_all != "",
            label = "At least one StreamCat Cropland file"
          ),
          if(length(input$streamcat_crop_files_all) > 0) {
            need(
              all(
                str_locate(
                  names(streamcat_files_all_reactive()), paste0(state.abb, collapse = "|")
                )[,"start"] == 1
              ), 
              "All state StreamCat cropland file names must begin with the two-letter state abbreviation. Please check your file names and rename as necessary."
            )
          }
        )
      )
      isolate(lapply(streamcat_files_all_reactive(), head))
    }
  )
  
  
  observeEvent(
    preview_tables_streamcat_crop_files_all(),
    {
      make_streamcat_previews(preview_tables_streamcat_crop_files_all(), output)
    }
  )
  
  
  output$streamcat_preview_ui <- renderUI({
    req(preview_tables_streamcat_crop_files_all)
    render_streamcat_previews(preview_tables_streamcat_crop_files_all()) 
  })
  
  ##################################################
  # 7. Render Preview UI ####
  ##################################################  
  output$previews <- renderUI(
    {
      input$preview
      req(input$preview)
      isolate({make_postprocessor_previews()})
    }
  )
  
  ##################################################
  # 8. Write Report Outputs ####
  ################################################## 
  observeEvent(
    eventExpr = {input$ViewReport},
    handlerExpr = {
      updateSelectInput(
        session, "scenario", choices = c(1:num.scen()), selected = 1
      )
    }
  )  
  
  output$errordisplay <- renderPrint(
    {req(error_display); return(paste(gsub("\t", "", error_display())))}
  )
  
  output$usernotes <- renderUI(
    expr = 
      {
        req(user_notes);
        text <- HTML(gsub("\n", "<br/>", user_notes()))
        return(value = text)
      }
  )
  
  output$costdisplay <- renderText(
    expr = {req(total_cost); return(value = print_costdisplay(total_cost()))}
  )
  
  ## Display Uncertainty ####
  output$costdisplay_uncertainty <- renderText(
    expr = {
      req(!is.null(cost_uncertainty_message()))
      return(value = cost_uncertainty_message())
    }
  )
  
  output$costdisplay_uncertaintyplot <- renderPlot(
    expr = {
      req(!is.null(cost_uncertainty_message()))
      return(render_costdisplay_uncertaintyplot(cost_bootstraps()))
    },
    res = 150
  )
  
  output$costuncertaintyresultsavail <- reactive(
    {req(!is.null(cost_uncertainty_message()))}
  )
  
  outputOptions(
    output, "costuncertaintyresultsavail", suspendWhenHidden = FALSE
  )
  
  output$loadingdisplay_uncertainty <- renderTable(
    expr = {
      req(!is.null(loading_uncertainty_message()))
      return(
        value = render_loadingdisplay_uncertainty(loading_uncertainty_message())
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
        render_loadingdisplay_uncertaintyplot(
          loading_bootstraps(), 
          UserSpecs_loadingtargets_munged(), 
          loading_targets()
        )
      )
    },
    res = 150,
    height = "auto"
  )
  
  output$loadingsummarymessage <- renderText({
    req(!is.null(loading_uncertainty_message()))
    return(value = render_loadingsummarymessage(loading_uncertainty_message()))
  })
  
  output$loadinguncertaintyresultsavail <- reactive(
    {req(!is.null(loading_uncertainty_message()))}
  )
  
  outputOptions(
    output, "loadinguncertaintyresultsavail", suspendWhenHidden = FALSE
  )
  
  ## Urban Output ####
  
  output$urban <- renderTable(
    expr = {
      req(urban_total, length(urban_bmp_vec()) > 0)
      return(value = urban_total())
    }
  )
  
  output$urbandownloadavail <- reactive(
    {req(urban_results_HUC); nrow(urban_results_HUC()) > 0}
  )
  
  outputOptions(output, "urbandownloadavail", suspendWhenHidden = FALSE)
  
  output$downloadurbandat <- downloadHandler(
    filename = reactive(
      {paste0("Urban_by_ComID_Scenario", scen.vis(), ".csv")}
    ),
    content = function(file) {write.csv(urban_results_HUC(), file)}
  )
  
  ## Ag Output
  output$ag <- renderTable(
    expr = {req(ag_total, length(ag_bmp_vec()) > 0); return(value = ag_total())}
  )
  
  output$agdownloadavail <-reactive(
    {req(ag_results_HUC); nrow(ag_results_HUC()) > 0}
  )
  outputOptions(output, "agdownloadavail", suspendWhenHidden = FALSE)
  
  output$downloadagdat <- downloadHandler(
    filename = reactive(
      {paste0("AgBMP_by_ComID_Scenario", scen.vis(), ".csv")}
    ),
    content = function(file) {write.csv(ag_results_HUC(), file)}
  )
  
  ## Riparian Output
  output$ripbuf <- renderTable(
    expr = {
      req(ripbuf_total, length(ripbuf_bmp_vec()) > 0)
      return(value = ripbuf_total())
    }
  )
  
  output$ripbufdownloadavail <-reactive(
    {req(ripbuf_results_HUC); nrow(ripbuf_results_HUC()) > 0}
  )
  
  outputOptions(output, "ripbufdownloadavail", suspendWhenHidden = FALSE)
  
  output$downloadripbufdat <- downloadHandler(
    filename = reactive(
      {paste0("RipBufBMP_by_ComID_Scenario", scen.vis(), ".csv")}
    ),
    content = function(file) {write.csv(ripbuf_results_HUC(), file)}
  )
  
  ## WWTP outputs
  output$wwtp <- renderTable(
    expr = {req(nrow(point_plants()) > 0); return(value = point_plants())}
  )
  
  output$pointresultsavail <- reactive(
    {req(point_plants); nrow(point_plants()) > 0}
  )
  
  outputOptions(output, "pointresultsavail", suspendWhenHidden = FALSE)
  
  output$solvedstatus <- reactive({req(solved); solved()[scen.vis()]})
  
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
          need(
            input$user_loading != "", 
            label = "User specified loading targets file"
          ),
          need(input$point_comid != "", label = "WWTP file"),
          need(input$sparrow_in != "", label = "SPARROW file"),
          need(
            input$streamcat_crop_files_all != "",
            label = "At least one StreamCat Cropland file"
          )
        )
      )
      # Warnings ####
      
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
            input$point_comid$size < 10000000,
            "WWTP file size detected as unusually large. Please double check inputs."
          ),
          need(
            all(input$streamcat_crop_files_all$size < 10000000),
            paste(paste0(input$streamcat_crop_files_all$name[which(!(input$streamcat_crop_files_all$size < 10000000))], collapse = ", "), "StreamCat file size(s) detected as unusually large. Please double check inputs.")
          )
        )
      )
      
      ## Check formatings ####
      isolate(
        validate(
          need(
            scen.vis() <= num.scen(),
            "Selected scenario does not exist in this result file. Please change 'View available scenarios' to 1 and press 'View NEOS Results' again."
          ),
          need(
            solved()[scen.vis()] != -99,
            "There is a problem with the provided NEOS results file, and no solve results were detected. Please ensure the entire output has been copied and pasted into the .txt document. If the problem persists, try resubmitting the NEOS job being careful to upload the .mod file to the Model File, the .dat file to the Data File, and the .amp file to the Commands File."
          ),
          need(
            solved()[scen.vis()] != -98,
            "An unexpected error has occured. Suggested to retry Preprocessing and NEOS submission steps."
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
              diff(
                unlist(
                  lapply(streamcat_files_all_reactive(), ncol),
                  use.names = FALSE
                )
              ) == 0
            ),
            "Number of columns in state Streamcat files differ. Please check inputs and formatting."
          ),
          if(solved()[scen.vis()] == 1 & isTruthy(tryCatch(loading_bootstraps(), error = function(x) {FALSE}))) {
            need(
              tryCatch(all(!is.na(loading_uncertainty_message()$Target.name)), error = function(x) {FALSE}),
              "The User Specified Loading Target file provided does not seem to match the result file you have selected. Please check inputs."
            )
          },
          if(solved()[scen.vis()] == 1 & isTruthy(tryCatch(loading_bootstraps(), error = function(x) {FALSE}))) {
            if(tryCatch(all(!is.na(loading_uncertainty_message()$Target.name)), error = function(x) {FALSE})) {
              need(
                nrow(loading_uncertainty_message()) ==
                  nrow(UserSpecs_loadingtargets_munged()),
                "The User Specified Loading Target file provided does not seem to match the result file you have selected. Please check inputs."
              )
            }
          },
          if(solved()[scen.vis()] == 1) {
            need(
              !(
                tryCatch(
                  any(is.na(urban_results_HUCmerge())),
                  error = function(x) {FALSE}
                ) |
                  tryCatch(
                    any(is.na(ag_results_HUCmerge())),
                    error = function(x) {FALSE}
                  ) |
                  tryCatch(
                    any(is.na(ripbuf_results_HUCmerge())),
                    error = function(x) {FALSE}
                  )
              ),
              "Catchment information (from the State Cropland Streamcat files) is missing for some catchments. Please ensure all files are uploaded that were used in the Preprocessing step."
            )
          },
          need(
            length(
              which(
                duplicated(str_sub(names(streamcat_files_all_reactive()), 1, 2))
              )
            ) == 0,
            paste0(
              "Duplicate streamcat files for ", 
              paste0(
                str_sub(names(streamcat_files_all_reactive()), 1, 2)[
                  duplicated(
                    str_sub(names(streamcat_files_all_reactive()), 1, 2)
                  )
                ], 
                collapse = ", "
              ), 
              " cropland files. Please check inputs."
            )
          )
        )
      )
      
      ## Display results ####
      isolate({render_results_display(UserSpecs_loadingtargets_reactive())})
    }
  )
}

