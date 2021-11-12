##############################################################
### TITLE: Postprocessing Shiny App User Interface         ###                                                 
### PURPOSE: To allow users to input a text file and see   ###                                                       
###   the results of their NEOS optimization               ###                                           
### BY: Cathy Chamberlin                                   ###                       
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

# This file is NOT intended to be run by itself. It is sourced through 02_Optimization_RunShiny.R (which in turn is sourced through RunRBEROST.Rmd) along with Optimization_ServerFile.R to run the shiny app.
# Lines marked #*# may be changed by the user.

# The components of the User Interface are roughly organized as such:

##################################################
# Table of Contents:
# 1. General Formatting
# 2. Side Bar for uploading files
# 3. Preview Tab
# 4. Results Tab
##################################################

# source(paste0(shinyapplocation, "Optimization_UI_Preprocessor.R"), local = TRUE)
# source(paste0(shinyapplocation, "Optimization_UI_NEOSInteraction.R"), local = TRUE)
source(paste0(shinyapplocation, "Optimization_UI_Postprocessor.R"), local = TRUE)


ui <- fluidPage(
  ##################################################
  # 1.General Formatting
  ##################################################
  tags$head(
    tags$style(
      HTML(
        ".shiny-output-error-validation {color: #0dc5c1; font-style: italic;}"
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      
      conditionalPanel(
        condition = "input.steps == 'Postprocessing Step'",
        fileInput( 
          inputId = "NEOS_results",
          label = "Choose Results File",
          multiple = FALSE, 
          accept = c("text/plain", ".txt")
        ),
        tags$hr() 
      ),   
      
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step' | input.steps == 'Postprocessing Step'",
        fileInput( 
          inputId = "user_loading",
          label = "Choose the corresponding `01_UserSpecs_loadingtargets.csv` file",
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr() 
      ),  
      
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step' | input.steps == 'Postprocessing Step'",
        fileInput( 
          inputId = "point_comid",
          label = "Choose WWTP File",
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr() 
      ),  
      
      
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step'",
        fileInput( 
          inputId = "point_loadings_adj",
          label = "Choose WWTP Baseline Removals File",
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr()
      ),  
      
      
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step'",
        fileInput( 
          inputId = "point_efficiencies",
          label = "Choose WWTP Efficiencies File",
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr() 
      ),  
      
      
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step'",
        fileInput( 
          inputId = "urban_efficiencies",
          label = "Choose Urban Efficiency Curves File",
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr() 
      ), 
      
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step' | input.steps == 'Postprocessing Step'", 
        fileInput( 
          inputId = "sparrow_in",
          label = "Choose SPARROW Inputs File",
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr() 
      ),  
      
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step'", 
        fileInput(
          "sparrow_tn", 
          label = "Choose SPARROW Nitrogen Outputs File", 
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr() 
      ),
      
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step'", 
        fileInput(
          "sparrow_tp", 
          label = "Choose SPARROW Phosphorus Outputs File", 
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr() 
      ),
      
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step'", 
        fileInput( 
          inputId = "streambank_in",
          label = "Choose Streambank File",
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr() 
      ),  
      
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step'", 
        fileInput( 
          inputId = "riparian_loadings",
          label = "Choose Riparian Loadings File",
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ), 
        tags$hr() 
      ),  
      
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step'", 
        fileInput( 
          inputId = "riparian_efficiencies",
          label = "Choose Riparian Efficiencies File",
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),  
        tags$hr() 
      ),  
      
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step'", 
        fileInput( 
          inputId = "ACRE_summary",
          label = "Choose ACRE HUC12 HRU Summary",
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),     
        tags$hr()
      ),  
      
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step'", 
        fileInput( 
          inputId = "fertmanure",
          label = "Choose Efficiency File for Fert_20 and Manure Injection",
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr()
      ),  
      
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step'", 
        fileInput( 
          inputId = "NdepChange",
          label = "Choose Ndep Change File",
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr()
      ),  
      
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step'", 
        fileInput( 
          inputId = "NHDplus_infiltration",
          label = "Choose NHD+v2 Infiltration Rates File",
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),  
        tags$hr()
      ),       
      
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step' | input.steps == 'Postprocessing Step'", 
        fileInput(
          inputId = "streamcat_crop_files_all", 
          label = "Choose State Cropland Streamcat Files", 
          multiple = TRUE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        tags$hr()
      ),
      
      conditionalPanel(
        condition = "input.steps == 'Preprocessing Step'", 
        selectInput(
          inputId = "streamcat_imp",
          label = "Select Streamcat 2011 Imperviousness State File",
          choices = c("Vermont" = "VT", "New Hampshire" = "NH")
        )
      ),
      
      conditionalPanel(
        condition = "input.streamcat_imp == 'VT' & input.steps == 'Preprocessing Step'", 
        fileInput(
          "streamcat_imp_file_vt", 
          label = "Choose Streamcat File", 
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.streamcat_imp == 'NH' & input.steps == 'Preprocessing Step'", 
        fileInput(
          "streamcat_imp_file_nh", 
          label = "Choose Streamcat File", 
          multiple = FALSE, 
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.steps == 'Postprocessing Step'", 
                p(em("Important: Selecting 'Preview Uploads' or 'View NEOS Results' before all uploads have completed may produce unexpected results. Previews can be found under the 'File Preview' tab, and generated reports can be found under the 'View Results' tab.")),
        actionButton("preview", label = "Preview Uploads"),
        actionButton("ViewReport", label = "View NEOS Results")
      )
    ),
    mainPanel = mainPanel(
      tabsetPanel(
        type = "pills",
        id = "steps",
        # PreprocessingTab,
        # NEOSInterfaceTab,
        PostprocessingTab
      )
    )
  )
)
