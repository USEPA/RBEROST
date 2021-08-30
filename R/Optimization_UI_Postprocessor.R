##############################################################
### RBEROST Postprocessing User Interface                  ###
### Date: August 28, 2021                                  ###
### Author: Cathy Chamberlin                               ###
### Purpose: Create the Postprocessing tab of the Shiny    ###
###   interface                                            ###
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

PostprocessingTab <- tabPanel(
  title = "Postprocessing Step",
  titlePanel(title = "RBEROST Results Postprocessing Step"), 
      tabsetPanel(
        type = "tabs", 
        tabPanel(
          "File Preview", 
          conditionalPanel(
            condition = "input.preview > 0", 
            uiOutput("previews") %>% withSpinner(color = "#0dc5c1")# having the spinner here for whatever reason doesn't work. The headers load long before the tables
          )
        ), 
        tabPanel(
          "View Results", 
            selectInput(
              inputId = "scenario", 
              label = "View available scenarios", 
              choices = c(1),
              selected = 1
                ),
          conditionalPanel(
            condition = "input.ViewReport > 0",
            uiOutput("report") %>% withSpinner(color = "#0dc5c1")
          )
        )
      ) 
    )
