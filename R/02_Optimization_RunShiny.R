##############################################################
### TITLE: Postprocessing Shiny App Run File               ###                           
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

# This file is intended to be run through RunRBEROST.Rmd. Users can select the green 'Run App' button, run each line of code individually, or source the document.
# Lines marked #*# may be changed by the user.

# Current input data refers to Northeastern Regional SPARROW loading data and land use data specific to Vermont and New Hampshire. For other states, users will need to build out the UserInferface and Server files.

##################################################
# 1. Setup
##################################################

packages <- c(
  "tidyverse",
  "shiny", 
  "foreach", 
  "shinycssloaders", 
  "data.table"
  )

# lapply(packages, install.packages) #*# Must install packages before running in R. Remove "#" symbols for "lapply(packages, install.packages)" line of code before running code.
invisible(
  suppressPackageStartupMessages(
    lapply(packages, library, character.only = TRUE)
    )
)

##################################################
# 2. Source User Interface and Server files
##################################################
rm(list=ls()) # Clears R memory so the code can be run multiple times in a single R session

shinyapplocation <- "./R/"

source(paste0(shinyapplocation, "Optimization_UserInterfaceFile.R"), local = TRUE) #*#
source(paste0(shinyapplocation, "Optimization_ServerFile.R"), local = TRUE) #*#
options(shiny.maxRequestSize = 100 * 1024 ^ 2, scipen = 999) # These option allow the SPARROW and StreamCat files to be uploaded, and prevent numeric data from being printed in scientific notation


##################################################
# 3. Create Shiny app
##################################################
app <- shinyApp(ui, server)

runApp(app, launch.browser = TRUE)



