##############################################################
### RBEROST Preprocessing Gateway                          ###
### Date: May 6, 2021                                      ###
### Author: Cathy Chamberlin                               ###
### Purpose: Bridge between RBEROST run file and R scripts ###
###                                                        ###  
###    Copyright (C) 2021  US EPA                          ###        
###                                                        ###  
###    This program is free software: you can redistribute ###                                                          
###    it and/or modify it under the terms of the GNU      ###                                                      
###    General Public License as published by the Free     ###                                                      
###    Software Foundation, either version 3 of the        ###                                                    
###    License, or(at your option) any later version.      ###                                                    
###                                                        ###           
###    This program is distributed in the hope that it     ###                                                               
###    will be useful, but WITHOUT ANY WARRANTY; without   ###                                                                 
###    even the implied warranty of MERCHANTABILITY or     ###                                                               
###    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      ###                                                               
###    General Public License for more details.            ###                                                       
###                                                        ###           
###    You should have received a copy of the GNU General  ###                                                                   
###    Public License along with this program.  If not,    ###                                                                 
###    see <https://www.gnu.org/licenses/>.                ###                                                   
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

if(exists("IncludeUncertainty")) {
  if(IncludeUncertainty == TRUE) {
  source("./R/01_Optimization_Preprocessing+Uncertainty.R")
} else if(IncludeUncertainty == FALSE) {
    source("./R/01_Optimization_Preprocessing.R")
} else {
    print(
      "Only allowable options are 'IncludeUncertainty = TRUE' or 'IncludeUncertainty = FALSE'. You cannot have both, only one or the other."
    )
  }
} else {
  print(
    "Did you delete the line of code that says 'IncludeUncertainty = TRUE' or 'IncludeUncertainty = FALSE'?"
  )
  }