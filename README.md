# RBEROST

This project expands the WMOST[1] framework for implementation in the upper Connecticut river HUC6. The River Basin Export Reduction Optimization Support Tool (RBEROST) uses data on best management practice (BMP) efficiencies, costs, and baseline nutrient loadings to create an optimization problem that will meet numerous loading targets in the watershed for the lowest financial cost.

The main branch of this github repository includes only the data and code files necessary to run RBEROST. Additional files that format data or create various illustrations for presentations or publication are available in the development branch.

There are 4 steps to running RBEROST.
1) Assembling and formatting data. Data for the case study in the Upper Connecticut River basin are provided, and RBEROST expects data to be formatted the same as these example files.
2) Writing the AMPL scripts. The mathematical optimization for RBEROST is written in AMPL. This step is accomplished by running the "Run Preprocessor" section in RunRBEROST.Rmd. There are several items users can edit before running this code, including the file path where the data is located, the file path where output files should be written to, the planning horizon (default is 15 years), the expected interest rate (default is 3%), and whether or not to include uncertainty analysis. If users choose to include uncertainty analysis, they additionally may choose the number of scenarios they wish to view, and the step change between these scenarios. The step change is the percent margin of error that will be added incrementally between each scenario. Running this code chunk will source the R script 01_Optimization_Preprocessor.R, which takes the data files, provided as csv files, and creates the AMPL scripts that describe the mathematical problem. The 3 scripts are the command script, the data script, and the model script. To define the problem, the user should edit the two 01_UserSpecs files to define (1) the BMPs they wish to implement, the extent of their implementation, BMP costs, and design depths and (2) the locations, type and target reduction of nutrient loading targets within the watershed. Defaults for many of these options are given, but the user may override any parameter they wish. If the user chooses to include uncertainty in their analysis, RBEROST will then source 01_Optimization_Preprocessor+Uncertainty.R, and three additional AMPL files will be created with the tags "_uncertainty". 
3) Submitting the AMPL scripts for optimization. RBEROST uses a free online server, NEOS[2], to solve the AMPL optimization problem. The tool currently recommends using the CPLEX solver. Users must navigate to https://neos-server.org/neos/solvers/lp:CPLEX/AMPL.html, upload their command, data, and model files, write a short description of their model and provide their email address before submitting. Solve times typically take less than 1 minute for models without uncertainty, or >10 minutes for models with uncertainty. The results can be retrieved through a link emailed to the user at the given email address. Occasionally the results may be printed in the body of the email instead. The results should be copied and pasted into a .txt file and saved to the local machine for use in the post processor.
4) Viewing NEOS results. The results of the NEOS optimization can be viewed by running the code chunk "Run Postprocessor" in RunRBEROST.Rmd. This code will launch an RShiny app by sourcing the User Interface and Server files provided in the "R" folder. Within the RShiny app, users can dynamically choose the NEOS results they wish to view. After providing necessary results .txt file and several of the input .csv files, the user will be able to preview the uploaded files, and see a summary report of total cost and BMP implementation. The user may also download more details of BMP implementation in each NHD+ subcatchment as csv files.

The project is organized into 3 folders.
1) R: The R folder contains all code files. 01_Optimization_Preprocessing.R runs the preprocessor and creates AMPL script files, 01_Optimization_Preprocessing+Uncertainty.R runs the preprocessor with the uncertainty analysis module included, and creates AMPL script files, and 01_Optimization_Preprocessing_gateway.R selects which script to run based on user inputs to RunRBEROST.Rmd. 02_Optimization_RunShiny.R sources Optimization_ServerFile.R, Optimization_ServerFunctions_Postprocessor.R, Optimization_UI_Postprocessor.R, and Optimization_UserInterfaceFile.R files to run the Shiny app that summarizes and reports the results. Optimization_HelperFunctions.R writes functions that are used elsewhere in the tool - this file is sourced in multiple other R scripts. 
2) Preprocessing: The Inputs folder contains all of the csv files necessary to run the preprocessor, including the 01_UserSpecs_BMPs.csv and 01_UserSpecs_loadingtargets.csv files. The Outputs folder contains the written AMPL scripts.
3) Postprocessing: The Inputs folder can optionally be a location to save AMPL results.

Questions about code can be directed to:
Catherine Chamberlin
chamberlin.catherine@epa.gov

Questions about the project can be directed to:
Naomi Detenbeck
detenbeck.naomi@epa.gov

## Copyright
Copyright (C) 2021  US EPA
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
  
## Credits
This repository contains contributions from ICF under contract to US EPA. The Agency has unlimited rights to all custom-developed code produced. The development of this repository was supported in part by an appointment to the U.S. Environmental Protection Agency (EPA) Research Participation Program administered by the Oak Ridge Institute for Science and Education (ORISE) through an interagency agreement between the U.S. Department of Energy (DOE) and the U.S. Environmental Protection Agency. ORISE is managed by ORAU under DOE contract number DE-SC0014664. All comments expressed in this source code are the author's and do not necessarily reflect the policies and views of US EPA, DOE, or ORAU/ORISE.

## Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use.  EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information.  Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA.  The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

Due to the current ownership structure of this repository, we are unable to update the name of the main branch. The code authors acknowledge and regret that the negative associations which the term 'master' has for many people as a result of historical institutionalized slavery may be hurtful to some code users. We suggest that this branch may be renamed locally if desired.


[1] https://www.epa.gov/ceam/wmost
[2] https://neos-server.org/neos/index.html
[3] https://www.jswconline.org/content/74/6/537.short

