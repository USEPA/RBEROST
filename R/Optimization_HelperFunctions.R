##############################################################
### PURPOSE: Tier 1 Optimization Helper Functions          ###                                                   
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

# Lines of code tagged with #*# may need to be edited by the user.

##################################################
# Table of Contents:
# 1. Setup
# 2. get_upstream_ComIDs()
# 3. left()
# 4. tmp_length()
# 5. custom.round()
# 6. my_str_split()
# 7. my_propogateerror()
# 8. my_index()
# 9. my_parse()
# 10. my_key_fun()
##################################################

##################################################
# 1. Setup
##################################################

packages.used.in.this.script <- c("tidyverse", "tidygraph")
# lapply(packages.used.in.this.script, install.packages) #*# # This line may need to be run if packages are not installed.
invisible(
  suppressPackageStartupMessages(
    lapply(packages.used.in.this.script, library, character.only = TRUE)
    )
  )

##################################################
# 2. get_upstream_ComIDs()
##################################################

### get_upstream_ComIDs() takes as inputs the sparrow inputs for the region modelling and a character string of the target comid, which is equivalent to the pore or pour points
### For intermediate targets, the target.comid is the comid that the TMDL relates to.
### There are 2 versions of this function, which undergo the catchment delineation in 2 different ways.
### Version 1 used to be the default version used in this package, however because it relies on nhdplusTools, and because nhdplusTools was temporarily removed from CRAN, version 2 is now dedicated as the default.

# ### Version 1: This function uses the nhdplusTools package to use a web service to find contributing catchments. 
# ### Trade-off: This version is about 5x faster, however it misses any inaccuracies in the NHD+ database.
# ### Note: nhdplusTools() was removed from CRAN, and IT firewalls were blocking the API call from navigate_nldi(). 
# ### Note (cont): Because of these things, the default function was changed to the local network graph function on 2021-06-24
# 
# get_upstream_ComIDs <- function(sparrow_in, target.comid) {
#   
#   upstream.comids.sf <- 
#     navigate_nldi(
#       nldi_feature = c("comid", target.comid),
#       mode = "upstreamTributaries", 
#       distance_km = 9999
#     )
#   
#   upstream.comids <- upstream.comids.sf$UT_flowlines$nhdplus_comid
#   
#   return(upstream.comids)
#   
# }

### Version 2: This function uses a network graph to determine comids that are upstream of a target comid based on the to & from nodes from the NHD+ inputs to SPARROW
### Trade-off: This version is ~5x slower, however it will include catchements that may be misclassified in the NHD+ database. E.g. in the upper Connecticut HUC6, one ComID that is part of the river network is inaccurately listed as in the lower Connecticut HUC6 and is not included using the nhdplusTools.

get_upstream_ComIDs <- function(sparrow_in, target.comid) {

  reaches <- sparrow_in

  river.graph <- as_tbl_graph(
    reaches %>% select(to = ctonode, from = cfromnode, comid, HUC_12_Rev)
    )

  comids <- river.graph %>% activate(edges) %>% as_tibble()

  head.of.comid <- comids$from[which(comids$comid == target.comid)]

  upstream <- river.graph %>%
    mutate(dist_to_comid = node_distance_to(head.of.comid)) %>%
    activate(nodes) %>%
    as_tibble() %>%
    mutate(
      upstream.flag = case_when(
        is.infinite(dist_to_comid) ~ FALSE, !is.infinite(dist_to_comid) ~ TRUE
        )
      )

  upstream.nodes <- with(upstream, which(upstream.flag))

  upstream.comids <- c(
    with(comids, comid[to %in% upstream.nodes]), target.comid
    )

  return(upstream.comids)

}

##################################################
# 3. left()
##################################################

### left() is a function to isolate the first part of a string. Helpful for moving between HUC levels.

left = function(text, num_char) {
  substr(text, 1, num_char)
}

##################################################
# 4. tmp_length
##################################################
### tmp_length() is a function used in an lapply argument that specifies it wants the number of unique values in the column titled 'comid' in a dataframe


tmp_length <- function(x) {
  length(unique(x$comid))
}


##################################################
# 5. custom.round()
##################################################
### custom.round() is a function that takes the user specs data and finds the nearest value for which data is available
### This function is used to assign urban BMP efficiencies to infiltration rates and riparian efficiencies to buffer widths.

custom.round <- function(x, breaks) { 
  breaks[
    unlist(
      lapply(
        X = lapply(
          X = lapply( 
            X = unlist(
              lapply(
                X = unlist(
                  lapply(X = x, FUN = as.character)
                  ),
                FUN = as.numeric
                )
              ), 
            FUN = "-",
            breaks
          ), 
          FUN = abs
        ), 
        FUN = which.min
      )
    )
  ]
}


##################################################
# 6. my_str_split()
##################################################

### my_str_split() is a function to use str_split() indexing over a list

my_str_split <- function(x) {str_split(x, "_")[[1]][1]}

##################################################
# 7. my_propogateerror()
##################################################

### my_propogateerror() is a function to propogate error throuhg addition, subtraction, multiplication and division
### values are entered as a list of douples (value, standard error) and the method can be described
### Only one method can be described per function call, so for complex transformations, the function needs to be nested
### Only 2 values are allowed for division, and the first must be the numerator, and the second is the denominator
### Yes, propagate is misspelled. Think of it as a quirk.

my_propogateerror <- function(vals, method = c("addsub", "mult", "div")) {
  if(!is.list(vals)) {
    stop(
      "Values must be provided as a list, where each entry is formatted as c(value, se)"
    )
  }
  
  sqr <- function(x) {x^2}
  
  if(method == "addsub") {
    sigma <- sqrt(sum(unlist(lapply(lapply(vals, "[[", 2), sqr))))
  }
  
  if(method == "mult") {
    sigma <- prod(unlist(lapply(vals, "[[", 1))) *
      sqrt(
        sum(
          (unlist(lapply(vals, "[[", 2)) / unlist(lapply(vals, "[[", 1))) ^ 2
        )
      )
  }
  
  if(method == "div") {
    if(length(vals) != 2) {
      stop(
        "For division, a list of length two (numerator, denominator) must be provided."
      )
    }
    sigma <- vals[[1]][1] / vals[[2]][1] *
      sqrt(
        sum(
          (unlist(lapply(vals, "[[", 2)) / unlist(lapply(vals, "[[", 1))) ^ 2
        )
      )
    
    
  }
  
  return(sigma)
}

##################################################
# 8. my_index()
##################################################
# This indexing function makes working with lists easier

my_index <- function(x, i) {lapply(x, "[[", i)}

##################################################
# 9. my_parse()
##################################################
# This function allows for parsing using lapply

my_parse <- function(x) {parse(text = x)}

##################################################
# 10. my_key_fun()
##################################################
# This function allows RBEROST to key values that are state specific from a State column in a data frame

my_key_fun <- function(df, key, key_transform) {
  x <- df %>% select(all_of(key)) %>% unlist()
  y <- unlist(imap(.x = x, .f = key_transform))
  z <- as.numeric(df[cbind(seq(1:nrow(df)), match(y, names(df)))])
}
