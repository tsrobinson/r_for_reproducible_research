######################################################################
##                                                                  ## 
##      PROJECT TITLE:    R for Reproducible Research               ##
##      PROJECT AUTHOR:   THOMAS S. ROBINSON                        ##
##      EMAIL:            THOMAS.ROBINSON@POLITICS.OX.AC.UK         ##
##                                                                  ##
##      DESCRIPTION:      GOT Functions                             ##
##                                                                  ##
######################################################################

#### Packages ####
library(tidyverse)
#####################################################################

#### Cleaning ####

data_clean <- function(filename) {
  #####################################################################
  # Loads raw data, cleans and converts numeric codes into factors
  #
  # Input: filename as string
  #
  # Output: cleaned dataframe
  #####################################################################
  
  ## Ordered vectors that correspond to the codebook
  religion_dict <- c("Great Stallion",
                     "Lord of Light","Faith of the Seven","Old Gods","Drowned God",
                     "Many Faced God","Great Shepard","White Walkers",
                     NA,"Ghiscari","None")
  
  allegiance_dict <- c("Stark","Targaryen","Night's Watch","Lannister",
                       "Greyjoy","Bolton","Frey","Other",NA)
  
  ## Load in data and rename vars
  data <- read_csv(filename) %>%
    
    mutate(full_name = name) %>%
    
    # extra = merge keeps double-barelled surnames etc. in one field
    separate(name, into = c("first_name","last_name"), extra = "merge") %>%
    mutate(sex = ifelse(sex == 1, "Male",
                        ifelse(sex == 2, "Female",NA)),
           
           religion = religion_dict[religion],
           
           occupation = ifelse(occupation == 1, "Silk collar",
                               ifelse(occupation == 2, "Boiled-leather collar",NA)),
           
           social_status = ifelse(social_status == 1, "High",
                                  ifelse(social_status == 2, "Low",NA)),
           
           allegiance_last = allegiance_dict[allegiance_last],
           
           allegiance_switched = ifelse(allegiance_switched == 2, 1,0),
           
           location = ifelse(location == 1, "Indoors",
                             ifelse(location == 2, "Outdoors", NA)),
           
           continent = ifelse(continent == 1, "Westeros",
                              ifelse(continent == 2, "Essos",NA)),
           
           time_of_day = ifelse(time_of_day == 1, "Day",
                                ifelse(time_of_day == 2, "Night", NA))) %>%
    
    # Raw data has nonblank empty columns, so remove:
    select(-grep("X",names(.)))
  
  # Save output
  write_csv(data, "data/got_data_cleaned.csv")
  return(data)
}