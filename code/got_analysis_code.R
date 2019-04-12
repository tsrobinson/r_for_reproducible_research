######################################################################
##                                                                  ## 
##      PROJECT TITLE:    R for Reproducible Research               ##
##      PROJECT AUTHOR:   THOMAS S. ROBINSON                        ##
##      EMAIL:            THOMAS.ROBINSON@POLITICS.OX.AC.UK         ##
##                                                                  ##
##      DESCRIPTION:      Demo code using GoT dataset               ##
##                                                                  ##
######################################################################

#### Init ####
library(tidyverse)
library(magrittr)
library(broom)
source("code/got_functions.R")

# Not run:
data <- data_clean("data/got_data_final.csv")

data <- read_csv("data/got_data_cleaned.csv")

#####################################################################
#### Grepl example ####

# Grepl takes a pattern  and an n-length vector
# Returns an n-length vector of boolean values

grepl("Stark",data$allegiance_last)

# Grep is the same, but returns the positions

data[grep("Stark",data$allegiance_last),]

#####################################################################
#### StyleR example ####

## This is an example of poorly structured (but working) code

dataStark <- data %>%
  filter(allegiance_last == "Stark")

dataStark$dth_time_min <- dataStark[, "dth_time_sec"] / 60.000


dataStark$dth_time_min
          
# To tidy it up, highlight code and go to Addins >> StyleR >> Style section

#####################################################################
#### Vectorisation ####

## Non-vectorised
# First, with non-vectorised operations we have to initialise an empty column
data$s8_alive <- NA

# Then do an iteration through each row
system.time(for (i in 1:nrow(data)) {
  data[i,]$s8_alive <- if (data[i,]$dth_flag == 1) {
    "Alive"
  } else {
      "Dead"
    }
})

# It works but it's clunk (and with larger data could be slow)
data$s8_alive

## Vectorised
system.time(data$s8_alive2 <- ifelse(data$dth_flag == 1, "Alive","Dead"))

#####################################################################
#### Example of quirky base R ####

# Let's take the nice clean GoT data...
data_poor <- data

# ... and unclean it by altering a single number to a character
data_poor[1, "dth_time_sec"] <- "3 42"

# Now let's save this as a .csv and imagine this is our raw data
write.csv(data_poor, "data/got_data_unclean.csv")

## Show how read.csv from base-R treats unclean column
data_base <- read.csv("data/got_data_unclean.csv")
data_base$dth_time_sec

# And what type of data has it saved the column as?
typeof(data_base$dth_time_sec)

# Ok, so I can just convert it to minutes right?
data_base$dth_time_sec/60

# What happens if I just try and convert it back?
as.numeric(data_base$dth_time_sec)

## Now in tidyverse, read the same unclean data:
data_tidy <- read_csv("data/got_data_unclean.csv")
data_tidy$dth_time_sec

# As a character vector, we can't do addition
data_tidy$dth_time_sec/60

# But we can convert without the weird factor issue!
as.numeric(data_tidy$dth_time_sec)

#####################################################################
#### Day 2 ####
#####################################################################

#### Base-R manipulation

data <- read_csv("data/got_data_cleaned.csv")

# Base-R manipulation of a dataframe:
living_male_names1 <- data[data$sex == "Male" & data$dth_flag == 0,"full_name"]

# Another base-R alternative:
living_male_names2 <- data$full_name[data$sex == "Male" & data$dth_flag == 0]

# And yet another base-R alternative:
living_male_names3 <- data[data$sex == "Male" & data$dth_flag == 0,]$full_name

# Or a tidyverse solution (to return a column vector):
living_males_names4 <- data %>%
  filter(sex == "Male",
         dth_flag == 0) %>%
  select(full_name)

#### Tidyverse demo ####
data <- read_csv("data/got_data_cleaned.csv")
##Find out which houses have the most prominent surviving members
prominence_surviving<- data %>%
  
  select(full_name, 
         allegiance_last, 
         sex, 
         dth_flag, 
         prominence) %>%
  
  filter(dth_flag == 0) %>%
  
  group_by(allegiance_last) %>%
  
  summarise(avg_prominence = mean(prominence, na.rm=TRUE))

# Separate by sex
prominence_surviving_gender <- data %>%
  
  select(full_name, 
         allegiance_last, 
         sex, 
         dth_flag, 
         prominence,
         exp_time_sec) %>%
  
  filter(dth_flag == 0) %>%
  
  mutate(exp_time_sec = exp_time_sec/60) %>%

  group_by(allegiance_last, sex) %>%
  
  summarise(avg_prominence = mean(prominence, na.rm=TRUE)) %T>%
  
  ggplot(aes(x = allegiance_last, y = avg_prominence, fill = sex)) + 
    geom_col(position = "dodge") +
    coord_flip() +
    theme_minimal() 

#### Nested regressions ####

# OLS function to pass through to map
survival_lm <- function(df) {
  mod <- lm("exp_time_sec ~ allegiance_last + 
                            occupation + 
                            social_status + 
                            prominence + 
                            allegiance_switched",
            data = df) %>%
    tidy()
  
  return(mod)
}

# Set up data and run models on subsetted dataframes
survival_models <- data %>%
  
  select(full_name, 
         allegiance_last,
         allegiance_switched,
         occupation,
         social_status,
         sex,
         prominence,
         exp_time_sec) %>%
  
  group_by(sex) %>%
  
  nest() %>%
  
  mutate(model = map(data, survival_lm)) %>%
  
  unnest(model) 

# View the results
summary(survival_models$model[[1]])
summary(survival_models$model[[2]])
  
# Plot the results

ggplot(survival_models,aes(x = term, y = estimate, color = sex)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error,
                    ymax = estimate + 1.96*std.error),
                width = 0.3) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom")





