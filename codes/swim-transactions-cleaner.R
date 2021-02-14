
############################################################
#
# DATA ANALYSIS TEXTBOOK
# Chapter 18 time series - swim ticket sales

#
# WHAT THIS CODES DOES:
# cleans data and aggregates to daily. 

# This code is slightly modified by Zsombor Hegedus to cater for given task at hand

#
###########################################################


# Clear memory -------------------------------------------------------
rm(list=ls())

# Import libraries ---------------------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(Hmisc)
library(timeDate)
library(caret)

# Change working directory -------------------------------------------
# Make sure you are set up such that 
# 1. you are in the folder designated for Data Analysis.
# 2. You have a folder called da_case_studies, with the saved folder for this case study
# 3. You have a folder called da_data_repo, with data for this case study on it.

# if you opened directly the code, you may need this
# setwd("../..")   # go up two levels
getwd()


data_in   <- "C:/Workspace/R_directory/CEU/Data_Analysis_3/da_data_repo/swim-transactions"


#############################################
# DATA CLEANING
#############################################

# Load raw data ------------------------------------------------------

data <- as.data.frame(read.table(paste0(data_in,"/raw/SwimmingPoolAdmissionsCABQ-en-us.csv"),
                                sep = "\t",
                                header = TRUE,
                                fileEncoding = "UCS-2LE",
                                strip.white = TRUE))

unique(data$Location)

data <- data %>%
    filter(Location %in% c("AQSP01","AQEI01","AQEJ01","AQMP01","AQRG01", "AQSV01", "AQWP01")) %>% #outdoorpools
    filter(Category %in% c("ADMISTIER1","ADMISTIER2","ADMISTIER3")) %>%
    mutate(date = as.Date(Date_Time, format = "%Y-%m-%d")) 
Hmisc::describe(data$ITEM)

data <- data %>%
  mutate(core1 =  (ITEM %in%  c("ADULT" , "SENIOR" ,"TEEN" ,"CHILD", "TOT"))) %>%
  mutate(core2 =  (ITEM %in%  c("CHILD PM","ADULT PM","SENIOR PM", "TOT PM", "TEEN PM"))) %>%
  filter(core1 | core2) %>%
  mutate(date = as.Date(Date_Time, format = "%Y-%m-%d")) 

# Agrregate date to daily freq --------------------------------------

daily_agg <- aggregate(QUANTITY ~ date, data = data, sum)

# replace missing days with 0 
daily_agg <- daily_agg %>% 
  merge(data.frame(date = seq(from = min(daily_agg[,"date"]), to = max(daily_agg[,"date"]), by = 1)),
        all = TRUE) %>% 
  mutate(QUANTITY = ifelse(is.na(QUANTITY),0,QUANTITY))

# Create date/time variables ----------------------------------------

# 2010-2016 only full years used. 
daily_agg <- daily_agg %>%
  filter(date >= as.Date("2010-01-01")) %>%
  filter(date < as.Date("2017-01-01"))
Hmisc::describe(daily_agg)

data_out <- 'C:/Users/T450s/Desktop/programming/git/forecast_ticket_volume/data/'
# Save workfile
write.csv(daily_agg,paste(data_out,"swim_work.csv",sep=""), row.names = FALSE)               
