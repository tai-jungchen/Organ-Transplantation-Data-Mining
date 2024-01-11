# patient_year.R -------------------------------------------------------------------
#' This R script add the patient type, patient year, and self calculated age to the dataframe

# Import ------------------------------------------------------------------
# libraries
library(dplyr)
library(utils)
library(Hmisc)
library(pastecs)
library(skimr)
library(ggplot2)
library(qcc)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(tibble)
library(survival)

# read data ---------------------------------------------------------------
df <- read.csv("processed_data/data123.csv", row.names = 1)

# change column's name #
colnames(df)[102] <- c("end_match_las")
colnames(df_tx)[102] <- c("end_match_las")
colnames(xl)[102] <- c("end_match_las")
colnames(large)[102] <- c("end_match_las")
colnames(med)[102] <- c("end_match_las")
colnames(small)[102] <- c("end_match_las")

colnames(df)[308] <- c("age_don")
colnames(df_tx)[308] <- c("age_don")
colnames(xl)[308] <- c("age_don")
colnames(large)[308] <- c("age_don")
colnames(med)[308] <- c("age_don")
colnames(small)[308] <- c("age_don")



# patient type ------------------------------------------------------------
df$ptype <- NA
df$pyear_p1 <- NA
df$pyear_p2 <- NA

start_date <- "2015-08-18"
mid_date <- "2017-11-23"
ter_date <- "2020-03-01"

for (i in 1:nrow(df)) {
  row <- df[i, ]
  end_date <- df[i, "end_date"]
  init_date <- df[i, "init_date"]
  
  if (init_date < start_date & end_date <= mid_date) {
    df$ptype[i] <- 0
    
    yrs_p1 <- difftime(end_date, start_date, unit = "days") / 365.25
    df$pyear_p1[i] <- yrs_p1
    df$pyear_p2[i] <- 0
  } 
  if (init_date >= start_date & end_date <= mid_date){
    df$ptype[i] <- 1
    
    yrs_p1 <- difftime(end_date, init_date, unit = "days") / 365.25
    df$pyear_p1[i] <- yrs_p1
    df$pyear_p2[i] <- 0
  }
  if (init_date > mid_date & end_date <= ter_date) {
    df$ptype[i] <- 2
    
    yrs_p2 <- difftime(end_date, init_date, unit = "days") / 365.25
    df$pyear_p1[i] <- 0
    df$pyear_p2[i] <- yrs_p2
  } 
  if (init_date >= start_date & init_date <= mid_date &  
      end_date > mid_date & end_date <= ter_date) {
    df$ptype[i] <- 3
    
    yrs_p1 <- difftime(mid_date, init_date, unit = "days") / 365.25
    yrs_p2 <- difftime(end_date, mid_date, unit = "days") / 365.25
    df$pyear_p1[i] <- yrs_p1
    df$pyear_p2[i] <- yrs_p2
  } 
  if (init_date > mid_date & init_date <= ter_date & end_date > ter_date) {
    df$ptype[i] <- 4
    
    yrs_p2 <- difftime(ter_date, init_date, unit = "days") / 365.25
    df$pyear_p1[i] <- 0
    df$pyear_p2[i] <- yrs_p2
    
  } 
  if (init_date < start_date & end_date > mid_date & end_date <= ter_date) {
    df$ptype[i] <- 5
    
    yrs_p1 <- difftime(mid_date, start_date, unit = "days") / 365.25
    yrs_p2 <- difftime(end_date, mid_date, unit = "days") / 365.25
    df$pyear_p1[i] <- yrs_p1
    df$pyear_p2[i] <- yrs_p2
  } 
  if (init_date < start_date & end_date > ter_date) {
    df$ptype[i] <- 6
    
    yrs_p1 <- difftime(mid_date, start_date, unit = "days") / 365.25
    yrs_p2 <- difftime(ter_date, mid_date, unit = "days") / 365.25
    df$pyear_p1[i] <- yrs_p1
    df$pyear_p2[i] <- yrs_p2
  } 
  if (init_date >= start_date & init_date <= mid_date & end_date > ter_date) {
    df$ptype[i] <- 7
    
    yrs_p1 <- difftime(mid_date, init_date, unit = "days") / 365.25
    yrs_p2 <- difftime(ter_date, mid_date, unit = "days") / 365.25
    df$pyear_p1[i] <- yrs_p1
    df$pyear_p2[i] <- yrs_p2
  } 
}


# calc_age ------------------------------------------------------------
days <- difftime(df$end_date, df$init_date, unit = "days")
yrs <- days / 365.25
calc_age <- df$init_age + yrs
df$calc_age <- as.integer(calc_age)


# split centers -----------------------------------------------------------
xl_ctr_data <- df[(df$ctr_scale=="xl"),]
large_ctr_data <- df[(df$ctr_scale=="large"),]
med_ctr_data <- df[(df$ctr_scale=="med"),]
small_ctr_data <- df[(df$ctr_scale=="small"),]


# export data -------------------------------------------------------------

#write.csv(df, "data123.csv", row.names = TRUE)
#write.csv(xl_ctr_data,"xl_centers.csv", row.names = TRUE)
#write.csv(large_ctr_data,"large_centers.csv", row.names = TRUE)
#write.csv(med_ctr_data,"med_centers.csv", row.names = TRUE)
#write.csv(small_ctr_data,"small_centers.csv", row.names = TRUE)
