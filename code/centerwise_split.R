# centerwise_split.R ------------------------------------------------------
#' This R script processes the data into center-wise data

# Imports -----------------------------------------------------------------
source("functions.R")
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

# Load data ---------------------------------------------------------------
data12 <- read.csv("processed_data/thoracic_data_2015_to_2020.csv", row.names = 1)
data1 <- read.csv("processed_data/thoracic_data_2015_to_2017.csv", row.names = 1)
data2 <- read.csv("processed_data/thoracic_data_2017_to_2020.csv", row.names = 1)

# testing -----------------------------------------------------------------
test <- subset(data123, rem_cd == 4 | rem_cd == 21)
test <- subset(test, period == 1)

# ctr_code ------------------------------------------------------------------
table(data12$ctr_code)
nrow(table(data12$ctr_code))

ctr_code_no_unknown <- subset(data12, ctr_code != "Unknown")
ctr_code_count <- table(ctr_code_no_unknown$ctr_code)

table(ctr_code_no_unknown$ctr_code)
nrow(table(ctr_code_no_unknown$ctr_code))

ctr_bar_graph <- single_bar_graph_of_count(ctr_code_no_unknown, 
                                           ctr_code_no_unknown$ctr_code)
ctr_bar_graph + labs(title = "tx center bar chart",
                     x = "center code")

# opo_ctr_code ------------------------------------------------------------------
table(data12$opo_ctr_code)
nrow(table(data12$opo_ctr_code))

opo_ctr_code_no_unknown <- subset(data12, opo_ctr_code != "Unknown")
table(opo_ctr_code_no_unknown$opo_ctr_code)
nrow(table(opo_ctr_code_no_unknown$opo_ctr_code))

opo_ctr_bar_graph <- single_bar_graph_of_count(opo_ctr_code_no_unknown, 
                                               opo_ctr_code_no_unknown$opo_ctr_code)
opo_ctr_bar_graph + labs(title = " OPO bar chart",
                         x = "OPO code")



# categorize center scales (listing ctr code version) --------------------------------------------------------
setdiff(data2$listing_ctr_code, data1$listing_ctr_code)
setdiff(data1$listing_ctr_code, data2$listing_ctr_code)

data123 <- subset(data12, listing_ctr_code != 7223 & listing_ctr_code != 5549 & 
                    listing_ctr_code != 13454)
data13 <- subset(data123, period == 1)
data23 <- subset(data123, period == 2)
data13_tx_only <- subset(data13, rem_cd == 4 | rem_cd == 21)

lctr_code_table <- table(data13_tx_only$listing_ctr_code)
lctr_table_head <- names(lctr_code_table)

# percentiles -------------------------------------------------------------
skim(lctr_code_table)
summary(lctr_code_table)

second_q <- 54
third_q <- 108
eighty_five_percentile <- quantile(lctr_code_table, 0.875)    # 148

sum(lctr_code_table > eighty_five_percentile)  # number of extra large center
sum(lctr_code_table > third_q)  # number of large center
sum(lctr_code_table > second_q & lctr_code_table <= third_q)  # number of medium center
sum(lctr_code_table <= second_q)  # number of small center

counter = 0

for (val in lctr_table_head)
{
  counter <- counter + 1
  name <- paste("center", val, sep = "_")
  print(name)
  #paste("center", val, sep = "_") <- subset(data123, ctr_code == val)
  assign(name, subset(data123, listing_ctr_code == val))
  
  #write.csv(subset(data123, listing_ctr_code == val), name, row.names = TRUE)
}
print(counter)
# partition to scale ------------------------------------------------------

xl_ctr_list <- list()
large_ctr_list <- list()
med_ctr_list <- list()
small_ctr_list <- list()

counter = 0
for (val in lctr_table_head)
{
  counter <- counter + 1
  if (lctr_code_table[counter] > eighty_five_percentile)
  {
    xl_ctr_list[counter] <- val
  }
  else if (lctr_code_table[counter] > third_q)
  {
    large_ctr_list[counter] <- val
  }
  else if (lctr_code_table[counter] <= second_q)
  {
    small_ctr_list[counter] <- val
  }
  else
  {
    med_ctr_list[counter] <- val
  }
}

xl_ctr_list <- xl_ctr_list[!sapply(xl_ctr_list, is.null)]
large_ctr_list <-large_ctr_list[!sapply(large_ctr_list, is.null)]
med_ctr_list <-med_ctr_list[!sapply(med_ctr_list, is.null)]
small_ctr_list <-small_ctr_list[!sapply(small_ctr_list, is.null)]

xl_ctr_list <- as.integer(xl_ctr_list)
large_ctr_list <- as.integer(large_ctr_list)
med_ctr_list <- as.integer(med_ctr_list)
small_ctr_list <- as.integer(small_ctr_list)
  
data123$ctr_scale <- "pre"
for (ctr in xl_ctr_list)
{
  data123$ctr_scale[data123$listing_ctr_code == ctr] <- "xl"
}
for (ctr in large_ctr_list)
{
  data123$ctr_scale[data123$listing_ctr_code == ctr] <- "large"
}
for (ctr in med_ctr_list)
{
  data123$ctr_scale[data123$listing_ctr_code == ctr] <- "med"
}
for (ctr in small_ctr_list)
{
  data123$ctr_scale[data123$listing_ctr_code == ctr] <- "small"
}

table(data123$ctr_scale)

xl_ctr_data <- data123[(data123$ctr_scale=="xl"),]
large_ctr_data <- data123[(data123$ctr_scale=="large"),]
med_ctr_data <- data123[(data123$ctr_scale=="med"),]
small_ctr_data <- data123[(data123$ctr_scale=="small"),]

# Export files ------------------------------------------------------------

#write.csv(data123, "data123.csv", row.names = TRUE)
#write.csv(xl_ctr_data,"xl_centers.csv", row.names = TRUE)
#write.csv(large_ctr_data,"large_centers.csv", row.names = TRUE)
#write.csv(med_ctr_data,"med_centers.csv", row.names = TRUE)
#write.csv(small_ctr_data,"small_centers.csv", row.names = TRUE)
