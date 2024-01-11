# centerwise_post_plot.R --------------------------------------------------
#' This R script analyzes the golden dataset

# Import ------------------------------------------------------------------

# self-functions
source("functions.R")

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

# pair t test mean #
#df <- data.frame(c(small$mean_las_tx_national_in_p1), c(small$mean_las_tx_national_in_p2))
#write.csv(df,"small.csv", row.names = FALSE)
# pair t test mean #


# Read data ---------------------------------------------------------------
d <- read.csv("processed_data/data123.csv")

centerwise_data <- read.csv("golden_dataset/centerwise_data.csv", header = TRUE)
#tx <- centerwise_data$num_of_tx
centerwise_data_lite <- centerwise_data[-c(73), ] # for removing outlier
#test <- subset(centerwise_data, num_of_tx != 0)
#test <- centerwise_data[-c(73, 44, 45), ]
anova_test <- read.csv("golden_dataset/anova_test.csv", header = TRUE)
anova_test <- subset(anova_test, select = c(center_id, mean_distance, center_scale, period) )
my_anova <-aov(mean_distance ~ center_scale * period, data = anova_test)
summary(my_anova)


small <- subset(centerwise_data, center_scale == 'small')
med <- subset(centerwise_data, center_scale == 'medium')
large <- subset(centerwise_data, center_scale == 'large')
xl <- subset(centerwise_data, center_scale == 'extra large')
#small_lite <- small[-c(37), ] # for removing outlier

# Distance ----------------------------------------------------------------

# overall
dist_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                            y = mean_distance_in_p1, 
                                            color = center_scale)) +
  geom_boxplot(outlier.color = "red")
dist_boxplot + coord_cartesian(ylim = c(0, 400)) + 
  labs(title = "Mean distance from donor hosp. to tx. center in pre-policy period\nfor overall cases among the 4 center scales",
       x = 'Center scale', y = 'Mean distance from donor hosp. to tx. center')

dist_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                            y = mean_distance_in_p2, 
                                            color = center_scale)) +
  geom_boxplot(outlier.color = "red")
dist_boxplot + coord_cartesian(ylim = c(0, 400)) + 
  labs(title = "Mean distance from donor hosp. to tx. center in post-policy period\nfor overall cases among the 4 center scales",
       x = 'Center scale', y = 'Mean distance from donor hosp. to tx. center')

# local
dist_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                            y = mean_distance_local_in_p1, 
                                            color = center_scale)) +
  geom_boxplot(outlier.color = "red")
dist_boxplot + coord_cartesian(ylim = c(0, 150)) + 
  labs(title = "Mean distance from donor hosp. to tx. center for local cases in pre-policy period\namong the 4 center scales",
       x = 'Center scale', y = 'Mean distance from donor hosp. to tx. center')

dist_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                            y = mean_distance_local_in_p2, 
                                            color = center_scale)) +
  geom_boxplot(outlier.color = "red")
dist_boxplot + coord_cartesian(ylim = c(0, 150)) + 
  labs(title = "Mean distance from donor hosp. to tx. center for local cases in post-policy period\namong the 4 center scales",
       x = 'Center scale', y = 'Mean distance from donor hosp. to tx. center')

# regional
dist_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                            y = mean_distance_reginal_in_p2, 
                                            color = center_scale)) +
  geom_boxplot(outlier.color = "red")
dist_boxplot + coord_cartesian(ylim = c(0, 500)) + 
  labs(title = "Mean distance from donor hosp. to tx. center for regional cases in period2\namong the 4 center scales",
       x = 'Center scale', y = 'Mean distance from donor hosp. to tx. center')

# national
dist_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                            y = mean_distance_national_in_p2, 
                                            color = center_scale)) +
  geom_boxplot(outlier.color = "red")
dist_boxplot + coord_cartesian(ylim = c(0, 1100)) + 
  labs(title = "Mean distance from donor hosp. to tx. center for national cases in period2\namong the 4 center scales",
       x = 'Center scale', y = 'Mean distance from donor hosp. to tx. center')

# total
t.test(xl$mean_distance_in_p1, xl$mean_distance_in_p2)
t.test(xl$mean_distance_in_p1, xl$mean_distance_in_p2, paired = TRUE)

t.test(large$mean_distance_in_p1, large$mean_distance_in_p2)
t.test(med$mean_distance_in_p1, med$mean_distance_in_p2)
t.test(small$mean_distance_in_p1, small$mean_distance_in_p2)
t.test(small$mean_distance_in_p1, small$mean_distance_in_p2, paired = TRUE)

# local
t.test(xl$mean_distance_local_in_p1, xl$mean_distance_local_in_p2)
t.test(large$mean_distance_local_in_p1, large$mean_distance_local_in_p2)
t.test(med$mean_distance_local_in_p1, med$mean_distance_local_in_p2)
t.test(small$mean_distance_local_in_p1, small$mean_distance_local_in_p2)

# regional
t.test(xl$mean_distance_reginal_in_p1, xl$mean_distance_reginal_in_p2)
t.test(large$mean_distance_reginal_in_p1, large$mean_distance_reginal_in_p2)
t.test(med$mean_distance_reginal_in_p1, med$mean_distance_reginal_in_p2)
t.test(small$mean_distance_reginal_in_p1, small$mean_distance_reginal_in_p2)

# national
t.test(xl$mean_distance_national_in_p1, xl$mean_distance_national_in_p2)
t.test(large$mean_distance_national_in_p1, large$mean_distance_national_in_p2)
t.test(med$mean_distance_national_in_p1, med$mean_distance_national_in_p2)
t.test(small$mean_distance_national_in_p1, small$mean_distance_national_in_p2)

# anova - p1
one.way <- aov(mean_distance_in_p1 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_distance_local_in_p1 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_distance_reginal_in_p1 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_distance_national_in_p1 ~ center_scale, data = centerwise_data)
summary(one.way)

# anova - p2
one.way <- aov(mean_distance_in_p2 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_distance_local_in_p2 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_distance_reginal_in_p2 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_distance_national_in_p2 ~ center_scale, data = centerwise_data)
summary(one.way)

# ancova
one.way <- aov(mean_distance_in_p2 ~ center_scale*mean_distance_in_p1, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_distance_local_in_p2 ~ center_scale*mean_distance_local_in_p1, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_distance_reginal_in_p2 ~ center_scale*mean_distance_reginal_in_p1, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_distance_national_in_p2 ~ center_scale*mean_distance_national_in_p1, data = centerwise_data)
summary(one.way)

# anova on gain

# overall
pre <- subset(centerwise_data, select = c(mean_distance_in_p1, center_scale))
post <- subset(centerwise_data, select = c(mean_distance_in_p2, center_scale))
anova_gain(pre, post)

# local
pre <- subset(centerwise_data, select = c(mean_distance_local_in_p1, center_scale))
post <- subset(centerwise_data, select = c(mean_distance_local_in_p2, center_scale))
anova_gain(pre, post)

# regional
pre <- subset(centerwise_data, select = c(mean_distance_reginal_in_p1, center_scale))
post <- subset(centerwise_data, select = c(mean_distance_reginal_in_p2, center_scale))
anova_gain(pre, post)

# national
pre <- subset(centerwise_data, select = c(mean_distance_national_in_p1, center_scale))
post <- subset(centerwise_data, select = c(mean_distance_national_in_p2, center_scale))
anova_gain(pre, post)

# RMANOVA

res.aov <- anova_test(data = centerwise_data, dv = score, wid = id, within = c(treatment, time))
get_anova_table(res.aov)

# Init age ----------------------------------------------------------------
age_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                           y = mean_init_age_in_p1, 
                                           color = center_scale)) +
  geom_boxplot(outlier.color = "red")
age_boxplot + coord_cartesian(ylim = c(30, 75)) + 
  labs(title = "Mean initial age of the patients in pre-policy change period among \nthe 4 center scales", 
       x = 'Center scale', y = 'Mean initial age of the patients')

age_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                            y = mean_init_age_in_p2, 
                                            color = center_scale)) +
  geom_boxplot(outlier.color = "red")
age_boxplot + coord_cartesian(ylim = c(30, 75)) + 
  labs(title = "Mean initial age of the patients in post-policy change period among \nthe 4 center scales", 
       x = 'Center scale', y = 'Mean initial age of the patients')

t.test(xl$mean_init_age_in_p1, xl$mean_init_age_in_p2, paired = TRUE)
t.test(large$mean_init_age_in_p1, large$mean_init_age_in_p2, paired = TRUE)
t.test(med$mean_init_age_in_p1, med$mean_init_age_in_p2, paired = TRUE)
t.test(small$mean_init_age_in_p1, small$mean_init_age_in_p2, paired = TRUE)

# anova - p1
one.way <- aov(mean_init_age_in_p1 ~ center_scale, data = centerwise_data)
summary(one.way)

# anova - p2
one.way <- aov(mean_init_age_in_p2 ~ center_scale, data = centerwise_data)
summary(one.way)

# ancova
one.way <- aov(mean_init_age_in_p2 ~ center_scale*mean_init_age_in_p1, data = centerwise_data)
summary(one.way)

# anova on gain

pre <- subset(centerwise_data, select = c(mean_init_age_in_p1, center_scale))
post <- subset(centerwise_data, select = c(mean_init_age_in_p2, center_scale))
anova_gain(pre, post)
# tx cases ----------------------------------------------------------------

# overall
cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = num_of_tx_p1, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(0, 300)) + 
  labs(title = "Number of tx. cases in period 1 among the 4 center scales", 
       x = 'Center scale', y='Number of tx. cases')

cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = num_of_tx_p2, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(0, 300)) + 
  labs(title = "Number of tx. cases in period 2 among the 4 center scales", 
       x = 'Center scale', y='Number of tx. cases')

# t test
t.test(xl$num_of_tx_p1, xl$num_of_tx_p2)
t.test(large$num_of_tx_p1, large$num_of_tx_p2)
t.test(med$num_of_tx_p1, med$num_of_tx_p2)
t.test(small$num_of_tx_p1, small$num_of_tx_p2)

# local
cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                           y = num_of_local_tx_cases_p1, 
                                           color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(0, 175)) + 
  labs(title = "Number of local tx. cases in period 1 among the 4 center scales", 
       x = 'Center scale', y='Number of local tx. cases')

cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = num_of_local_tx_cases_p2, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(0, 175)) + 
  labs(title = "Number of local tx. cases in period 2 among the 4 center scales", 
       x = 'Center scale', y='Number of local tx. cases')

# local
t.test(xl$num_of_local_tx_cases_p1, xl$num_of_local_tx_cases_p2)
t.test(large$num_of_local_tx_cases_p1, large$num_of_local_tx_cases_p2)
t.test(med$num_of_local_tx_cases_p1, med$num_of_local_tx_cases_p2)
t.test(small$num_of_local_tx_cases_p1, small$num_of_local_tx_cases_p2)

# regional
cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = num_of_regional_tx_cases_p1, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(0, 175)) + 
  labs(title = "Number of regional tx. cases in period 1 among the 4 center scales", 
       x = 'Center scale', y='Number of regional tx. cases')

cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = num_of_regional_tx_cases_p2, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(0, 175)) + 
  labs(title = "Number of regional tx. cases in period 2 among the 4 center scales", 
       x = 'Center scale', y='Number of regional tx. cases')

# t test
t.test(xl$num_of_regional_tx_cases_p1, xl$num_of_regional_tx_cases_p2)
t.test(large$num_of_regional_tx_cases_p1, large$num_of_regional_tx_cases_p2)
t.test(med$num_of_regional_tx_cases_p1, med$num_of_regional_tx_cases_p2)
t.test(small$num_of_regional_tx_cases_p1, small$num_of_regional_tx_cases_p2)

# national
cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = num_of_national_tx_cases_p1, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(0, 175)) + 
  labs(title = "Number of national tx. cases in period 1 among the 4 center scales", 
       x = 'Center scale', y='Number of national tx. cases')

cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = num_of_national_tx_cases_p2, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(0, 175)) + 
  labs(title = "Number of national tx. cases in period 2 among the 4 center scales", 
       x = 'Center scale', y='Number of national tx. cases')

# t test
t.test(xl$num_of_national_tx_cases_p1, xl$num_of_national_tx_cases_p2)
t.test(large$num_of_national_tx_cases_p1, large$num_of_national_tx_cases_p2)
t.test(med$num_of_national_tx_cases_p1, med$num_of_national_tx_cases_p2)
t.test(small$num_of_national_tx_cases_p1, small$num_of_national_tx_cases_p2)

# anova - p1
one.way <- aov(num_of_tx_p1 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(num_of_local_tx_cases_p1 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(num_of_regional_tx_cases_p1 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(num_of_national_tx_cases_p1 ~ center_scale, data = centerwise_data)
summary(one.way)

# anova - p2
one.way <- aov(num_of_tx_p2 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(num_of_local_tx_cases_p2 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(num_of_regional_tx_cases_p2 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(num_of_national_tx_cases_p2 ~ center_scale, data = centerwise_data)
summary(one.way)


# time on waitlist --------------------------------------------------------

my_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_time_on_waitlist, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
my_boxplot + coord_cartesian(ylim = c(0, 600)) + 
  labs(title = "mean time on waitlist among the 4 center scales")

# waitlist mortality ------------------------------------------------------

my_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                          y = wl_mortality_p1.per_100.patient_years., 
                                          color = center_scale)) +
  geom_boxplot(outlier.color = "red")
my_boxplot + coord_cartesian(ylim = c(0, 80)) + 
  labs(title = "Waitlist mortality in pre-policy change period among the 4 center scales",
       x='Center scale', y='Waitlist mortality')

my_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                          y = wl_mortality_p2.per_100.patient_years., 
                                          color = center_scale)) +
  geom_boxplot(outlier.color = "red")
my_boxplot + coord_cartesian(ylim = c(0, 80)) + 
  labs(title = "Waitlist mortality in post-policy change period among the 4 center scales",
       x='Center scale', y='Waitlist mortality')

# t test
t.test(xl$wl_mortality_p1.per_100.patient_years., xl$wl_mortality_p2.per_100.patient_years., paired = TRUE)
t.test(large$wl_mortality_p1.per_100.patient_years., large$wl_mortality_p2.per_100.patient_years., paired = TRUE)
t.test(med$wl_mortality_p1.per_100.patient_years., med$wl_mortality_p2.per_100.patient_years., paired = TRUE)
t.test(small_lite$wl_mortality_p1.per_100.patient_years., small_lite$wl_mortality_p2.per_100.patient_years., paired = TRUE)

# anova - p1
one.way <- aov(wl_mortality_p1.per_100.patient_years. ~ center_scale, data = centerwise_data_lite)
summary(one.way)

# anova - p2
one.way <- aov(wl_mortality_p2.per_100.patient_years. ~ center_scale, data = centerwise_data_lite)
summary(one.way)

# ancova
one.way <- aov(wl_mortality_p2.per_100.patient_years. ~ center_scale*wl_mortality_p1.per_100.patient_years., data = centerwise_data_lite)
summary(one.way)

# anova on gain

pre <- subset(centerwise_data, select = c(wl_mortality_p1.per_100.patient_years., center_scale))
post <- subset(centerwise_data, select = c(wl_mortality_p2.per_100.patient_years., center_scale))
anova_gain(pre, post)

# tx patient years --------------------------------------------------------

# overall
my_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                          y = tx_p1.per_100.patient_years., 
                                          color = center_scale)) +
  geom_boxplot(outlier.color = "red")
my_boxplot + coord_cartesian(ylim = c(0, 750)) + 
  labs(title = "Tx.per 100 patient years in pre-policy period among the 4 center scales",
       x='Center scale', y='Tx. per 100 patient years')

my_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                          y = tx_p2.per_100.patient_years., 
                                          color = center_scale)) +
  geom_boxplot(outlier.color = "red")
my_boxplot + coord_cartesian(ylim = c(0, 750)) + 
  labs(title = "Tx.per 100 patient years in post-policy period among the 4 center scales",
       x='Center scale', y='Tx. per 100 patient years')

# t test
t.test(xl$tx_p1.per_100.patient_years., xl$tx_p2.per_100.patient_years., paired = TRUE)
t.test(large$tx_p1.per_100.patient_years., large$tx_p2.per_100.patient_years., paired = TRUE)
t.test(med$tx_p1.per_100.patient_years., med$tx_p2.per_100.patient_years., paired = TRUE)
t.test(small$tx_p1.per_100.patient_years., small$tx_p2.per_100.patient_years., paired = TRUE)

# local
my_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                          y = tx_p1_local.per_100.patient_years., 
                                          color = center_scale)) +
  geom_boxplot(outlier.color = "red")
my_boxplot + coord_cartesian(ylim = c(0, 600)) + 
  labs(title = "Local tx.per 100 patient years in pre-policy period among the 4 center scales",
       x='Center scale', y='Tx. per 100 patient years')

my_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                          y = tx_p2_local.per_100.patient_years., 
                                          color = center_scale)) +
  geom_boxplot(outlier.color = "red")
my_boxplot + coord_cartesian(ylim = c(0, 600)) + 
  labs(title = "Local tx.per 100 patient years in post-policy period among the 4 center scales",
       x='Center scale', y='Tx. per 100 patient years')

# t test
t.test(xl$tx_p1_local.per_100.patient_years., xl$tx_p2_local.per_100.patient_years., paired = TRUE)
t_test = t.test(xl$tx_p1_local.per_100.patient_years., xl$tx_p2_local.per_100.patient_years.)
((t_test$estimate[2]-t_test$estimate[1]) / t_test$estimate[1]) * 100 # delta in percentage

t.test(large$tx_p1_local.per_100.patient_years., large$tx_p2_local.per_100.patient_years., paired = TRUE)
t_test = t.test(large$tx_p1_local.per_100.patient_years., large$tx_p2_local.per_100.patient_years.)
((t_test$estimate[2]-t_test$estimate[1]) / t_test$estimate[1]) * 100 # delta in percentage

t.test(med$tx_p1_local.per_100.patient_years., med$tx_p2_local.per_100.patient_years., paired = TRUE)
t_test = t.test(med$tx_p1_local.per_100.patient_years., med$tx_p2_local.per_100.patient_years.)
((t_test$estimate[2]-t_test$estimate[1]) / t_test$estimate[1]) * 100 # delta in percentage

t.test(small$tx_p1_local.per_100.patient_years., small$tx_p2_local.per_100.patient_years., paired = TRUE)
t_test = t.test(small$tx_p1_local.per_100.patient_years., small$tx_p2_local.per_100.patient_years.)
((t_test$estimate[2]-t_test$estimate[1]) / t_test$estimate[1]) * 100 # delta in percentage

# regional
my_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                          y = tx_p1_regional.per_100.patient_years., 
                                          color = center_scale)) +
  geom_boxplot(outlier.color = "red")
my_boxplot + coord_cartesian(ylim = c(0, 400)) + 
  labs(title = "Regional tx.per 100 patient years in pre-policy period among the 4 center scales",
       x='Center scale', y='Tx. per 100 patient years')

my_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                          y = tx_p2_regional.per_100.patient_years., 
                                          color = center_scale)) +
  geom_boxplot(outlier.color = "red")
my_boxplot + coord_cartesian(ylim = c(0, 400)) + 
  labs(title = "Regional tx.per 100 patient years in post-policy period among the 4 center scales",
       x='Center scale', y='Tx. per 100 patient years')

# t test
t.test(xl$tx_p1_regional.per_100.patient_years., xl$tx_p2_regional.per_100.patient_years., paired = TRUE)
t_test = t.test(xl$tx_p1_regional.per_100.patient_years., xl$tx_p2_regional.per_100.patient_years.)
((t_test$estimate[2]-t_test$estimate[1]) / t_test$estimate[1]) * 100 # delta in percentage

t.test(large$tx_p1_regional.per_100.patient_years., large$tx_p2_regional.per_100.patient_years., paired = TRUE)
t_test = t.test(large$tx_p1_regional.per_100.patient_years., large$tx_p2_regional.per_100.patient_years.)
((t_test$estimate[2]-t_test$estimate[1]) / t_test$estimate[1]) * 100 # delta in percentage

t.test(med$tx_p1_regional.per_100.patient_years., med$tx_p2_regional.per_100.patient_years., paired = TRUE)
t_test = t.test(med$tx_p1_regional.per_100.patient_years., med$tx_p2_regional.per_100.patient_years.)
((t_test$estimate[2]-t_test$estimate[1]) / t_test$estimate[1]) * 100 # delta in percentage

t.test(small$tx_p1_regional.per_100.patient_years., small$tx_p2_regional.per_100.patient_years., paired = TRUE)
t_test = t.test(small$tx_p1_regional.per_100.patient_years., small$tx_p2_regional.per_100.patient_years.)
((t_test$estimate[2]-t_test$estimate[1]) / t_test$estimate[1]) * 100 # delta in percentage

# national
my_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                          y = tx_p1_national.per_100.patient_years., 
                                          color = center_scale)) +
  geom_boxplot(outlier.color = "red")
my_boxplot + coord_cartesian(ylim = c(0, 300)) + 
  labs(title = "National tx.per 100 patient years in pre-policy period among the 4 center scales",
       x='Center scale', y='Tx. per 100 patient years')

my_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                          y = tx_p2_national.per_100.patient_years., 
                                          color = center_scale)) +
  geom_boxplot(outlier.color = "red")
my_boxplot + coord_cartesian(ylim = c(0, 300)) + 
  labs(title = "National tx.per 100 patient years in post-policy period among the 4 center scales",
       x='Center scale', y='Tx. per 100 patient years')

# t test
t.test(xl$tx_p1_national.per_100.patient_years., xl$tx_p2_national.per_100.patient_years., paired = TRUE)
t_test = t.test(xl$tx_p1_national.per_100.patient_years., xl$tx_p2_national.per_100.patient_years.)
((t_test$estimate[2]-t_test$estimate[1]) / t_test$estimate[1]) * 100 # delta in percentage

t.test(large$tx_p1_national.per_100.patient_years., large$tx_p2_national.per_100.patient_years., paired = TRUE)
t_test = t.test(large$tx_p1_national.per_100.patient_years., large$tx_p2_national.per_100.patient_years.)
((t_test$estimate[2]-t_test$estimate[1]) / t_test$estimate[1]) * 100 # delta in percentage

t.test(med$tx_p1_national.per_100.patient_years., med$tx_p2_national.per_100.patient_years., paired = TRUE)
t_test = t.test(med$tx_p1_national.per_100.patient_years., med$tx_p2_national.per_100.patient_years.)
((t_test$estimate[2]-t_test$estimate[1]) / t_test$estimate[1]) * 100 # delta in percentage

t.test(small$tx_p1_national.per_100.patient_years., small$tx_p2_national.per_100.patient_years., paired = TRUE)
t_test = t.test(small$tx_p1_national.per_100.patient_years., small$tx_p2_national.per_100.patient_years.)
((t_test$estimate[2]-t_test$estimate[1]) / t_test$estimate[1]) * 100 # delta in percentage

# anova - p1
one.way <- aov(tx_p1.per_100.patient_years. ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(tx_p1_local.per_100.patient_years. ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(tx_p1_regional.per_100.patient_years. ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(tx_p1_national.per_100.patient_years. ~ center_scale, data = centerwise_data)
summary(one.way)

# anova - p2
one.way <- aov(tx_p2.per_100.patient_years. ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(tx_p2_local.per_100.patient_years. ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(tx_p2_regional.per_100.patient_years. ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(tx_p2_national.per_100.patient_years. ~ center_scale, data = centerwise_data)
summary(one.way)

# ancova
one.way <- aov(tx_p2.per_100.patient_years. ~ center_scale*tx_p1.per_100.patient_years., data = centerwise_data)
summary(one.way)

one.way <- aov(tx_p2_local.per_100.patient_years. ~ center_scale*tx_p1_local.per_100.patient_years., data = centerwise_data)
summary(one.way)

one.way <- aov(tx_p2_regional.per_100.patient_years. ~ center_scale*tx_p1_regional.per_100.patient_years., data = centerwise_data)
summary(one.way)

one.way <- aov(tx_p2_national.per_100.patient_years. ~ center_scale*tx_p1_national.per_100.patient_years., data = centerwise_data)
summary(one.way)

# anova on gain

# overall
pre <- subset(centerwise_data, select = c(tx_p1.per_100.patient_years., center_scale))
post <- subset(centerwise_data, select = c(tx_p2.per_100.patient_years., center_scale))
anova_gain(pre, post)

# local
pre <- subset(centerwise_data, select = c(tx_p1_local.per_100.patient_years., center_scale))
post <- subset(centerwise_data, select = c(tx_p2_local.per_100.patient_years., center_scale))
anova_gain(pre, post)

# regional
pre <- subset(centerwise_data, select = c(tx_p1_regional.per_100.patient_years., center_scale))
post <- subset(centerwise_data, select = c(tx_p2_regional.per_100.patient_years., center_scale))
anova_gain(pre, post)

# national
pre <- subset(centerwise_data, select = c(tx_p1_national.per_100.patient_years., center_scale))
post <- subset(centerwise_data, select = c(tx_p2_national.per_100.patient_years., center_scale))
anova_gain(pre, post)

# ischemic time -----------------------------------------------------------
# overall
cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_isch_in_p1.hr., 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(0, 10)) + 
  labs(title = "Mean ischemic time in pre-policy change period among the 4 center scales",
       x='Center scale', y='Mean ischemic time (hr)')

cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_isch_in_p2.hr., 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(0, 10)) + 
  labs(title = "Mean ischemic time in post-policy change period among the 4 center scales",
       x='Center scale', y='Mean ischemic time (hr)')

# t test
t.test(xl$mean_isch_in_p1.hr., xl$mean_isch_in_p2.hr., paired = TRUE)
t.test(large$mean_isch_in_p1.hr., large$mean_isch_in_p2.hr., paired = TRUE)
t.test(med$mean_isch_in_p1.hr., med$mean_isch_in_p2.hr., paired = TRUE)
t.test(small$mean_isch_in_p1.hr., small$mean_isch_in_p2.hr., paired = TRUE)

# local
cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_isch_local_in_p1.hr., 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(0, 10)) + 
  labs(title = "Mean ischemic time for local cases in period 1 among the 4 center scales",
       x='Center scale', y='Mean ischemic time for local cases (hr)')

cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_isch_local_in_p2.hr., 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(0, 10)) + 
  labs(title = "Mean ischemic time for local cases in period 2 among the 4 center scales",
       x='Center scale', y='Mean ischemic time for local cases (hr)')

# t test
t.test(xl$mean_isch_local_in_p1.hr., xl$mean_isch_local_in_p2.hr.)
t.test(large$mean_isch_local_in_p1.hr., large$mean_isch_local_in_p2.hr.)
t.test(med$mean_isch_local_in_p1.hr., med$mean_isch_local_in_p2.hr.)
t.test(small$mean_isch_local_in_p1.hr., small$mean_isch_local_in_p2.hr.)

# regional
cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_isch_regional_in_p1.hr., 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(0, 10)) + 
  labs(title = "Mean ischemic time for regional cases in period 1 among the 4 center scales",
       x='Center scale', y='Mean ischemic time for regional cases (hr)')

cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_isch_regional_in_p2.hr., 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(0, 10)) + 
  labs(title = "Mean ischemic time for regional cases in period 2 among the 4 center scales",
       x='Center scale', y='Mean ischemic time for regional cases (hr)')

# t test
t.test(xl$mean_isch_regional_in_p1.hr., xl$mean_isch_regional_in_p2.hr.)
t.test(large$mean_isch_regional_in_p1.hr., large$mean_isch_regional_in_p2.hr.)
t.test(med$mean_isch_regional_in_p1.hr., med$mean_isch_regional_in_p2.hr.)
t.test(small$mean_isch_regional_in_p1.hr., small$mean_isch_regional_in_p2.hr.)

# national
cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_isch_national_in_p1.hr., 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(0, 10)) + 
  labs(title = "Mean ischemic time for national cases in period 1 among the 4 center scales",
       x='Center scale', y='Mean ischemic time for national cases (hr)')

cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_isch_national_in_p2.hr., 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(0, 10)) + 
  labs(title = "Mean ischemic time for national cases in period 2 among the 4 center scales",
       x='Center scale', y='Mean ischemic time for national cases (hr)')

# t test
t.test(xl$mean_isch_national_in_p1.hr., xl$mean_isch_national_in_p2.hr.)
t.test(large$mean_isch_national_in_p1.hr., large$mean_isch_national_in_p2.hr.)
t.test(med$mean_isch_national_in_p1.hr., med$mean_isch_national_in_p2.hr.)
t.test(small$mean_isch_national_in_p1.hr., small$mean_isch_national_in_p2.hr.)

# anova - p1
one.way <- aov(mean_isch_in_p1.hr. ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_isch_local_in_p1.hr. ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_isch_regional_in_p1.hr. ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_isch_national_in_p1.hr. ~ center_scale, data = centerwise_data)
summary(one.way)

# anova - p2
one.way <- aov(mean_isch_in_p2.hr. ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_isch_local_in_p2.hr. ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_isch_regional_in_p2.hr. ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_isch_national_in_p2.hr. ~ center_scale, data = centerwise_data)
summary(one.way)

# ancova
one.way <- aov(mean_isch_in_p2.hr. ~ center_scale*mean_isch_in_p1.hr., data = centerwise_data)
summary(one.way)

one.way <- aov(mean_isch_local_in_p2.hr. ~ center_scale*mean_isch_local_in_p1.hr., data = centerwise_data)
summary(one.way)

one.way <- aov(mean_isch_regional_in_p2.hr. ~ center_scale*mean_isch_regional_in_p1.hr., data = centerwise_data)
summary(one.way)

one.way <- aov(mean_isch_national_in_p2.hr. ~ center_scale*mean_isch_national_in_p1.hr., data = centerwise_data)
summary(one.way)

# anova on gain

# overall
pre <- subset(centerwise_data, select = c(mean_isch_in_p1.hr., center_scale))
post <- subset(centerwise_data, select = c(mean_isch_in_p2.hr., center_scale))
anova_gain(pre, post)

# local
pre <- subset(centerwise_data, select = c(mean_isch_local_in_p1.hr., center_scale))
post <- subset(centerwise_data, select = c(mean_isch_local_in_p2.hr., center_scale))
anova_gain(pre, post)

# regional
pre <- subset(centerwise_data, select = c(mean_isch_regional_in_p1.hr., center_scale))
post <- subset(centerwise_data, select = c(mean_isch_regional_in_p2.hr., center_scale))
anova_gain(pre, post)

# national
pre <- subset(centerwise_data, select = c(mean_isch_national_in_p1.hr., center_scale))
post <- subset(centerwise_data, select = c(mean_isch_national_in_p2.hr., center_scale))
anova_gain(pre, post)

# LAS ---------------------------------------------------------------------

# list date
cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_las_listdate_tx_in_p1, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(30, 60)) + 
  labs(title = "Mean list date LAS in pre-policy change period among the 4 center scales", 
       x='Center scale', y='Mean list date LAS')

cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_las_listdate_tx_in_p2, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(30, 60)) + 
  labs(title = "Mean list date LAS in post-policy change period  among the 4 center scales", 
       x='Center scale', y='Mean list date LAS')

# t test
t.test(xl$mean_las_listdate_tx_in_p1, xl$mean_las_listdate_tx_in_p2, paired = TRUE)
t_test = t.test(xl$mean_las_listdate_tx_in_p1, xl$mean_las_listdate_tx_in_p2)
((t_test$estimate[2]-t_test$estimate[1]) / t_test$estimate[1]) * 100 # delta in percentage

t.test(large$mean_las_listdate_tx_in_p1, large$mean_las_listdate_tx_in_p2, paired = TRUE)
t_test = t.test(large$mean_las_listdate_tx_in_p1, large$mean_las_listdate_tx_in_p2)
((t_test$estimate[2]-t_test$estimate[1]) / t_test$estimate[1]) * 100 # delta in percentage

t.test(med$mean_las_listdate_tx_in_p1, med$mean_las_listdate_tx_in_p2, paired = TRUE)
t_test = t.test(med$mean_las_listdate_tx_in_p1, med$mean_las_listdate_tx_in_p2)
((t_test$estimate[2]-t_test$estimate[1]) / t_test$estimate[1]) * 100 # delta in percentage

t.test(small$mean_las_listdate_tx_in_p1, small$mean_las_listdate_tx_in_p2, paired = TRUE)
t_test = t.test(small$mean_las_listdate_tx_in_p1, small$mean_las_listdate_tx_in_p2)
((t_test$estimate[2]-t_test$estimate[1]) / t_test$estimate[1]) * 100 # delta in percentage

# local
cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_las_listdate_tx_local_in_p1, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(30, 60)) + 
  labs(title = "Mean list date LAS for local cases in period 1 among the 4 center scales", 
       x='Center scale', y='Mean list date LAS for local cases')

cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_las_listdate_tx_local_in_p2, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(30, 60)) + 
  labs(title = "Mean list date LAS for local cases in period 2 among the 4 center scales", 
       x='Center scale', y='Mean list date LAS for local cases')

# t test
t.test(xl$mean_las_listdate_tx_local_in_p1, xl$mean_las_listdate_tx_local_in_p2)
t.test(large$mean_las_listdate_tx_local_in_p1, large$mean_las_listdate_tx_local_in_p2)
t.test(med$mean_las_listdate_tx_local_in_p1, med$mean_las_listdate_tx_local_in_p2)
t.test(small$mean_las_listdate_tx_local_in_p1, small$mean_las_listdate_tx_local_in_p2)

# regional
cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_las_listdate_tx_regional_in_p1, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(30, 70)) + 
  labs(title = "Mean list date LAS for regional cases in period 1 among the 4 center scales", 
       x='Center scale', y='Mean list date LAS for regional cases')

cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_las_listdate_tx_regional_in_p2, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(30, 70)) + 
  labs(title = "Mean list date LAS for regional cases in period 2 among the 4 center scales", 
       x='Center scale', y='Mean list date LAS for regional cases')

# t test
t.test(xl$mean_las_listdate_tx_regional_in_p1, xl$mean_las_listdate_tx_regional_in_p2)
t.test(large$mean_las_listdate_tx_regional_in_p1, large$mean_las_listdate_tx_regional_in_p2)
t.test(med$mean_las_listdate_tx_regional_in_p1, med$mean_las_listdate_tx_regional_in_p2)
t.test(small$mean_las_listdate_tx_regional_in_p1, small$mean_las_listdate_tx_regional_in_p2)

# national
cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_las_listdate_tx_national_in_p1, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(30, 70)) + 
  labs(title = "Mean list date LAS for national cases in period 1 among the 4 center scales", 
       x='Center scale', y='Mean list date LAS for national cases')

cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_las_listdate_tx_national_in_p2, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(30, 70)) + 
  labs(title = "Mean list date LAS for national cases in period 2 among the 4 center scales", 
       x='Center scale', y='Mean list date LAS for national cases')

# t test
t.test(xl$mean_las_listdate_tx_national_in_p1, xl$mean_las_listdate_tx_national_in_p2)
t.test(large$mean_las_listdate_tx_national_in_p1, large$mean_las_listdate_tx_national_in_p2)
t.test(med$mean_las_listdate_tx_national_in_p1, med$mean_las_listdate_tx_national_in_p2)
t.test(small$mean_las_listdate_tx_national_in_p1, small$mean_las_listdate_tx_national_in_p2)

# anova - p1
one.way <- aov(mean_las_listdate_tx_in_p1 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_las_listdate_tx_local_in_p1 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_las_listdate_tx_regional_in_p1 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_las_listdate_tx_national_in_p1 ~ center_scale, data = centerwise_data)
summary(one.way)

# anova - p2
one.way <- aov(mean_las_listdate_tx_in_p2 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_las_listdate_tx_local_in_p2 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_las_listdate_tx_regional_in_p2 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_las_listdate_tx_national_in_p2 ~ center_scale, data = centerwise_data)
summary(one.way)

# ancova
one.way <- aov(mean_las_listdate_tx_in_p2 ~ center_scale*mean_las_listdate_tx_in_p1, data = centerwise_data)
summary(one.way)

# anova on gain

# overall
pre <- subset(centerwise_data, select = c(mean_las_listdate_tx_in_p1, center_scale))
post <- subset(centerwise_data, select = c(mean_las_listdate_tx_in_p2, center_scale))
anova_gain(pre, post)

# end match LAS -----------------------------------------------------------
cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_las_tx_in_p1, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(30, 70)) + 
  labs(title = "Mean end match LAS in pre-policy change period among the 4 center scales",
       x='Center scale', y='Mean end match LAS')

cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_las_tx_in_p2, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(30, 70)) + 
  labs(title = "Mean end match LAS in pre-policy change period among the 4 center scales",
       x='Center scale', y='Mean end match LAS')

# t test
t.test(xl$mean_las_tx_in_p1, xl$mean_las_tx_in_p2, paired = TRUE)
t.test(large$mean_las_tx_in_p1, large$mean_las_tx_in_p2, paired = TRUE)
t.test(med$mean_las_tx_in_p1, med$mean_las_tx_in_p2, paired = TRUE)
t.test(small$mean_las_tx_in_p1, small$mean_las_tx_in_p2, paired = TRUE)

# local
cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_las_tx_local_in_p1, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(30, 70)) + 
  labs(title = "Mean end match LAS for local cases in pre-policy change period \namong the 4 center scales",
       x='Center scale', y='Mean end match LAS for local cases')

cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_las_tx_local_in_p2, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(30, 70)) + 
  labs(title = "Mean end match LAS for local cases in post-policy change period \namong the 4 center scales",
       x='Center scale', y='Mean end match LAS for local cases')

# t test
t.test(xl$mean_las_tx_local_in_p1, xl$mean_las_tx_local_in_p2, paired = TRUE)
t.test(large$mean_las_tx_local_in_p1, large$mean_las_tx_local_in_p2, paired = TRUE)
t.test(med$mean_las_tx_local_in_p1, med$mean_las_tx_local_in_p2, paired = TRUE)
t.test(small$mean_las_tx_local_in_p1, small$mean_las_tx_local_in_p2, paired = TRUE)

# regional
cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_las_tx_regional_in_p1, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(30, 90)) + 
  labs(title = "Mean end match LAS for regional cases in pre-policy change period \namong the 4 center scales",
       x='Center scale', y='Mean end match LAS for regional cases')

cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_las_tx_regional_in_p2, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(30, 90)) + 
  labs(title = "Mean end match LAS for regional cases in post-policy change period \namong the 4 center scales",
       x='Center scale', y='Mean end match LAS for regional cases')

# t test
t.test(xl$mean_las_tx_regional_in_p1, xl$mean_las_tx_regional_in_p2, paired = TRUE)
t.test(large$mean_las_tx_regional_in_p1, large$mean_las_tx_regional_in_p2, paired = TRUE)
t.test(med$mean_las_tx_regional_in_p1, med$mean_las_tx_regional_in_p2, paired = TRUE)
t.test(small$mean_las_tx_regional_in_p1, small$mean_las_tx_regional_in_p2, paired = TRUE)


# national
cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_las_tx_national_in_p1, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(30, 90)) + 
  labs(title = "Mean end match LAS for national cases in pre-policy change period \namong the 4 center scales",
       x='Center scale', y='Mean end match LAS for national cases')

cases_boxplot <- ggplot(centerwise_data, aes(x = center_scale, 
                                             y = mean_las_tx_national_in_p2, 
                                             color = center_scale)) +
  geom_boxplot(outlier.color = "red")
cases_boxplot + coord_cartesian(ylim = c(30, 90)) + 
  labs(title = "Mean end match LAS for national cases in post-policy change period \namong the 4 center scales",
       x='Center scale', y='Mean end match LAS for national cases')
# t test
t.test(xl$mean_las_tx_national_in_p1, xl$mean_las_tx_national_in_p2, paired = TRUE)
t.test(large$mean_las_tx_national_in_p1, large$mean_las_tx_national_in_p2, paired = TRUE)
t.test(med$mean_las_tx_national_in_p1, med$mean_las_tx_national_in_p2, paired = TRUE)
t.test(small$mean_las_tx_national_in_p1, small$mean_las_tx_national_in_p2, paired = TRUE)

# anova - p1
one.way <- aov(mean_las_tx_in_p1 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_las_tx_local_in_p1 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_las_tx_regional_in_p1 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_las_tx_national_in_p1 ~ center_scale, data = centerwise_data)
summary(one.way)

# anova - p2
one.way <- aov(mean_las_tx_in_p2 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_las_tx_local_in_p2 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_las_tx_regional_in_p2 ~ center_scale, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_las_tx_national_in_p2 ~ center_scale, data = centerwise_data)
summary(one.way)

# ancova
one.way <- aov(mean_las_tx_in_p2 ~ center_scale*mean_las_tx_in_p1, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_las_tx_local_in_p2 ~ center_scale*mean_las_tx_local_in_p1, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_las_tx_regional_in_p2 ~ center_scale*mean_las_tx_regional_in_p1, data = centerwise_data)
summary(one.way)

one.way <- aov(mean_las_tx_national_in_p2 ~ center_scale*mean_las_tx_national_in_p1, data = centerwise_data)
summary(one.way)

# anova on gain

# overall
pre <- subset(centerwise_data, select = c(mean_las_tx_in_p1, center_scale))
post <- subset(centerwise_data, select = c(mean_las_tx_in_p2, center_scale))
anova_gain(pre, post)

# local
pre <- subset(centerwise_data, select = c(mean_las_tx_local_in_p1, center_scale))
post <- subset(centerwise_data, select = c(mean_las_tx_local_in_p2, center_scale))
anova_gain(pre, post)

# regional
pre <- subset(centerwise_data, select = c(mean_las_tx_regional_in_p1, center_scale))
post <- subset(centerwise_data, select = c(mean_las_tx_regional_in_p2, center_scale))
anova_gain(pre, post)

# national
pre <- subset(centerwise_data, select = c(mean_las_tx_national_in_p1, center_scale))
post <- subset(centerwise_data, select = c(mean_las_tx_national_in_p2, center_scale))
anova_gain(pre, post)
