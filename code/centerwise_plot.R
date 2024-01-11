# centerwise_plot.R -------------------------------------------------------
#' This R script carries out the analysis for old centerwise data

# Import ------------------------------------------------------------------
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


# Load data ---------------------------------------------------------------
data12 <- read.csv("processed_data/data12.csv")
large_centers <- read.csv("processed_data/large_centers.csv")
med_centers <- read.csv("processed_data/med_centers.csv")
small_centers <- read.csv("processed_data/small_centers.csv")

data12$period <- as.character(data12$period)

pure_tx <- subset(data12, rem_cd == 4 | rem_cd == 21)

# Cases count -------------------------------------------------------------
myGraph <- ggplot(data12, aes(x = ctr_scale, fill = period)) +
  stat_count(geom = 'bar', position = 'dodge') +
  geom_text(stat='count', aes(label=..count..), 
            position = position_dodge(width = 1.0))

myGraph + labs(title = "Cases in each scale for the two periods", 
               x = "Center scale")

myGraph <- ggplot(pure_tx, aes(x = ctr_scale, fill = period)) +
  stat_count(geom = 'bar', position = 'dodge') +
  geom_text(stat='count', aes(label=..count..), 
            position = position_dodge(width = 1.0))

myGraph + labs(title = "Cases in each scale for the two periods", 
               x = "Center scale")

# region ------------------------------------------------------------------
thoracic_data_2014_to_2017$region <- factor(thoracic_data_2014_to_2017$region, 
                                            levels = c(1,2,3,4,5,6,7,8,9,10,11))
region_2014 <- single_bar_graph_of_count(thoracic_data_2014_to_2017, 
                                         thoracic_data_2014_to_2017$region)
region_2014 + labs(title = "Cases in each region (2014-2017)", x = "Region")

thoracic_data_2017_to_2020$region <- factor(thoracic_data_2017_to_2020$region, 
                                            levels = c(1,2,3,4,5,6,7,8,9,10,11))
region_2017 <- single_bar_graph_of_count(thoracic_data_2017_to_2020, 
                                         thoracic_data_2017_to_2020$region)
region_2017 + labs(title = "Cases in each region (2017-2020)", x = "Region")

# Allocation type ---------------------------------------------------------
myGraph <- ggplot(pure_tx, aes(x = share_ty, fill = period)) +
  stat_count(geom = 'bar', position = 'dodge') +
  geom_text(stat='count', aes(label=..count..), 
            position = position_dodge(width = 1.0))

myGraph + labs(title = "Cases in each allocation type for the two periods", 
               x = "Allocation type")

myGraph <- ggplot(pure_tx, aes(x = share_ty, fill = ctr_scale)) +
  stat_count(geom = 'bar', position = 'dodge') +
  geom_text(stat='count', aes(label=..count..), 
            position = position_dodge(width = 1.0))

myGraph + labs(title = "Cases in each allocation type for the three scales", 
               x = "Allocation type")

pure_tx_large <- subset(pure_tx, ctr_scale == "large")

myGraph <- ggplot(pure_tx_large, aes(x = share_ty, fill = period)) +
  stat_count(geom = 'bar', position = 'dodge') +
  geom_text(stat='count', aes(label=..count..), 
            position = position_dodge(width = 1.0))

myGraph + labs(title = "Cases in each allocation type for large centers", 
                 x = "Allocation type")

pure_tx_med <- subset(pure_tx, ctr_scale == "med")

myGraph <- ggplot(pure_tx_med, aes(x = share_ty, fill = period)) +
  stat_count(geom = 'bar', position = 'dodge') +
  geom_text(stat='count', aes(label=..count..), 
            position = position_dodge(width = 1.0))

myGraph + labs(title = "Cases in each allocation type for medium centers", 
               x = "Allocation type")

pure_tx_small <- subset(pure_tx, ctr_scale == "small")

myGraph <- ggplot(pure_tx_small, aes(x = share_ty, fill = period)) +
  stat_count(geom = 'bar', position = 'dodge') +
  geom_text(stat='count', aes(label=..count..), 
            position = position_dodge(width = 1.0))

myGraph + labs(title = "Cases in each allocation type for small centers", 
               x = "Allocation type")

# Distance ----------------------------------------------------------------

# single box plot
dist_box_plot <- single_box_plot(thoracic_data_2014_to_2020, 
                                 thoracic_data_2014_to_2020$nautical_mile,
                                 thoracic_data_2014_to_2020$period)
dist_box_plot + labs(title = "Distance between two periods", 
                     x = "Period", y = "Distance (NM)")

dist_box_plot + labs(title = "Distance between two periods", 
                     x = "Period", y = "Distance (NM)") +
  coord_cartesian(ylim = c(0, 400))
# histogram
dist_single_hist <- single_histogram_of_count(thoracic_data_2014_to_2017,
                                              thoracic_data_2014_to_2017$nautical_mile)
dist_single_hist + labs(title = "Distance histogram (2014-2017)", 
                        x = "Distance (NM)") +
  coord_cartesian(xlim = c(0, 3600), ylim = c(0, 3000))
skim(thoracic_data_2014_to_2017$nautical_mile)
summary(thoracic_data_2014_to_2017$nautical_mile)

dist_single_hist_2 <- single_histogram_of_count(thoracic_data_2017_to_2020,
                                                thoracic_data_2017_to_2020$nautical_mile)
dist_single_hist_2 + labs(title = "Distance histogram (2017-2020)", 
                          x = "Distance (NM)") +
  coord_cartesian(xlim = c(0, 3600), ylim = c(0, 3000))
skim(thoracic_data_2017_to_2020$nautical_mile)
summary(thoracic_data_2017_to_2020$nautical_mile)

# grouped box plot
dist_grouped_box_plot <- grouped_box_plot(thoracic_data_2014_to_2020,
                                          thoracic_data_2014_to_2020$share_ty,
                                          thoracic_data_2014_to_2020$nautical_mile,
                                          thoracic_data_2014_to_2020$period)

dist_grouped_box_plot + labs(title = "Distance vs. Allocation type between two periods", 
                             x = "Allocation type", y = "Distance (NM)")

dist_grouped_box_plot + labs(title = "Distance vs. Allocation type between two periods", 
                             x = "Allocation type", y = "Distance (NM)") +
  coord_cartesian(ylim = c(0, 500))

# stats
skim(subset(thoracic_data_2014_to_2017$nautical_mile, thoracic_data_2014_to_2017$share_ty == 3))
summary(subset(thoracic_data_2014_to_2017$nautical_mile, thoracic_data_2014_to_2017$share_ty == 3))

skim(subset(thoracic_data_2017_to_2020$nautical_mile, thoracic_data_2017_to_2020$share_ty == 3))
summary(subset(thoracic_data_2017_to_2020$nautical_mile, thoracic_data_2017_to_2020$share_ty == 3))

skim(subset(thoracic_data_2014_to_2017$nautical_mile, thoracic_data_2014_to_2017$share_ty == 4))
summary(subset(thoracic_data_2014_to_2017$nautical_mile, thoracic_data_2014_to_2017$share_ty == 4))

skim(subset(thoracic_data_2017_to_2020$nautical_mile, thoracic_data_2017_to_2020$share_ty == 4))
summary(subset(thoracic_data_2017_to_2020$nautical_mile, thoracic_data_2017_to_2020$share_ty == 4))

skim(subset(thoracic_data_2014_to_2017$nautical_mile, thoracic_data_2014_to_2017$share_ty == 5))
summary(subset(thoracic_data_2014_to_2017$nautical_mile, thoracic_data_2014_to_2017$share_ty == 5))

skim(subset(thoracic_data_2017_to_2020$nautical_mile, thoracic_data_2017_to_2020$share_ty == 5))
summary(subset(thoracic_data_2017_to_2020$nautical_mile, thoracic_data_2017_to_2020$share_ty == 5))

# t-test among period
t.test(thoracic_data_2014_to_2020$nautical_mile~thoracic_data_2014_to_2020$period)
# t-test among share type
share_ty_ttest(thoracic_data_2014_to_2020$period, 
               thoracic_data_2014_to_2020$nautical_mile, 
               thoracic_data_2014_to_2020$share_ty, 3)
share_ty_ttest(thoracic_data_2014_to_2020$period, 
               thoracic_data_2014_to_2020$nautical_mile, 
               thoracic_data_2014_to_2020$share_ty, 4)
share_ty_ttest(thoracic_data_2014_to_2020$period, 
               thoracic_data_2014_to_2020$nautical_mile, 
               thoracic_data_2014_to_2020$share_ty, 5)


# ischemic time -----------------------------------------------------------

# single box plot
ische_box_plot <- single_box_plot(thoracic_data_2014_to_2020, 
                                  thoracic_data_2014_to_2020$ischtime,
                                  thoracic_data_2014_to_2020$period)
ische_box_plot + labs(title = "Ischemic time between two periods", 
                      x = "Period", y = "Ischemic time (HR)")

ische_box_plot + labs(title = "Ischemic time between two periods", 
                      x = "Period", y = "Ischemic time (HR)") +
  coord_cartesian(ylim = c(0, 10))

# histogram
ische_single_hist <- single_histogram_of_count(thoracic_data_2014_to_2017,
                                               thoracic_data_2014_to_2017$ischtime)
ische_single_hist + labs(title = "Ischemic time histogram (2014-2017)", 
                         x = "Ischemic time (HR)") +
  coord_cartesian(xlim = c(0, 25), ylim = c(0, 1750))
skim(thoracic_data_2014_to_2017$ischtime)
summary(thoracic_data_2014_to_2017$ischtime)

ische_single_hist_2 <- single_histogram_of_count(thoracic_data_2017_to_2020,
                                                 thoracic_data_2017_to_2020$ischtime)
ische_single_hist_2 + labs(title = "Ischemic time histogram (2017-2020)", 
                           x = "Ischemic time (HR)") +
  coord_cartesian(xlim = c(0, 25), ylim = c(0, 1750))
skim(thoracic_data_2017_to_2020$ischtime)
summary(thoracic_data_2017_to_2020$ischtime)

# grouped box plot
ische_grouped_box_plot <- grouped_box_plot(thoracic_data_2014_to_2020,
                                           thoracic_data_2014_to_2020$share_ty,
                                           thoracic_data_2014_to_2020$ischtime,
                                           thoracic_data_2014_to_2020$period)

ische_grouped_box_plot + labs(title = "Ischemic time vs. Allocation type between two periods", 
                              x = "Allocation type", y = "Ischemic time (HR)")

ische_grouped_box_plot + labs(title = "Distance vs. Allocation type between two periods", 
                              x = "Allocation type", y = "Ischemic time (HR)") +
  coord_cartesian(ylim = c(0, 10))

# stats
skim(subset(thoracic_data_2014_to_2017$ischtime, thoracic_data_2014_to_2017$share_ty == 3))
summary(subset(thoracic_data_2014_to_2017$ischtime, thoracic_data_2014_to_2017$share_ty == 3))

skim(subset(thoracic_data_2017_to_2020$ischtime, thoracic_data_2017_to_2020$share_ty == 3))
summary(subset(thoracic_data_2017_to_2020$ischtime, thoracic_data_2017_to_2020$share_ty == 3))

skim(subset(thoracic_data_2014_to_2017$ischtime, thoracic_data_2014_to_2017$share_ty == 4))
summary(subset(thoracic_data_2014_to_2017$ischtime, thoracic_data_2014_to_2017$share_ty == 4))

skim(subset(thoracic_data_2017_to_2020$ischtime, thoracic_data_2017_to_2020$share_ty == 4))
summary(subset(thoracic_data_2017_to_2020$ischtime, thoracic_data_2017_to_2020$share_ty == 4))

skim(subset(thoracic_data_2014_to_2017$ischtime, thoracic_data_2014_to_2017$share_ty == 5))
summary(subset(thoracic_data_2014_to_2017$ischtime, thoracic_data_2014_to_2017$share_ty == 5))

skim(subset(thoracic_data_2017_to_2020$ischtime, thoracic_data_2017_to_2020$share_ty == 5))
summary(subset(thoracic_data_2017_to_2020$ischtime, thoracic_data_2017_to_2020$share_ty == 5))

# t-test among period
t.test(thoracic_data_2014_to_2020$ischtime~thoracic_data_2014_to_2020$period)
# t-test among share type
share_ty_ttest(thoracic_data_2014_to_2020$period, 
               thoracic_data_2014_to_2020$ischtime, 
               thoracic_data_2014_to_2020$share_ty, 3)
share_ty_ttest(thoracic_data_2014_to_2020$period, 
               thoracic_data_2014_to_2020$ischtime, 
               thoracic_data_2014_to_2020$share_ty, 4)
share_ty_ttest(thoracic_data_2014_to_2020$period, 
               thoracic_data_2014_to_2020$ischtime, 
               thoracic_data_2014_to_2020$share_ty, 5)

# Pareto Chart ------------------------------------------------------------

COD <- table(thoracic_data_2014_to_2017$cod)
top_COD <- COD[COD >= 100]
top_COD <- sort(top_COD, decreasing = TRUE)

GRF <- table(thoracic_data_2014_to_2017$grf_fail_cause)

pareto.chart(GRF, ylab = "Frequency", 
             ylab2 = "Cumulative Percentage",
             cumperc = c(20,40,60,80,100),
             col=rainbow(length(GRF)))


# ANOVA -------------------------------------------------------------------

two.way <- aov(calc_las_listdate ~ region + share_ty, data = thoracic_data)
summary(two.way)
two.way <- aov(calc_las_listdate ~ region * share_ty , data = thoracic_data)
summary(two.way)

sur <- aov(ptime ~ region + share_ty + wl_org, data = thoracic_data)
summary(sur)
sur <- aov(ptime ~ region * share_ty * wl_org, data = thoracic_data)
summary(sur)

isch <- aov(ischtime ~ region + share_ty + wl_org, data = thoracic_data)
summary(isch)
isch <- aov(ischtime ~ region * share_ty * wl_org, data = thoracic_data)
summary(isch)


# Survival Analysis -------------------------------------------------------


# Dead cases only
thoracic_data_2014_to_2020_dead <- subset(thoracic_data_2014_to_2020, px_stat == "D" & share_ty != "6")
thoracic_data_2014_to_2017_dead <- subset(thoracic_data_2014_to_2020_dead, tx_date >= "2014-01-01" 
                                          & tx_date <= "2016-12-31")
thoracic_data_2017_to_2020_dead <- subset(thoracic_data_2014_to_2020_dead, tx_date >= "2017-01-01" 
                                          & tx_date <= "2020-12-31")

# Alive and Dead cases only
thoracic_data_2014_to_2020_AD <- subset(thoracic_data_2014_to_2020, 
                                        px_stat != "" & px_stat != "L" & 
                                          px_stat != "R" & share_ty != "6")
thoracic_data_2014_to_2020_AD <- thoracic_data_2014_to_2020_AD %>%
  add_column(death = if_else(.$px_stat == "A", 0, 1))

thoracic_data_2014_to_2020_AD <- thoracic_data_2014_to_2020_AD %>%
  add_column(pmonth_censored = if_else(.$pmonth >= 36, 36, .$pmonth))

# censored data processing
thoracic_data_2014_to_2020_AD$pmonth_censored <- 
  ifelse(thoracic_data_2014_to_2020_AD$pmonth_censored >= 6, 
         6, thoracic_data_2014_to_2020_AD$pmonth_censored)


km.model <- survfit(Surv(thoracic_data_2014_to_2020_AD$pmonth_censored, 
                         thoracic_data_2014_to_2020_AD$death) 
                    ~ thoracic_data_2014_to_2020_AD$period)
summary(km.model)

plot(km.model, conf.int = F, xlab = "Time (months)", ylab = "% Alive",
     main = "KM-Model", col = c("red", "blue"), las = 1, lwd = 1, 
     mark.time = FALSE)

legend(50, 0.95, legend = c("2014-2017", "2017-2020"), 
       lty = 1, lwd = 1, col = c("red", "blue"),
       bty = "", cex = 0.6)

# log-rank test
survdiff(Surv(thoracic_data_2014_to_2020_AD$pmonth_censored, 
              thoracic_data_2014_to_2020_AD$death) 
         ~ thoracic_data_2014_to_2020_AD$period)