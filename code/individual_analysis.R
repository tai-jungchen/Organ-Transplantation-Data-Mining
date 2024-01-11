# individual_analysis.R ------------------------------------------------
#' This R script carries out the individual analysis

# Init --------------------------------------------------------------------
# import functions
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

# Assign the data to a variable #
thoracic_data <- read.delim("raw_dataset/THORACIC_DATA.DAT", header=FALSE, na.strings = c(".", "" ) )

# change column's name #
colnames(thoracic_data)[106] <- c("rem_cd")
colnames(thoracic_data)[144] <- c("wl_id_code")
colnames(thoracic_data)[450] <- c("trr_id_code")
colnames(thoracic_data)[473] <- c("donor_id")

colnames(thoracic_data)[1] <- c("wl_org")
colnames(thoracic_data)[418] <- c("age")
colnames(thoracic_data)[429] <- c("age_group")
colnames(thoracic_data)[110] <- c("init_age")
colnames(thoracic_data)[14] <- c("gender")

colnames(thoracic_data)[137] <- c("region")
colnames(thoracic_data)[426] <- c("share_ty")
colnames(thoracic_data)[445] <- c("distance")

colnames(thoracic_data)[421] <- c("ischtime")
colnames(thoracic_data)[255] <- c("ptime")
colnames(thoracic_data)[114] <- c("end_date")
colnames(thoracic_data)[163] <- c("admission_date")
colnames(thoracic_data)[427] <- c("los")

colnames(thoracic_data)[422] <- c("grf_fail_cause")
colnames(thoracic_data)[251] <- c("gstatus")
#colnames(thoracic_data)[252] <- c("gtime")
#colnames(thoracic_data)[425] <- c("grf_stat")

colnames(thoracic_data)[103] <- c("calc_las_listdate")

colnames(thoracic_data)[256] <- c("px_stat")
colnames(thoracic_data)[393] <- c("cod_cad_don")
#colnames(thoracic_data)[394] <- c("cod_liv_don")
colnames(thoracic_data)[245] <- c("cod")

colnames(thoracic_data)[115] <- c("init_date")
colnames(thoracic_data)[457] <- c("admit_don_date")
colnames(thoracic_data)[163] <- c("admission_date")
colnames(thoracic_data)[112] <- c("activate_date")
colnames(thoracic_data)[413] <- c("tx_date")
colnames(thoracic_data)[414] <- c("discharge_date")
colnames(thoracic_data)[113] <- c("death_date")
colnames(thoracic_data)[114] <- c("end_date")

colnames(thoracic_data)[529] <- c("ctr_code")
colnames(thoracic_data)[530] <- c("opo_ctr_code")
colnames(thoracic_data)[531] <- c("init_opo_ctr_code")
colnames(thoracic_data)[532] <- c("end_opo_ctr_code")
colnames(thoracic_data)[533] <- c("listing_ctr_code")

thoracic_data <- transform(thoracic_data, distance = as.numeric(distance))
thoracic_data <- transform(thoracic_data, ischtime = as.numeric(ischtime))
thoracic_data <- transform(thoracic_data, ptime = as.numeric(ptime))
thoracic_data <- transform(thoracic_data, calc_las_listdate = as.numeric(calc_las_listdate))
thoracic_data <- transform(thoracic_data, region = as.character(region))
thoracic_data <- transform(thoracic_data, gstatus = as.character(gstatus))
thoracic_data <- transform(thoracic_data, cod_cad_don = as.character(cod_cad_don))
thoracic_data <- transform(thoracic_data, rem_cd = as.character(rem_cd))

# change date format
thoracic_data$tx_date <- as.Date(thoracic_data$tx_date, format= "%m/%d/%Y")
thoracic_data$end_date <- as.Date(thoracic_data$end_date, format= "%m/%d/%Y")
thoracic_data$init_date <- as.Date(thoracic_data$init_date, format= "%m/%d/%Y")

# include lung-only cases
thoracic_data <- thoracic_data[(thoracic_data$wl_org=="LU"),]

# fill na cases with 999 for rem_cd
thoracic_data$rem_cd[is.na(thoracic_data$rem_cd)] <- c(999) 

# 2015 - 2020
thoracic_data_2015_to_2020 <- subset(thoracic_data, end_date >= "2015-08-18" & init_date <= "2020-03-01")

# calculate age manually
days <- difftime(thoracic_data_2015_to_2020$end_date, thoracic_data_2015_to_2020$init_date, unit = "days")
yrs <- days / 365.25
age_self <- thoracic_data_2015_to_2020$init_age + yrs
thoracic_data_2015_to_2020$age_self <- as.integer(age_self)

# label periods
thoracic_data_2015_to_2020 <- thoracic_data_2015_to_2020 %>%
  add_column(period = if_else(.$end_date >= "2015-08-18" & .$end_date <= "2017-11-23"
  , "1", "2"))

# 2015 - 2017
thoracic_data_2015_to_2017 <- thoracic_data_2015_to_2020[thoracic_data_2015_to_2020$period == 1, ]

# 2017 - 2020
thoracic_data_2017_to_2020 <- thoracic_data_2015_to_2020[thoracic_data_2015_to_2020$period == 2, ]

##### testing #####
test <- subset(thoracic_data_2015_to_2020, init_date == "1997-07-07")

censored_data <- subset(thoracic_data_2015_to_2020, rem_cd == '999') 
p3_data <- subset(thoracic_data_2015_to_2020, period == 2 & init_date <= "2017-11-24")
p4_data <- subset(thoracic_data_2015_to_2020, end_date > "2020-03-01")
##### testing #####


# Export files ------------------------------------------------------------

#write.csv(thoracic_data_2015_to_2020,"thoracic_data_2015_to_2020.csv", row.names = TRUE)
#write.csv(thoracic_data_2015_to_2017,"thoracic_data_2015_to_2017.csv", row.names = TRUE)
#write.csv(thoracic_data_2017_to_2020,"thoracic_data_2017_to_2020.csv", row.names = TRUE)


########## old codes ##########
# rem_cd ------------------------------------------------------------------
rem_cd_bar_graph <- single_bar_graph_of_count(thoracic_data_2015_to_2020, 
                                        thoracic_data_2015_to_2020$rem_cd)
rem_cd_bar_graph + labs(title = "reason for removal from the wait list bar chart",
                  x = "removal reason")

# age ---------------------------------------------------------------------

# 2014 - 2017 histogram
skim(thoracic_data_2014_to_2017$age)
summary(thoracic_data_2014_to_2017$age)
age_histogram_2014 <- single_histogram_of_count(thoracic_data_2014_to_2017, 
                                           thoracic_data_2014_to_2017$age)
age_histogram_2014 + labs(title = "Recipient age (2014 - 2017)", 
                          x = 'Recipient age', y = "counts") +
                      coord_cartesian(xlim = c(0, 90), ylim = c(0, 1200))
# 2017 - 2020 histogram
skim(thoracic_data_2017_to_2020$age)
summary(thoracic_data_2017_to_2020$age)
age_histogram_2017 <- single_histogram_of_count(thoracic_data_2017_to_2020, 
                                               thoracic_data_2017_to_2020$age)
age_histogram_2017 + labs(title = "Recipient age (2017 - 2020)", 
                            x = 'Recipient age', y = "counts") +
                          coord_cartesian(xlim = c(0, 90), ylim = c(0, 1200))

# single box plot
age_single_box_plot <- single_box_plot(thoracic_data_2014_to_2020, 
                                         thoracic_data_2014_to_2020$age,
                                         thoracic_data_2014_to_2017$period)
age_single_box_plot + labs(title = "Recipient age in the two periods", 
                           x = 'Period', y = "Age")

# grouped box plot
age_grouped_box_plot <- grouped_box_plot(thoracic_data_2014_to_2020, 
                                         thoracic_data_2014_to_2020$share_ty,
                                         thoracic_data_2014_to_2020$age,
                                         thoracic_data_2014_to_2017$period)
age_grouped_box_plot + labs(title = "Recipient age in the four allocation types", 
                            x = 'Allocation type', y = "Age")

# t-test among period
t.test(thoracic_data_2014_to_2020$age~thoracic_data_2014_to_2020$period)
# t-test among share type
share_ty_ttest(thoracic_data_2014_to_2020$period, 
               thoracic_data_2014_to_2020$age, 
               thoracic_data_2014_to_2020$share_ty, 3)
share_ty_ttest(thoracic_data_2014_to_2020$period, 
               thoracic_data_2014_to_2020$age, 
               thoracic_data_2014_to_2020$share_ty, 4)
share_ty_ttest(thoracic_data_2014_to_2020$period, 
               thoracic_data_2014_to_2020$age, 
               thoracic_data_2014_to_2020$share_ty, 5)

skim(subset(thoracic_data_2014_to_2017$age, thoracic_data_2014_to_2017$share_ty == 3))
summary(subset(thoracic_data_2014_to_2017$age, thoracic_data_2014_to_2017$share_ty == 3))

skim(subset(thoracic_data_2017_to_2020$age, thoracic_data_2017_to_2020$share_ty == 3))
summary(subset(thoracic_data_2017_to_2020$age, thoracic_data_2017_to_2020$share_ty == 3))

skim(subset(thoracic_data_2014_to_2017$age, thoracic_data_2014_to_2017$share_ty == 4))
summary(subset(thoracic_data_2014_to_2017$age, thoracic_data_2014_to_2017$share_ty == 4))

skim(subset(thoracic_data_2017_to_2020$age, thoracic_data_2017_to_2020$share_ty == 4))
summary(subset(thoracic_data_2017_to_2020$age, thoracic_data_2017_to_2020$share_ty == 4))

skim(subset(thoracic_data_2014_to_2017$age, thoracic_data_2014_to_2017$share_ty == 5))
summary(subset(thoracic_data_2014_to_2017$age, thoracic_data_2014_to_2017$share_ty == 5))

skim(subset(thoracic_data_2017_to_2020$age, thoracic_data_2017_to_2020$share_ty == 5))
summary(subset(thoracic_data_2017_to_2020$age, thoracic_data_2017_to_2020$share_ty == 5))


# Gender ------------------------------------------------------------------
gender_bar_graph <- mixed_bar_chart(thoracic_data_2014_to_2020, 
                                    thoracic_data_2014_to_2020$gender,
                                     thoracic_data_2014_to_2020$period)
gender_bar_graph + labs(title = "Gender within the two periods",
                        x = "Gender")


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
myGraph <- ggplot(thoracic_data_2015_to_2020, aes(x = thoracic_data_2015_to_2020$share_ty, 
                                                  fill = thoracic_data_2015_to_2020$ctr_scale)) +
  stat_count(geom = 'bar', position = 'dodge') +
  geom_text(stat='count', aes(label=..count..), 
            position = position_dodge(width = 1.0))

myGraph + labs(title = "Cases in each allocation type for the three scales", 
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
