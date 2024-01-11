# ana.R -------------------------------------------------------------------
#' This R script performs the t-test and two-way ANOVA on center-wise data

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

# read data ---------------------------------------------------------------
df <- read.csv("processed_data/data123.csv", row.names = 1)
df_tx <- subset(df, rem_cd == 4 | rem_cd == 21)
df1 <- subset(df, period == 1)
df2 <- subset(df, period == 2)
df2_local <- subset(df2, share_ty==3)
df2_reg <- subset(df2, share_ty==4)
df2_na <- subset(df2, share_ty==5)

xl <- read.csv("processed_data/xl_centers.csv", row.names = 1)
large <- read.csv("processed_data/large_centers.csv", row.names = 1)
med <- read.csv("processed_data/med_centers.csv", row.names = 1)
small <- read.csv("processed_data/small_centers.csv", row.names = 1)

xl_tx <- subset(df_tx, ctr_scale == 'xl')
large_tx <- subset(df_tx, ctr_scale == 'large')
med_tx <- subset(df_tx, ctr_scale == 'med')
small_tx <- subset(df_tx, ctr_scale == 'small')

xl_p1 <- subset(xl, period == 1)
xl_p2 <- subset(xl, period == 2)
large_p1 <- subset(large, period == 1)
large_p2 <- subset(large, period == 2)
med_p1 <- subset(med, period == 1)
med_p2 <- subset(med, period == 2)
small_p1 <- subset(small, period == 1)
small_p2 <- subset(small, period == 2)

xl_p1_tx <- subset(xl_tx, period == 1)
xl_p2_tx <- subset(xl_tx, period == 2)
large_p1_tx <- subset(large_tx, period == 1)
large_p2_tx <- subset(large_tx, period == 2)
med_p1_tx <- subset(med_tx, period == 1)
med_p2_tx <- subset(med_tx, period == 2)
small_p1_tx <- subset(small_tx, period == 1)
small_p2_tx <- subset(small_tx, period == 2)

# rem cd ------------------------------------------------------------------
table_data <- table(df$rem_cd)

# Create the bar chart
my_bar <- barplot(table_data, xlab = "Removal Reason", ylab = "Count", 
        main = "Removal Reason from 2015-2020", ylim = c(0, max(table_data) + 1000))
text(x = my_bar, y = table_data, labels = table_data, pos = 3, 
     col = "black", cex = 0.8)

# distance ----------------------------------------------------------------
t.test(small_p1$distance, small_p2$distance)
t.test(med_p1$distance, med_p2$distance)
t.test(large_p1$distance, large_p2$distance)
t.test(xl_p1$distance, xl_p2$distance)

model <- aov(distance ~ ctr_scale, data=df2)
summary(model)
model <- aov(distance ~ ctr_scale, data=df2_local)
summary(model)
model <- aov(distance ~ ctr_scale, data=df2_reg)
summary(model)
model <- aov(distance ~ ctr_scale, data=df2_na)
summary(model)

model <- aov(distance ~ ctr_scale*period, data=df)
summary(model)

interaction.plot(df_tx$ctr_scale, df_tx$period, df_tx$distance, 
                 xlab = "center scale", ylab = "Mean of distance",
                 main = "Main Effect of center scale on distance", col = c("red", "blue"))
interaction.plot(df_tx$period, df_tx$ctr_scale, df_tx$distance, 
                 xlab = "period", ylab = "Mean of distance",
                 main = "Main Effect of period on distance", col = c("red", "blue"))

stype <- 6    # share_type

t.test(small_p1[small_p1$share_ty==stype,]$distance, small_p2[small_p2$share_ty==stype, ]$distance)
t.test(med_p1[med_p1$share_ty==stype,]$distance, med_p2[med_p2$share_ty==stype, ]$distance)
t.test(large_p1[large_p1$share_ty==stype,]$distance, large_p2[large_p2$share_ty==stype, ]$distance)
t.test(xl_p1[xl_p1$share_ty==stype,]$distance, xl_p2[xl_p2$share_ty==stype, ]$distance)

model <- aov(distance ~ ctr_scale*period, data=df[df$share_ty==stype, ])
summary(model)

interaction.plot(df[df$share_ty==stype, ]$ctr_scale, df[df$share_ty==stype, ]$period, 
                 df[df$share_ty==stype, ]$distance, 
                 xlab = "center scale", ylab = "Mean of distance",
                 main = "Main Effect of center scale on distance", col = c("red", "blue"))
interaction.plot(df[df$share_ty==stype, ]$period, df[df$share_ty==stype, ]$ctr_scale, 
                 df[df$share_ty==stype, ]$distance, 
                 xlab = "period", ylab = "Mean of distance",
                 main = "Main Effect of period on distance", col = c("red", "blue"))

data <- data.frame(
  Category = c("Local cases", "Regional cases", "National cases"),
  Count1 = c(926, 312, 627),
  Count2 = c(693, 749, 718)
)

data_long <- data %>%
  pivot_longer(cols = starts_with("Count"), names_to = "Count", values_to = "Value")

ggplot(data_long, aes(x = Category, y = Value, fill = Count)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Value), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "Category", y = "Count", title = "XL centers allocation type in pre-policy period vs. post-policy period") +
  scale_fill_manual(values = c("Count1" = "blue", "Count2" = "red"),
                    labels = c("pre-policy period", "post-policy period")) +
  theme_minimal()

model <- lm(df_tx[df_tx$period == 2, ]$distance ~ 
              df_tx[df_tx$period == 2, ]$ctr_scale + df_tx[df_tx$period == 1, ]$distance)
anova_result <- aov(model)
summary(anova_result)

# ages ----------------------------------------------------------------
var <- "init_age"
var <- "age"
var <- "age_don"

t.test(small_p1[var], small_p2[var])
t.test(med_p1[var], med_p2[var])
t.test(large_p1[var], large_p2[var])
t.test(xl_p1[var], xl_p2[var])

model <- aov(init_age ~ ctr_scale*period, data=df)
summary(model)

interaction.plot(df$ctr_scale, df$period, df$init_age, 
                 xlab = "center scale", ylab = "Mean of init age",
                 main = "Main Effect of center scale on init age", col = c("red", "blue"))
interaction.plot(df$period, df$ctr_scale, df$init_age, 
                 xlab = "period", ylab = "Mean of init age",
                 main = "Main Effect of period on init age", col = c("red", "blue"))

model <- aov(age ~ ctr_scale*period, data=df)
summary(model)

interaction.plot(df_tx$ctr_scale, df_tx$period, df_tx$age, 
                 xlab = "center scale", ylab = "Mean of age",
                 main = "Main Effect of center scale on age", col = c("red", "blue"))
interaction.plot(df_tx$period, df_tx$ctr_scale, df_tx$age, 
                 xlab = "period", ylab = "Mean of age",
                 main = "Main Effect of period on age", col = c("red", "blue"))

model <- aov(age_don ~ ctr_scale*period, data=df)
summary(model)

interaction.plot(df_tx$ctr_scale, df_tx$period, df_tx$age_don, 
                 xlab = "center scale", ylab = "Mean of donor age",
                 main = "Main Effect of center scale on donor age", col = c("red", "blue"))
interaction.plot(df_tx$period, df_tx$ctr_scale, df_tx$age_don, 
                 xlab = "period", ylab = "Mean of donor age",
                 main = "Main Effect of period on donor age", col = c("red", "blue"))

model <- aov(init_age ~ ctr_scale, data=df2)
summary(model)
model <- aov(V308 ~ ctr_scale, data=df2)
summary(model)
model <- aov(age ~ ctr_scale, data=df2)
summary(model)

# ischemic time -----------------------------------------------------------
test <- df[complete.cases(df$ischtime), ]
cor(test$distance, test$ischtime, method = "pearson")

var <- "ischtime"

na_subset <- df_tx[!complete.cases(df_tx$ischtime), ]

t.test(small_p1[var], small_p2[var])
t.test(med_p1[var], med_p2[var])
t.test(large_p1[var], large_p2[var])
t.test(xl_p1[var], xl_p2[var])

model <- aov(ischtime ~ ctr_scale*period, data=df)
summary(model)

temp <- df[complete.cases(df$ischtime), ]
# create a main effect plot for group1
interaction.plot(temp$ctr_scale, temp$period, temp$ischtime, 
                 xlab = "center scale", ylab = "Mean of ischtime",
                 main = "Main Effect of center scale on ischtime", col = c("red", "blue"))
interaction.plot(temp$period, temp$ctr_scale, temp$ischtime, 
                 xlab = "period", ylab = "Mean of ischtime",
                 main = "Main Effect of period on ischtime", col = c("red", "blue"))

stype <- 5    # share_type

t.test(small_p1[small_p1$share_ty==stype,]$ischtime, small_p2[small_p2$share_ty==stype, ]$ischtime)
t.test(med_p1[med_p1$share_ty==stype,]$ischtime, med_p2[med_p2$share_ty==stype, ]$ischtime)
t.test(large_p1[large_p1$share_ty==stype,]$ischtime, large_p2[large_p2$share_ty==stype, ]$ischtime)
t.test(xl_p1[xl_p1$share_ty==stype,]$ischtime, xl_p2[xl_p2$share_ty==stype, ]$ischtime)

model <- aov(ischtime ~ ctr_scale*period, data=df_tx[df_tx$share_ty==stype, ])
summary(model)

interaction.plot(temp[temp$share_ty==stype, ]$ctr_scale, temp[temp$share_ty==stype, ]$period, 
                 temp[temp$share_ty==stype, ]$distance, 
                 xlab = "center scale", ylab = "Mean of ischtime",
                 main = "Main Effect of center scale on ischtime", col = c("red", "blue"))
interaction.plot(temp[temp$share_ty==stype, ]$period, temp[temp$share_ty==stype, ]$ctr_scale, 
                 temp[temp$share_ty==stype, ]$distance, 
                 xlab = "period", ylab = "Mean of ischtime",
                 main = "Main Effect of period on ischtime", col = c("red", "blue"))

model <- aov(ischtime ~ ctr_scale, data=df2)
summary(model)
model <- aov(ischtime ~ ctr_scale, data=df2_local)
summary(model)
model <- aov(ischtime ~ ctr_scale, data=df2_reg)
summary(model)
model <- aov(ischtime ~ ctr_scale, data=df2_na)
summary(model)
# list date las -----------------------------------------------------------
var <- "calc_las_listdate"

t.test(small_p1[var], small_p2[var])
t.test(med_p1[var], med_p2[var])
t.test(large_p1[var], large_p2[var])
t.test(xl_p1[var], xl_p2[var])

na_subset <- df[!complete.cases(df$calc_las_listdate), ]
model <- aov(calc_las_listdate ~ ctr_scale*period, data=df)
summary(model)

temp <- df[complete.cases(df$calc_las_listdate), ]
interaction.plot(temp$ctr_scale, temp$period, temp$calc_las_listdate, 
                 xlab = "center scale", ylab = "Mean of listdate las",
                 main = "Main Effect of center scale on listdate las", col = c("red", "blue"))
interaction.plot(temp$period, temp$ctr_scale, temp$calc_las_listdate, 
                 xlab = "period", ylab = "Mean of listdate las",
                 main = "Main Effect of period on listdate las", col = c("red", "blue"))

model <- aov(calc_las_listdate ~ ctr_scale, data=df2)
summary(model)
# end match las ----------------------------------------------------------------
var <- "end_match_las"

t.test(small_p1[var], small_p2[var])
t.test(med_p1[var], med_p2[var])
t.test(large_p1[var], large_p2[var])
t.test(xl_p1[var], xl_p2[var])

model <- aov(end_match_las ~ ctr_scale*period, data=df)
# view the results
summary(model)

temp <- df[complete.cases(df$end_match_las), ]
# create a main effect plot for group1
interaction.plot(temp$ctr_scale, temp$period, temp$end_match_las, 
                 xlab = "center scale", ylab = "Mean of end match LAS",
                 main = "Main Effect of center scale on end match LAS", col = c("red", "blue"))
interaction.plot(temp$period, temp$ctr_scale, temp$end_match_las, 
                 xlab = "period", ylab = "Mean of end match LAS",
                 main = "Main Effect of period on end match LAS", col = c("red", "blue"))

model <- aov(end_match_las ~ ctr_scale, data=df2)
summary(model)
# wl mortality preprocess ------------------------------------------------------------
wlm <- data.frame(wlm_p1 = numeric(), wlm_p2 = numeric(), tx_p1 = numeric(), 
                  tx_p2 = numeric(), ctr_scale=character())


lctr_table_head <- names(table(small$listing_ctr_code))
death_count <- 0
tx_count <- 0

for (val in lctr_table_head)
{
  ctr_p1 <- small_p1[small_p1$listing_ctr_code == val, ]
  ctr_p2 <- small_p2[small_p2$listing_ctr_code == val, ]
  ctr <- small[small$listing_ctr_code == val, ]
  
  num_death_p1 <- nrow(ctr_p1[ctr_p1$rem_cd == 8, ])
  num_death_p2 <- nrow(ctr_p2[ctr_p2$rem_cd == 8, ])
  num_tx_p1 <- nrow(ctr_p1[(ctr_p1$rem_cd == 4 | ctr_p1$rem_cd == 21), ])
  num_tx_p2 <- nrow(ctr_p2[(ctr_p2$rem_cd == 4 | ctr_p2$rem_cd == 21), ])
  ctr_pyear_p1 <- sum(ctr$pyear_p1)
  ctr_pyear_p2 <- sum(ctr$pyear_p2)
  
  death_count = death_count + num_death_p1 + num_death_p2
  tx_count = tx_count + num_tx_p1 + num_tx_p2

  wlm[val, ]$wlm_p1 <- num_death_p1 / ctr_pyear_p1 * 100
  wlm[val, ]$wlm_p2 <- num_death_p2 / ctr_pyear_p2 * 100
  wlm[val, ]$tx_p1 <- num_tx_p1 / ctr_pyear_p1 * 100
  wlm[val, ]$tx_p2 <- num_tx_p2 / ctr_pyear_p2 * 100
  wlm[val, ]$ctr_scale <- "small"
  
  print(paste("center:", val))
  print(paste("num_death_p1:", num_death_p1))
  print(paste("num_death_p2:", num_death_p2))
  print(paste("num_tx_p1:", num_tx_p1))
  print(paste("num_tx_p2:", num_tx_p2))
  print(paste("ctr_pyear_p1:", ctr_pyear_p1))
  print(paste("ctr_pyear_p2:", ctr_pyear_p2))
  cat("\n")
}

#write.csv(wlm, "wlm.csv", row.names = TRUE)
wlm_anova <- data.frame(wlm = rep(NA, 140), tx = rep(NA, 140), 
                        ctr_scale = rep(NA, 140), period = rep(NA, 140))

wlm_anova[1:70, ]$wlm <- wlm$wlm_p1
wlm_anova[1:70, ]$tx <- wlm$tx_p1
wlm_anova[1:70, ]$ctr_scale <- wlm$ctr_scale
wlm_anova[1:70, ]$period <- rep(1, 70)

wlm_anova[71:140, ]$wlm <- wlm$wlm_p2
wlm_anova[71:140, ]$tx <- wlm$tx_p2
wlm_anova[71:140, ]$ctr_scale <- wlm$ctr_scale
wlm_anova[71:140, ]$period <- rep(2, 70)

#write.csv(wlm_anova, "wlm_anova.csv", row.names = TRUE)

# wlm and tx ---------------------------------------------------------------------
wlm <- df <- read.csv("processed_data/wlm.csv", row.names = 1)

model <- aov(wlm ~ ctr_scale*period, data=wlm_anova)
summary(model)

# create a main effect plot for group1
interaction.plot(wlm_anova$ctr_scale, wlm_anova$period, wlm_anova$wlm, 
                 xlab = "center scale", ylab = "Mean of waitlist mortality",
                 main = "Main Effect of center scale on waitlist mortality", col = c("red", "blue"))
interaction.plot(wlm_anova$period, wlm_anova$ctr_scale, wlm_anova$wlm, 
                 xlab = "period", ylab = "Mean of waitlist mortality",
                 main = "Main Effect of period on waitlist mortality", col = c("red", "blue"))

model <- aov(tx ~ ctr_scale*period, data=wlm_anova)
summary(model)

# create a main effect plot for group1
interaction.plot(wlm_anova$ctr_scale, wlm_anova$period, wlm_anova$tx, 
                 xlab = "center scale", ylab = "Mean of tx. per 100-patient-year",
                 main = "Main Effect of center scale on tx. per 100-patient-year", col = c("red", "blue"))
interaction.plot(wlm_anova$period, wlm_anova$ctr_scale, wlm_anova$tx, 
                 xlab = "period", ylab = "Mean of tx. per 100-patient-year",
                 main = "Main Effect of period on tx. per 100-patient-year", col = c("red", "blue"))

model <- aov(wlm_p2 ~ ctr_scale, data=wlm)
summary(model)

model <- aov(tx_p2 ~ ctr_scale, data=wlm)
summary(model)


# graft time --------------------------------------------------------------
test_p1 <- subset(xl_p1, gstatus == 1)
test_p2 <- subset(xl_p2, gstatus == 1)

test_p1 <- subset(xl_p1, V425 == 'N')
test_p2 <- subset(xl_p2, V425 == 'N')

t.test(test_p1$V252, test_p2$V252)

test <- subset(df2, V425 == 'N')
test <- subset(df2, gstatus == 1)

model <- aov(V252 ~ ctr_scale, data=test)
summary(model)


# Length of Stay ----------------------------------------------------------
var <- "los"

t.test(small_p1[var], small_p2[var])
t.test(med_p1[var], med_p2[var])
t.test(large_p1[var], large_p2[var])
t.test(xl_p1[var], xl_p2[var])

model <- aov(los ~ ctr_scale, data=df2)
summary(model)


# allocation type ---------------------------------------------------------

observed <- matrix(c(636, 379))    # small center local cases
observed <- matrix(c(763, 412))    # small center local cases
observed <- matrix(c(614, 423))    # small center local cases
observed <- matrix(c(926, 693))    # XL center local cases

observed <- matrix(c(144, 509))    # small center regional cases
observed <- matrix(c(185, 500))    # small center regional cases
observed <- matrix(c(186, 364))    # small center regional cases
observed <- matrix(c(312, 749))    # XL center regional cases

observed <- matrix(c(270, 593))    # small center national cases
observed <- matrix(c(297, 492))    # small center national cases
observed <- matrix(c(334, 559))    # small center national cases
observed <- matrix(c(627, 718))    # XL center national cases

observed <- matrix(c(1050, 1481))    # small center overall cases
observed <- matrix(c(1245, 1404))    # small center overall cases
observed <- matrix(c(1135, 1348))    # small center overall cases
observed <- matrix(c(1867, 2161))    # XL center overall cases

# Perform the chi-squared test
result <- chisq.test(observed)
result$p.value

model <- aov(V252 ~ ctr_scale, data=test)
summary(model)