
# importing ---------------------------------------------------------------


library(ggplot2)


# bar chart ---------------------------------------------------------------

# single bar chart #
single_bar_graph_of_count <- function(data, var)
{
  myGraph <- ggplot(data, aes(var)) +
    geom_bar() +
    geom_text(stat='count', aes(label=..count..))
  return(myGraph)
}

# mixed 2 periods' bar chart #
mixed_bar_chart <- function(data, var, period)
{
  myGraph <- ggplot(data, aes(var, fill = period)) +
  stat_count(geom = 'bar', position = 'dodge') +
  geom_text(stat='count', aes(label=..count..), 
            position = position_dodge(width = 1.0))
  return(myGraph)
}

# allocation type bar chart #
allo_bar_chart <- function(data, var, period)
{
  myGraph <- ggplot(data, aes(x = var, fill = period)) +
    stat_count(geom = 'bar', position = 'dodge') +
    geom_text(stat='count', aes(label=..count..), 
              position = position_dodge(width = 1.0))
  return(myGraph)
}

# stacked bar chart #
mixed_bar_chart <- function(data, var, period)
{
  myGraph <- ggplot(data, aes(period, fill = var)) +
                stat_count(geom = 'bar')
  return(myGraph)
}


# histogram ---------------------------------------------------------------


# single histogram #
single_histogram_of_count <- function(data, var)
{
  myGraph <- ggplot(data, aes(var)) +
    geom_histogram(bins = 30, fill = "white", color = "black")
  return(myGraph)
}



# box plot ----------------------------------------------------------------

# single box plot #
single_box_plot <- function(data, var, period)
{
  myGraph <- ggplot(data, aes(period, var, color = period)) +
    geom_boxplot(outlier.color = "red")
  return(myGraph)
}

# grouped box plot #
grouped_box_plot <- function(data, var1, var2, period)
{
  myGraph <- ggplot(data, aes(x = factor(var1), y = var2, color = period)) +
                geom_boxplot(outlier.color = "red")
  return(myGraph)
}



# t-test ------------------------------------------------------------------

share_ty_ttest <- function(period, response, share_ty, share_type)
{
  t.test(subset(response, share_ty == share_type)
         ~ subset(period, share_ty == share_type))
}
# analysis ----------------------------------------------------------------

analyze <- function(var)
{
  skim(var)
  summary(var)
}


# ANOVA on gain -----------------------------------------------------------

anova_gain <- function(pre_var, post_var)
{
  pre <- subset(pre_var, select = c(1))
  post <- subset(post_var, select = c(1))
  center_scale <- data.frame(subset(post_var, select = c(2)))
  gain <- data.frame(post - pre)
  
  gain_data <- cbind(gain, center_scale)
  colnames(gain_data) <- c('gain','center_scale')
  
  one.way <- aov(gain ~ center_scale, data = gain_data)
  summary(one.way)
}

# RMANOVA -----------------------------------------------------------------

rmanova <- function(var)
{
  selfesteem2 <- selfesteem2 %>%
    gather(key = "time", value = "score", t1, t2) %>%
    convert_as_factor(id, time)
  
  res.aov <- anova_test(data = selfesteem2, dv = score, wid = id, 
                        within = c(treatment, time))
  get_anova_table(res.aov)
}

