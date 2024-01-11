
# import ------------------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)

# RMANOVA - one-way -------------------------------------------------------
data("selfesteem", package = "datarium")
head(selfesteem, 3)

selfesteem <- selfesteem %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)
head(selfesteem, 3)

res.aov <- anova_test(data = selfesteem, dv = score, wid = id, within = time)
get_anova_table(res.aov)


# RMANOVA - two-way -------------------------------------------------------
set.seed(123)
data("selfesteem2", package = "datarium")
selfesteem2 %>% sample_n_by(treatment, size = 1)

selfesteem2 <- selfesteem2 %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)
set.seed(123)
selfesteem2 %>% sample_n_by(treatment, time, size = 1)

bxp <- ggboxplot(selfesteem2, x = "time", y = "score", color = "treatment", 
                 palette = "jco")
bxp

res.aov <- anova_test(data = selfesteem2, dv = score, wid = id, 
                      within = c(treatment, time))
get_anova_table(res.aov)

