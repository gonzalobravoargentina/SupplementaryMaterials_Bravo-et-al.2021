
#The Friedman test is a non-parametric alternative to the one-way repeated measures ANOVA test. It extends the Sign test in the situation where there are more than two groups to compare.

#Friedman test is used to assess whether there are any statistically significant differences between the distributions of three or more paired groups. It’s recommended when the normality assumptions of the one-way repeated measures ANOVA test is not met or when the dependent variable is measured on an ordinal scale.


#Source https://www.datanovia.com/en/lessons/friedman-test-in-r/

library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)
library(ggpubr)

#Example
data("selfesteem", package = "datarium")
head(selfesteem, 3)

selfesteem <- selfesteem %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)
head(selfesteem, 3)

selfesteem %>%
  group_by(time) %>%
  get_summary_stats(score, type = "common")

ggboxplot(selfesteem, x = "time", y = "score", add = "jitter")

res.fried <- selfesteem %>% friedman_test(score ~ time |id)
res.fried

#Effect size
#The Kendall’s W coefficient assumes the value from 0 (indicating no relationship) to 1 (indicating a perfect relationship).Kendall’s W uses the Cohen’s interpretation guidelines of 0.1 - < 0.3 (small effect), 0.3 - < 0.5 (moderate effect) and >= 0.5 (large effect). Confidence intervals are calculated by bootstap.

selfesteem %>% friedman_effsize(score ~ time |id)


#Multiple pairwise-comparisons
# pairwise comparisons
pwc <- selfesteem %>%
  wilcox_test(score ~ time, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "time")
ggboxplot(selfesteem, x = "time", y = "score", add = "point") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )



PQ_VQ_long.FG <- subset(PQ_VQ_long.FG,Comments!="PQ.Robot.bycountry") #extract PQ robot by country
PQ_VQ_FG_stats <- split(PQ_VQ_long.FG, list(PQ_VQ_long.FG$country, PQ_VQ_long.FG$strata,PQ_VQ_long.FG$CATAMI))

#apply wilcox test
pairwise.wilcox <- lapply(PQ_VQ_FG_stats, function (j) (wilcox.test(j$cover.rel~j$Comments,paired = TRUE, p.adjust.method = "bonferroni")))




#Prepare data for analisis 
DF.friedman <- subset(PQ_VQ,Comments!="PQ.Robot.bycountry")

DF.friedman <- DF.friedman %>%
  select("Name", "country", "locality","site","strata","Comments","Annotation.status","CRB","MOB","SC","MAF","MAEN","MAA","MAS","MAEC")

DF.friedman.SC <- DF.friedman %>%
  select("Name", "country","locality","site","strata","Comments","Annotation.status","SC")

DF.friedman.SC <- subset(DF.friedman.SC,country=="ARGENTINA"& site=="PUNTA CUEVAS" &strata=="HIGHTIDE")

DF.friedman.SC <- DF.friedman.SC %>% group_by(Comments) %>% mutate(id = row_number())

DF.friedman.SC %>%
  group_by(Comments) %>%
  get_summary_stats(SC, type = "common")

ggboxplot(DF.friedman.SC, x = "Comments", y = "SC", add = "jitter")

res.fried <- DF.friedman.SC %>% friedman_test(SC ~ Comments|id)
res.fried

pwc <- DF.friedman.SC %>%
  wilcox_test(SC~Comments, paired = TRUE, p.adjust.method = "bonferroni")
pwc
