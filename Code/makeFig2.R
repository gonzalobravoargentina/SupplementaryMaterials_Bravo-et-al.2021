## Make Figure 2

library(readr)
library(ggplot2)
library(dplyr)
library(ggpubr)

selectedLabels <- c("CRB", "MOB", "SC", "MAF", "MAEN", "MAA", "MAS", "MAEC")


###################
## Human vs Robot
##################
DF_HR <- read_csv("./DataClean/DF_HumanRobot.csv", col_types = cols())
DF_HR$strata <- factor(DF_HR$strata, levels = c("LT", "MT", "HT"))

sourceColor <- c(visual="#f7fcb9", human="#addd8e", robot="#31a354")
pp <- ggplot(DF_HR, aes(Label, Cover))
pp + geom_boxplot(position=position_dodge(1), aes(fill=source), outlier.size = 0.3) +
  labs(x="", y="Cover %") + 
  scale_colour_manual("Source", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~.) + 
  theme_pubclean(base_size=14)


## if log transformed, you can see better the labels with low cover values
pp <- ggplot(DF_HR, aes(Label, log10(Cover+1)))
pp + geom_boxplot(position=position_dodge(1), aes(fill=source), outlier.size = 0.3) +
  labs(x="", y="Cover % (Log transformed)") + 
  scale_colour_manual("Source", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~.) + 
  theme_pubclean(base_size=14)


## Idem but without transforming the variable but using log scale Y-axis
pp <- ggplot(DF_HR, aes(Label, Cover))
pp + geom_boxplot(position=position_dodge(1), aes(fill=source), outlier.size = 0.3) +
  labs(x="", y="Cover %") + 
  scale_y_log10()  +
  scale_colour_manual("Source", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~.) + 
  theme_pubclean(base_size=14)


## Let's try plotting the DIFFERENCE, so we can see all the labels around zero
DF_HRdiff <- DF_HR %>% group_by(Name, country, site, strata, Label) %>% 
  summarise(CoverDiff = Cover[source=="human"] - Cover[source=="robot"])

pp <- ggplot(DF_HRdiff, aes(Label, CoverDiff))
pp + geom_boxplot(position=position_dodge(1),outlier.size = 0.3) +
  labs(x="", y="Cover %") + 
  #scale_colour_manual("Source", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~.) + 
  theme_pubclean(base_size=14)


## Bar plot with confidence intervals
DF_HRmean <- DF_HR %>% group_by(country, Label, source) %>% 
  summarise(Cover.mean = mean(Cover, na.rm=T),
            Cover.sd = sd(Cover, na.rm=T),
            Cover.n = n(),
            Cover.CIlower = Cover.mean - 1.96*Cover.sd/sqrt(Cover.n), 
            Cover.CIupper = Cover.mean + 1.96*Cover.sd/sqrt(Cover.n))

pp <- ggplot(DF_HRmean, aes(Label, Cover.mean, fill=source))
pp + geom_bar(stat="identity", position=position_dodge(1), aes(fill=source)) +
  geom_errorbar(aes(ymin=Cover.CIlower, ymax=Cover.CIupper), width = 0.2, position=position_dodge(1)) + 
  labs(x="", y="Cover %") + 
  scale_colour_manual("Source", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~.) + 
  theme_pubclean(base_size=14)



## THIS IS A REPLICA OF FIGURE 2
## Cover ~strata faceted by country and label. mean and sd calculated using log of Cover+1
DF_HRmean <- DF_HR %>% filter(Label %in% selectedLabels ) %>% 
  group_by(country, strata, Label, source) %>% 
  summarise(Cover.mean = mean(log10(Cover+1), na.rm=T),
            Cover.sd = sd(log10(Cover+1), na.rm=T),
            Cover.n = n(),
            Cover.CIlower = Cover.mean - 1.96*Cover.sd/sqrt(Cover.n), 
            Cover.CIupper = Cover.mean + 1.96*Cover.sd/sqrt(Cover.n))

pp <- ggplot(DF_HRmean, aes(strata, Cover.mean, fill=source))
pp + geom_bar(stat="identity", position=position_dodge(1), aes(fill=source)) +
  geom_errorbar(aes(ymin=Cover.CIlower, ymax=Cover.CIupper), width = 0.2, position=position_dodge(1)) + 
  labs(x="Stratum", y="Log Cover %") + 
  scale_x_discrete(breaks=c("HT", "MT", "LT"), labels=c("H", "M", "L")) + 
  scale_colour_manual("Source", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~Label) + 
  theme_pubclean(base_size=14)







###################
## Visual vs Robot
##################

DF_VR <- read_csv("./DataClean/DF_VisualRobot.csv", col_types = cols())
DF_VR$strata <- factor(DF_VR$strata, levels = c("LT", "MT", "HT"))

## remove US from Visual
pp <- ggplot(DF_VR %>% filter(country != "US", Label %in% selectedLabels), aes(strata, Cover))
pp + geom_boxplot(position=position_dodge(1), aes(fill=source), outlier.size = 0.3) +
  labs(x="", y="Cover %") + 
  scale_y_log10() + 
  scale_colour_manual("Source", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~Label) + 
  theme_pubclean(base_size=14)


## With Log transformation

pp <- ggplot(DF_VR %>% filter(country != "US"), aes(Label, log10(Cover+1)))
pp + geom_boxplot(position=position_dodge(1), aes(fill=source), outlier.size = 0.3) +
  labs(x="", y="Cover %") + 
  scale_colour_manual("Source", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~.) + 
  theme_pubclean(base_size=14)

## Plot the DIFFERENCE
DF_VRdiff <- DF_VR %>% group_by(Name, country, site, strata, Label) %>% 
  summarise(CoverDiff = Cover[source=="visual"] - Cover[source=="robot"])

pp <- ggplot(DF_VRdiff, aes(Label, CoverDiff))
pp + geom_boxplot(position=position_dodge(1),outlier.size = 0.3) +
  labs(x="", y="Cover %") + 
  #scale_colour_manual("Source", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~.) + 
  theme_pubclean(base_size=14)


## THIS IS A REPLICA OF FIGURE 2
## Cover ~strata faceted by country and label. mean and sd calculated using log of Cover+1
DF_VRmean <- DF_VR %>% filter(Label %in% selectedLabels , country !="US") %>% 
  group_by(country, strata, Label, source) %>% 
  summarise(Cover.mean = mean(log10(Cover+1), na.rm=T),
            Cover.sd = sd(log10(Cover+1), na.rm=T),
            Cover.n = n(),
            Cover.CIlower = Cover.mean - 1.96*Cover.sd/sqrt(Cover.n), 
            Cover.CIupper = Cover.mean + 1.96*Cover.sd/sqrt(Cover.n))

pp <- ggplot(DF_VRmean, aes(strata, Cover.mean, fill=source))
pp + geom_bar(stat="identity", position=position_dodge(1), aes(fill=source)) +
  geom_errorbar(aes(ymin=Cover.CIlower, ymax=Cover.CIupper), width = 0.2, position=position_dodge(1)) + 
  labs(x="Stratum", y="Log Cover %") + 
  scale_x_discrete(breaks=c("HT", "MT", "LT"), labels=c("H", "M", "L")) + 
  scale_colour_manual("Source", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~Label) + 
  theme_pubclean(base_size=14)


