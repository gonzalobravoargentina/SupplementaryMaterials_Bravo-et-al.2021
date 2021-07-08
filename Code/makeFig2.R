## Make Figure 2

library(readr)
library(ggplot2)
library(ggpubr)
library(dplyr)


selectedLabels <- c("CRB", "MOB", "SC", "MAF", "MAEN", "MAA", "MAS", "MAEC")  
sourceColor <- c(visual="#f7fcb9", human="#addd8e", robot="#31a354")


###################
## Human vs Robot
##################
DF_HR <- read_csv("./DataClean/DF_HumanRobot.csv", col_types = cols())
DF_HR$strata <- factor(DF_HR$strata, levels = c("LT", "MT", "HT"))
DF_HR$source <- factor(DF_HR$source, levels = c("human", "visual", "robot"))
DF_HR$country <- factor(DF_HR$country, levels = c("AR", "CO", "EC", "US"), 
                        labels = c("ARGENTINA", "COLOMBIA", "ECUADOR", "USA"))

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



## THIS IS A REPLICA OF FIGURE 2, but with BOX PLOTS
## Cover ~strata faceted by country and label. mean and sd calculated using log of Cover+1
pp <- ggplot(DF_HR %>% filter(Label %in% selectedLabels), aes(strata, Cover, fill=source))
pp <- pp + geom_boxplot(position=position_dodge2(preserve = "single"), aes(fill=source), outlier.size = 0.3, notch = FALSE) +
  labs(x="Stratum", y="Cover %") + 
  scale_y_log10() + 
  scale_x_discrete(breaks=c("HT", "MT", "LT"), labels=c("H", "M", "L")) + 
  scale_colour_manual("Source", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~Label) + 
  theme_pubclean(base_size=14)

pp

pp %>% ggexport(filename = "./Figures/Fig_2A_new.png", width = 3000, height = 1615, res = 300, pointsize = 12)







###################
## Visual vs Robot
##################

DF_VR <- read_csv("./DataClean/DF_VisualRobot.csv", col_types = cols())
DF_VR$strata <- factor(DF_VR$strata, levels = c("LT", "MT", "HT"))
DF_VR$source <- factor(DF_VR$source, levels = c("human", "visual", "robot"))
DF_VR$country <- factor(DF_VR$country, levels = c("AR", "CO", "EC", "US"), 
                        labels = c("ARGENTINA", "COLOMBIA", "ECUADOR", "USA"))

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



## THIS IS A REPLICA OF FIGURE 2, but with BOX PLOTS
## Cover ~strata faceted by country and label. mean and sd calculated using log of Cover+1
pp <- ggplot(DF_VR %>% filter(Label %in% selectedLabels, country != "USA"), aes(strata, Cover, fill=source))
pp <- pp + geom_boxplot(position=position_dodge2(preserve="single"), aes(fill=source), outlier.size = 0.3, notch = FALSE, width=0.7) +
  labs(x="Stratum", y="Cover %") + 
  scale_y_log10() + 
  scale_x_discrete(breaks=c("HT", "MT", "LT"), labels=c("H", "M", "L")) + 
  scale_colour_manual("Source", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~Label) + 
  theme_pubclean(base_size=14)
pp

pp %>% ggexport(filename = "./Figures/Fig_2B_new.png", width = 3000, height = 1615, res = 300, pointsize = 12)




###############
## FUNCTIONAL GROUPS
###############

## HUMAN ROBOT
## go directly to the boxplots
FG_HR <- read_csv("./DataClean/DF_FG_HumanRobot.csv", col_types = cols())
FG_HR$strata <- factor(FG_HR$strata, levels = c("LT", "MT", "HT"))
FG_HR$source <- factor(FG_HR$source, levels = c("human", "visual", "robot"))
FG_HR$country <- factor(FG_HR$country, levels = c("AR", "CO", "EC", "US"), 
                        labels = c("ARGENTINA", "COLOMBIA", "ECUADOR", "USA"))

pp <- ggplot(FG_HR, aes(strata, Cover, fill=source))
pp <- pp + geom_boxplot(position=position_dodge2(preserve = "single"), aes(fill=source), outlier.size = 0.3, notch = FALSE) +
  labs(x="Stratum", y="Cover %") + 
  scale_y_log10() + 
  scale_x_discrete(breaks=c("HT", "MT", "LT"), labels=c("H", "M", "L")) + 
  scale_colour_manual("Source", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~Label) + 
  theme_pubclean(base_size=14)

pp

pp %>% ggexport(filename = "./Figures/Fig_3A_new.png", width = 3000, height = 1615, res = 300, pointsize = 12)


## VISUAL ROBOT
## go directly to the boxplots
FG_VR <- read_csv("./DataClean/DF_FG_VisualRobot.csv", col_types = cols())
FG_VR$strata <- factor(FG_VR$strata, levels = c("LT", "MT", "HT"))
FG_VR$source <- factor(FG_VR$source, levels = c("human", "visual", "robot"))
FG_VR$country <- factor(FG_VR$country, levels = c("AR", "CO", "EC", "US"), 
                        labels = c("ARGENTINA", "COLOMBIA", "ECUADOR", "USA"))

pp <- ggplot(FG_VR %>% filter(country != "USA"), aes(strata, Cover, fill=source))
pp <- pp + geom_boxplot(position=position_dodge2(preserve = "single"), aes(fill=source), outlier.size = 0.3, notch = FALSE) +
  labs(x="Stratum", y="Cover %") + 
  scale_y_log10() + 
  scale_x_discrete(breaks=c("HT", "MT", "LT"), labels=c("H", "M", "L")) + 
  scale_colour_manual("Source", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~Label) + 
  theme_pubclean(base_size=14)

pp

pp %>% ggexport(filename = "./Figures/Fig_3B_new.png", width = 3000, height = 1615, res = 300, pointsize = 12)


