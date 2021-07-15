## Make Figures

library(readr)
library(ggplot2)
library(ggpubr)
library(dplyr)


selectedLabels <- c("CRB", "MOB", "SC", "MAF", "MAEN", "MAA", "MAS", "MAEC")  
sourceColor <- c(VQ="#f7fcb9", PQ.human="#addd8e", PQ.robot="#31a354")

## Figure 2 A-----

## HUMAN vs ROBOT
DF_HR <- read_csv("./DataClean/DF_HumanRobot.csv", col_types = cols())
DF_HR$strata <- factor(DF_HR$strata, levels = c("LT", "MT", "HT"))
DF_HR$source <- factor(DF_HR$source, levels = c("human", "visual", "robot"),labels = c("PQ.human", "VQ", "PQ.robot"))
DF_HR$country <- factor(DF_HR$country, levels = c("AR", "CO", "EC", "US"),labels = c("ARGENTINA", "COLOMBIA", "ECUADOR", "USA"))

## Cover ~strata faceted by country and label. mean and sd calculated using log of Cover+1
pp <- ggplot(DF_HR %>% filter(Label %in% selectedLabels), aes(strata, Cover, fill=source))
pp <- pp + geom_boxplot(position=position_dodge2(preserve = "single"), aes(fill=source), outlier.size = 0.3, notch = FALSE) +
  labs(x="Stratum", y="Cover %") + 
  scale_y_log10() + 
  scale_x_discrete(breaks=c("HT", "MT", "LT"), labels=c("H", "M", "L")) + 
  scale_colour_manual("", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~Label) + 
  theme_pubclean(base_size=14)

pp

#save figure 2A png
pp %>% ggexport(filename = "./Figures/Fig_2A_new.png", width = 3000, height = 1800, res = 300, pointsize = 12)


## Figure 2 B--------

## VISUAL vs ROBOT
DF_VR <- read_csv("./DataClean/DF_VisualRobot.csv", col_types = cols())
DF_VR$strata <- factor(DF_VR$strata, levels = c("LT", "MT", "HT"))
DF_VR$source <- factor(DF_VR$source, levels = c("human", "visual", "robot"),labels = c("PQ.human", "VQ", "PQ.robot"))
DF_VR$country <- factor(DF_VR$country, levels = c("AR", "CO", "EC", "US"), 
                        labels = c("ARGENTINA", "COLOMBIA", "ECUADOR", "USA"))
selectedLabels <- c("CRB", "MOB", "SC", "MAF", "MAEN", "MAA", "MAS", "MAEC")
sourceColor <- c(VQ="#f7fcb9", PQ.human="#addd8e", PQ.robot="#31a354")

## Cover ~strata faceted by country and label. mean and sd calculated using log of Cover+1

pp <- ggplot(DF_VR %>% filter(Label %in% selectedLabels, country != "USA"), aes(strata, Cover, fill=source))
pp <- pp + geom_boxplot(position=position_dodge2(preserve="single"), aes(fill=source), outlier.size = 0.3, notch = FALSE, width=0.7) +
  labs(x="Stratum", y="Cover %") + 
  scale_y_log10() + 
   scale_x_discrete(breaks=c("HT", "MT", "LT"), labels=c("H", "M", "L")) + 
  scale_colour_manual("", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~Label) + 
  theme_pubclean(base_size=14)
pp

pp %>% ggexport(filename = "./Figures/Fig_2B_new.png", width = 3000, height = 1800, res = 300, pointsize = 12)


##Figure 3------

###############
## FUNCTIONAL GROUPS
###############

## HUMAN vs ROBOT
## go directly to the boxplots
FG_HR <- read_csv("./DataClean/DF_FG_HumanRobot.csv", col_types = cols())
FG_HR$strata <- factor(FG_HR$strata, levels = c("LT", "MT", "HT"))
FG_HR$source <- factor(FG_HR$source, levels = c("human", "visual", "robot"),labels = c("PQ.human", "VQ", "PQ.robot"))
FG_HR$country <- factor(FG_HR$country, levels = c("AR", "CO", "EC", "US"), 
                        labels = c("ARGENTINA", "COLOMBIA", "ECUADOR", "USA"))

pp <- ggplot(FG_HR, aes(strata, Cover, fill=source))
pp <- pp + geom_boxplot(position=position_dodge2(preserve = "single"), aes(fill=source), outlier.size = 0.3, notch = FALSE) +
  labs(x="Stratum", y="Cover % (log10)") + 
  scale_y_log10() + 
  scale_x_discrete(breaks=c("HT", "MT", "LT"), labels=c("H", "M", "L")) + 
  scale_colour_manual("Source", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~Label) + 
  theme_pubclean(base_size=14)

pp

pp %>% ggexport(filename = "./Figures/Fig_3A_new.png", width = 3000, height = 1800, res = 300, pointsize = 12)


## VISUAL vs ROBOT
## go directly to the boxplots
FG_VR <- read_csv("./DataClean/DF_FG_VisualRobot.csv", col_types = cols())
FG_VR$strata <- factor(FG_VR$strata, levels = c("LT", "MT", "HT"))
FG_VR$source <- factor(FG_VR$source, levels = c("human", "visual", "robot"),labels = c("PQ.human", "VQ", "PQ.robot"))
FG_VR$country <- factor(FG_VR$country, levels = c("AR", "CO", "EC", "US"), 
                        labels = c("ARGENTINA", "COLOMBIA", "ECUADOR", "USA"))

pp <- ggplot(FG_VR %>% filter(country != "USA"), aes(strata, Cover, fill=source))
pp <- pp + geom_boxplot(position=position_dodge2(preserve = "single"), aes(fill=source), outlier.size = 0.3, notch = FALSE) +
  labs(x="Stratum", y="Cover % (log10)") + 
  scale_y_log10() + 
  scale_x_discrete(breaks=c("HT", "MT", "LT"), labels=c("H", "M", "L")) + 
  scale_colour_manual("Source", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~Label) + 
  theme_pubclean(base_size=14)

pp

pp %>% ggexport(filename = "./Figures/Fig_3B_new.png", width = 3000, height = 1800, res = 300, pointsize = 12)


## Figure 4------
source('./Code/Bray-Curtis_calculations.R') 
pp <- ggplot(BC_RH, aes(Country, BC, fill=Strata))
pp <-pp + geom_boxplot(width=0.5, aes(fill=Strata), colour="black", position=position_dodge2(preserve="single") , outlier.size = 0.3, lwd=0.2) + 
  scale_color_manual(values = strataColor, aesthetics = 'fill') + 
  ylim(0, 0.3) + 
  facet_grid(~Strata) + 
  theme_pubclean() + 
  theme(legend.position = 'none')

pp %>% ggexport(filename = "./Figures/Fig_4A_new.png", width = 1800, height = 800, res = 300, pointsize = 12)


pp <- ggplot(BC_VR995, aes(Country, BC, fill=Strata))
pp <-pp + geom_boxplot(width=0.5, aes(fill=Strata), colour="black", position=position_dodge2(preserve="single") , outlier.size = 0.3, lwd=0.2) + 
  scale_color_manual(values = strataColor, aesthetics = 'fill') + 
  ylim(0, 0.3) + 
  facet_grid(~Strata) + 
  theme_pubclean() + 
  theme(legend.position = 'none')

pp %>% ggexport(filename = "./Figures/Fig_4B_new.png", width = 1800, height = 800, res = 300, pointsize = 12)


pp <- ggplot(BC_FGHR, aes(Country, BC, fill=Strata))
pp <-pp + geom_boxplot(width=0.5, aes(fill=Strata), colour="black", position=position_dodge2(preserve="single") , outlier.size = 0.3, lwd=0.2) + 
  scale_color_manual(values = strataColor, aesthetics = 'fill') + 
  ylim(0, 0.3) + 
  facet_grid(~Strata) + 
  theme_pubclean()+
  theme(legend.position = 'none')

pp %>% ggexport(filename = "./Figures/Fig_4C_new.png", width = 1800, height = 800, res = 300, pointsize = 12)

pp <- ggplot(BC_FGVR, aes(Country, BC, fill=Strata))
pp <-pp+ geom_boxplot(width=0.5, aes(fill=Strata), colour="black", position=position_dodge2(preserve="single") , outlier.size = 0.3, lwd=0.2) + 
  scale_color_manual(values = strataColor, aesthetics = 'fill') + 
  ylim(0, 0.3) + 
  facet_grid(~Strata) + 
  theme_pubclean()+
  theme(legend.position = 'none')

pp %>% ggexport(filename = "./Figures/Fig_4D_new.png", width = 1800, height = 800, res = 300, pointsize = 12)
