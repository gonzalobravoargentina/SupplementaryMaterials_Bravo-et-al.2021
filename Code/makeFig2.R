## Make Figure 2

library(readr)
library(ggplot2)
library(dplyr)

## Human Robot

DF_HR <- read_csv("./DataClean/DF_HumanRobot.csv", col_types = cols())

sourceColor <- c(visual="#f7fcb9", human="#addd8e", robot="#31a354")
pp <- ggplot(DF_HR, aes(Label, Cover))
pp + geom_boxplot(position=position_dodge(1), aes(fill=source), outlier.size = 0.3) +
  labs(x="", y="Cover %") + 
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


DF_VR <- read_csv("./DataClean/DF_VisualRobot.csv", col_types = cols())

## remove US from Visual
pp <- ggplot(DF_VR %>% filter(country != "US"), aes(Label, Cover))
pp + geom_boxplot(position=position_dodge(1), aes(fill=source), outlier.size = 0.3) +
  labs(x="", y="Cover %") + 
  scale_colour_manual("Source", values = sourceColor, aesthetics = 'fill') + 
  facet_grid(country~.) + 
  theme_pubclean(base_size=14)


