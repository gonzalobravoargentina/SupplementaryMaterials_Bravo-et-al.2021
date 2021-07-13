library(RColorBrewer)
palette(brewer.pal(8, "Set2"))


## load packages
library(readr)
library(ggplot2)
library(ggpubr)
library(kableExtra)
library(patchwork)

library(caret)
library(vegan)

library(stringr)
library(tidyr)
library(dplyr)
library(forcats)

library(lme4)
library(multcomp)

library(formattable)

## data sources
robotDir <- "./Source_MBON_AR_CO_EC_US_Robot/New"
humanDir <- "./Source_MBON_AR_CO_EC_US_Human/New"
confmatDir <- "./confusion_matrix"
cleandataDir <- "./DataClean"
labelset <- read.csv("./Labelset/labelset_used.csv")

df <- read.csv(file.path(robotDir, "annotations.csv"))
dfHuman <- df %>% filter(Comments=='random')
dfRobot <- df %>% filter(Comments=='robot')
dfSummary <- dfHuman %>% group_by(Label) %>% 
  summarise(nPoints = n(), Countries = paste0(unique(country), collapse = ", ")) %>% 
  mutate(Percent = round(100*nPoints/nrow(df),1)) %>% 
  arrange(-nPoints)
dfSummary <- left_join(dfSummary, labelset[,c('Name', 'Short.Code')], by=c('Label'='Short.Code')) %>% 
relocate(Name, Label, nPoints, Percent, Countries)

  
dfFuncGroup <- left_join(df, labelset[,c('Functional.Group', 'Short.Code')], by=c('Label'='Short.Code') )
dfSummary <- dfFuncGroup %>% group_by(Functional.Group) %>% 
  summarise(nPoints = n(), Countries = paste0(unique(country), collapse = ", ")) %>% 
  mutate(Percent = round(100*nPoints/nrow(df),1)) %>% 
  relocate(Functional.Group, nPoints, Percent, Countries) %>% 
  arrange(-nPoints)

## get fotoquadrat human community data
CM_HumanRobot <- read.csv(file.path(cleandataDir, "DF_HumanRobot.csv"))
humanLabels <- CM_HumanRobot %>% filter(source=="human") %>% 
  group_by(Label) %>% 
  summarise(n=sum(Cover)) %>% 
  arrange(-n) %>% 
  mutate(acum=cumsum(n), acumP = acum/sum(n))
humanLabels95 <- humanLabels$Label[humanLabels$acumP<=0.96]                                                  

## make community matrix from robot
robotLabels <- CM_HumanRobot %>% filter(source=="robot") %>% 
  group_by(Label) %>% 
  summarise(n=sum(Cover)) %>% 
  arrange(-n) %>% 
  mutate(acum=cumsum(n), acumP = acum/sum(n))
robotLabels95 <- robotLabels$Label[robotLabels$acumP<=0.96]                                                  

## intersect the set of both labels and use only 95% labels
labels95 <- intersect(humanLabels95, robotLabels95)
CM_HumanRobot95 <- CM_HumanRobot %>% filter(Label %in% labels95)

## make it wide
CM_HumanRobot95_wide <- CM_HumanRobot95 %>% pivot_wider(id_cols = Name:strata, names_from = 'Label', values_from = 'Cover')
CM_HumanRobot95_wide[is.na(CM_HumanRobot95_wide)] <- 0
CM_Human95 <- CM_HumanRobot95_wide %>% filter(source=="human")
CM_Robot95 <- CM_HumanRobot95_wide %>% filter(source=="robot")


## calculate BC index
BC_RH <- data.frame(Name=character(), BC=numeric())
nLabels <- ncol(CM_Human95)
for (i in 1:nrow(CM_Human95)){
  robotIndex <- which(CM_Robot95$Name==CM_Human95$Name[i])
  if (length(robotIndex) > 0 ){
    CM_one <- bind_rows(CM_Human95[i,6:nLabels], CM_Robot95[robotIndex,6:nLabels])
    CM_one[is.na(CM_one)] <- 0
    BC_RH <- bind_rows(BC_RH, 
                    data.frame(Name = CM_Human95$Name[i],
                               BC = as.numeric(vegdist(CM_one, na.rm = T))))
  }else {
    print(paste0("NOT FOUND in Robot: ", CM_Visual$Name[i]))
  }
}

## fix the mess US did with the Human and Robot Name (that sounds deep, very deep!)
BC_RH$Country <- str_sub(BC_RH$Name,1,2)
BC_RH$Site <- ifelse(BC_RH$Country=='US', str_sub(BC_RH$Name,3,6),str_split(BC_RH$Name, "_", simplify = T)[,3] )
BC_RH$Strata <- ifelse(BC_RH$Country=='US', str_split(BC_RH$Name, "_", simplify = T)[,3],
                            str_split(BC_RH$Name, "_", simplify = T)[,4])
BC_RH$Strata <- recode_factor(BC_RH$Strata, low="MT", high="HT")


## recode locations
BC_RH$Site <- recode_factor(BC_RH$Site, PC="Punta Cuevas", PE="Punta Este", PL="Punta Lobos", 
                            LM="La Mancora", LV="La Ventana", PV="Playa Verde", 
                            CD="Charles Darwin Station", RA="Ratonera", TO="Tortuga Bay", 
                            MAMH="MAMH", MAPH="MAPH", MECH="MECH", MEGS="MEGS")
BC_RH$Strata <- recode_factor(BC_RH$Strata, LT="LOWTIDE", MT="MIDTIDE", HT="HIGHTIDE")



strataColor = c(LOWTIDE="#f7fcb9", MIDTIDE="#addd8e", HIGHTIDE="#31a354")


########
## it may be repeated but letś do it again in case we want to do a separate analysis
## get Visual data
CM_Visual <- read_csv(file.path(cleandataDir, "CM_visual.csv"), col_types = cols())
CM_Robot <- read_csv(file.path(cleandataDir, "CM_robot.csv"), col_types = cols())

##match the robot labels to visual labels
labelsVisual <- colnames(CM_Visual)[6:ncol(CM_Visual)]
CM_Robot <- CM_Robot %>% dplyr::select(Name:strata,  contains(labelsVisual))

## replace NA by zero
CM_Visual[is.na(CM_Visual)] <- 0
CM_Robot[is.na(CM_Robot)] <- 0

## calculate BC index
nLabels <- ncol(CM_Visual)

BC_VR <- data.frame(Country=character(), Site=character(), Strata=character(), BC=numeric())
for (i in 1:nrow(CM_Visual)){
  robotIndex <- which(CM_Robot$Name==CM_Visual$Name[i])
  if (length(robotIndex) > 0 ){
    CM_one <- bind_rows(CM_Visual[i,6:nLabels], CM_Robot[robotIndex,6:nLabels])
    CM_one[is.na(CM_one)] <- 0
    BC_VR <- bind_rows(BC_VR, 
                       data.frame(Country = CM_Visual$country[i],
                                  Site = CM_Visual$site[i],
                                  Strata = CM_Visual$strata[i],
                                  BC = as.numeric(vegdist(CM_one, na.rm = T))))
  }else {
    print(paste0("NOT FOUND in Robot: ", CM_Visual$Name[i]))
  }
}
BC_VR$Site <- recode_factor(BC_VR$Site, PC="Punta Cuevas", PE="Punta Este", PL="Punta Lobos", 
                            LM="La Mancora", LV="La Ventana", PV="Playa Verde", 
                            CD="Charles Darwin Station", RA="Ratonera", TO="Tortuga Bay", 
                            MAMH="MAMH", MAPH="MAPH", MECH="MECH", MEGS="MEGS")
BC_VR$Strata <- factor(BC_VR$Strata, levels = c("LT", "MT", "HT"), labels = c("LOWTIDE", "MIDTIDE", "HIGHTIDE"))
BC_VR$Country <- factor(BC_VR$Country, levels = c("AR", "CO", "EC"), labels = c("ARGENTINA", "COLOMBIA", "ECUADOR"))



#### Reduced Label Set
#Let's try to reduce the number of labels to the most representative ones. It makes sense as the infrequent labels have poorly accuracy in the classifier as the number of training points is very low Try with the labels that are present in 99.5% of the points in the Visual file:This is the number of points and accumulated % per label:

DF_VisualRobot <- read_csv(file.path(cleandataDir, "DF_VisualRobot.csv"), col_types = cols())

labelTable <- DF_VisualRobot %>% group_by(Label) %>% 
  summarise(VISUAL = sum(Cover[source=='visual']), ROBOT = sum(Cover[source=='robot'])) %>% 
  arrange(-VISUAL) %>% 
  mutate(VISUALcum = round(100*cumsum(VISUAL)/sum(VISUAL),2), 
         ROBOTcum = round(100*cumsum(ROBOT)/sum(ROBOT),2))

kable(labelTable) %>% kable_styling("striped")
labels995 <- labelTable$Label[labelTable$VISUALcum <= 99.6]
cat(labels995)

## Filter robot labels using selected visual labels
CM_Robot995 <- CM_Robot %>% dplyr::select(Name:strata, contains(labels995))
CM_Visual995 <- CM_Visual %>% dplyr::select(Name:strata, contains(labels995))

## calculate BC index
BC_VR995 <- data.frame(Country=character(), Site=character(), Strata=character(), BC=numeric())
for (i in 1:nrow(CM_Visual995)){
  robotIndex <- which(CM_Robot995$Name==CM_Visual995$Name[i])
  if (length(robotIndex) > 0 ){
    CM_one <- bind_rows(CM_Visual995[i,6:ncol(CM_Visual995)], CM_Robot995[robotIndex,6:ncol(CM_Robot995)])
    CM_one[is.na(CM_one)] <- 0
    BC_VR995 <- bind_rows(BC_VR995, 
                          data.frame(Country = CM_Visual995$country[i],
                                     Site = CM_Visual995$site[i],
                                     Strata = CM_Visual995$strata[i],
                                     BC = as.numeric(vegdist(CM_one, na.rm = T))))
  }else {
    print(paste0("NOT FOUND in Robot: ", CM_Visual$Name[i]))
  }
}
BC_VR995$Site <- recode_factor(BC_VR995$Site, PC="Punta Cuevas", PE="Punta Este", PL="Punta Lobos", 
                               LM="La Mancora", LV="La Ventana", PV="Playa Verde", 
                               CD="Charles Darwin Station", RA="Ratonera", TO="Tortuga Bay", 
                               MAMH="MAMH", MAPH="MAPH", MECH="MECH", MEGS="MEGS")
BC_VR995$Strata <- factor(BC_VR995$Strata, levels = c("LT", "MT", "HT"), labels = c("LOWTIDE", "MIDTIDE", "HIGHTIDE"))
BC_VR995$Country <- factor(BC_VR995$Country, levels = c("AR", "CO", "EC"), labels = c("ARGENTINA", "COLOMBIA", "ECUADOR"))
BC_VR995$Country <- recode_factor(BC_VR995$Country, ARGENTINA="AR", COLOMBIA="CO", ECUADOR="EC")



# Functional groups-------
### Human vs Robot
DF_FG_HumanRobot <- read_csv(file.path(cleandataDir, "DF_FG_HumanRobot.csv"), col_types = cols())

CM_FG_Human <- DF_FG_HumanRobot %>% filter(source=='human') %>% 
  pivot_wider(names_from = 'Label', values_from = 'Cover')

CM_FG_Robot <- DF_FG_HumanRobot %>% filter(source=='robot') %>% 
  pivot_wider(names_from = 'Label', values_from = 'Cover')

## calculate BC index
nLabels <- ncol(CM_FG_Human)

BC_FGHR <- data.frame(Country=character(), Site=character(), Strata=character(), BC=numeric())
for (i in 1:nrow(CM_FG_Human)){
  robotIndex <- which(CM_FG_Robot$Name==CM_FG_Human$Name[i])
  if (length(robotIndex) > 0 ){
    CM_one <- bind_rows(CM_FG_Human[i,6:nLabels], CM_FG_Robot[robotIndex,6:nLabels])
    CM_one[is.na(CM_one)] <- 0
    BC_FGHR <- bind_rows(BC_FGHR, 
                         data.frame(Country = CM_FG_Human$country[i],
                                    Site = CM_FG_Human$site[i],
                                    Strata = CM_FG_Human$strata[i],
                                    BC = as.numeric(vegdist(CM_one, na.rm = T))))
  }else {
    print(paste0("NOT FOUND in Robot: ", CM_FG_Human$Name[i]))
  }
}
BC_FGHR$Site <- recode_factor(BC_FGHR$Site, PC="Punta Cuevas", PE="Punta Este", PL="Punta Lobos", 
                              LM="La Mancora", LV="La Ventana", PV="Playa Verde", 
                              CD="Charles Darwin Station", RA="Ratonera", TO="Tortuga Bay", 
                              MAMH="MAMH", MAPH="MAPH", MECH="MECH", MEGS="MEGS")
BC_FGHR$Strata <- factor(BC_FGHR$Strata, levels = c("LT", "MT", "HT"), labels = c("LOWTIDE", "MIDTIDE", "HIGHTIDE"))
BC_FGHR$Country <- factor(BC_FGHR$Country, levels = c("AR", "CO", "EC", "US"), labels = c("ARGENTINA", "COLOMBIA", "ECUADOR", "USA"))
BC_FGHR$Country <- recode_factor(BC_FGHR$Country, ARGENTINA="AR", COLOMBIA="CO", ECUADOR="EC",USA="US")

### Visual vs Robot

DF_FG_VisualRobot <- read_csv(file.path(cleandataDir, "DF_FG_VisualRobot.csv"), col_types = cols())

CM_FG_Visual <- DF_FG_VisualRobot %>% filter(source=='visual') %>% 
  pivot_wider(names_from = 'Label', values_from = 'Cover')

CM_FG_Robot <- DF_FG_VisualRobot %>% filter(source=='robot') %>% 
  pivot_wider(names_from = 'Label', values_from = 'Cover')

## calculate BC index
nLabels <- ncol(CM_FG_Visual)

BC_FGVR <- data.frame(Country=character(), Site=character(), Strata=character(), BC=numeric())
for (i in 1:nrow(CM_FG_Visual)){
  robotIndex <- which(CM_FG_Robot$Name==CM_FG_Visual$Name[i])
  if (length(robotIndex) > 0 ){
    CM_one <- bind_rows(CM_FG_Visual[i,6:nLabels], CM_FG_Robot[robotIndex,6:nLabels])
    CM_one[is.na(CM_one)] <- 0
    BC_FGVR <- bind_rows(BC_FGVR, 
                         data.frame(Country = CM_FG_Visual$country[i],
                                    Site = CM_FG_Visual$site[i],
                                    Strata = CM_FG_Visual$strata[i],
                                    BC = as.numeric(vegdist(CM_one, na.rm = T))))
  }else {
    print(paste0("NOT FOUND in Robot: ", CM_FG_Visual$Name[i]))
  }
}
BC_FGVR$Site <- recode_factor(BC_FGVR$Site, PC="Punta Cuevas", PE="Punta Este", PL="Punta Lobos", 
                              LM="La Mancora", LV="La Ventana", PV="Playa Verde", 
                              CD="Charles Darwin Station", RA="Ratonera", TO="Tortuga Bay", 
                              MAMH="MAMH", MAPH="MAPH", MECH="MECH", MEGS="MEGS")
BC_FGVR$Strata <- factor(BC_FGVR$Strata, levels = c("LT", "MT", "HT"), labels = c("LOWTIDE", "MIDTIDE", "HIGHTIDE"))
BC_FGVR$Country <- factor(BC_FGVR$Country, levels = c("AR", "CO", "EC"), labels = c("ARGENTINA", "COLOMBIA", "ECUADOR"))
BC_FGVR$Country <- recode_factor(BC_FGVR$Country, ARGENTINA="AR", COLOMBIA="CO", ECUADOR="EC",USA="US")

