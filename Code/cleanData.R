### Clean data from Visual, Robot and Human
### the data is in WIDE format, with labels at the end
### Save in a DataClean directory

library(stringr)
library(tidyr)
library(readr)
library(dplyr)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)


## data sources
visualDir <- "Visual_quadrats"
humanDir <- "Source_MBON_AR_CO_EC_US_Human/New"
robotDir <- "Source_MBON_AR_CO_EC_US_Robot/New"
outDir <- "DataClean"


##########################
## ROBOT

dfRobot <- read_csv(file.path(robotDir, "percent_covers.csv"), col_types = cols())
dfRobot <- dfRobot[,-1] ## remove the first column in the new format
colnames(dfRobot)[1] <- "Name"
dfRobot <- dfRobot %>% filter(`Annotation status`=='Unconfirmed') %>% 
  dplyr::select(-matches("^Annotation"), -Points) %>% 
  mutate(source='robot') 

## parse Name to extract country, site, and strata
dfRobot$country <- str_sub(dfRobot$Name,1,2)
dfRobot$site <- ifelse(dfRobot$country=='US', str_sub(dfRobot$Name,3,6),str_split(dfRobot$Name, "_", simplify = T)[,3] )
dfRobot$strata <- ifelse(dfRobot$country=='US', str_split(dfRobot$Name, "_", simplify = T)[,3],
                         str_split(dfRobot$Name, "_", simplify = T)[,4])

## recode strata for US. high is HT and low is MT. REorder to LT, MT, HT
dfRobot$strata <- recode_factor(dfRobot$strata, low="MT", high="HT")
dfRobot$strata <- factor(dfRobot$strata, levels = c("LT", "MT", "HT"))

## clean US name
dfRobot$Name <- gsub("_robot", "", dfRobot$Name)
dfRobot$Name <- gsub(".jpg", "", dfRobot$Name)
dfRobot$Name <- gsub(".JPG", "", dfRobot$Name)   ## US did it again!


## re arrange the fields
dfRobot <- dfRobot %>% relocate(Name, source, country, site, strata, everything())

## replace NA by zero
dfRobot[is.na(dfRobot)] <- 0

## Save clean data
write_csv(dfRobot, file.path(outDir, "CM_robot.csv"))

############################




###########################
## HUMAN

dfHuman <- read_csv(file.path(humanDir, "percent_covers.csv"), col_types = cols())
dfHuman <- dfHuman[,-1] ## remove the first column in the new format
colnames(dfHuman)[1] <- "Name"

dfHuman <- dfHuman %>% filter(`Annotation status`=='Confirmed') %>% 
  dplyr::select(-matches("^Annotation"), -Points) %>% 
  mutate(source='human') 

## parse Name to extract country, site, and strata
dfHuman$country <- str_sub(dfHuman$Name,1,2)
dfHuman$site <- ifelse(dfHuman$country=='US', str_sub(dfHuman$Name,3,6),str_split(dfHuman$Name, "_", simplify = T)[,3] )
dfHuman$strata <- ifelse(dfHuman$country=='US', str_split(dfHuman$Name, "_", simplify = T)[,3],
                         str_split(dfHuman$Name, "_", simplify = T)[,4])
## recode strata for US. high is HT and low is MT
dfHuman$strata <- recode_factor(dfHuman$strata, low="MT", high="HT")
dfHuman$strata <- factor(dfHuman$strata, levels = c("LT", "MT", "HT"))

## clean US name
dfHuman$Name <- gsub("_human", "", dfHuman$Name)
dfHuman$Name <- gsub(".jpg", "", dfHuman$Name)

## re arrange the fields
dfHuman <- dfHuman %>% relocate(Name, source, country, site, strata, everything())

## replace NA by zero
dfHuman[is.na(dfHuman)] <- 0

## Save clean data
write_csv(dfHuman, file.path(outDir, "CM_human.csv"))

############################


############################
## VISUAL
dfVisual <- bind_rows(read_csv(file.path(visualDir, "argentina_visual.csv"), col_types = cols()),
                      read_csv(file.path(visualDir, "colombia_visual.csv"), col_types = cols()),
                      read_csv(file.path(visualDir, "galapagos_visual.csv"), col_types = cols()))
## remove un-necessary variables 
dfVisual <- dfVisual %>% dplyr::select(!(Date:`Annotation area`))
dfVisual$Name <- gsub(".jpg", "", dfVisual$Name)

## add source
dfVisual$source <- 'visual'

## parse Name to extract country, site, and strata
dfVisual$country <- str_sub(dfVisual$Name,1,2)
dfVisual$site <- ifelse(dfVisual$country=='US', str_sub(dfVisual$Name,3,6),str_split(dfVisual$Name, "_", simplify = T)[,3] )
dfVisual$strata <- ifelse(dfVisual$country=='US', str_split(dfVisual$Name, "_", simplify = T)[,3],
                         str_split(dfVisual$Name, "_", simplify = T)[,4])
## recode strata for US. high is HT and low i_s MT
dfVisual$strata <- recode_factor(dfVisual$strata, low="MT", high="HT")
dfVisual$strata <- factor(dfVisual$strata, levels = c("LT", "MT", "HT"))

## re arrange the fields
dfVisual <- dfVisual %>% relocate(Name, source, country, site, strata, everything())

## replace NA by zero
dfVisual[is.na(dfVisual)] <- 0

## Save clean data
write_csv(dfVisual, file.path(outDir, "CM_visual.csv"))

############################



############################
## Create matching ComMat with common labels only
## the first 5 cols are id cols

## pick labels and common labels
labelsRobot <- colnames(dfRobot)[6:ncol(dfRobot)]
labelsHuman <- colnames(dfHuman)[6:ncol(dfHuman)]
labelsVisual <- colnames(dfHuman)[6:ncol(dfVisual)]

labelsHumRob <- intersect(labelsHuman, labelsRobot)
labelsVisRob <- intersect(labelsVisual, labelsRobot)

## bind df by rows and select only common labels
dfHumRob <- bind_rows(dfHuman, dfRobot)
dfHumRob <- dfHumRob %>% dplyr::select(Name:strata, contains(labelsHumRob))
dfHumRob[is.na(dfHumRob)] <- 0

dfVisRob <- bind_rows(dfVisual, dfRobot)
dfVisRob <- dfVisRob %>% dplyr::select(Name:strata, contains(labelsVisRob))
dfVisRob[is.na(dfVisRob)] <- 0

## Save data frames
write_csv(dfHumRob, file.path(outDir, "CM_HumanRobot.csv"))
write_csv(dfVisRob, file.path(outDir, "CM_VisualRobot.csv"))

##############################



##############################
## Long format data frame of bind community matrices
## labels are under "Label" and covers under "Cover"

df_HumanRobot <- dfHumRob %>%  pivot_longer(cols = 6:ncol(dfHumRob), names_to = 'Label', values_to = 'Cover')
df_VisualRobot <- dfVisRob %>%  pivot_longer(cols = 6:ncol(dfVisRob), names_to = 'Label', values_to = 'Cover')

write_csv(df_HumanRobot, file.path(outDir, "DF_HumanRobot.csv"))
write_csv(df_VisualRobot, file.path(outDir, "DF_VisualRobot.csv"))



#############################
## Functional groups

CM_FG_Robot <- dfRobot %>% group_by(Name) %>% 
  summarise(ALGAE = sum(MAF,MAEN,MAA,MAS,MALCB,MAEC, na.rm=T),
            SUBSTRATE = SC,
            INVERTEBRATES = sum(CRB,MOB,WPOT,MOG, na.rm=T))

CM_FG_Visual <- dfVisual %>% group_by(Name) %>% 
  summarise(ALGAE = sum(MAF,MAEN,MAA,MAG,MAS,MALCB, na.rm=T),
            SUBSTRATE = SC,
            INVERTEBRATES = sum(CRB,MOB,CNTR,WPOT,MOG,CNCA, na.rm = T))

CM_FG_Human <- dfHuman %>% group_by(Name) %>% 
  summarise(ALGAE = sum(MAF,MAEN,MAA,MAS,MALCB,MAEC, na.rm=T),
            SUBSTRATE = SC,
            INVERTEBRATES = sum(CRB,MOB,WPOT,MOG, na.rm=T))

write_csv(CM_FG_Robot, file.path(outDir, "CM_FG_Robot.csv"))
write_csv(CM_FG_Human, file.path(outDir, "CM_FG_Human.csv"))
write_csv(CM_FG_Visual, file.path(outDir, "CM_FG_Visual.csv"))

####################
## Functional groups
## Long formats
## Robot
## parse Name to extract country, site, and strata
CM_FG_Robot$country <- str_sub(CM_FG_Robot$Name,1,2)
CM_FG_Robot$site <- ifelse(CM_FG_Robot$country=='US', str_sub(CM_FG_Robot$Name,3,6),str_split(CM_FG_Robot$Name, "_", simplify = T)[,3] )
CM_FG_Robot$strata <- ifelse(CM_FG_Robot$country=='US', str_split(CM_FG_Robot$Name, "_", simplify = T)[,3],
                         str_split(CM_FG_Robot$Name, "_", simplify = T)[,4])
CM_FG_Robot$source <- "robot"
## recode strata for US. high is HT and low is MT. REorder to LT, MT, HT
CM_FG_Robot$strata <- recode_factor(CM_FG_Robot$strata, low="MT", high="HT")
CM_FG_Robot$strata <- factor(CM_FG_Robot$strata, levels = c("LT", "MT", "HT"))
## clean US name
CM_FG_Robot$Name <- gsub("_robot", "", CM_FG_Robot$Name)
CM_FG_Robot$Name <- gsub(".jpg", "", CM_FG_Robot$Name)
CM_FG_Robot$Name <- gsub(".JPG", "", CM_FG_Robot$Name)   ## US did it again!
## re arrange the fields
CM_FG_Robot <- CM_FG_Robot %>% relocate(Name, source, country, site, strata, everything())


## HUMAN
CM_FG_Human$country <- str_sub(CM_FG_Human$Name,1,2)
CM_FG_Human$site <- ifelse(CM_FG_Human$country=='US', str_sub(CM_FG_Human$Name,3,6),str_split(CM_FG_Human$Name, "_", simplify = T)[,3] )
CM_FG_Human$strata <- ifelse(CM_FG_Human$country=='US', str_split(CM_FG_Human$Name, "_", simplify = T)[,3],
                             str_split(CM_FG_Human$Name, "_", simplify = T)[,4])
CM_FG_Human$source <- "human"
## recode strata for US. high is HT and low is MT. REorder to LT, MT, HT
CM_FG_Human$strata <- recode_factor(CM_FG_Human$strata, low="MT", high="HT")
CM_FG_Human$strata <- factor(CM_FG_Human$strata, levels = c("LT", "MT", "HT"))
## clean US name
CM_FG_Human$Name <- gsub("_robot", "", CM_FG_Human$Name)
CM_FG_Human$Name <- gsub(".jpg", "", CM_FG_Human$Name)
CM_FG_Human$Name <- gsub(".JPG", "", CM_FG_Human$Name)   ## US did it again!
## re arrange the fields
CM_FG_Human <- CM_FG_Human %>% relocate(Name, source, country, site, strata, everything())


## VISUAL
CM_FG_Visual$country <- str_sub(CM_FG_Visual$Name,1,2)
CM_FG_Visual$site <- ifelse(CM_FG_Visual$country=='US', str_sub(CM_FG_Visual$Name,3,6),str_split(CM_FG_Visual$Name, "_", simplify = T)[,3] )
CM_FG_Visual$strata <- ifelse(CM_FG_Visual$country=='US', str_split(CM_FG_Visual$Name, "_", simplify = T)[,3],
                             str_split(CM_FG_Visual$Name, "_", simplify = T)[,4])
CM_FG_Visual$source <- "visual"
## recode strata for US. high is HT and low is MT. REorder to LT, MT, HT
CM_FG_Visual$strata <- recode_factor(CM_FG_Visual$strata, low="MT", high="HT")
CM_FG_Visual$strata <- factor(CM_FG_Visual$strata, levels = c("LT", "MT", "HT"))
## clean US name
CM_FG_Visual$Name <- gsub("_robot", "", CM_FG_Visual$Name)
CM_FG_Visual$Name <- gsub(".jpg", "", CM_FG_Visual$Name)
CM_FG_Visual$Name <- gsub(".JPG", "", CM_FG_Visual$Name)   ## US did it again!
## re arrange the fields
CM_FG_Visual <- CM_FG_Visual %>% relocate(Name, source, country, site, strata, everything())


## Combine FG tables
## Human-Robot
DF_FG_HumanRobot <- bind_rows(CM_FG_Human, CM_FG_Robot)
DF_FG_HumanRobot <- DF_FG_HumanRobot %>%  pivot_longer(cols = 6:ncol(DF_FG_HumanRobot), names_to = 'Label', values_to = 'Cover')

## Visual-Robot
DF_FG_VisualRobot <- bind_rows(CM_FG_Visual, CM_FG_Robot)
DF_FG_VisualRobot <- DF_FG_VisualRobot %>%  pivot_longer(cols = 6:ncol(DF_FG_VisualRobot), names_to = 'Label', values_to = 'Cover')

write_csv(DF_FG_HumanRobot, file.path(outDir, "DF_FG_HumanRobot.csv"))
write_csv(DF_FG_VisualRobot, file.path(outDir, "DF_FG_VisualRobot.csv"))

