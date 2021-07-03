### Clean data from Visual, Robot and Human
### the data is in WIDE format, with labels at the end
### Save in a DataClean directory


library(tidyr)
library(readr)
library(dplyr)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)


## data sources
visualDir <- "Visual_quadrats"
humanDir <- "Source_MBON_AR_CO_EC_US_Human"
robotDir <- "Source_MBON_AR_CO_EC_US_Robot"
outDir <- "DataClean"


##########################
## ROBOT

dfRobot <- read_csv(file.path(robotDir, "percent_covers_new.csv"), col_types = cols())
dfRobot <- dfRobot %>% filter(`Annotation status`=='Unconfirmed') %>% 
  dplyr::select(-matches("^Annotation")) %>% 
  mutate(source='robot') 

## parse Name to extract country, site, and strata
dfRobot$country <- str_sub(dfRobot$Name,1,2)
dfRobot$site <- ifelse(dfRobot$country=='US', str_sub(dfRobot$Name,3,6),str_split(dfRobot$Name, "_", simplify = T)[,3] )
dfRobot$strata <- ifelse(dfRobot$country=='US', str_split(dfRobot$Name, "_", simplify = T)[,3],
                         str_split(dfRobot$Name, "_", simplify = T)[,4])

## recode strata for US. high is HT and low is MT
dfRobot$strata <- recode_factor(dfRobot$strata, low="MT", high="HT")
## clean US name
dfRobot$Name <- gsub("_robot", "", dfRobot$Name)
dfRobot$Name <- gsub(".jpg", "", dfRobot$Name)

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
dfHuman <- dfHuman %>% filter(`Annotation status`=='Confirmed') %>% 
  dplyr::select(-matches("^Annotation")) %>% 
  mutate(source='human') 
## parse Name to extract country, site, and strata
dfHuman$country <- str_sub(dfHuman$Name,1,2)
dfHuman$site <- ifelse(dfHuman$country=='US', str_sub(dfHuman$Name,3,6),str_split(dfHuman$Name, "_", simplify = T)[,3] )
dfHuman$strata <- ifelse(dfHuman$country=='US', str_split(dfHuman$Name, "_", simplify = T)[,3],
                         str_split(dfHuman$Name, "_", simplify = T)[,4])
## recode strata for US. high is HT and low is MT
dfHuman$strata <- recode_factor(dfHuman$strata, low="MT", high="HT")
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

## re arrange the fields
dfVisual <- dfVisual %>% relocate(Name, source, country, site, strata, everything())

## replace NA by zero
dfVisual[is.na(dfVisual)] <- 0

## Save clean data
write_csv(dfVisual, file.path(outDir, "CM_visual.csv"))

############################
