## calculate Bray Curtis between human and Robot
## use Colombia as example
library(vegan)
library(readr)
library(tidyr)
library(dplyr)
options(dplyr.summarise.inform = FALSE)

## read data
COL <- read_csv("Data/annotations27.csv")
COL2 = COL[!is.na(COL$`Machine suggestion 1`),]

COL_h = COL2 %>%  group_by(Name, site, strata, Label) %>%  summarise(n = length(Label))
COL_b = COL2 %>%  group_by(Name, site, strata, Label) %>%  summarise(n = length(`Machine suggestion 1`))

COL_hw = COL_h %>% pivot_wider(names_from = Label, values_from = n)
COL_bw = COL_b %>% pivot_wider(names_from = Label, values_from = n)

COL_BC = data.frame()
for (i in 1: nrow(COL_bw)){
    bc = as.numeric(vegdist(bind_rows(COL_bw[i,4:8], COL_hw[i,4:8]), na.rm=T))
    COL_BC = bind_rows(COL_BC, 
                       data.frame(COL_bw[i,1:3], bc = bc))
    
    
    
}
