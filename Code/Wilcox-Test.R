
#Stats for FG 
#Split the dataframe into list which each combination of country, strata and FG
PQ_VQ_long.FG <- subset(PQ_VQ_long.FG,Comments!="PQ.Robot.bycountry") #extract PQ robot by country
PQ_VQ_FG_stats <- split(PQ_VQ_long.FG, list(PQ_VQ_long.FG$country, PQ_VQ_long.FG$strata,PQ_VQ_long.FG$CATAMI))

#apply wilcox test
pairwise.wilcox <- lapply(PQ_VQ_FG_stats, function (j) (pairwise.wilcox.test(j$cover.rel,j$Comments,p.adjust.method = "BH")$p.value))


#Stats for CATAMI
#Subset more aboundat cat from each country  SC,MAEN, MOB, MAF, MAS, MAA,MAEC,CRB
PQ_VQ_long <- subset(PQ_VQ_long,Comments!="PQ.Robot.bycountry") #extract PQ robot by country
PQ_VQ_CATAMI <- subset(PQ_VQ_long,CATAMI=="SC"|CATAMI=="MAEN"|CATAMI=="MOB"|CATAMI=="MAF"|CATAMI=="MAS"|CATAMI=="MAA"|CATAMI=="MAEC"|CATAMI=="CRB")
PQ_VQ_CATAMI_stats <- split(PQ_VQ_CATAMI, list(PQ_VQ_CATAMI$country, PQ_VQ_CATAMI$strata,PQ_VQ_CATAMI$CATAMI))

#apply wilcox test
pairwise.wilcox.CATAMI <- lapply(PQ_VQ_CATAMI_stats, function (j) (pairwise.wilcox.test(j$cover.rel,j$Comments,p.adjust.method = "BH")$p.value))
