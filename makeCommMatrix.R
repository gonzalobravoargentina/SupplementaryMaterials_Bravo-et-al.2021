## generate community matrices for human vs robot using the "Comment" field
## input: data source path. A csv from CoralNet export
## returns a named list with community matrix dataframes and save each CommMat
## EKlein
# Thu Jun 17 10:48:30 2021 ------------------------------

library(tidyr)
library(dplyr)
options(dplyr.summarise.inform = FALSE)


dataDir <-  "Source_MBON_AR_CO_EC_US_Human/"
fileName <-  "annotations.csv"
outFileName <- gsub(".csv", "", fileName)


df = read.csv(file.path(dataDir, fileName))

## clean filenames. Comment if not needed
df$Name <-  gsub("_robot","", df$Name)
df_Name <-  gsub("_Random", "", df$Name)

annotatorsList = unique(df$Comments)

dfList <-  list()   ## empty list to store the resulting df
for (aa in annotatorsList){
  dfAnn <-df[df$Comments==aa,]
  dfAnn <- dfAnn %>% group_by(Name, Label) %>% 
    summarise(n = n())
  dfAnn <- dfAnn %>% pivot_wider(id_cols = 'Name', names_from = 'Label', values_from = 'n')
  ## replace NA by zero. Comment if not needed
  dfAnn[is.na(dfAnn)] <- 0
  ## keep df in a named list to further analysis
  dfList[[aa]] <- dfAnn
  ## write community matrix
  write.csv(x = dfAnn, file = file.path(dataDir, paste0(outFileName, "_CommMat_", aa, ".csv")), quote = FALSE, row.names = FALSE)
}







