## read confusion matrix from Gonza
library(caret)
library(stringr)

library(here)
df <- read.csv("confusion_matrix_MBON_Robot Training (Southamerica)_90%.csv",header=FALSE)

##get the Label. It is tricky as the first label has two parenthesis. The valid label is the last one in parenthesis
tblLabels = str_split( df[,1], "\\(")
tblLabels = gsub("\\)", "", unlist(lapply(tblLabels, tail,1)))

tt = as.table(as.matrix(df[,-1]))
rownames(tt) = tblLabels
colnames(tt) = tblLabels

confusionMatrix(tt)



