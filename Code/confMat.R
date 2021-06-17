## read confusion matrix from Coralnet output and calculate some stats using caret package
## EK 20210617


library(caret)
library(stringr)

dataDir = "./confusion_matrix/New model/"
fileName = "confusion_matrix_full_0.csv"


df = read.csv(file.path(dataDir, fileName), header=FALSE)

##get the Label. It is tricky as the first label has two parenthesis. The valid label is the last one in parenthesis
tblLabels = str_split( df[,1], "\\(")
tblLabels = gsub("\\)", "", unlist(lapply(tblLabels, tail,1)))

nLabels = nrow(df)
## modify here to select the first n lables
nLabels = 7

tt = as.table(as.matrix(df[1:nLabels,2:(nLabels+1)]))
rownames(tt) = tblLabels[1:nLabels]
colnames(tt) = tblLabels[1:nLabels]

confusionMatrix(tt)

