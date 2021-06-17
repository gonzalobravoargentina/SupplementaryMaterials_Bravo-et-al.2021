## read confusion matrix from Gonza
library(caret)
library(stringr)

#confusion matrix new algorithm (EfficientNet extractor)
library(here)
CM_NEW <- read.csv(here("/confusion_matrix/New model","confusion_matrix_full_0.csv"),header=FALSE)

##get the Label. It is tricky as the first label has two parenthesis. The valid label is the last one in parenthesis
tblLabels = str_split( CM_NEW[,1], "\\(")
tblLabels = gsub("\\)", "", unlist(lapply(tblLabels, tail,1)))

tt_new = as.table(as.matrix(CM_NEW[,-1]))
rownames(tt_new) = tblLabels
colnames(tt_new) = tblLabels

confusionMatrix(tt_new)


#confusion matrix oldalgorithm (legacy VGG16 extractor)

library(here)
CM_OLD <- read.csv(here("/confusion_matrix/Old Model","confusion_matrix_full_0.csv"),header=FALSE)

##get the Label. It is tricky as the first label has two parenthesis. The valid label is the last one in parenthesis
tblLabels = str_split( CM_OLD [,1], "\\(")
tblLabels = gsub("\\)", "", unlist(lapply(tblLabels, tail,1)))

tt_old = as.table(as.matrix(CM_OLD [,-1]))
rownames(tt_old) = tblLabels
colnames(tt_old) = tblLabels

confusionMatrix(tt_old)
