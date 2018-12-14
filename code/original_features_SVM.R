library(readxl)
fulldataset_cleaned <- read_excel("~/Desktop/Capstone/Dataset 1-6/dataset5.xlsx")
attach(fulldataset_cleaned)

cols = colnames(fulldataset_cleaned)[c(2,4:5,7:12,15:18,20,21,24)]
fulldataset_cleaned[cols] <- lapply(fulldataset_cleaned[cols], as.factor)

cols.n = colnames(fulldataset_cleaned)[c(1,3,6,13,14,19,22,23,25,26)]
fulldataset_cleaned[cols.n] <- lapply(fulldataset_cleaned[cols.n], as.numeric)

#### drop original text ####
#fulldataset_cleaned$content_cleaned <- NULL

#### move Y to the first column ####
fulldataset_cleaned$Y = as.factor(Y)
library(dplyr)
fulldataset_cleaned <- fulldataset_cleaned%>%select(Y,everything())  


#### split data into train data and test data ####
set.seed(1234)

train.indicies = sample(nrow(fulldataset_cleaned), .7*nrow(fulldataset_cleaned)) 
train.samples = fulldataset_cleaned[train.indicies,]
test.samples = fulldataset_cleaned[-train.indicies,]

library(e1071)

# Training the SVM classifier model
svm_model = svm(Y~., data = train.samples, kernel  = "linear", cost = 10)
y_pred = predict(svm_model, newdata = test.samples)
prediction <- as.numeric(y_pred)
svm_test_pred = ifelse(prediction > 1, 1, 0)

cm <- table(svm_test_pred, test.samples$Y)
cm
test.accuracy=sum(diag(cm))/sum(cm)
test.accuracy

precision <- cm['1','1'] / ifelse(sum(cm[,'1'])== 0, 1, sum(cm[,'1']))
recall <- cm['1','1'] / ifelse(sum(cm['1',])== 0, 1, sum(cm['1',]))
fmeasure <- 2 * precision * recall / ifelse(precision + recall == 0, 1, precision + recall)

#ROC
install.packages("pROC")
library("pROC")

my_svm <-roc(test.samples$Y~as.numeric(y_pred))
plot(my_svm)



