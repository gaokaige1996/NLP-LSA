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




```
```{r}
#Logistic
fit <- glm(train.samples$Y~.,data=train.samples,family="binomial")
summary(fit)
log_preds <- predict(fit,newdata=test.samples,type="response")
baseline = sum(ifelse(test.samples$Y==0,1,0))/nrow(test.samples)
baseline #0.6882

my_lr <-roc(test.samples$Y~log_preds)
plot(my_lr)

log_class <- ifelse(log_preds>baseline,1,0)

test = table(test.samples$Y,log_class)
test.accuracy=sum(diag(test))/sum(test)
test.accuracy	 

precision <- test['1','1'] / ifelse(sum(test[,'1'])== 0, 1, sum(test[,'1']))
recall <- test['1','1'] / ifelse(sum(test['1',])== 0, 1, sum(test['1',]))
fmeasure <- 2 * precision * recall / ifelse(precision + recall == 0, 1, precision + recall)



#Stepwise Feature Selection
backward_model_both = step(fit, direction="both")
summary(backward_model_both)
backward_model_pred = predict(backward_model_both, newdata=test.samples,type="response")
stepwise_class <- ifelse(backward_model_pred>baseline,1,0)
stepwise_test = table(test.samples$Y,stepwise_class)
stepwise.test.accuracy=sum(diag(stepwise_test))/sum(stepwise_test)
stepwise.test.accuracy	

my_lr <-roc(test.samples$Y~backward_model_pred)
plot(my_lr)

precision <- stepwise_test['1','1'] / ifelse(sum(stepwise_test[,'1'])== 0, 1, sum(stepwise_test[,'1']))
recall <- stepwise_test['1','1'] / ifelse(sum(stepwise_test['1',])== 0, 1, sum(stepwise_test['1',]))
fmeasure <- 2 * precision * recall / ifelse(precision + recall == 0, 1, precision + recall)