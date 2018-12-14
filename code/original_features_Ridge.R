attach(fulldataset_cleaned)
###### set numeric #######
## must retain ##
document_word_count <- as.numeric(document_word_count)
standard_isnarative <- as.numeric(standard_isnarative)
standard_ispersonal <- as.numeric(standard_ispersonal)
`topic~debt~neg~noemo` <- as.numeric(`topic~debt~neg~noemo`)
`topic~forward_looking~neu~noemo`<-as.numeric(`topic~forward_looking~neu~noemo`)
`topic~government_policy_change~neu~noemo` <- as.numeric(`topic~government_policy_change~neu~noemo`)
`topic~taxation_policy_environment~neu~noemo`<-as.numeric(`topic~taxation_policy_environment~neu~noemo`)
`topic~unemployment_rate_trends~neu~noemo`<- as.numeric(`topic~unemployment_rate_trends~neu~noemo`)
`other_classifier~standard_adjectives~neu~noemo`<- as.numeric(`other_classifier~standard_adjectives~neu~noemo`)
`other_classifier~standard_adverbs_of_time~neu~noemo`<- as.numeric(`other_classifier~standard_adverbs_of_time~neu~noemo`)
`other_classifier~standard_adverbs~neu~noemo` <- as.numeric(`other_classifier~standard_adverbs~neu~noemo`)
`other_classifier~standard_comparatives~neu~noemo` <- as.numeric(`other_classifier~standard_comparatives~neu~noemo`)
`other_classifier~standard_period~neu~noemo` <- as.numeric(`other_classifier~standard_period~neu~noemo`)
`other_classifier~standard_question~neu~noemo`<- as.numeric(`other_classifier~standard_question~neu~noemo`)



##################################
#fulldataset_cleaned$`topic~conflict~neg~noemo` <- as.numeric(fulldataset_cleaned$`topic~conflict~neg~noemo`)
fulldataset_cleaned$`topic~debt~neg~noemo` <- as.numeric(fulldataset_cleaned$`topic~debt~neg~noemo`)
#fulldataset_cleaned$`topic~exchange_rate_currency_trend~neu~noemo` <- as.numeric(fulldataset_cleaned$`topic~exchange_rate_currency_trend~neu~noemo`)
fulldataset_cleaned$`topic~forward_looking~neu~noemo` <- as.numeric(fulldataset_cleaned$`topic~forward_looking~neu~noemo`)
fulldataset_cleaned$`topic~government_policy_change~neu~noemo` <- as.numeric(fulldataset_cleaned$`topic~government_policy_change~neu~noemo`)
#fulldataset_cleaned$`topic~natural_disaster~neg~noemo` <- as.numeric(fulldataset_cleaned$`topic~natural_disaster~neg~noemo`)
####################################


##### set dummies ####
library(bindr)
###### set dummies #######
`performance~profit_bool~pos~noemo` = factor(`performance~profit_bool~pos~noemo`)
performance_profit_dummy = model.matrix(~`performance~profit_bool~pos~noemo`)
fulldataset_cleaned = data.frame(fulldataset_cleaned,performance_profit_dummy)

`performance~strong_performance_bool~pos~noemo` = factor(`performance~strong_performance_bool~pos~noemo`)
performance_strong_performance_dummy = model.matrix(~`performance~strong_performance_bool~pos~noemo`)
fulldataset_cleaned = data.frame(fulldataset_cleaned, performance_strong_performance_dummy)

`topic~weak_demand_bool~neg~noemo` = factor(`topic~weak_demand_bool~neg~noemo`)
topic_week_demand_dummy = model.matrix(~`topic~weak_demand_bool~neg~noemo`)
fulldataset_cleaned = data.frame(fulldataset_cleaned, topic_week_demand_dummy)



##### drop intercept column #####
fulldataset_cleaned$X.Intercept. <- NULL
fulldataset_cleaned$X.Intercept..1 <- NULL
fulldataset_cleaned$X.Intercept..2 <- NULL

#### drop original column ####
fulldataset_cleaned$performance.profit_bool.pos.noemo <- NULL
fulldataset_cleaned$performance.strong_performance_bool.pos.noemo <- NULL
fulldataset_cleaned$topic.weak_demand_bool.neg.noemo <- NULL

#### drop original text ####
fulldataset_cleaned$content_cleaned <- NULL

#### move Y to the first column ####
fulldataset_cleaned$Y = as.factor(Y)
library(dplyr)
fulldataset_cleaned <- fulldataset_cleaned%>%select(Y,everything())  


#### split data into train data and test data ####
set.seed(1234)
#### 10% for test dataset ####
#test_data = sample(nrow(fulldataset_cleaned), 0.1*nrow(fulldataset_cleaned))
#### 90% for training dataset ####
#train_data = fulldataset_cleaned[-test_data,]


train.indicies = sample(nrow(fulldataset_cleaned), .7*nrow(fulldataset_cleaned)) 
train.samples = fulldataset_cleaned[train.indicies,]
test.samples = fulldataset_cleaned[-train.indicies,]


#Ridge
library(glmnet)
glmnet_ridge = glmnet(as.matrix(train.samples[,c(2:18)]),train.samples$Y,alpha=0,family='binomial')

coef(glmnet_ridge,best.lambda)
glmnet_ridge.cv=cv.glmnet(as.matrix(train.samples[,c(2:18)]),train.samples$Y,alpha=0,family="binomial")
plot(glmnet_ridge.cv)
best.lambda=glmnet_ridge.cv$lambda.min
best.lambda
ridge.pred = predict(glmnet_ridge,s=best.lambda,newx=as.matrix(test.samples[,c(2:18)]))

ridge_class <- ifelse(ridge.pred>.5,1,0)
ridge_test = table(test.samples$Y, ridge_class)
ridge.accuracy=sum(diag(ridge_test))/sum(ridge_test)
ridge.accuracy 

precision <- ridge_test['1','1'] / ifelse(sum(ridge_test[,'1'])== 0, 1, sum(ridge_test[,'1']))
recall <- ridge_test['1','1'] / ifelse(sum(ridge_test['1',])== 0, 1, sum(ridge_test['1',]))
fmeasure <- 2 * precision * recall / ifelse(precision + recall == 0, 1, precision + recall)

















  