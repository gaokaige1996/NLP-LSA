attach(dataset1)
library(ROCR)
library(e1071)
library(caret)
library(ggplot2)
library(dplyr)
###### set numeric #######
`document_word_count` <- as.numeric(`document_word_count`)
`standard_isnarative` <- as.numeric(`standard_isnarative`)
`standard_ispersonal` <- as.numeric(`standard_ispersonal`)
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

###### as.numeric (Yellow)########
`performance~underperform~neg~noemo` = as.numeric(`performance~underperform~neg~noemo`)
`performance~weak_performance~neg~noemo` = as.numeric(`performance~weak_performance~neg~noemo`)
`topic~GDP_trend~neu~noemo` = as.numeric(`topic~GDP_trend~neu~noemo`)
`topic~business_cycle~neu~noemo` = as.numeric(`topic~business_cycle~neu~noemo`)
`topic~civil_unrest~neg~noemo` = as.numeric(`topic~civil_unrest~neg~noemo`)
`topic~conflict~neg~noemo` = as.numeric(`topic~conflict~neg~noemo`)
`topic~economic_comparison~neu~noemo` = as.numeric(`topic~economic_comparison~neu~noemo`)
`topic~exchange_rate_currency_trend~neu~noemo` = as.numeric(`topic~exchange_rate_currency_trend~neu~noemo`)
`topic~good_economy~pos~noemo` = as.numeric(`topic~good_economy~pos~noemo`)
`topic~interest_rate_decrease~neu~noemo` = as.numeric(`topic~interest_rate_decrease~neu~noemo`)
`topic~interest_rate_increase~neu~noemo` = as.numeric(`topic~interest_rate_increase~neu~noemo`)
`topic~monetary_policy~neu~noemo` = as.numeric(`topic~monetary_policy~neu~noemo`)
`topic~natural_disaster~neg~noemo` = as.numeric(`topic~natural_disaster~neg~noemo`)
`topic~subsidy_government_incentive~neu~noemo` = as.numeric(`topic~subsidy_government_incentive~neu~noemo`)
`other_classifier~standard_exclam~neu~noemo` = as.numeric(`other_classifier~standard_exclam~neu~noemo`)



###### set dummies #######
primary_emotion_confidence = as.factor(primary_emotion_confidence)
primary_emotion_confidence_dummy = model.matrix(~primary_emotion_confidence)
dataset1 = data.frame(dataset1,primary_emotion_confidence_dummy)

valence_confidence = as.factor(valence_confidence)
valence_confidence_dummy = model.matrix(~valence_confidence)
dataset1 = data.frame(dataset1,valence_confidence_dummy)

`performance~profit_bool~pos~noemo` = factor(`performance~profit_bool~pos~noemo`)
performance_profit_dummy = model.matrix(~`performance~profit_bool~pos~noemo`)
dataset1 = data.frame(dataset1,performance_profit_dummy)

`performance~strong_performance_bool~pos~noemo` = factor(`performance~strong_performance_bool~pos~noemo`)
performance_strong_performance_dummy = model.matrix(~`performance~strong_performance_bool~pos~noemo`)
dataset1 = data.frame(dataset1, performance_strong_performance_dummy)

`topic~weak_demand_bool~neg~noemo` = factor(`topic~weak_demand_bool~neg~noemo`)
topic_week_demand_dummy = model.matrix(~`topic~weak_demand_bool~neg~noemo`)
dataset1 = data.frame(dataset1, topic_week_demand_dummy)

primary_emotion = factor(primary_emotion)
primary_emotion_dummy = model.matrix(~primary_emotion)
dataset1 = data.frame(dataset1, primary_emotion_dummy)

valence_direction = factor(valence_direction)
valence_direction_dummy = model.matrix(~valence_direction)
dataset1 = data.frame(dataset1, valence_direction_dummy)

`performance~strong_sales_bool~pos~noemo` = as.factor(`performance~strong_sales_bool~pos~noemo`)
performance_strong_sales_bool_dummy = model.matrix(~`performance~strong_sales_bool~pos~noemo`)
dataset1 = data.frame(dataset1, performance_strong_sales_bool_dummy)

`performance~weak_performance_bool~neg~noemo` = as.factor(`performance~weak_performance_bool~neg~noemo`)
performance_weak_performance_bool_dummy = model.matrix(~`performance~weak_performance_bool~neg~noemo`)
dataset1 = data.frame(dataset1, performance_weak_performance_bool_dummy)

`performance~weak_sales_bool~neg~noemo` = as.factor(`performance~weak_sales_bool~neg~noemo`)
performance_weak_sales_bool_dummy = model.matrix(~`performance~weak_sales_bool~neg~noemo`)
dataset1 = data.frame(dataset1, performance_weak_sales_bool_dummy)

`topic~lower_prices_bool~neg~noemo` = as.factor(`topic~lower_prices_bool~neg~noemo`)
topic_lower_prices_bool_dummy = model.matrix(~`topic~lower_prices_bool~neg~noemo`)
dataset1 = data.frame(dataset1,topic_lower_prices_bool_dummy)

`topic~strong_demand_bool~pos~noemo` = as.factor(`topic~strong_demand_bool~pos~noemo`)
topic_strong_demand_bool_dummy = model.matrix(~`topic~strong_demand_bool~pos~noemo`)
dataset1 = data.frame(dataset1, topic_strong_demand_bool_dummy)

`topic~strong_strategy_bool~pos~noemo` = as.factor(`topic~strong_strategy_bool~pos~noemo`)
topic_strong_strategy_bool_dummy = model.matrix(~`topic~strong_strategy_bool~pos~noemo`)
dataset1 = data.frame(dataset1, topic_strong_strategy_bool_dummy)

##### drop intercept column #####
dataset1$X.Intercept. <- NULL
dataset1$X.Intercept..1 <- NULL
dataset1$X.Intercept..2 <- NULL
dataset1$X.Intercept..3 <- NULL
dataset1$X.Intercept..4 <- NULL
dataset1$X.Intercept..5 <- NULL
dataset1$X.Intercept..6 <- NULL
dataset1$X.Intercept..7 <- NULL
dataset1$X.Intercept..8 <- NULL
dataset1$X.Intercept..9 <- NULL
dataset1$X.Intercept..10 <- NULL
dataset1$X.Intercept..11 <- NULL
dataset1$X.Intercept..12 <- NULL



#### drop original column ####
dataset1$performance.profit_bool.pos.noemo <- NULL
dataset1$performance.strong_performance_bool.pos.noemo <- NULL
dataset1$topic.weak_demand_bool.neg.noemo <- NULL
dataset1$primary_emotion <- NULL
dataset1$valence_direction <- NULL
dataset1$performance.strong_sales_bool.pos.noemo <- NULL
dataset1$performance.weak_performance_bool.neg.noemo <- NULL
dataset1$performance.weak_sales_bool.neg.noemo <- NULL
dataset1$topic.lower_prices_bool.neg.noemo <- NULL
dataset1$topic.strong_demand_bool.pos.noemo <- NULL
dataset1$topic.strong_strategy_bool.pos.noemo <- NULL
dataset1$primary_emotion_confidence <- NULL
dataset1$valence_confidence <- NULL

#### drop original text ####
dataset1$content_cleaned <- NULL

#### move Y to the first column ####
dataset1$Y = as.factor(Y)
library(dplyr)
dataset1<- dataset1%>%select(Y,everything())  


#### split data into train data and test data ####
set.seed(1234)
#### 70% for training dataset ####
train <- sample(nrow(dataset1), 0.7*nrow(dataset1),replace=FALSE)
train_set <- dataset1[train,]
test_set <- dataset1[-train,]

### Naive Bayes ###
#install.packages("e1071")
library(e1071)
#Fitting the Naive Bayes model
NB=naiveBayes(Y~., data=train_set)
#What does the model say? Print the model summary
NB
#Prediction on the dataset
NB.pred=predict(NB,test_set)
#Confusion matrix
test = table(NB.pred,test_set$Y)
print(confusionMatrix(data = NB.pred, reference = test_set$Y))
#### Roc Curve ####
NB.pred = prediction(as.numeric(NB.pred), as.numeric(test_set$Y))
NB.perf = performance(NB.pred,"tpr","fpr")
plot(NB.perf,main="ROC Curve for Naive Bayes",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
#compute area under curve
auc <- performance(NB.pred,"auc")
auc <- unlist(slot(auc, "y.values"))
minauc<-min(round(auc, digits = 2))
maxauc<-max(round(auc, digits = 2))
minauct <- paste(c("min(AUC) = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")
