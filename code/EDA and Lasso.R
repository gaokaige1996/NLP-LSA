#load data
#fulldataset_cleaned_withouttext=read.csv("C:/Users/lindsay/Desktop/758W/modelling/data/fulldataset_cleaned_withouttext.csv")

fulldata=fulldataset_cleaned_withouttext[,1:43]
any(is.na(fulldata))

#categorical numerial deal 18
fulldata$document_word_count=as.numeric(fulldata$document_word_count)
fulldata$standard_isnarative=as.numeric(fulldata$standard_isnarative)
fulldata$standard_ispersonal=as.numeric(fulldata$standard_ispersonal)

fulldata$`topic~conflict~neg~noemo`=as.numeric(fulldata$`topic~conflict~neg~noemo`)
fulldata$`topic~debt~neg~noemo`=as.numeric(fulldata$`topic~debt~neg~noemo`)
fulldata$`topic~exchange_rate_currency_trend~neu~noemo`=as.numeric(fulldata$`topic~exchange_rate_currency_trend~neu~noemo`)

fulldata$`topic~forward_looking~neu~noemo`=as.numeric(fulldata$`topic~forward_looking~neu~noemo`)
fulldata$`topic~government_policy_change~neu~noemo`=as.numeric(fulldata$`topic~government_policy_change~neu~noemo`)
fulldata$`topic~natural_disaster~neg~noemo`=as.numeric(fulldata$`topic~natural_disaster~neg~noemo`)
fulldata$`topic~taxation_policy_environment~neu~noemo`=as.numeric(fulldata$`topic~taxation_policy_environment~neu~noemo`)
fulldata$`topic~unemployment_rate_trends~neu~noemo`=as.numeric(fulldata$`topic~unemployment_rate_trends~neu~noemo`)

fulldata$`other_classifier~standard_adjectives~neu~noemo`=as.numeric(fulldata$`other_classifier~standard_adjectives~neu~noemo`)
fulldata$`other_classifier~standard_adverbs~neu~noemo`=as.numeric(fulldata$`other_classifier~standard_adverbs~neu~noemo`)
fulldata$`other_classifier~standard_adverbs_of_time~neu~noemo`=as.numeric(fulldata$`other_classifier~standard_adverbs_of_time~neu~noemo`)
fulldata$`other_classifier~standard_comparatives~neu~noemo`=as.numeric(fulldata$`other_classifier~standard_comparatives~neu~noemo`)
fulldata$`other_classifier~standard_exclam~neu~noemo`=as.numeric(fulldata$`other_classifier~standard_exclam~neu~noemo`)
fulldata$`other_classifier~standard_period~neu~noemo`=as.numeric(fulldata$`other_classifier~standard_period~neu~noemo`)
fulldata$`other_classifier~standard_question~neu~noemo`=as.numeric(fulldata$`other_classifier~standard_question~neu~noemo`)

# as factor 25
fulldata$primary_emotion=as.factor(fulldata$primary_emotion)
fulldata$primary_emotion_confidence=as.factor(fulldata$primary_emotion_confidence)
fulldata$valence_direction=as.factor(fulldata$valence_direction)
fulldata$valence_confidence=as.factor(fulldata$valence_confidence)
fulldata$`performance~profit_bool~pos~noemo`=as.factor(fulldata$`performance~profit_bool~pos~noemo`)
fulldata$`performance~strong_performance_bool~pos~noemo`=as.factor(fulldata$`performance~strong_performance_bool~pos~noemo`)
fulldata$`performance~strong_sales_bool~pos~noemo`=as.factor(fulldata$`performance~strong_sales_bool~pos~noemo`)
fulldata$`performance~underperform~neg~noemo`=as.factor(fulldata$`performance~underperform~neg~noemo`)
fulldata$`performance~weak_performance~neg~noemo`=as.factor(fulldata$`performance~weak_performance~neg~noemo`)
fulldata$`performance~weak_performance_bool~neg~noemo`=as.factor(fulldata$`performance~weak_performance_bool~neg~noemo`)
fulldata$`performance~weak_sales_bool~neg~noemo`=as.factor(fulldata$`performance~weak_sales_bool~neg~noemo`)

fulldata$`topic~business_cycle~neu~noemo`=as.factor(fulldata$`topic~business_cycle~neu~noemo`)
fulldata$`topic~GDP_trend~neu~noemo`=as.factor(fulldata$`topic~GDP_trend~neu~noemo`)
fulldata$`topic~civil_unrest~neg~noemo`=as.factor(fulldata$`topic~civil_unrest~neg~noemo`)

fulldata$`topic~economic_comparison~neu~noemo`=as.factor(fulldata$`topic~economic_comparison~neu~noemo`)
fulldata$`topic~good_economy~pos~noemo`=as.factor(fulldata$`topic~good_economy~pos~noemo`)
fulldata$`topic~interest_rate_decrease~neu~noemo`=as.factor(fulldata$`topic~interest_rate_decrease~neu~noemo`)
fulldata$`topic~interest_rate_increase~neu~noemo`=as.factor(fulldata$`topic~interest_rate_increase~neu~noemo`)
fulldata$`topic~lower_prices_bool~neg~noemo`=as.factor(fulldata$`topic~lower_prices_bool~neg~noemo`)
fulldata$`topic~monetary_policy~neu~noemo`=as.factor(fulldata$`topic~monetary_policy~neu~noemo`)
fulldata$`topic~strong_demand_bool~pos~noemo`=as.factor(fulldata$`topic~strong_demand_bool~pos~noemo`)
fulldata$`topic~strong_strategy_bool~pos~noemo`=as.factor(fulldata$`topic~strong_strategy_bool~pos~noemo`)
fulldata$`topic~subsidy_government_incentive~neu~noemo`=as.factor(fulldata$`topic~subsidy_government_incentive~neu~noemo`)
fulldata$`topic~weak_demand_bool~neg~noemo`=as.factor(fulldata$`topic~weak_demand_bool~neg~noemo`)
fulldata$Y=as.factor(fulldata$Y)
# sample train and valiate data
set.seed(1234)
trainindex=sample(nrow(fulldata),0.7*nrow(fulldata))
traindata=fulldata[trainindex,]
validata=fulldata[-trainindex,]

#x and y
fullx=fulldata[,1:42]
fully=fulldata[,43]
trainx=traindata[,1:42]
trainy=traindata[,43]
validx=validata[,1:42]
validy=validata[,43]

fullxmatrix=data.matrix(fullx)-1
fullymatrix=data.matrix(fully)-1
trainxmatrix=data.matrix(trainx)-1
trainymatrix=data.matrix(trainy)-1
validxmatrix=data.matrix(validx)-1
validymatrix=data.matrix(validy)-1
# correaltion calculation
res=cor(fullxmatrix)
round(res,2)
res2=rcorr(fullxmatrix)
res2$r
#write.csv(res,"C:/Users/lindsay/Desktop/758W/modelling/data/datacorr_matrix.csv")

#draw heatmap
library(pheatmap)
library(gplots)
col<- colorRampPalette(c("blue", "white", "red"))(20)
fontsize_row = 10 - nrow(res) / 15
pheatmap(res, col = col,main="my heatmap", cluster_cols = F, 
         fontsize = fontsize_row)

# run lasso
library(glmnet)
lasso_model=glmnet(trainxmatrix,trainymatrix,family="binomial",alpha=1)
lasso.cv=cv.glmnet(trainxmatrix,trainymatrix,family="binomial",alpha=1)
best.lambda=lasso.cv$lambda.min
plot.glmnet(lasso_model)
coefficients(lasso_model,s=best.lambda)
coef(lasso_model,lasso.cv$lambda.1se)
     #predict using lmabda.min
prediction=predict(lasso_model,s=best.lambda,newx=validxmatrix, type="response")
prediction_class=ifelse(prediction>=0.5,1,0)
library(caret)
accuracy=mean(validymatrix==prediction_class)
conmatrix=confusionMatrix(data=as.factor(prediction_class),reference=as.factor(validymatrix))
conmatrix   
     #prediction using lambda.1se
prediction_se=predict(lasso_model,s=lasso.cv$lambda.1se,newx=validxmatrix, type="response")
prediction_se_class=ifelse(prediction_se>=0.5,1,0)
accuracy_se=mean(validymatrix==prediction_se_class)
conmatrix_se=confusionMatrix(data=as.factor(prediction_se_class),reference=as.factor(validymatrix))
conmatrix_se

# functions
# confusion matrix
library(ROCR)
confusion_matrix <- function(preds, actuals, cutoff){
  
  classifications <- ifelse(preds>cutoff,1,0)
  
  ##careful with positives and negatives here!
  confusion_matrix <- table(actuals,classifications)
}

# print performances
class_performance <- function(confusion_matrix){
  
  TP <- confusion_matrix[2,2]
  TN <- confusion_matrix[1,1]
  FP <- confusion_matrix[1,2]
  FN <- confusion_matrix[2,1]
  
  ##accuracy = total number of correct classifications/total number of classifications
  acc <- (TP+TN)/(TP+TN+FP+FN)
  
  ##TPR = Percent of actual positives identified as such (sensitivity)
  tpr <- TP/(TP+FN)
  
  ##TNR = Percent of actual negatives identified as such (specificity)
  tnr <- TN/(TN+FP)
  ppv<- TP/(TP+FP)
  ##I'll leave it as an exercise for you to compute the other basic confusion matrix metrics
  
  ##return the list of metrics you want
  return(c(acc, tpr, tnr,ppv))
}


