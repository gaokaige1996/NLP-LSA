#read data
tfidf=read.csv("C:/Users/lindsay/Desktop/758W/modelling/data/tfidf.csv")
dtm=read.csv("C:/Users/lindsay/Desktop/758W/modelling/data/dtm.csv")

#tfidf2 transform tfidf as numerical, then combine with dataset1
dtm2=data.frame(lapply(dtm[,-1], function(x) as.numeric(x)))
dataset1_dtm2=cbind(dtm2,dataset1[,1:43])
fulldata=dataset1_dtm2

#dtm
#dtm2=data.frame(lapply(dtm[,-1], function(x) as.numeric(x)))
#dataset1_dtm2=cbind(dtm2,dataset1[,1:43])
#fulldata=dataset1_dtm2
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
fulldata$Y=as.factor(ifelse(fulldata$Y=="meaningful",1,0))
#fulldata$Y=as.factor(fulldata$Y)
index=grep("Y",colnames(fulldata))
#create dummy
fulldata=model.matrix(~.,data=fulldata)[,-1]
#normalize data
data_norm=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
fulldata=as.data.frame(lapply(data.frame(fulldata),data_norm))
#-------------------------------------------------------------------------------


#transform data into the form required by xgboost
library(xgboost)
library(Matrix)
index2=grep("Y1",colnames(fulldata))
sparse_matrix=sparse.model.matrix(Y1~.,data=fulldata)[,-1]
label=fulldata$Y1==1
#split data
set.seed(1234)
trainindex=sample(nrow(sparse_matrix),0.7*nrow(sparse_matrix))
traindata=fulldata[trainindex,]
validata=fulldata[-trainindex,]

train_sparse=sparse.model.matrix(Y1~.,data=traindata)[,-1]
valid_sparse=sparse.model.matrix(Y1~.,data=validata)[,-1]
labeltrain=traindata$Y1==1
labelvalid=validata$Y1==1
#x and y
fullx=fulldata[,-index2]
fully=fulldata[,index2]
trainx=traindata[,-index2]
trainy=traindata[,index2]
validx=validata[,-index2]
validy=validata[,index2]

trainmatrix=as.matrix(traindata)
validmatrix=as.matrix(validata)
fullxmatrix=as.matrix(fullx)
fullymatrix=as.matrix(fully)
trainxmatrix=as.matrix(trainx)
trainymatrix=as.matrix(trainy)
validxmatrix=as.matrix(validx)
validymatrix=as.matrix(validy)

#functions
library(ROCR)
confusion_matrix <- function(preds, actuals, cutoff){
  
  classifications <- ifelse(preds>cutoff,1,0)
  
  confusion_matrix <- table(actuals,classifications)
}

# print performances
class_performance <- function(confusion_matrix){
  
  TP <- confusion_matrix[2,2]
  TN <- confusion_matrix[1,1]
  FP <- confusion_matrix[1,2]
  FN <- confusion_matrix[2,1]
  acc <- (TP+TN)/(TP+TN+FP+FN)
  rec <- TP/(TP+FN)
  prec<- TP/(TP+FP)
  f1=2*rec* prec/(rec+prec)
  return(c(acc,prec,rec,f1))
}

# manual tune parameter
best_para=function(paralist,train_sparse,labeltrain,valid_sparse,labelvalid){
  all_f1=c()
  all_cutoff=c()
  all_precision=c()
  all_recall=c()
  all_auc=c()
  all_accuracy=c()

  for (i in paralist){
    set.seed(1234)
    xgbmodel=xgboost(data=train_sparse,label=labeltrain,nthread=4,nrounds=71,max_depth=i,
                     objective="binary:logistic")
    
    xgbpred=predict(xgbmodel,valid_sparse)
    
    xgbroc=prediction(xgbpred,labelvalid)
    fper=performance(xgbroc,"f")
    precisionper=performance(xgbroc,"ppv")
    recallper=performance(xgbroc,"tpr")
    accuracyper=performance(xgbroc,"acc")
    auc.tmp=performance(xgbroc,"auc")
    auc=as.numeric(auc.tmp@y.values)
    
    best = which.max(slot(fper,"y.values")[[1]])
    max.f1 = slot(fper,"y.values")[[1]][best]
    max.cutoff=slot(fper,"x.values")[[1]][best]
    max.precision=slot(precisionper,"y.values")[[1]][best]
    max.recall=slot(recallper,"y.values")[[1]][best]
    max.accuracy=slot(accuracyper,"y.values")[[1]][best]
    
    all_f1=append(all_f1,max.f1)
    all_cutoff=append(all_cutoff,max.cutoff)
    all_precision=append(all_precision,max.precision)
    all_recall=append(all_recall,max.recall)
    all_auc=append(all_auc,auc)
    all_accuracy=append(all_accuracy,max.accuracy)}
  
  return(c(f1=max(all_f1),para=paralist[which.max(all_f1)],cutoff=all_cutoff[which.max(all_f1)]
           ,precision=all_precision[which.max(all_f1)],recall=all_recall[which.max(all_f1)],
           auc=all_auc[which.max(all_f1)],accuracy=all_accuracy[which.max(all_f1)]))}


paralist=seq(1,8,1)
result=best_para(paralist,train_sparse,labeltrain,valid_sparse,labelvalid)
result
best_cutoff=result[3]
best_cutoff
best_para=result[2]
best_para


xgbmodel=xgboost(data=train_sparse,label=labeltrain,nthread=4,nrounds=80,max_depth=1,
                 objective="binary:logistic")
xgbpred=predict(xgbmodel,valid_sparse)


#plot "precion recall" curves 
roc_xgb=prediction(xgbpred,labelvalid)
rp=performance(roc_xgb,"prec","rec")
plot(rp,main="rp curve xgb dataset1 dummy")
#plot roc curve
roc=performance(roc_xgb,"tpr","fpr")
roc2=performance(roc_xgb,"f","rec")
plot(roc,main="roc knn dataset4 dummy")
#roc area under the curve
auc.tmp=performance(roc_xgb,"auc")
auc=as.numeric(auc.tmp@y.values)
auc 
importance=xgb.importance(model=xgbmodel)
print(importance)
xgb.plot.importance(importance_matrix = importance)

write.csv(importance,"C:/Users/lindsay/Desktop/758W/modelling/data/out_xgb_dtm.csv",fileEncoding = "UTF-8")
write.csv(sparsmatrix2,"C:/Users/lindsay/Desktop/758W/modelling/data/sparsmatrix.csv",fileEncoding = "UTF-8")
