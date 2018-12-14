tfidf=read.csv("C:/Users/lindsay/Desktop/758W/modelling/data/tfidf.csv")
fulldata=as.data.frame(tfidf$tax)
fulldata$said=tfidf$said
fulldata$economic=tfidf$economic
fulldata$rate=tfidf$rate
fulldata$economy=tfidf$economy
fulldata$interest=tfidf$interest
fulldata$currency=tfidf$currency
fulldata$policy=tfidf$policy
fulldata$inflation=tfidf$inflation
fulldata$tariff=tfidf$tariff
fulldata$usd=tfidf$usd
fulldata$growth=tfidf$growth
fulldata$trade=tfidf$trade
fulldata$unemployment=tfidf$unemployment
fulldata$economics=tfidf$economics
fulldata$eps=tfidf$eps
fulldata$china=tfidf$china
fulldata$revenue=tfidf$revenue
fulldata$guidance=tfidf$guidance
fulldata$company=tfidf$company
fulldata$employment=tfidf$employment
fulldata$share=tfidf$share
fulldata$trump=tfidf$trump
fulldata$government=tfidf$government
fulldata$sale=tfidf$sale
fulldata$u=tfidf$u
fulldata$uncertainty=tfidf$uncertainty
fulldata$america=tfidf$america
fulldata$income=tfidf$income
fulldata$sat=tfidf$say
fulldata$cut=tfidf$cut
fulldata$reform=tfidf$reform
fulldata$higher=tfidf$higher
fulldata$decline=tfidf$decline
fulldata$currency=tfidf$currency

fulldata=data.frame(lapply(fulldata, function(x) as.numeric(x)))
fulldata=cbind(fulldata,dataset7[,2:31])
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
#fulldata$Y=as.factor(ifelse(fulldata$Y=="meaningful",1,0))
fulldata$Y=as.factor(fulldata$Y)
index=grep("Y",colnames(fulldata))

# as numeric back for knn variables
fulldata$primary_emotion=as.numeric(fulldata$primary_emotion)-1
fulldata$primary_emotion_confidence=as.numeric(fulldata$primary_emotion_confidence)-1
fulldata$valence_direction=as.numeric(fulldata$valence_direction)-1
fulldata$valence_confidence=as.numeric(fulldata$valence_confidence)-1
fulldata$`performance~profit_bool~pos~noemo`=as.numeric(fulldata$`performance~profit_bool~pos~noemo`)-1
fulldata$`performance~strong_performance_bool~pos~noemo`=as.numeric(fulldata$`performance~strong_performance_bool~pos~noemo`)-1
fulldata$`performance~strong_sales_bool~pos~noemo`=as.numeric(fulldata$`performance~strong_sales_bool~pos~noemo`)-1
fulldata$`performance~underperform~neg~noemo`=as.numeric(fulldata$`performance~underperform~neg~noemo`)-1
fulldata$`performance~weak_performance~neg~noemo`=as.numeric(fulldata$`performance~weak_performance~neg~noemo`)-1
fulldata$`performance~weak_performance_bool~neg~noemo`=as.numeric(fulldata$`performance~weak_performance_bool~neg~noemo`)-1
fulldata$`performance~weak_sales_bool~neg~noemo`=as.numeric(fulldata$`performance~weak_sales_bool~neg~noemo`)-1

fulldata$`topic~business_cycle~neu~noemo`=as.numeric(fulldata$`topic~business_cycle~neu~noemo`)-1
fulldata$`topic~GDP_trend~neu~noemo`=as.numeric(fulldata$`topic~GDP_trend~neu~noemo`)-1
fulldata$`topic~civil_unrest~neg~noemo`=as.numeric(fulldata$`topic~civil_unrest~neg~noemo`)-1

fulldata$`topic~economic_comparison~neu~noemo`=as.numeric(fulldata$`topic~economic_comparison~neu~noemo`)-1
fulldata$`topic~good_economy~pos~noemo`=as.numeric(fulldata$`topic~good_economy~pos~noemo`)-1
fulldata$`topic~interest_rate_decrease~neu~noemo`=as.numeric(fulldata$`topic~interest_rate_decrease~neu~noemo`)-1
fulldata$`topic~interest_rate_increase~neu~noemo`=as.numeric(fulldata$`topic~interest_rate_increase~neu~noemo`)-1
fulldata$`topic~lower_prices_bool~neg~noemo`=as.numeric(fulldata$`topic~lower_prices_bool~neg~noemo`)-1
fulldata$`topic~monetary_policy~neu~noemo`=as.numeric(fulldata$`topic~monetary_policy~neu~noemo`)-1
fulldata$`topic~strong_demand_bool~pos~noemo`=as.numeric(fulldata$`topic~strong_demand_bool~pos~noemo`)-1
fulldata$`topic~strong_strategy_bool~pos~noemo`=as.numeric(fulldata$`topic~strong_strategy_bool~pos~noemo`)-1
fulldata$`topic~subsidy_government_incentive~neu~noemo`=as.numeric(fulldata$`topic~subsidy_government_incentive~neu~noemo`)-1
fulldata$`topic~weak_demand_bool~neg~noemo`=as.numeric(fulldata$`topic~weak_demand_bool~neg~noemo`)-1
fulldata$Y=as.numeric(fulldata$Y)-1

#create dummy
#fulldata=model.matrix(~.,data=fulldata)[,-1]
#normalize data
data_norm=function(x){
return((x-min(x))/(max(x)-min(x)))
}
fulldata=as.data.frame(lapply(data.frame(fulldata),data_norm))

# sample train and valiate data
set.seed(1234)
trainindex=sample(nrow(fulldata),0.7*nrow(fulldata))
traindata=fulldata[trainindex,]
validata=fulldata[-trainindex,]

#x and y
fullx=fulldata[,-index]
fully=fulldata[,index]
trainx=traindata[,-index]
trainy=traindata[,index]
validx=validata[,-index]
validy=validata[,index]

trainmatrix=data.matrix(traindata)
validmatrix=data.matrix(validata)
fullxmatrix=data.matrix(fullx)
fullymatrix=data.matrix(fully)
trainxmatrix=data.matrix(trainx)
trainymatrix=data.matrix(trainy)
validxmatrix=data.matrix(validx)
validymatrix=data.matrix(validy)

# KNN--find the best K and cutoff
library(class)
best_cutoff_k= function (klist,trainx,validx,traindata,validata){
  all_f1=c()
  all_cutoff=c()
  all_precision=c()
  all_recall=c()
  all_auc=c()
  all_accuracy=c()
  for (i in klist) {
    knn.pred_valid=knn(trainx,validx,traindata$Y,k=i,prob=TRUE)
    prob=attr(knn.pred_valid,"prob")
    prob2=ifelse(knn.pred_valid=="0",1-prob,prob)
    roc.valid=prediction(prob2,validata$Y)
    
    f1.perf=performance(roc.valid,measure="f")
    precisionper=performance(roc.valid,"ppv")
    recallper=performance(roc.valid,"tpr")
    accuracyper=performance(roc.valid,"acc")
    auc.tmp=performance(roc.valid,"auc")
    auc=as.numeric(auc.tmp@y.values)
    
    best = which.max(slot(f1.perf,"y.values")[[1]])
    max.f1 = slot(f1.perf,"y.values")[[1]][best]
    max.cutoff=slot(f1.perf,"x.values")[[1]][best]
    max.precision=slot(precisionper,"y.values")[[1]][best]
    max.recall=slot(recallper,"y.values")[[1]][best]
    max.accuracy=slot(accuracyper,"y.values")[[1]][best]
    
    all_f1=append(all_f1,max.f1)
    all_cutoff=append(all_cutoff,max.cutoff)
    all_precision=append(all_precision,max.precision)
    all_recall=append(all_recall,max.recall)
    all_auc=append(all_auc,auc)
    all_accuracy=append(all_accuracy,max.accuracy)}
  
  return(c(f1=max(all_f1),k=klist[which.max(all_f1)],cutoff=all_cutoff[which.max(all_f1)]
           ,precision=all_precision[which.max(all_f1)],recall=all_recall[which.max(all_f1)],
           auc=all_auc[which.max(all_f1)],accuracy=all_accuracy[which.max(all_f1)]))}

#test function
klist=seq(1,50,1)
result=best_cutoff_k(klist,trainxmatrix,validxmatrix,traindata,validata)
result 
