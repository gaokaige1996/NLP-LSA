tfidf=read.csv("C:/Users/lindsay/Desktop/758W/modelling/data/tfidf.csv")
#length=read.csv("C:/Users/lindsay/Desktop/758W/modelling/data/lengths.csv")
#distance=read.csv("C:/Users/lindsay/Desktop/758W/modelling/data/distance.csv")
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
#---------------------------------
#other words suggested by dtm
fulldata$X5x=tfidf$X5x
#fulldta$year=tfidf$year
#fulldata$guidance=tfidf$guidance
fulldata$loss=tfidf$loss
fulldata$spread=tfidf$spread
fulldata$fell=tfidf$fell
fulldata$disposable=tfidf$disposable
#fulldata$valuation=tfidf$valuation
fulldata$job=tfidf$job
fulldata$think=tfidf$think
fulldata$expected=tfidf$expected
fulldata$production=tfidf$production
fulldata$growing=tfidf$growing
fulldata$keep=tfidf$keep
fulldata$international=tfidf$international
fulldata$budget=tfidf$budget
fulldata$deflation=tfidf$deflation
#fulldata$recession=tfidf$recession
#fulldata$risk=tfidf$risk
#fulldata$economist=tfidf$economist

#----------------------
#deal data for logistic model
#length=data.frame(lapply(length, function(x) as.numeric(x)))
fulldata=data.frame(lapply(fulldata, function(x) as.numeric(x)))
fulldata=cbind(fulldata,dataset7[,2:31])
any(is.na(fulldata))

##categorical numerial deal 18
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
fulldata$Y=as.factor(fulldata$Y)

index=grep("Y",colnames(fulldata))
#split data
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

#------------------------------------------------------------------------------
logmodel=glm(traindata$Y ~.,data=traindata,family="binomial")
log_prob=predict(logmodel,newdata =validx,type="response")
log_ROCR=prediction(log_prob,validy)
f1.perf=performance(log_ROCR,measure ="f")
best = which.max(slot(f1.perf,"y.values")[[1]])
max.f1 = slot(f1.perf,"y.values")[[1]][best]
max.cutoff=slot(f1.perf,"x.values")[[1]][best]
max.cutoff
pr.perf=performance(log_ROCR,"prec","rec")
acc.perf=performance(log_ROCR,"acc")
max.ppv=slot(pr.perf,"y.values")[[1]][best]
max.recall=slot(pr.perf,"x.values")[[1]][best]
max.accuracy=slot(acc.perf,"y.values")[[1]][best]
max.ppv
max.recall
max.accuracy
#logmatrix=confusion_matrix(log_prob,validymatrix,max.cutoff)
#logperformance=class_performance(logmatrix)
#logperformance

rp=performance(log_ROCR,"prec","rec")
plot(rp,main="rp logistic dataset1")

roc=performance(log_ROCR,"tpr","fpr")
plot(roc,main="roc logistic dataset1")

#roc area under the curve
auc.tmp=performance(log_ROCR,"auc")
auc=as.numeric(auc.tmp@y.values)
auc

#get important variables
summary(logmodel)
