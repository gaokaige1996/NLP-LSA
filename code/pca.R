#load data
fulldata=dataset1[,1:43]
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
fulldata$Y=as.factor(ifelse(fulldata$Y=="meaningful",1,0))
#dummy
fulldata=model.matrix(~.,data=fulldata)[,-1]
# log of variables
#fulldata2=log(fulldata[,1:48])
# sample train and valiate data
set.seed(1234)
trainindex=sample(nrow(fulldata),0.7*nrow(fulldata))
traindata=fulldata[trainindex,]
validata=fulldata[-trainindex,]

#x and y
fullx=fulldata[,1:48]
fully=fulldata[,49]
trainx=traindata[,1:48]
trainy=traindata[,49]
validx=validata[,1:48]
validy=validata[,49]

fullxmatrix=as.matrix(fullx)
fullymatrix=as.matrix(fully)
trainxmatrix=as.matrix(trainx)
trainymatrix=as.matrix(trainy)
validxmatrix=as.matrix(validx)
validymatrix=as.matrix(validy)

#run pca
pca=prcomp(fullx,center=TRUE, scale. = TRUE)
print(pca)

plot(pca, type = "l")
summary(pca)
