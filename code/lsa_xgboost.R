library(tm)
library(SnowballC)
library(lsa)
#creat TFIDF matrix
text_content=nlp_w2v_matrix$text_really_cleaned
corp=Corpus(VectorSource(text_content))
tdm <- TermDocumentMatrix(corp)
dtm=DocumentTermMatrix(corp,control=list(stopwords=FALSE,wordLengths=c(1,Inf)))
inspect(dtm)
n=as.matrix(dtm)

tfidf=weightTfIdf(dtm)
inspect(tfidf)
m=as.matrix(tfidf)
m
#write.csv(m,"C:/Users/lindsay/Desktop/758W/modelling/data/tfidf.csv",fileEncoding = "UTF-8")
#write.csv(n,"C:/Users/lindsay/Desktop/758W/modelling/data/dtm.csv",fileEncoding = "UTF-8")

# readdataset
tfidf=read.csv("C:/Users/lindsay/Desktop/758W/modelling/data/tfidf.csv")
fulldata=read.csv("C:/Users/lindsay/Desktop/758W/modelling/data/dataset1.csv")
lsa.tfidf=lsa(tfidf[,-1], dims=50)
words.df=as.data.frame(as.matrix(lsa.tfidf$tk))
variabes=as.data.frame(as.matrix(lsa.tfidf$dk))
dim(words.df)
dim(tfidf)
dim(variabes)
write.csv(variabes,"C:/Users/lindsay/Desktop/758W/modelling/data/lsa_variables50.csv",fileEncoding = "UTF-8")

words.df$Y=fulldata$Y
words.df$Y=as.factor(ifelse(words.df$Y=="meaningful",1,0))
#words.df$Y=as.numeric(words.df$Y)-1

data_norm=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
words.df=as.data.frame(lapply(data.frame(words.df),data_norm))


set.seed(1234)
trainindex=sample(nrow(words.df),0.7*nrow(words.df))
traindata=words.df[trainindex,]
validata=words.df[-trainindex,]

fullx=words.df[,1:50]
fully=words.df[,51]
trainx=traindata[,1:50]
trainy=traindata[,51]
validx=validata[,1:50]
validy=validata[,51]

# functions
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
klist=seq(1,300,1)
result=best_cutoff_k(klist,trainxmatrix,validxmatrix,traindata,validata)
result 

#plot "precion recall" curves 
roc_k=prediction(prob_k2,validy)
rp=performance(roc_k,"prec","rec")
plot(rp,main="rp curve knn lsa 100")
#plot roc curve
roc=performance(roc_k,"tpr","fpr")
roc2=performance(roc_k,"f","rec")
plot(roc,main="roc knn lsa 100")

#roc area under the curve
auc.tmp=performance(roc_k,"auc")
auc=as.numeric(auc.tmp@y.values)
auc  #auc=0.7781




#-----------------
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

auc.tmp=performance(log_ROCR,"auc")
auc=as.numeric(auc.tmp@y.values)
auc
#--------------
#xgboost
fulldata=model.matrix(~.,data=words.df)[,-1]
#normalize data
data_norm=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
fulldata=as.data.frame(lapply(data.frame(fulldata),data_norm))

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
    xgbmodel=xgboost(data=train_sparse,label=labeltrain,nthread=4,nrounds=17,max_depth=i,
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

paralist=seq(1,20,1)
result=best_para(paralist,train_sparse,labeltrain,valid_sparse,labelvalid)
result

xgbmodel=xgboost(data=train_sparse,label=labeltrain,nthread=4,nrounds=45,max_depth=9,
                 objective="binary:logistic")
xgbpred=predict(xgbmodel,valid_sparse)
#plot "precion recall" curves 
roc_xgb=prediction(xgbpred,labelvalid)
rp=performance(roc_xgb,"prec","rec")
plot(rp,main="rp curve xgb lsa")
#plot roc curve
roc=performance(roc_xgb,"tpr","fpr")
roc2=performance(roc_xgb,"f","rec")
plot(roc,main="roc xgb lsa ")
importance=xgb.importance(model=xgbmodel)
print(importance)
xgb.plot.importance(importance_matrix = importance)
write.csv(importance,"C:/Users/lindsay/Desktop/758W/modelling/data/out_xgb_lsa_300dimension.csv",fileEncoding = "UTF-8")
