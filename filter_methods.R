library(randomForest)
library(pROC)
library(caret)
require(ROCR)
library(FSinR)
library(ggthemes)
library(reshape)
##############################
# Filter methods
##############################

# relief
# roughsetConsistency
# binaryConsistency
# determinationCoefficient
# mutualInformation
# gainRatio
# symmetricalUncertain
# giniIndex

# line 55 : replace filter_method with the above mentioned methods
# [evaluator <- filterEvaluator('filter_method')]
################################


R=10
K=5  # using 5 folds
n=nrow(One_Three2) 
folds = cut(1:n, K, labels=FALSE)
st=cm=list()
acc.rf=auc.rf=acc.rf_final=rf.p_final=auc.rf_final=numeric(K)
accuracy.rf=precision.rf=recall.rf=f1.score.rf=numeric(K)
acc.mean=mean_acc=mean_auc=pre.mean=rec.mean=auc.mean=f1.mean=numeric(R)
acc_for_box=auc_for_box=acc_for_box2=auc_for_box2=numeric(15)
a=1
b=1
for(r in 1:R){
  forward.matrix = matrix(nrow = K, ncol = ncol(One_Three2))
  colnames(forward.matrix) = colnames(One_Three2)
  forward.matrix = forward.matrix[,-143]
  
  for (k in 1:K){
    i.train=which(folds!=k)
    i.test=which(folds==k)
    x.train=One_Three2[i.train,]
    x.test=One_Three2[i.test,]
    
    rf.out=randomForest(stg~.,data=x.train)
    rf.pred = predict(rf.out,x.test,'class')
    tb.rf=table(rf.pred,x.test$stg)
    acc.rf[k]=sum(diag(tb.rf)) / sum(tb.rf)
    acc_for_box[a]=acc.rf[k]
    
    rf.p=predict(rf.out,x.test,'prob')[,1]
    auc.rf[k] = roc(x.test$stg, rf.p)$auc
    
    auc_for_box[a]=auc.rf[k]
    
    
    evaluator <- filterEvaluator('relief')
    directSearcher <- directSearchAlgorithm('selectPercentile', list(percentile = 10))
    results <- directFeatureSelection(x.train, 'stg', directSearcher, evaluator)
    
    
    vars = match(results$featuresSelected,colnames(One_Three2))
    len[a]=length(vars)
    a=a+1
    forward.matrix[k,match(results$featuresSelected,colnames(One_Three2))] = 1
    forward.matrix[k,-match(results$featuresSelected,colnames(One_Three2))] = 0
    
  }
  
  mean_acc[r] = mean(acc.rf)
  mean_auc[r] = mean(auc.rf)
  
  fs=c()
  for(i in 1:ncol(forward.matrix)){
    if((sum(forward.matrix[,i])/nrow(forward.matrix)) >= 0.30){
      fs = c(fs, colnames(forward.matrix)[i])
    }
  }
  fs = c(fs, "stg")
  fs_sel[[r]]= fs
  for (k in 1:K){
    i.train2=which(folds!=k)
    i.test2=which(folds==k)
    x.train2=One_Three2[i.train2,fs]
    x.test2=One_Three2[i.test2,fs]
    
    rf.sel=randomForest(stg~.,data=x.train2)
    
    rf.pred_final = predict(rf.sel,x.test2,'class')
    tb.rf_final=table(rf.pred_final,x.test2$stg)
    acc.rf_final[k]=sum(diag(tb.rf_final)) / sum(tb.rf_final)
    acc_for_box2[b]=acc.rf_final[k]
    
    rf.p_final=predict(rf.sel,x.test2,'prob')[,1]
    auc.rf_final[k] =roc(x.test2$stg, rf.p_final)$auc
    
    auc_for_box2[b]=auc.rf_final[k]
    
    roc=roc(x.test2$stg, rf.p_final)
    proc[[k]]=roc
    
    cm=caret::confusionMatrix(rf.pred_final, x.test2$stg)
    accuracy.rf[k] = round(cm$overall[1],2)
    precision.rf[k] = round(cm$byClass[5],2)
    recall.rf[k] = round(cm$byClass[6],2)
    f1.score.rf[k] = round(cm$byClass[7],2)
    b=b+1
  }
  plot(proc[[1]], col='black')
  plot(proc[[2]], add=TRUE, col='red')
  plot(proc[[3]], add=TRUE, col='skyblue')
  plot(proc[[4]], add=TRUE, col='green')
  plot(proc[[5]], add=TRUE, col='orange')
  legend(0.65,0.85, legend=c("Fold 1", "Fold 2", "Fold 3","Fold 4","Fold 5"), col = c("black","red",'skyblue', 'green','orange'), lty=1, cex=1.0, title = '5 fold cv')
  acc.mean[r]=mymean(accuracy.rf)
  auc.mean[r]=mymean(auc.rf_final)
  pre.mean[r]=mymean(precision.rf)
  rec.mean[r]=mymean(recall.rf)
  f1.mean[r]=mymean(f1.score.rf)
}