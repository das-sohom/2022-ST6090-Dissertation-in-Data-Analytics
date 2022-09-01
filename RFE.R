library(randomForest)
library(pROC)
library(caret)
require(ROCR)
########################################
#     Feature Selection - RFE
########################################

# dataframe

# one_three - with pre-filter nrow(82)
# One_Three2 - without pre-filter nrow(143)
# two_four- with pre-filter nrow(82)
# Two_Four2 - without pre-filter nrow(143)
# one_four- with pre-filter nrow(82)
# One_Four2 - without pre-filter nrow(143)
# lung.cor_rem2- with pre-filter nrow(82)
# lung.cor2 - without pre-filter nrow(143)


R=10
K=5  # using 5 folds
n=nrow(one_four) 
folds = cut(1:n, K, labels=FALSE)
st=cm=list()
acc.rf=auc.rf=acc.rf_final=rf.p_final=auc.rf_final=numeric(K)
accuracy.rf=precision.rf=recall.rf=f1.score.rf=numeric(K)
acc.mean=mean_acc=mean_auc=pre.mean=rec.mean=auc.mean=f1.mean=numeric(R)
acc_for_box=auc_for_box=acc_for_box2=auc_for_box2=numeric(15)
forward.matrix1 = matrix(nrow = 50, ncol = ncol(one_four))
colnames(forward.matrix1) = colnames(one_four)
a=1
b=1
for(r in 1:R){
  forward.matrix = matrix(nrow = K, ncol = ncol(one_four))
  colnames(forward.matrix) = colnames(one_four)
  forward.matrix = forward.matrix[,-82]
  
  for (k in 1:K){
    i.train=which(folds!=k)
    i.test=which(folds==k)
    x.train=one_four[i.train,]
    x.test=one_four[i.test,]
    
    rf.out=randomForest(stg~.,data=x.train)
    rf.pred = predict(rf.out,x.test,'class')
    tb.rf=table(rf.pred,x.test$stg)
    acc.rf[k]=sum(diag(tb.rf)) / sum(tb.rf)
    acc_for_box[a]=acc.rf[k]
    
    
    
    rf.p=predict(rf.out,x.test,'prob')[,1]
    auc.rf[k] = roc(x.test$stg, rf.p)$auc
    
    auc_for_box[a]=auc.rf[k]
    
    
    x=x.train[,-82]
    y=x.train[,82]
    control <- rfeControl(functions = rfFuncs,method = "cv")
    result_rfe1 <- rfe(x,y, 
                       sizes = c(1:5,10,15,20,25,30),
                       rfeControl = control)
    pred<-predict(result_rfe1, x.test)[,1]

    st<-result_rfe1$optVariables

    
    vars = match(result_rfe1$optVariables,colnames(one_four))
    len[a] = length(vars)
    forward.matrix[k,match(result_rfe1$optVariables,colnames(one_four))] = 1
    forward.matrix[k,-match(result_rfe1$optVariables,colnames(one_four))] = 0
    forward.matrix1[a,match(result_rfe1$optVariables,colnames(one_four))] = 1
    forward.matrix1[a,-match(result_rfe1$optVariables,colnames(one_four))] = 0
    a=a+1

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
    x.train2=one_four[i.train2,fs]
    x.test2=one_four[i.test2,fs]
    
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

    cm=caret::confusionMatrix(rf.pred_final, x.test2$stg,positive="stage4") # positive class
    accuracy.rf[k] = round(cm$overall[1],2)
    precision.rf[k] = round(cm$byClass[5],2)
    recall.rf[k] = round(cm$byClass[6],2)
    f1.score.rf[k] = round(cm$byClass[7],2)
    roc.outer[[k]] = roc
    auc.outer[k] = round(roc$auc,2)
    b=b+1
  }
  
  acc.mean[r]=mymean(accuracy.rf)
  auc.mean[r]=mymean(auc.rf_final)
  pre.mean[r]=mymean(precision.rf)
  rec.mean[r]=mymean(recall.rf)
  f1.mean[r]=mymean(f1.score.rf)
}

box=cbind(acc_for_box,acc_for_box2,auc_for_box,auc_for_box2)
colnames(box)=c('Acc No FS','Acc with FS','AUC No FS','AUC with FS')
boxplot(box)

round(mean(acc_for_box),2)
round(sd(acc_for_box),2)
round(mean(auc_for_box),2)

round(mean(acc_for_box2),2)
round(sd(acc_for_box2),2)
round(mean(auc_for_box2),2)

round(mean(pre.mean),2)
round(mean(rec.mean),2)
round(mean(f1.mean),2)
round(mean(len),0)
