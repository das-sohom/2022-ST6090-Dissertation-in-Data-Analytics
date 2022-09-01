library(glmnet)
library(randomForest)
library(pROC)

########################################
#     Feature Selection -LASSO
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
n=nrow(lung_corr_rem2) 
folds = cut(1:n, K, labels=FALSE)
fsel=st=cm=list()
acc.rf=auc.rf=acc.rf_final=rf.p_final=auc.rf_final=numeric(K)
len=accuracy.rf=precision.rf=recall.rf=f1.score.rf=numeric(K)
acc.mean=mean_acc=mean_auc=pre.mean=rec.mean=auc.mean=f1.mean=numeric(R)
acc_for_box=auc_for_box=acc_for_box2=auc_for_box2=numeric(15)
forward.matrix1 = matrix(nrow = 50, ncol = ncol(lung_corr_rem2))
colnames(forward.matrix1) = colnames(lung_corr_rem2)

a=1
b=1
for(r in 1:R){
  forward.matrix = matrix(nrow = K, ncol = ncol(lung_corr_rem2))
  colnames(forward.matrix) = colnames(lung_corr_rem2)
  forward.matrix = forward.matrix[,-82]
  
  for (k in 1:K){
    i.train=which(folds!=k)
    i.test=which(folds==k)
    x.train=lung_corr_rem2[i.train,]
    x.test=lung_corr_rem2[i.test,]
    
    rf.out=randomForest(stg~.,data=x.train)
    rf.pred = predict(rf.out,x.test,'class')
    tb.rf=table(rf.pred,x.test$stg)
    acc.rf[k]=sum(diag(tb.rf)) / sum(tb.rf)
    acc_for_box[a]=acc.rf[k]
    
    x=as.matrix(x.train[,-82])
    y=x.train[,82]
    
    
    lasso.opt = cv.glmnet(x,
                          y,
                          alpha=1,
                          family='binomial')
    lasso.mod = glmnet(x,
                       y,
                       alpha=1,family='binomial',
                       lambda=lasso.opt$lambda.min)
    
    rf.p=predict(rf.out,x.test,'prob')[,1]
    auc.rf[k] = roc(x.test$stg, rf.p)$auc
    
    auc_for_box[a]=auc.rf[k]
    

    
    if(length((which(coef(lasso.mod,s=lasso.opt$lambda.min)!=0)-1))==0){
      forward.matrix[k,] = 0
    } else{
      forward.matrix[k,(which(coef(lasso.mod,s=lasso.opt$lambda.min)!=0)-1)] = 1
      forward.matrix[k,-(which(coef(lasso.mod,s=lasso.opt$lambda.min)!=0)-1)] = 0
      forward.matrix1[a,(which(coef(lasso.mod,s=lasso.opt$lambda.min)!=0)-1)] = 1
      forward.matrix1[a,-(which(coef(lasso.mod,s=lasso.opt$lambda.min)!=0)-1)] = 0
    }
    len[a]=length((which(coef(lasso.mod,s=lasso.opt$lambda.min)!=0)-1))

    a=a+1
  }
  forward.matrix[1,]=0
  mean_acc[r] = mean(acc.rf)
  mean_auc[r] = mean(auc.rf)
  
  fs=c()
  for(i in 1:ncol(forward.matrix)){
    if((sum(forward.matrix[,i])/nrow(forward.matrix)) >= 0.30){ # threshold
      fs = c(fs, colnames(forward.matrix)[i])
    }
  }
  fs = c(fs, "stg")
  fs_sel[[r]]= fs
  for (k in 1:K){
    i.train2=which(folds!=k)
    i.test2=which(folds==k)
    x.train2=lung_corr_rem2[i.train2,fs]
    x.test2=lung_corr_rem2[i.test2,fs]
    
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
    

    cm=caret::confusionMatrix(rf.pred_final, x.test2$stg,positive="advanced") # positive class
    accuracy.rf[k] = round(cm$overall[1],2)
    precision.rf[k] = round(cm$byClass[5],2)
    recall.rf[k] = round(cm$byClass[6],2)
    f1.score.rf[k] = round(cm$byClass[7],2)
    roc.outer[[k]] = roc
    auc.outer[k] = round(roc$auc,2)
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

box=cbind(acc_for_box,acc_for_box2,auc_for_box,auc_for_box2)
colnames(box)=c('Acc No FS','Acc with FS','AUC No FS','AUC with FS')
boxplot(box)

# Without feature selection
round(mean(acc_for_box),2)
round(sd(acc_for_box),2)
round(mean(auc_for_box),2)
# With feature selection
round(mean(acc_for_box2),2)
round(sd(acc_for_box2),2)
round(mean(auc_for_box2),2)
# precision recall f1-score
round(mean(pre.mean),2)
round(mean(rec.mean),2)
round(mean(f1.mean),2)
round(mean(len),0)
