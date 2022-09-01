################################################################
# Feature pre-filtering and selection for prediction of cancer 
# stage in lung cancer dataset - Data Pre-Processing       
################################################################

rm(list=ls())
setwd("C:/Users/MSI1/OneDrive/Desktop/Dissertation in Data Analytics(ST6090)/Datasets/")
############################
#      Data Import
############################

lung<-read.csv("C:/Users/MSI1/OneDrive/Desktop/Dissertation in Data Analytics(ST6090)/Datasets/lung.csv")

# assigning the first column name as serial_num
colnames(lung)[1] <- "serial_num"

# backup original dataset
lung_org <- lung

#Dimension
dim(lung)



# lung$surind=as.numeric(as.factor(lung$surind))
lung$sex=as.numeric(as.factor(lung$sex))
lung$sex=ifelse(lung$sex==1,0,1)
# lung$stg=as.numeric(as.factor(lung$stg))

##################################################
#Remove variables having same value in all rows
##################################################

lung$dimx=NULL
lung$dimy=NULL
lung$dimz=NULL 
lung$min_HIST=NULL
lung$max_HIST=NULL
lung$range_HIST=NULL

###############################################################
# Remove variables which logically don't contribute to cancer
###############################################################

lung$xid=NULL
lung$ptid=NULL
lung$serial_num=NULL
lung$id=NULL

# number of columns left
ncol(lung)

#backup the dataset
lung_remvar=lung

##################################################
#     NULL VALUE CHECK
##################################################

### Check NA's in the dataframe
df<-as.data.frame(cbind(
  lapply(
    lapply(lung, is.na)
    , sum)
))

# Check NA count
subset(df, df$V1 != 0)

#    	    V1
#	inj 50
#	wt  56
#	age  1

imales = which(lung$sex==1)

t.test(lung$age[imales], lung$age[-imales])

# significant difference, hence must use different imputation values for each gender


which(is.na(lung$age))
lung$sex[71] # Missing value for gender female
age.fem=median(lung$age[-imales],na.rm=TRUE)
lung$age[(is.na(lung$age) & (lung$sex==0))] = age.fem # data imputation based on sex

# Check
which(is.na(lung$age))

# Remove inj and wt from dataset as 

lung$inj=NULL
lung$wt=NULL

# Backup after data imputation
lung.after_impute=lung

#names(lung)[1:50]

############################################
# Recode cancer stage
############################################
lung$stg[startsWith(lung$stg,"1")] = 1
lung$stg[startsWith(lung$stg,"2")] = 2
lung$stg[startsWith(lung$stg,"3")] = 3
lung$stg[startsWith(lung$stg,"4")] = 4
lung$stg=as.numeric(as.factor(lung$stg))
lung$stg=as.ordered(as.factor(lung$stg))
levels(lung$stg)

table(lung$stg)
table(lung$stg)/93

############################################################
#           Correlation Analysis
############################################################

# library(corrplot)

#backup
lung.cor=lung
#Removing response variable(Unsupervised filter)
lung.cor$surind=NULL
lung.cor$surtim=NULL
lung.cor$stg=NULL


library('caret')
library(corrplot)
# lung.scale<- scale(lung.cor[1:ncol(lung.cor)],center=TRUE,scale=TRUE);
# correlation = cor(lung.scale)
correlation = cor(lung.cor)

corrplot(correlation, order = "hclust",tl.cex=0.3,addrect = 2)
# corrplot(correlation, method = "circle")
# corrplot(correlation, order = "hclust")
highlyCor <- caret::findCorrelation(correlation, 0.95,exact = F)


lungfiltered <- lung.cor[,-highlyCor]


# correlation.df=data.frame(correlation2)
# 
# write.csv(correlation.df,"C:/Users/MSI1/OneDrive/Desktop/Dissertation in Data Analytics(ST6090)/correlation.csv",row.names = F)
# 
# write.csv(lungfiltered,"lungfiltered.csv",row.names = F)
# write.csv(lung.cor,"lung_cor.csv",row.names = F)

lung_corr_rem=lungfiltered
lung_corr_rem$stg=as.factor(lung$stg)

################################################
# Converting into Binary problem
################################################


# Stage 2 vs stage 4
two_four=lung_corr_rem[lung_corr_rem$stg %in% c("2","4"),]
two_four$stg = as.numeric(two_four$stg)
two_four$stg = as.factor(two_four$stg)
levels(two_four$stg) = c("stage2","stage4")

# Stage 1 vs stage 3
one_three=lung_corr_rem[lung_corr_rem$stg %in% c("1","3"),]
one_three$stg = as.numeric(one_three$stg)
one_three$stg = as.factor(one_three$stg)
levels(one_three$stg) = c("stage1","stage3")

# Stage 1 vs stage 4
one_four=lung_corr_rem[lung_corr_rem$stg %in% c("1","4"),]
one_four$stg = as.numeric(one_four$stg)
one_four$stg = as.factor(one_four$stg)
levels(one_four$stg) = c("stage1","stage4")

# combine classes
lung.cor2=lung.cor # initial stage vs advanced stage without pre filter
lung.cor2$stg = as.factor(as.numeric(lung.cor2$stg))
levels(lung.cor2$stg) = c("initial","initial","advanced","advanced")

lung_corr_rem2=lung_corr_rem # initial stage vs advanced stage with pre filter
lung_corr_rem2$stg = as.factor(as.numeric(lung_corr_rem2$stg))
levels(lung_corr_rem2$stg) = c("initial","initial","advanced","advanced")

