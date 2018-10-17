# training data preprocessing and onehot encoding

library(dplyr)
# default
# false=0
# true=1

train_dataset_without_missing_target[train_dataset_without_missing_target$default==FALSE,'default']<-0
train_dataset_without_missing_target[train_dataset_without_missing_target$default==TRUE,'default']<-1

levels(train_dataset_without_missing_target$score_1)
#[1] ""                         "1Rk8w4Ucd5yR3KcqZzLdow==" "4DLlLW62jReXaqbPaHp1vQ=="
#[4] "8k8UDR4Yx0qasAjkGrUZLw==" "DGCQep2AE5QRkNCshIAlFQ==" "e4NYDor1NOw6XKGE60AWFw=="
#[7] "fyrlulOiZ+5hoFqLa6UbDQ==" "smzX0nxh5QlePvtVf6EAeg=="
train_dataset_without_missing_target$score_1<-as.character(train_dataset_without_missing_target$score_1)
train_dataset_without_missing_target[train_dataset_without_missing_target$score_1=='','score_1']<-1
train_dataset_without_missing_target[train_dataset_without_missing_target$score_1=='1Rk8w4Ucd5yR3KcqZzLdow==','score_1']<-2
train_dataset_without_missing_target[train_dataset_without_missing_target$score_1=='4DLlLW62jReXaqbPaHp1vQ==','score_1']<-3
train_dataset_without_missing_target[train_dataset_without_missing_target$score_1=='8k8UDR4Yx0qasAjkGrUZLw==','score_1']<-4
train_dataset_without_missing_target[train_dataset_without_missing_target$score_1=='DGCQep2AE5QRkNCshIAlFQ==','score_1']<-5
train_dataset_without_missing_target[train_dataset_without_missing_target$score_1=='e4NYDor1NOw6XKGE60AWFw==','score_1']<-6
train_dataset_without_missing_target[train_dataset_without_missing_target$score_1=='fyrlulOiZ+5hoFqLa6UbDQ==','score_1']<-7
train_dataset_without_missing_target[train_dataset_without_missing_target$score_1=='smzX0nxh5QlePvtVf6EAeg==','score_1']<-8
train_dataset_without_missing_target$score_1 <- as.numeric(as.character(train_dataset_without_missing_target$score_1))

onehotencoder<- function(df,col){
  count_unique<- length(levels(df[,col])) 
  levels_list<-list(levels(df[,col]))
  print(levels(df[,col]))
  df[,col]<-as.character(df[,col])
  for(i in 1:count_unique){
    df[df[,col]==levels_list[[1]][i],col]<-i
  }
  df[,col]<-as.numeric(df[,col])
  return(df)
  
}

train_dataset_without_missing_target<-onehotencoder(train_dataset_without_missing_target,'score_2')
train_dataset_without_missing_target<-onehotencoder(train_dataset_without_missing_target,'reason')
train_dataset_without_missing_target<-onehotencoder(train_dataset_without_missing_target,'gender')

table(train_dataset_without_missing_target$sign)
train_dataset_without_missing_target<-onehotencoder(train_dataset_without_missing_target,'sign')

table(train_dataset_without_missing_target$facebook_profile)
train_dataset_without_missing_target<-onehotencoder(train_dataset_without_missing_target,'facebook_profile')

table(train_dataset_without_missing_target$state)
train_dataset_without_missing_target<-onehotencoder(train_dataset_without_missing_target,'state')

table(train_dataset_without_missing_target$zip)
train_dataset_without_missing_target<-onehotencoder(train_dataset_without_missing_target,'zip')

table(train_dataset_without_missing_target$job_name)
train_dataset_without_missing_target<-onehotencoder(train_dataset_without_missing_target,'job_name')

table(train_dataset_without_missing_target$real_state)
train_dataset_without_missing_target<-onehotencoder(train_dataset_without_missing_target,'real_state')

train_dataset_without_missing_target$end_last_loan <- as.Date(as.character(train_dataset_without_missing_target$end_last_loan))
train_dataset_without_missing_target$last_payment <- as.Date(as.character(train_dataset_without_missing_target$last_payment))
library(lubridate)
train_dataset_without_missing_target$last_payment_year <- year(train_dataset_without_missing_target$last_payment)
train_dataset_without_missing_target$last_payment_month <- month(train_dataset_without_missing_target$last_payment)
train_dataset_without_missing_target$end_last_loan_year <- year(train_dataset_without_missing_target$end_last_loan)
train_dataset_without_missing_target$end_last_loan_month <- month(train_dataset_without_missing_target$end_last_loan)

train_dataset_without_missing_target <- select(train_dataset_without_missing_target,-c(last_payment,end_last_loan))
