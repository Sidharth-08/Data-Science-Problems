# getting the output

library(dplyr)
#puzzle_test_dataset <- read.csv("~/Downloads/Puzzle/puzzle_test_dataset.csv")
puzzle_test_dataset <- read.csv("C:/Users/sidharth.suman/Desktop/hacktemp/puzzle_test_dataset.csv")

levels(puzzle_test_dataset$score_1)
#[1] ""                         "1Rk8w4Ucd5yR3KcqZzLdow==" "4DLlLW62jReXaqbPaHp1vQ=="
#[4] "8k8UDR4Yx0qasAjkGrUZLw==" "DGCQep2AE5QRkNCshIAlFQ==" "e4NYDor1NOw6XKGE60AWFw=="
#[7] "fyrlulOiZ+5hoFqLa6UbDQ==" "smzX0nxh5QlePvtVf6EAeg=="
puzzle_test_dataset$score_1<-as.character(puzzle_test_dataset$score_1)
puzzle_test_dataset[puzzle_test_dataset$score_1=='','score_1']<-1
puzzle_test_dataset[puzzle_test_dataset$score_1=='1Rk8w4Ucd5yR3KcqZzLdow==','score_1']<-2
puzzle_test_dataset[puzzle_test_dataset$score_1=='4DLlLW62jReXaqbPaHp1vQ==','score_1']<-3
puzzle_test_dataset[puzzle_test_dataset$score_1=='8k8UDR4Yx0qasAjkGrUZLw==','score_1']<-4
puzzle_test_dataset[puzzle_test_dataset$score_1=='DGCQep2AE5QRkNCshIAlFQ==','score_1']<-5
puzzle_test_dataset[puzzle_test_dataset$score_1=='e4NYDor1NOw6XKGE60AWFw==','score_1']<-6
puzzle_test_dataset[puzzle_test_dataset$score_1=='fyrlulOiZ+5hoFqLa6UbDQ==','score_1']<-7
puzzle_test_dataset[puzzle_test_dataset$score_1=='smzX0nxh5QlePvtVf6EAeg==','score_1']<-8
puzzle_test_dataset$score_1 <- as.numeric(as.character(puzzle_test_dataset$score_1))

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

puzzle_test_dataset<-onehotencoder(puzzle_test_dataset,'score_2')
puzzle_test_dataset<-onehotencoder(puzzle_test_dataset,'reason')
puzzle_test_dataset<-onehotencoder(puzzle_test_dataset,'gender')

table(puzzle_test_dataset$sign)
puzzle_test_dataset<-onehotencoder(puzzle_test_dataset,'sign')

table(puzzle_test_dataset$facebook_profile)
puzzle_test_dataset<-onehotencoder(puzzle_test_dataset,'facebook_profile')

table(puzzle_test_dataset$state)
puzzle_test_dataset<-onehotencoder(puzzle_test_dataset,'state')

table(puzzle_test_dataset$zip)
puzzle_test_dataset<-onehotencoder(puzzle_test_dataset,'zip')

table(puzzle_test_dataset$job_name)
puzzle_test_dataset<-onehotencoder(puzzle_test_dataset,'job_name')

table(puzzle_test_dataset$real_state)
puzzle_test_dataset<-onehotencoder(puzzle_test_dataset,'real_state')

puzzle_test_dataset$end_last_loan <- as.Date(as.character(puzzle_test_dataset$end_last_loan))
puzzle_test_dataset$last_payment <- as.Date(as.character(puzzle_test_dataset$last_payment))
library(lubridate)
puzzle_test_dataset$last_payment_year <- year(puzzle_test_dataset$last_payment)
puzzle_test_dataset$last_payment_month <- month(puzzle_test_dataset$last_payment)
puzzle_test_dataset$end_last_loan_year <- year(puzzle_test_dataset$end_last_loan)
puzzle_test_dataset$end_last_loan_month <- month(puzzle_test_dataset$end_last_loan)

puzzle_test_dataset <- select(puzzle_test_dataset,-c(last_payment,end_last_loan,channel))
summary(puzzle_test_dataset)

for(i in 2:ncol(puzzle_test_dataset)){
  puzzle_test_dataset[is.na(puzzle_test_dataset[,i]), i] <- median(puzzle_test_dataset[,i], na.rm = TRUE)
}

load("C:/Users/sidharth.suman/Desktop/hacktemp/random_forest.rda")
puzzle_test_dataset$predicted_default<- predict(fit,puzzle_test_dataset[,-1])
test_pred <- puzzle_test_dataset
profit_cal <- select(test_pred,-c(ids,predicted_default))

load('C:/Users/sidharth.suman/Desktop/hacktemp/linear_regression_profit.rda')
test_pred$predicted_profit<- predict(linear_regression_profit,profit_cal)
test_pred[test_pred$predicted_default==1,'predicted_profit']<-NA
test_pred <- data.frame(select(test_pred,c(ids,predicted_default,predicted_profit)))

#write.csv(test_pred,'~/Downloads/Puzzle/test_pred.csv',row.names = FALSE)
write.csv(test_pred,"C:/Users/sidharth.suman/Desktop/hacktemp/test_predictions.csv",row.names = FALSE)
