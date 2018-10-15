
# importing data
library(readr)
merged_aggregated_data <- read_csv("D:/Hackathon/PredictDriverChurn-DataSet/6. merged_aggregated_data.csv")

merged_aggregated_data$X1 <- NULL

# adding column churn status
merged_aggregated_data[substr(as.character(merged_aggregated_data$last_ride),1,7) >= '2017-10','churn_status']<-F 
merged_aggregated_data[is.na(merged_aggregated_data$churn_status),'churn_status'] <- T

#79731

true_index <- which(merged_aggregated_data$dr_onduty==1)
false_index <- which(merged_aggregated_data$dr_onduty==0)

merged_aggregated_data[true_index,'off_duty']<-F
merged_aggregated_data[false_index,'off_duty']<-T
index <- which(merged_aggregated_data$churn_status!=merged_aggregated_data$off_duty)
merged_aggregated_data$off_duty<-NULL
merged_aggregated_data[merged_aggregated_data$dr_wkbase=='NULL','dr_wkbase']<-NA
merged_aggregated_data$dr_wkbase <- as.numeric(merged_aggregated_data$dr_wkbase)
names(merged_aggregated_data)
merged_aggregated_data[, 2:9][is.na(merged_aggregated_data[, 2:9])] <- 'Not Specified'

might_be_mismatch <- merged_aggregated_data[index,]
write.csv(might_be_mismatch,'7. might_be_mismatch.csv')
write.csv(merged_aggregated_data,'8. churn_status_with_merged_data_jan.csv')
