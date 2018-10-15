# importing data
library(readr)
merged_aggregated_data <- read_csv("D:/Hackathon/PredictDriverChurn-DataSet/8. churn_status_with_merged_data.csv")
merged_aggregated_data$X1 <- NULL


merged_aggregated_data[merged_aggregated_data$dr_wkbase=='NULL','dr_wkbase']<-NA
merged_aggregated_data$dr_wkbase <- as.numeric(merged_aggregated_data$dr_wkbase)


unique(merged_aggregated_data$dr_paytype)
unique(merged_aggregated_data$dr_wkbase)
