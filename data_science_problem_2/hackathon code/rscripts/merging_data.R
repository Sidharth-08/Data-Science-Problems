# reading the data
aggregated_data <- read.csv('D:/Hackathon/PredictDriverChurn-DataSet/5. aggregated_driver_job_data.csv')
master_data <- read.csv('D:/Hackathon/PredictDriverChurn-DataSet/4. Driver_Master - CSV.csv')
aggregated_data$X <-NULL



# checking for driver id mismatch
'%ni%' <- Negate('%in%')
missing_index <- which( master_data$Driver_ID %ni% aggregated_data$Driver_Id ==T)
master_data<- master_data[-missing_index,]
missing_row <- aggregated_data[missing_index,]
master_data<-master_data[-missing_index,]
# removing the mismatched rows
aggregated_data <-aggregated_data[-missing_index,]

# sorting both the datas as per driver ids
aggregated_data<- aggregated_data[order(aggregated_data$Driver_Id),]
master_data<-master_data[order(master_data$Driver_ID),]
# merging the data
merged_data<- cbind(master_data[master_data$Driver_ID %in% aggregated_data$Driver_Id,],aggregated_data)
# no row mismatach
which(merged_data$Driver_ID != merged_data$Driver_Id)
merged_data$Driver_Id<-NULL
#exporting the data
write.csv(merged_data,'6. merged_aggregated_data_jan.csv')
