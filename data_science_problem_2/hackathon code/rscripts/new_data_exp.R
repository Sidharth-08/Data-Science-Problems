str(DriverInfo_2017_12)
DriverInfo_2018_01
DriverInfo_2018_02

names(DriverInfo_2017_12)<- c('Driver_Id','Job_ID','Drop_City',	'Drop_State',	'Drop_Country',	'Distance_Miles','Deliverd_DateTime',	'Standard_Pay_Ammount',	'Non_Standard_Pay_Ammount','Total_Pay','VOC_SENT','VOC_SATISFACTION')
names(DriverInfo_2018_02)<- c('Driver_Id','Job_ID','Drop_City',	'Drop_State',	'Drop_Country',	'Distance_Miles','Deliverd_DateTime',	'Standard_Pay_Ammount',	'Non_Standard_Pay_Ammount','Total_Pay','VOC_SENT','VOC_SATISFACTION')
names(DriverInfo_2018_01)<- c('Driver_Id','Job_ID','Drop_City',	'Drop_State',	'Drop_Country',	'Distance_Miles','Deliverd_DateTime',	'Standard_Pay_Ammount',	'Non_Standard_Pay_Ammount','Total_Pay','VOC_SENT','VOC_SATISFACTION')


DriverInfo_2017_12$VOC_SENT<-NULL

library(svMisc)

drivers <- unique(DriverInfo_2017_12$Driver_Id)
driver_rides_detail <- data.frame()

for(i in 1:length(drivers)){
  
  print(i)
  #progress(i,progress.bar = T,max.value = length(drivers))
  temp_driver_data<- subset(DriverInfo_2017_12,Driver_Id==drivers[i],)
  driver_rides_detail[i,'Driver_Id'] <- drivers[i]
  city_max <- as.data.frame(table(temp_driver_data$Drop_City))
  city_max$Var1 <- as.character(city_max$Var1)
  city<-city_max[order(city_max$Freq,decreasing = T),'Var1'][1]
  driver_rides_detail[i,'max_rides_to_city'] <- city
  driver_rides_detail[i,'Avg_dist'] <- mean(temp_driver_data$Distance_Miles,na.rm = T)
  driver_rides_detail[i,'Total_dist'] <- sum(temp_driver_data$Distance_Miles,na.rm = T)
  
}
df1<-driver_rides_detail
###########################################################
DriverInfo_2018_01$Distance_Miles <- as.numeric(as.character(DriverInfo_2018_01$Distance_Miles))
drivers <- unique(DriverInfo_2018_01$Driver_Id)
driver_rides_detail <- data.frame()

for(i in 1:length(drivers)){
  
  print(i)
  #progress(i,progress.bar = T,max.value = length(drivers))
  temp_driver_data<- subset(DriverInfo_2018_01,Driver_Id==drivers[i],)
  driver_rides_detail[i,'Driver_Id'] <- drivers[i]
  city_max <- as.data.frame(table(temp_driver_data$Drop_City))
  city_max$Var1 <- as.character(city_max$Var1)
  city<-city_max[order(city_max$Freq,decreasing = T),'Var1'][1]
  driver_rides_detail[i,'max_rides_to_city'] <- city
  driver_rides_detail[i,'Avg_dist'] <- mean(temp_driver_data$Distance_Miles,na.rm = T)
  driver_rides_detail[i,'Total_dist'] <- sum(temp_driver_data$Distance_Miles,na.rm = T)
  
}
df2<-driver_rides_detail
#############################################################
drivers <- unique(DriverInfo_2018_02$Driver_Id)
driver_rides <- data.frame()

for(i in 1:length(drivers)){
  
  print(i)
  #progress(i,progress.bar = T,max.value = length(drivers))
  temp_driver_data<- subset(DriverInfo_2018_02,Driver_Id==drivers[i],)
  driver_rides_detail[i,'Driver_Id'] <- drivers[i]
  city_max <- as.data.frame(table(temp_driver_data$Drop_City))
  city_max$Var1 <- as.character(city_max$Var1)
  city<-city_max[order(city_max$Freq,decreasing = T),'Var1'][1]
  driver_rides_detail[i,'max_rides_to_city'] <- city
  driver_rides_detail[i,'Avg_dist'] <- mean(temp_driver_data$Distance_Miles,na.rm = T)
  driver_rides_detail[i,'Total_dist'] <- sum(temp_driver_data$Distance_Miles,na.rm = T)
  
}
df3<-driver_rides_detail
write.csv(df3,'df3.csv')