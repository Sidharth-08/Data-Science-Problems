library(readr)
churn_status_with_merged_data <- read_csv("D:/Hackathon/PredictDriverChurn-DataSet/8. churn_status_with_merged_data.csv")
churn_status_with_merged_data$X1<-NULL
sddata<-churn_status_with_merged_data
sddata$dr_onduty<-NULL
sddata$Tenure.in_categories.<-as.factor(sddata$Tenure.in_categories.)
sddata$churn_status <- as.factor(sddata$churn_status)
sddata$dr_state <- as.factor(sddata$dr_state)

plot(sddata$churn_status~sddata$Tenure.in_categories., xlab = 'TIme',ylab = 'Churn status',main = 'Churn rate as per the tenure of drivers')
options(scipen = 999)
plot(sddata$rides_in_last_3_month~sddata$sum_total_payment_3_month.,xlab = 'total_payments in last 3 months',ylab = 'rides in last 3 months', main = 'Rides v/s Payments')
plot(states_drivers,ylab='Number of drivers',main = 'Drivers in every state')




ggplot(sddata, aes(x = sddata$rides_in_last_6_month, y = sddata$average_total_payment_6_month.))+
  geom_bar(
    aes(fill = churn_status), stat = "identity", color = "white",
    position = position_dodge(0.9)
  )+xlab('Rides in last 6 months')+ylab('Payments in last 6 months')
   


plot(sddata$average_total_payment_6_month.~sddata$churn_status,,xlab='Churn status',ylab='Average total payments in 6 months',main='Churn status as per payments')
max(sddata$average_total_payment_6_month.)


plot(sddata$churn_status~sddata$dr_state)


ggplot(sddata,aes(sddata$rides_in_last_3_month~sddata$churn_status))+geom_bar()

View(sddata[sddata$average_total_payment_6_month.>30000,])


hist(sddata$dr_commission)
ggplot(aes(y=sddata$rides_in_last_6_month,x=sddata$churn_status),data = sddata)+geom_line()

sddata$dr_contract <- as.factor(as.character(sddata$dr_contract))

plot(sddata$average_satisfaction_6_month.~sddata$churn_status)
x<- as.numeric(as.character(sddata$average_satisfaction_6_month.))
class(x)
hist(x,as.numeric(sddata$churn_status), xlab = 'TIme',ylab = 'Churn status',main = 'Churn rate as per the tenure of drivers')


p <- ggplot(sddata, aes(churn_status, sddata$total_jobs_6_month.)) +xlab('Churn Status')+ylab('Total jobs in last 6 months')
p+geom_violin()+geom_jitter(alpha=1/2)

ggplot(sddata,aes(sddata$average_non_standard_payment_6_month.,sddata$churn_status))+geom_bar()

table(sddata$dr_contract)
table(sddata$dr_commission)
table(sddata$dr_paytype)
table(sddata$dr_wkbase)

summary(sddata[sddata$churn_status==T,'sum_non_standard_payment_6_month.'])
summary(sddata[sddata$churn_status==F,'sum_non_standard_payment_6_month.'])

summary(sddata[sddata$churn_status==F,'rides_in_last_6_month'])
summary(sddata[sddata$churn_status==T,'rides_in_last_6_month'])


sddata$rides_in_last_6_month
table(sddata$dr_status)
table(sddata$churn_status)

library(dplyr) 
barplot(sddata$dr_state~sddata$churn_status)
