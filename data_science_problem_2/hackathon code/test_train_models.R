# testing and training model
names()<- names(regression_data_jan[,-48])
# predicting using alredy made model
regression_data_jan$X1<-NULL
regression_data_test_jan$X1<-NULL
log_bin_test <- regression_data_test_jan
load(file = "re_jan_train.rda")

names(regression_data_test_jan) <- make.names(names(regression_data_test_jan))
pred<- predict(rf_jan_train,regression_data_test_jan[,-1])
names(regression_data_test_jan[,-1]) <- 
names(regression_data_jan)
x<- round(pred,digits = 5)
y<-as.factor(regression_data_test_jan$churn_status)
unique(y)

sdf<-as.data.frame(cbind(regression_data_test_jan$churn_status,x))
sdf[sdf$x>=0.2,'churn_status']<-'High risk'
sdf[sdf$x>0.17 & sdf$x<0.2,'churn_status']<-'Medium risk'
sdf[sdf$x<=0.17, 'churn_status']<-'Low risk'
sdf<-sdf[order(sdf$x),]
sdf[sdf$x>=0.3,'will_churn']<-1
sdf[sdf$x<0.3, 'will_churn']<-0

names(sdf)<-c('Actual','Prob of churning','Category','will_churn')
accuracy(sdf$Actual,sdf$will_churn)

library(e1071)
library(caret)

#write.csv( ,'rf_next_one_month.csv')
sdf$Actual<-as.factor(sdf$Actual)
sdf$will_churn <- as.factor(sdf$will_churn)
confusionMatrix(sdf$will_churn,sdf$Actual)

#####################################################################

# prune tree

library(rpart)
library(rpart.plot)

set.seed(123)
tree<- rpart(rf_jan_train)
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(tree, cp = bestcp)

# confusion matrix (training data)
predict()
conf.matrix <- table(regression_data_test_jan$churn_status, predict(tree.pruned,regression_data_test_jan[,-48],type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

plot(tree.pruned)
text(tree.pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)
#####################################################################

# logistic regression model

require(mgcv)

log_reg_bin <- regression_data_jan
# Making output variable as factors <categorial>

set.seed(42)

log_bin_train <- log_reg_bin

# Building logistic regression model
log_bin_model <- glm(formula = churn_status~.,
                     
                     data = log_bin_train,
                     
                     family = binomial(link = "logit"))

summary(log_bin_model)

save(log_bin_model,file="log_bin_model.rda")
load('log_bin_model.rda')

pred<-predict(log_bin_model,log_bin_test[,-48],type = 'response')
pred<-as.numeric(pred)
x<-round(pred,digits = 5)



y<-as.factor(log_bin_test$churn_status)
unique(y)

sdf<-as.data.frame(cbind(log_bin_test$churn_status,x))
sdf[sdf$x>=0.2,'churn_status']<-'High risk'
sdf[sdf$x>0.17 & sdf$x<0.2,'churn_status']<-'Medium risk'
sdf[sdf$x<=0.17, 'churn_status']<-'Low risk'
sdf<-sdf[order(sdf$x),]
sdf[sdf$x>=0.2,'will_churn']<-1
sdf[sdf$x<0.2, 'will_churn']<-0

names(sdf)<-c('Actual','Prob of churning','Category','will_churn')

accuracy(sdf$Actual,sdf$will_churn) #93.64

library(e1071)
sdf$Actual <- as.factor(sdf$Actual)
sdf$will_churn <- as.factor(sdf$will_churn)
confusionMatrix(sdf$will_churn,sdf$Actual)


############################################################################
load(file = 'rf_jan_train.rda')
library(readxl)
library(Metrics)
library(randomForest)
library(ggplot2)
library(miscTools)
library(caret)


new_name_train_data <- regression_data_jan
set.seed(150)
names(new_name_train_data) <- make.names(names(regression_data_jan))
rf_jan_train <- randomForest(churn_status ~ ., data = new_name_train_data,ntree =50,nodesize=10)

new_name_test_data <- regression_data_test_jan
names(new_name_test_data)<-make.names(names(regression_data_test_jan))
pred <-predict(rf_jan_train,new_name_test_data[,-48])

varImpPlot(rf_jan_train,sort = T,main = 'variable importance for Revenue')


p <- ggplot(aes(x=actual,y=pred),
            data = data.frame(actual = new_name_test_data$churn_status,pred = pred))
p + geom_point() +
  geom_abline(color = 'red') +
  ggtitle("Random Forest Regression for Churn rate")


save(rf_jan_train,file = 'rf_jan_train.rda')
rSquared(new_name_test_data$churn_status,new_name_test_data$churn_status - pred)
mse(new_name_test_data$churn_status,pred)
rmse(new_name_test_data$churn_status,pred)
mape(new_name_test_data$churn_status,pred)

x<- data.frame(abs(round(pred)),new_name_test_data$churn_status)
View(x[x$abs.round.pred..!= x$new_name_test_data.churn_status,])

#########################################################################################
plot_coeffs_S <- function(mlr_model,label) {
  coeffs <- sort(coefficients(mlr_model), decreasing = TRUE)  ### changed
  
  mp <-barplot(coeffs, col="#3F97D0", xaxt='n', main=label)
  lablist <- names(coeffs)
  
  text(mp, par("usr")[3], labels = lablist, srt = 40, adj = c(.8,.8), xpd = TRUE,cex = .9)
}


mult_reg <- lm(formula = `churn_status`~.,data = regression_data_jan)

coefficients <- mult_reg$coefficients
features <- names(coef(mult_reg))
imp_df<- as.data.frame(cbind(features,coefficients))
imp_df <- imp_df[order(coefficients,decreasing = T),]

plot_coeffs_S(mult_reg,'Regression Coefficients for Page Views')
regression_data<- make.names(names(regression_data))
pred<-predict(mult_reg,regression_data[,-48])

summary(mult_reg)
mse(test_df$churn_status,pred)
rmse(test_df$churn_status,pred)
mape(test_df$churn_status,pred)
summary(pred)
x<-data.frame(pred,regression_data$churn_status)
x[x$pred>0.08,'res'] <- 1
x[x$pred<=0.08,'res'] <- 0
nrow(x[x$res != x$regression_data.churn_status,])

