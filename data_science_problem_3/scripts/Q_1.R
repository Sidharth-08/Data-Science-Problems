# training data preprocessing

#puzzle_train_dataset <- read.csv("~/Downloads/Puzzle/puzzle_train_dataset.csv")
puzzle_train_dataset <- read.csv("C:/Users/sidharth.suman/Desktop/hacktemp/puzzle_train_dataset.csv")

# first task to check which customer will default in the training data.

length(unique(puzzle_train_dataset$ids))
colnames(puzzle_train_dataset)

length(which(puzzle_train_dataset$default==''))
train_dataset_without_missing_target <- puzzle_train_dataset[puzzle_train_dataset$default!='',]
train_dataset_without_missing_target$default <- as.logical(as.character(train_dataset_without_missing_target$default))

length(which(is.na(train_dataset_without_missing_target$credit_limit)))
options(scipen = 999)
summary(train_dataset_without_missing_target$credit_limit)
boxplot(train_dataset_without_missing_target$credit_limit)
train_dataset_without_missing_target <- train_dataset_without_missing_target[(!is.na(train_dataset_without_missing_target$credit_limit)),]
train_dataset_without_missing_target <- train_dataset_without_missing_target[(train_dataset_without_missing_target$credit_limit != max(train_dataset_without_missing_target$credit_limit,na.rm = TRUE)),]

length(which(train_dataset_without_missing_target$reason==''))
length(which(train_dataset_without_missing_target$sign==''))
length(which(train_dataset_without_missing_target$gender==''))
length(which(train_dataset_without_missing_target$facebook_profile==''))

length(which(train_dataset_without_missing_target$last_payment==''))
train_dataset_without_missing_target <- train_dataset_without_missing_target[train_dataset_without_missing_target$last_payment!='',]

length(which(train_dataset_without_missing_target$end_last_loan==''))
train_dataset_without_missing_target <- train_dataset_without_missing_target[train_dataset_without_missing_target$end_last_loan!='',]

table(train_dataset_without_missing_target$state)
table(train_dataset_without_missing_target$zip)

table(train_dataset_without_missing_target$channel) # we can remove channel
library(dplyr)
train_dataset_without_missing_target <- select(train_dataset_without_missing_target, -c(channel))

length(which(train_dataset_without_missing_target$job_name==''))
length(which(is.na(train_dataset_without_missing_target$ok_since)))
summary(train_dataset_without_missing_target$ok_since)
train_dataset_without_missing_target[is.na(train_dataset_without_missing_target$ok_since),'ok_since']<- median(train_dataset_without_missing_target$ok_since,na.rm = TRUE)

length(which(is.na(train_dataset_without_missing_target$n_bankruptcies)))
length(which(is.na(train_dataset_without_missing_target$n_defaulted_loans)))
length(which(is.na(train_dataset_without_missing_target$n_issues)))

colnames(train_dataset_without_missing_target)
