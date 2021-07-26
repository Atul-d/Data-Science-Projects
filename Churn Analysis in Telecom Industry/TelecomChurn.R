# Load the Libraries ====

library(Hmisc) # describe() to use
library(visdat)    # For plotting missing value as graphical format.
library(gridExtra) # gridExtra()
library(ggplot2)
library(corrplot)   # For Correlation Plot
library(caret)    # For splitting the data
library(e1071)
library(plyr)
library(ggthemes)
library(caret)
library(MASS)
library(party)
library(RColorBrewer)
library(ROCR)
library(class)
library(rpart)
library(rattle)
library(rpart.plot)
library(pROC)      # For AUC value
library(InformationValue)    # For Optimum cutoff
library(DMwR)  # SMOTE

# Set the Path where file is located.
getwd()
setwd("C:/Users/singh/OneDrive/Desktop/DATA/PROJECTS/Project 3  Churn Analysis in Telecom Industry")
getwd()

# Load the File
churn_df <- read.csv("churn.csv")
View(churn_df)

dim(churn_df)
cat("The dimension of Bank data is: ", dim(churn_df))
cat("The Number of Rows in Bank data has: ", nrow(churn_df))
cat("The Number of Column in Bank data has: ", ncol(churn_df))

names(churn_df)

str(churn_df)

summary(churn_df)

describe(churn_df)

sum(is.na(churn_df))

colSums(is.na(churn_df))

# Viuslizing if there are any missing value present in the data;
vis_miss(churn_df) + theme(axis.text.x = element_text(angle = 90))

count(churn_df, 'gender')

str(churn_df)

# As per problem statement value for few column should have values = Yes or No.
# But in our dataset value is defined as Yes, No and No Internet Service. 
# We need to manually change No Internet Service to No since it means the same thing

# OnlineSecurity
count(churn_df, "OnlineSecurity")
levels(churn_df$OnlineSecurity)
# Replacing No Internet Service to No
levels(churn_df$OnlineSecurity)[levels(churn_df$OnlineSecurity) == "No internet service"] <- "No"
levels(churn_df$OnlineSecurity)

# OnlineBackup
count(churn_df, "OnlineBackup")
levels(churn_df$OnlineBackup)
# Replacing No Internet Service to No
levels(churn_df$OnlineBackup)[levels(churn_df$OnlineBackup) == "No internet service"] <- "No"
levels(churn_df$OnlineBackup)
count(churn_df, "OnlineBackup")


# DeviceProtectionService
count(churn_df, "DeviceProtectionService")
levels(churn_df$DeviceProtectionService)
# Replacing No Internet Service to No
levels(churn_df$DeviceProtectionService)[levels(churn_df$DeviceProtectionService) == "No internet service"] <- "No"
levels(churn_df$DeviceProtectionService)
count(churn_df, "DeviceProtectionService")

# TechnicalHelp
count(churn_df, "TechnicalHelp")
levels(churn_df$TechnicalHelp)
# Replacing No Internet Service to No
levels(churn_df$TechnicalHelp)[levels(churn_df$TechnicalHelp) == "No internet service"] <- "No"
levels(churn_df$TechnicalHelp)
count(churn_df, "TechnicalHelp")

# OnlineTV
count(churn_df, "OnlineTV")
levels(churn_df$OnlineTV)
# Replacing No Internet Service to No
levels(churn_df$OnlineTV)[levels(churn_df$OnlineTV) == "No internet service"] <- "No"
levels(churn_df$OnlineTV)
count(churn_df, "OnlineTV")

# OnlineMovies
count(churn_df, "OnlineMovies")
levels(churn_df$OnlineMovies)
# Replacing No Internet Service to No
levels(churn_df$OnlineMovies)[levels(churn_df$OnlineMovies) == "No internet service"] <- "No"
levels(churn_df$OnlineMovies)
count(churn_df, "OnlineMovies")

str(churn_df)

"There are certain columns in which the value defined in Dataset is not defined
in problem statement and hence replacing that value"

# MultipleConnections
count(churn_df, "MultipleConnections")
levels(churn_df$MultipleConnections)
# Replacing No Internet Service to No
levels(churn_df$MultipleConnections)[levels(churn_df$MultipleConnections) == "No phone service"] <- "No"
levels(churn_df$MultipleConnections)
count(churn_df, "MultipleConnections")

# For SeniorCitizen value defined in dataset is 0, 1 and in Problem statment
# It is defined as Yes or No and hence replacing it

# SeniorCitizen. 
# before converting it inot factor we need to First make all value in 0 and 1.
# There are certain value which is having value 0.029291 and so on.
# So value greater than 0.5 would be 1 and less than would be 0.
churn_df$SeniorCitizen <- ifelse(churn_df$SeniorCitizen >= 0.5, 1, 0)
summary(churn_df$SeniorCitizen)
churn_df$SeniorCitizen <- as.factor(churn_df$SeniorCitizen)
levels(churn_df$SeniorCitizen)
str(churn_df$SeniorCitizen)
count(churn_df, "SeniorCitizen")
# Replacing No Internet Service to No
levels(churn_df$SeniorCitizen)[levels(churn_df$SeniorCitizen) == "0"] <- "No"
levels(churn_df$SeniorCitizen)[levels(churn_df$SeniorCitizen) == "1"] <- "Yes"
levels(churn_df$SeniorCitizen)
count(churn_df, "SeniorCitizen")

# Removing Customer ID column since it's not relevant
churn_df$customerID <- NULL
dim(churn_df)

# As per problem statement tenure column describes how many months 
# a customer got a telephone connection.
str(churn_df$tenure)
# We can convert the Value intot months or years i.e. for each 12 month = 1 Year
tenure_year <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('1Y')
  }else if(tenure > 12 & tenure <= 24){
    return('2Y')
  }else if (tenure > 24 & tenure <= 48){
    return('3Y')
  }else if (tenure > 48 & tenure <=60){
    return('4Y')
  }else if (tenure > 60){
    return('> 5Y')
  }
}
churn_df$tenure <- sapply(churn_df$tenure, tenure_year)
churn_df$tenure <- as.factor(churn_df$tenure)
str(churn_df$tenure)
levels(churn_df$tenure)
count(churn_df, "tenure")

str(churn_df)

# EDA Of the Data

# Seperating Numeric and Categorical data
numeric_cols <- which(sapply(churn_df, is.numeric))
names(numeric_cols)

# Correlation Matrix
corr_matrix <- cor(churn_df[, numeric_cols])
corrplot(corr_matrix, method = "number", type = "lower")
# TotalAmount is correlated and if we want we can remove it.

"If you see the dataset we can see that the first 4 column of our dataset
is related i.e Gender, Senior Citizen, Partners and Dependents"
"Hence visualizing them all at once"
visual1 <- ggplot(churn_df, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

visual2 <- ggplot(churn_df, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

visual3 <- ggplot(churn_df, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
visual4 <- ggplot(churn_df, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

grid.arrange(visual1, visual2, visual3, visual4, ncol=2)

# Gender vs Churn
table(churn_df$gender)
table(churn_df$gender, churn_df$Churn)
tbl <- with(churn_df, table(gender, Churn))
ggplot(as.data.frame(tbl), aes(factor(Churn), Freq, fill = gender)) + 
  geom_col(position = 'dodge')
cat("Majority of people based on Gender have not churned. And Female are 
    the one who are mostly churned")

# SeniorCitizen vs Churn
table(churn_df$SeniorCitizen)
table(churn_df$SeniorCitizen, churn_df$Churn)
tbl <- with(churn_df, table(SeniorCitizen, Churn))
ggplot(as.data.frame(tbl), aes(factor(Churn), Freq, fill = SeniorCitizen)) + 
  geom_col(position = 'dodge')
cat("Majority of people are not Senior Citizen. If the people are not Senior 
    citizen then less than 50% of people have not churned. If the people are 
    Senior Citizen then more than 50% people have churned.")

# Partner vs Churn
table(churn_df$Partner)
table(churn_df$Partner, churn_df$Churn)
tbl <- with(churn_df, table(Partner, Churn))
ggplot(as.data.frame(tbl), aes(factor(Churn), Freq, fill = Partner)) + 
  geom_col(position = 'dodge')
cat("Majority of people are married. If people are married then less than 50%
    of people have Churned. If people are not married then 50-50% people have
    churned or not churned") 

table(churn_df$Partner, churn_df$SeniorCitizen)

# Dependents vs Churn
table(churn_df$Dependents)
table(churn_df$Dependents, churn_df$Churn)
tbl <- with(churn_df, table(Dependents, Churn))
ggplot(as.data.frame(tbl), aes(factor(Churn), Freq, fill = Dependents)) + 
  geom_col(position = 'dodge')

visual5 <- ggplot(churn_df, aes(x=CallService)) + ggtitle("Call Service") + xlab("Call Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

visual6 <- ggplot(churn_df, aes(x=MultipleConnections)) + ggtitle("Multiple Connections") + xlab("Multiple Connections") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

visual7 <- ggplot(churn_df, aes(x=InternetConnection)) + ggtitle("Internet Connections") + xlab("Internet Connections") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

visual8 <- ggplot(churn_df, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

visual9 <- ggplot(churn_df, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

grid.arrange(visual5, visual6, visual7, visual8, visual9, ncol = 2)

# Call Service vs Churn
table(churn_df$CallService)
table(churn_df$CallService, churn_df$Churn)
tbl <- with(churn_df, table(CallService, Churn))
ggplot(as.data.frame(tbl), aes(factor(Churn), Freq, fill = CallService)) + 
  geom_col(position = 'dodge')
cat("Most of the user have phone connection service with them and less
    than 50% of the user has churned who are having Phone connection.")

# MultipleConnections vs Churn
table(churn_df$MultipleConnections)
table(churn_df$MultipleConnections, churn_df$Churn)
tbl <- with(churn_df, table(MultipleConnections, Churn))
ggplot(as.data.frame(tbl), aes(factor(Churn), Freq, fill = MultipleConnections)) + 
  geom_col(position = 'dodge')
cat("Majority of the user does not have MultipleConnections. User who have 
    MultipleConnections there 50-50% chance that user might churn or not.")

# Internet Connection vs Churn
table(churn_df$InternetConnection)
table(churn_df$InternetConnection, churn_df$Churn)
tbl <- with(churn_df, table(InternetConnection, Churn))
ggplot(as.data.frame(tbl), aes(factor(Churn), Freq, fill = InternetConnection)) + 
  geom_col(position = 'dodge')

# Online Security vs Churn
table(churn_df$OnlineSecurity)
table(churn_df$OnlineSecurity, churn_df$Churn)
tbl <- with(churn_df, table(OnlineSecurity, Churn))
ggplot(as.data.frame(tbl), aes(factor(Churn), Freq, fill = OnlineSecurity)) + 
  geom_col(position = 'dodge')

# Online Backup vs Churn
table(churn_df$OnlineBackup)
table(churn_df$OnlineBackup, churn_df$Churn)
tbl <- with(churn_df, table(OnlineBackup, Churn))
ggplot(as.data.frame(tbl), aes(factor(Churn), Freq, fill = OnlineBackup)) + 
  geom_col(position = 'dodge')

# Device Protection Service vs Churn
table(churn_df$DeviceProtectionService)
table(churn_df$DeviceProtectionService, churn_df$Churn)
tbl <- with(churn_df, table(DeviceProtectionService, Churn))
ggplot(as.data.frame(tbl), aes(factor(Churn), Freq, fill = DeviceProtectionService)) + 
  geom_col(position = 'dodge')

# TechnicalHelp vs Churn
table(churn_df$TechnicalHelp)
table(churn_df$TechnicalHelp, churn_df$Churn)
tbl <- with(churn_df, table(TechnicalHelp, Churn))
ggplot(as.data.frame(tbl), aes(factor(Churn), Freq, fill = TechnicalHelp)) + 
  geom_col(position = 'dodge')

# OnlineTV vs Churn
table(churn_df$OnlineTV)
table(churn_df$OnlineTV, churn_df$Churn)
tbl <- with(churn_df, table(OnlineTV, Churn))
ggplot(as.data.frame(tbl), aes(factor(Churn), Freq, fill = OnlineTV)) + 
  geom_col(position = 'dodge')

# OnlineMovies vs Churn
table(churn_df$OnlineMovies)
table(churn_df$OnlineMovies, churn_df$Churn)
tbl <- with(churn_df, table(OnlineMovies, Churn))
ggplot(as.data.frame(tbl), aes(factor(Churn), Freq, fill = OnlineMovies)) + 
  geom_col(position = 'dodge')

# Agreement vs Churn
table(churn_df$Agreement)
table(churn_df$Agreement, churn_df$Churn)
tbl <- with(churn_df, table(Agreement, Churn))
ggplot(as.data.frame(tbl), aes(factor(Churn), Freq, fill = Agreement)) + 
  geom_col(position = 'dodge')

# BillingMethod vs Churn
table(churn_df$BillingMethod)
table(churn_df$BillingMethod, churn_df$Churn)
tbl <- with(churn_df, table(BillingMethod, Churn))
ggplot(as.data.frame(tbl), aes(factor(Churn), Freq, fill = BillingMethod)) + 
  geom_col(position = 'dodge')

# PaymentMethod vs Churn
table(churn_df$PaymentMethod)
table(churn_df$PaymentMethod, churn_df$Churn)
tbl <- with(churn_df, table(PaymentMethod, Churn))
ggplot(as.data.frame(tbl), aes(factor(Churn), Freq, fill = PaymentMethod)) + 
  geom_col(position = 'dodge')

# Splitting Original Data
set.seed(123)
data <- createDataPartition(y = churn_df$Churn, p = 0.7, list = FALSE)
train <- churn_df[data, ]
test <- churn_df[-data, ]

dim(train);dim(test)

cat("The dimension of Train data is: ", dim(train))
cat("The Number of Rows Train data has: ", nrow(train))
cat("The Number of Column Train data has: ", ncol(train))

cat("The dimension of Train data is: ", dim(test))
cat("The Number of Rows Train data has: ", nrow(test))
cat("The Number of Column Train data has: ", ncol(test))

model_tree <- rpart(Churn ~ .,  train, method = 'class')

fancyRpartPlot(model_tree)

printcp(model_tree)

plotcp(model_tree)

# Prediction on Train data
pred_tree_train <- predict(model_tree, train, type = "class")
head(pred_tree_train)

conf <- table(pred_tree_train, train$Churn)
conf
caret::confusionMatrix(data = pred_tree_train, reference = train$Churn)

# Prediction on Test data
pred_test_tree <- predict(model_tree, test, type = "class")
head(pred_test_tree)

conf_test_tree <- table(pred_test_tree, test$Churn)
conf_test_tree
caret::confusionMatrix(data = pred_test_tree, reference = test$Churn)

false_negative_model <- 753
false_positive_model <- 248
error <- false_negative_model + false_positive_model
Accuracy <- (1265 + 1434) / 3700
cat("Confusion Matrix for Full Decission Model: ", "\n",
    "Accuracy for Full model with threshold = 0.5 is ", Accuracy, "\n",
    "Type-1 Error is ", false_positive_model, "\n", 
    "Type-2 Error is ", false_negative_model, "\n",
    "Sum of Type-1 & Type-2 error is", error)

model_tree_1 <- rpart(Churn ~ .,  train, method = 'class', 
                      control = list(maxdepth = 5, mindepth = 1))

fancyRpartPlot(model_tree_1)

printcp(model_tree_1)

plotcp(model_tree_1)

# Prediction on Train data
pred_tree_train_1 <- predict(model_tree_1, train, type = "class")
head(pred_tree_train_1)

conf_1 <- table(pred_tree_train_1, train$Churn)
conf_1
caret::confusionMatrix(data = pred_tree_train_1, reference = train$Churn)

# Prediction on Test data
pred_test_tree_1 <- predict(model_tree_1, test, type = "class")
head(pred_test_tree_1)

conf_test_tree_1 <- table(pred_test_tree_1, test$Churn)
conf_test_tree_1
caret::confusionMatrix(data = pred_test_tree_1, reference = test$Churn)

false_negative_model1 <- 792
false_positive_model1 <- 235
error_1 <- false_negative_model1 + false_positive_model1
Accuracy_1 <- (1226 + 1447) / 3700
cat("Confusion Matrix for Full Decission Model with MaxDepth = 5: ", "\n",
    "Accuracy for Full model with threshold = 0.5 is ", Accuracy_1, "\n",
    "Type-1 Error is ", false_positive_model1, "\n", 
    "Type-2 Error is ", false_negative_model1, "\n",
    "Sum of Type-1 & Type-2 error is", error_1)

model_tree_2 <- rpart(Churn ~ .,  train, method = 'class', 
                      control = list(maxdepth = 5, mindepth = 1), cp = 0.02)

fancyRpartPlot(model_tree_2)

printcp(model_tree_2)

plotcp(model_tree_2)

# Prediction on Train data
pred_tree_train_2 <- predict(model_tree_2, train, type = "class")
head(pred_tree_train_2)

conf_2 <- table(pred_tree_train_2, train$Churn)
conf_2
caret::confusionMatrix(data = pred_tree_train_2, reference = train$Churn)

# Prediction on Test data
pred_test_tree_2 <- predict(model_tree_2, test, type = "class")
head(pred_test_tree_2)

conf_test_tree_2 <- table(pred_test_tree_2, test$Churn)
conf_test_tree_2
caret::confusionMatrix(data = pred_test_tree_2, reference = test$Churn)

false_negative_model2 <- 740
false_positive_model2 <- 446
error_2 <- false_negative_model2 + false_positive_model2
Accuracy_2 <- (1278 + 1236) / 3700
cat("Confusion Matrix for Full Decission Model with MaxDepth = 5 &
    cost penalty = 0.02: ", "\n",
    "Accuracy for Full model with threshold = 0.5 is ", Accuracy_2, "\n",
    "Type-1 Error is ", false_positive_model2, "\n", 
    "Type-2 Error is ", false_negative_model2, "\n",
    "Sum of Type-1 & Type-2 error is", error_2)

cat("--------------------*********************-----------------------",'\n',
    "-------Decission Tree on FULL MODEL with various Parameter:------",'\n',    
    "Confusion Matrix for Full Decission Model: ", "\n",
    "Accuracy for Full model with threshold = 0.5 is ", Accuracy, "\n",
    "Type-1 Error is ", false_positive_model, "\n", 
    "Type-2 Error is ", false_negative_model, "\n",
    "Sum of Type-1 & Type-2 error is", error, 
    "----------------------------------------------------------------", "\n",
    "Confusion Matrix for Full Decission Model with MaxDepth = 5: ", "\n",
    "Accuracy for Full model with threshold = 0.5 is ", Accuracy_1, "\n",
    "Type-1 Error is ", false_positive_model1, "\n", 
    "Type-2 Error is ", false_negative_model1, "\n",
    "Sum of Type-1 & Type-2 error is", error_1, '\n',
    "----------------------------------------------------------------", '\n',
    "Confusion Matrix for Full Decission Model with MaxDepth = 5 &
    cost penalty = 0.02: ", "\n",
    "Accuracy for Full model with threshold = 0.5 is ", Accuracy_2, "\n",
    "Type-1 Error is ", false_positive_model2, "\n", 
    "Type-2 Error is ", false_negative_model2, "\n",
    "Sum of Type-1 & Type-2 error is", error_2)

# Based on above 3 model: Full Model without any parameter is doing good Job.

# Let us remove certain variable which are highly correlated or
# not having significant importance with out Dependent Variable

# Correlation Matrix
corr_matrix <- cor(churn_df[, numeric_cols])
corrplot(corr_matrix, method = "number", type = "lower")
# TotalAmount is correlated.

dim(churn_df)

churn_df$TotalAmount <- NULL

dim(churn_df)

which(sapply(churn_df, is.factor))

count(churn_df,"gender")
count(churn_df,"SeniorCitizen")
count(churn_df,"Partner")
count(churn_df,"Dependents")
count(churn_df,"tenure")
count(churn_df,"CallService")
count(churn_df,"MultipleConnections")
count(churn_df,"InternetConnection")
count(churn_df,"OnlineSecurity")
count(churn_df,"OnlineBackup")
count(churn_df,"DeviceProtectionService")
count(churn_df,"TechnicalHelp")
count(churn_df,"OnlineTV")
count(churn_df,"OnlineMovies")
count(churn_df,"Agreement")
count(churn_df,"BillingMethod")
count(churn_df,"PaymentMethod")

"Gender does not have any relation if the customer is going to churn or not.
Based on Domain Knowledge we can remove this variable.
PaymentMethod also does not have any kind of impact on Churn because it is related
to payment and as per domain knowledge it does not have any kind of relation 
with out dependent variable hence we can remove it if we want."

churn_df$gender <- NULL
churn_df$PaymentMethod <- NULL
dim(churn_df)

set.seed(123)
data_sig <- createDataPartition(y = churn_df$Churn, p = 0.7, list = FALSE)
train_sig <- churn_df[data_sig, ]
test_sig <- churn_df[-data_sig, ]
View(train_sig)
View(test_sig)
dim(train_sig);dim(test_sig)

cat("The dimension of Train data is: ", dim(train_sig), '\n',
    "The Number of Rows Train data has: ", nrow(train_sig), '\n',
    "The Number of Column Train data has: ", ncol(train))

cat("The dimension of Train data is: ", dim(test_sig), '\n',
    "The Number of Rows Train data has: ", nrow(test_sig), '\n',
    "The Number of Column Train data has: ", ncol(test_sig))

model_sig <- rpart(Churn ~ .,  train_sig, method = 'class')

fancyRpartPlot(model_sig)

printcp(model_sig)

plotcp(model_sig)

# Prediction on Train data
pred_tree_sig_train <- predict(model_sig, train_sig, type = "class")
head(pred_tree_sig_train)

conf_sig <- table(pred_tree_sig_train, train_sig$Churn)
conf_sig
caret::confusionMatrix(data = pred_tree_sig_train, reference = train_sig$Churn)

# Prediction on Test data
pred_test_sig_tree <- predict(model_sig, test, type = "class")
head(pred_test_sig_tree)

conf_test_sig_tree <- table(pred_test_sig_tree, test_sig$Churn)
conf_test_sig_tree
caret::confusionMatrix(data = pred_test_sig_tree, reference = test_sig$Churn)

false_negative_model_sig <- 753
false_positive_model_sig <- 248
error_sig <- false_negative_model_sig + false_positive_model_sig
Accuracy_sig <- (1265 + 1434) / 3700
cat("Confusion Matrix for Full Decission Model: ", "\n",
    "Accuracy for Full model with threshold = 0.5 is ", Accuracy_sig, "\n",
    "Type-1 Error is ", false_positive_model_sig, "\n", 
    "Type-2 Error is ", false_negative_model_sig, "\n",
    "Sum of Type-1 & Type-2 error is", error_sig)

model_sig_1 <- rpart(Churn ~ .,  train_sig, method = 'class', 
                     control = list(maxdepth = 5, mindepth = 1))

fancyRpartPlot(model_sig_1)

printcp(model_sig_1)

plotcp(model_sig_1)

# Prediction on Train data
pred_tree_sig_train_1 <- predict(model_sig_1, train_sig, type = "class")
head(pred_tree_sig_train_1)

conf_sig_1 <- table(pred_tree_sig_train_1, train_sig$Churn)
conf_sig_1
caret::confusionMatrix(data = pred_tree_sig_train_1, reference = train_sig$Churn)

# Prediction on Test data
pred_test_sig_tree_1 <- predict(model_sig_1, test_sig, type = "class")
head(pred_test_sig_tree_1)

conf_test_sig_tree_1 <- table(pred_test_sig_tree_1, test_sig$Churn)
conf_test_sig_tree_1
caret::confusionMatrix(data = pred_test_sig_tree_1, reference = test_sig$Churn)

false_negative_model_sig1 <- 792
false_positive_model_sig1 <- 235
error__sig1 <- false_negative_model_sig1 + false_positive_model_sig1
Accuracy__sig1 <- (1226 + 1447) / 3700
cat("Confusion Matrix for Full Decission Model with MaxDepth = 5: ", "\n",
    "Accuracy for Full model with threshold = 0.5 is ", Accuracy__sig1, "\n",
    "Type-1 Error is ", false_positive_model_sig1, "\n", 
    "Type-2 Error is ", false_negative_model_sig1, "\n",
    "Sum of Type-1 & Type-2 error is", error__sig1)

"Even if we remove the value Accuracy, Sensitivity, specificity and all other
values are same for both the model."

cat("-----------------------Final Output:-------------------------", "\n", '\n')
cat("--------------------*********************-----------------------",'\n',
    "-------Decission Tree on FULL MODEL with various Parameter:------",'\n',    
    "Confusion Matrix for Full Decission Model: ", "\n",
    "Accuracy for Full model with threshold = 0.5 is ", Accuracy, "\n",
    "Type-1 Error is ", false_positive_model, "\n", 
    "Type-2 Error is ", false_negative_model, "\n",
    "Sum of Type-1 & Type-2 error is", error, "\n",
    "Sensitivity : 0.6269          
     Specificity : 0.8526          
     Pos Pred Value : 0.8361          
     Neg Pred Value : 0.6557          
     Prevalence : 0.5454          
     Detection Rate : 0.3419          
     Detection Prevalence : 0.4089          
     Balanced Accuracy : 0.7397          
     Positive' Class : No    ")

dim(test)

test$Prediction <- pred_test_tree

dim(test)

View(test)
