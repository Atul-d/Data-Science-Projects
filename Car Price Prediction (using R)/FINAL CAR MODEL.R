# Load the Library
library(Hmisc)
library(e1071)          # To calculate Skewness
library(ggplot2)        # To plot visualization diagram
library(gridExtra)      # To plot to visualization graph at one time.
library(GGally)         # To plot correlation using ggcorr()
library(dplyr)          # To use some of dlyr function like select etc.
library(caTools)        # To use sample.split()
library(Metrics)        # To calculate rmse()
library(MASS)           # To use stepAIC function
library(car)            # To use VIF()
library(ggpubr)         
library(lmtest)         # To do bptest() or breusch-pagan test

# Set the Path
getwd()
setwd("C:/Users/singh/OneDrive/Desktop/DATA/PROJECTS/PROJECTS COMPLETED/Linear Regression on Car Price Prediction -- R")
getwd()

# Load the File
car <- read.csv("carmodel.csv")
dim(car)

# Describe the Model
describe(car)
# Some of the variable contains value = ?. Let's find out which all variable or column contains it.
# normalized.losses, num.of.doors, bore, stroke, horsepower, peak.rpm, price. are the variable having ?.
names(car)

# View the Model
View(car)

# normalized.losses has value = ?.
str(car$normalized.losses)
levels(car$normalized.losses)
# There are certain levels having value i.e. ? and we will convert that level to NA.
levels(car$normalized.losses)[levels(car$normalized.losses) == "?"] <- "NA"
levels(car$normalized.losses)
# Converting normalized.losses to numeric.
car$normalized.losses <- as.numeric(as.character(car$normalized.losses))
summary(car$normalized.losses)
hist(car$normalized.losses)
skewness(car$normalized.losses, na.rm = TRUE)
car$normalized.losses[is.na(car$normalized.losses)] <- median(car$normalized.losses, na.rm = TRUE)
summary(car$normalized.losses)
skewness(car$normalized.losses)

# num.of.doors has value = ?
str(car$num.of.doors)
levels(car$num.of.doors)
# There are certain levels having value i.e. ? and we will convert that level to NA.
levels(car$num.of.doors)[levels(car$num.of.doors) == "?"] <- "NA"
summary(car$num.of.doors)
# Replacing NA with mode
levels(car$num.of.doors)[levels(car$num.of.doors) == "NA"] <- "four"
summary(car$num.of.doors)

# Bore has value = ?
str(car$bore)
levels(car$bore)
# There are certain levels having value i.e. ? and we will convert that level to NA.
levels(car$bore)[levels(car$bore) == "?"] <- "NA"
levels(car$bore)
# Converting bore to numeric.
car$bore <- as.numeric(as.character(car$bore))
summary(car$bore)
hist(car$bore)
skewness(car$bore, na.rm = TRUE)
car$bore[is.na(car$bore)] <- round(mean(car$bore, na.rm = TRUE), 2)
summary(car$bore)
car$bore <- round(car$bore, 2)

# stroke has value = ?
str(car$stroke)
levels(car$stroke)
# There are certain levels having value i.e. ? and we will convert that level to NA.
levels(car$stroke)[levels(car$stroke) == "?"] <- "NA"
levels(car$stroke)
# Converting normalized.losses to numeric.
car$stroke <- as.numeric(as.character(car$stroke))
summary(car$stroke)
hist(car$stroke)
skewness(car$stroke, na.rm = TRUE)
car$stroke[is.na(car$stroke)] <- median(car$stroke, na.rm = TRUE)
summary(car$stroke)
skewness(car$stroke)

# horsepower has value = ?
str(car$horsepower)
levels(car$horsepower)
# There are certain levels having value i.e. ? and we will convert that level to NA.
levels(car$horsepower)[levels(car$horsepower) == "?"] <- "NA"
levels(car$horsepower)
# Converting normalized.losses to numeric.
car$horsepower <- as.numeric(as.character(car$horsepower))
summary(car$horsepower)
hist(car$horsepower)
skewness(car$horsepower, na.rm = TRUE)
car$horsepower[is.na(car$horsepower)] <- median(car$horsepower, na.rm = TRUE)
summary(car$horsepower)
skewness(car$horsepower)

# peak.rpm has ? value
str(car$peak.rpm)
levels(car$peak.rpm)
# There are certain levels having value i.e. ? and we will convert that level to NA.
levels(car$peak.rpm)[levels(car$peak.rpm) == "?"] <- "NA"
levels(car$peak.rpm)
# Converting normalized.losses to numeric.
car$peak.rpm <- as.numeric(as.character(car$peak.rpm))
summary(car$peak.rpm)
hist(car$peak.rpm)
skewness(car$peak.rpm, na.rm = TRUE)
car$peak.rpm[is.na(car$peak.rpm)] <- median(car$peak.rpm, na.rm = TRUE)
summary(car$peak.rpm)
skewness(car$peak.rpm)

# price has value = ?
str(car$price)
levels(car$price)
# There are certain levels having value i.e. ? and we will convert that level to NA.
levels(car$price)[levels(car$price) == "?"] <- "NA"
levels(car$price)
# Converting normalized.losses to numeric.
car$price <- as.numeric(as.character(car$price))
summary(car$price)
hist(car$price)
skewness(car$price, na.rm = TRUE)
car$price[is.na(car$price)] <- median(car$price, na.rm = TRUE)
summary(car$price)
skewness(car$price)

# Symboling needs to be converted to factor
car$symboling <- as.factor(car$symboling)

# Visualization of the Data.
numeric_Cols <- which(sapply(car, is.numeric))
names(numeric_Cols)

# normalized.losses vs price.
str(car$normalized.losses)
summary(car$normalized.losses)
boxplot(car$normalized.losses)
skewness(car$normalized.losses)
ggplot(car, aes(x=normalized.losses)) + geom_density()
ggplot(car, aes(x = normalized.losses, y = price)) + geom_point() + geom_smooth(method=lm)
# There is no relation between normalized.losses and price
# There are certain outlier in the data i.e. greater than 200.
car$normalized.losses[car$normalized.losses > 200] <- median(car$normalized.losses)
skewness(car$normalized.losses)
# There is no Relation between normalized.losses and Price.

# wheel.base vs Price
str(car$wheel.base)
summary(car$wheel.base)
boxplot(car$wheel.base)
skewness(car$wheel.base)
ggplot(car, aes(x=wheel.base)) + geom_density()
ggplot(car, aes(x = wheel.base, y = price)) + geom_point() + geom_smooth(method=lm)
# There is Slightly Positive relation between Wheel Base and Price.

# length vs Price
str(car$length)
summary(car$length)
boxplot(car$length)
skewness(car$length)
ggplot(car, aes(x=length)) + geom_density()
ggplot(car, aes(x = length, y = price)) + geom_point() + geom_smooth(method=lm)
# Has slightly positive relation with Price of the car.

# width vs price
str(car$width)
summary(car$width)
boxplot(car$width)
skewness(car$width)
ggplot(car, aes(x=width)) + geom_density()
ggplot(car, aes(x = width, y = price)) + geom_point() + geom_smooth(method=lm)
# Has slightly positive relation with Price of the car.

# height vs Price
str(car$height)
summary(car$height)
boxplot(car$height)
skewness(car$height)
ggplot(car, aes(x=height)) + geom_density()
ggplot(car, aes(x = height, y = price)) + geom_point() + geom_smooth(method=lm)
# No relation with Y variable.

# curb.weight vs price
str(car$curb.weight)
summary(car$curb.weight)
boxplot(car$curb.weight)
skewness(car$curb.weight)
ggplot(car, aes(x=curb.weight)) + geom_density()
ggplot(car, aes(x = curb.weight, y = price)) + geom_point() + geom_smooth(method=lm)
# Positive Relation with Y

# engine.size vs price
str(car$engine.size)
summary(car$engine.size)
boxplot(car$engine.size)
skewness(car$engine.size)
ggplot(car, aes(x=engine.size)) + geom_density()
ggplot(car, aes(x = engine.size, y = price)) + geom_point() + geom_smooth(method=lm)
# There are some outliers in the data but engine size can have this much value.
# Positive relation with Y

# bore vs price
str(car$bore)
summary(car$bore)
boxplot(car$bore)
skewness(car$bore)
ggplot(car, aes(x=bore)) + geom_density()
ggplot(car, aes(x = bore, y = price)) + geom_point() + geom_smooth(method=lm)
# Has slightly positive relation with Y

# stroke vs price
str(car$stroke)
summary(car$stroke)
boxplot(car$stroke)
skewness(car$stroke)
ggplot(car, aes(x=stroke)) + geom_density()
ggplot(car, aes(x = stroke, y = price)) + geom_point() + geom_smooth(method=lm)
# No relation with Y

# compression.ratio vs price
str(car$compression.ratio)
summary(car$compression.ratio)
boxplot(car$compression.ratio)
skewness(car$compression.ratio)
ggplot(car, aes(x=compression.ratio)) + geom_density()
ggplot(car, aes(x = compression.ratio, y = price)) + geom_point() + geom_smooth(method=lm)
# Not at all good variable and should be removed.

# horsepower vs price
str(car$horsepower)
summary(car$horsepower)
boxplot(car$horsepower)
skewness(car$horsepower)
ggplot(car, aes(x=horsepower)) + geom_density()
ggplot(car, aes(x = horsepower, y = price)) + geom_point() + geom_smooth(method=lm)
# Positive relation with Y variable.

# peak.rpm vs price
str(car$peak.rpm)
summary(car$peak.rpm)
boxplot(car$peak.rpm)
skewness(car$peak.rpm)
ggplot(car, aes(x=peak.rpm)) + geom_density()
ggplot(car, aes(x = peak.rpm, y = price)) + geom_point() + geom_smooth(method=lm)
# No relation with Y

# city.mpg vs price
str(car$city.mpg)
summary(car$city.mpg)
boxplot(car$city.mpg)
skewness(car$city.mpg)
ggplot(car, aes(x=city.mpg)) + geom_density()
ggplot(car, aes(x = city.mpg, y = price)) + geom_point() + geom_smooth(method=lm)
# Negative relation with Y

# highway.mpg vs price
str(car$highway.mpg)
summary(car$highway.mpg)
boxplot(car$highway.mpg)
skewness(car$highway.mpg)
ggplot(car, aes(x=highway.mpg)) + geom_density()
ggplot(car, aes(x = highway.mpg, y = price)) + geom_point() + geom_smooth(method=lm)
# negative relation with Y

str(car)

# Visualization of Categorical data
cat_cols <- which(sapply(car, is.factor))
names(cat_cols)

# symboling vs price
summary(car$symboling)
ggplot(car, aes(x = symboling)) + geom_bar() +
  geom_text(stat = 'count',aes(label =..count..), vjust = -0.2)
grid.arrange(ggplot(car, aes(x = symboling)) + geom_bar() +
               geom_text(stat = 'count',aes(label =..count..), vjust = -0.2),
             ggplot(car, aes(x = symboling, y = price)) + geom_boxplot(), ncol = 2)
# -3 indicates less risky while +3 indicates more risky.
# Most people have brought car with risk factor = 0, 1
# There is no data for factor -3
# Not so Important variable since car with Symboling Rate = -2 indicate less risky it's car_price should be high but at our case it is not.

# make vs price
summary(car$make)
ggplot(car, aes(x = make)) + geom_bar() +
  geom_text(stat = 'count',aes(label =..count..), vjust = -0.2) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
grid.arrange(ggplot(car, aes(x = make)) + geom_bar() +
               geom_text(stat = 'count',aes(label =..count..), vjust = -0.2) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5)),
             ggplot(car, aes(x = make, y = price)) + geom_boxplot() + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5)), ncol = 2)
# Most car bought by people is Toyota.
# In terms of Car_Price Bmw, Mercedes and porsche has High car_price.
# Make model has lot of variable and not much impacted and hence we will remove it.

# fuel.type vs price.
summary(car$fuel.type)
ggplot(car, aes(x = fuel.type)) + geom_bar() +
  geom_text(stat = 'count',aes(label =..count..), vjust = -0.2)
grid.arrange(ggplot(car, aes(x = fuel.type)) + geom_bar() +
               geom_text(stat = 'count',aes(label =..count..), vjust = -0.2),
             ggplot(car, aes(x = fuel.type, y = price)) + geom_boxplot(), ncol = 2)
# Most people have bought car having Fuel = Gas. And in term of Car_price both factor is contributing equally.

# aspiration vs price
summary(car$aspiration)
ggplot(car, aes(x = aspiration)) + geom_bar() +
  geom_text(stat = 'count',aes(label =..count..), vjust = -0.2)
grid.arrange(ggplot(car, aes(x = aspiration)) + geom_bar() +
               geom_text(stat = 'count',aes(label =..count..), vjust = -0.2),
             ggplot(car, aes(x = aspiration, y = price)) + geom_boxplot(), ncol = 2)
# Most people have bought car with aspiration type = std. In terms of Car_Prics both have contributed equally.

# num.of.doors vs price
summary(car$num.of.doors)
ggplot(car, aes(x = num.of.doors)) + geom_bar() +
  geom_text(stat = 'count',aes(label =..count..), vjust = -0.2)
grid.arrange(ggplot(car, aes(x = num.of.doors)) + geom_bar() +
               geom_text(stat = 'count',aes(label =..count..), vjust = -0.2),
             ggplot(car, aes(x = num.of.doors, y = price)) + geom_boxplot(), ncol = 2)
# Most people have car with 4doors i.e. it might be mid range car i.e. Sedan, SUV and 2 door car is like Sports car.

# body.style vs price
summary(car$body.style)
ggplot(car, aes(x = body.style)) + geom_bar() +
  geom_text(stat = 'count',aes(label =..count..), vjust = -0.2)
grid.arrange(ggplot(car, aes(x = body.style)) + geom_bar() +
               geom_text(stat = 'count',aes(label =..count..), vjust = -0.2),
             ggplot(car, aes(x = body.style, y = price)) + geom_boxplot(), ncol = 2)
# Most people bought sedan followed by hatchback.
# In Terms of price Hardtop and Convertible is costly.

# drive.wheels vs price
summary(car$drive.wheels)
ggplot(car, aes(x = drive.wheels)) + geom_bar() +
  geom_text(stat = 'count',aes(label =..count..), vjust = -0.2)
grid.arrange(ggplot(car, aes(x = drive.wheels)) + geom_bar() +
               geom_text(stat = 'count',aes(label =..count..), vjust = -0.2),
             ggplot(car, aes(x = drive.wheels, y = price)) + geom_boxplot(), ncol = 2)
# In terms of drive.wheels car with drive.wheels = rwd is costly.

# engine.location vs price
summary(car$engine.location)
ggplot(car, aes(x = engine.location)) + geom_bar() +
  geom_text(stat = 'count',aes(label =..count..), vjust = -0.2)
grid.arrange(ggplot(car, aes(x = engine.location)) + geom_bar() +
               geom_text(stat = 'count',aes(label =..count..), vjust = -0.2),
             ggplot(car, aes(x = engine.location, y = price)) + geom_boxplot(), ncol = 2)
# Biased data

# engine.type vs price
summary(car$engine.type)
# dohcv has only 1 value in 205 data so we will either remove it or we will replace it with dohcv thinking it might be human mistake instead of dohc it has written dohcv.
levels(car$engine.type)[levels(car$engine.type) == "dohcv"] <- "dohc"
summary(car$engine.type)
ggplot(car, aes(x = engine.type)) + geom_bar() +
  geom_text(stat = 'count',aes(label =..count..), vjust = -0.2)
grid.arrange(ggplot(car, aes(x = engine.type)) + geom_bar() +
               geom_text(stat = 'count',aes(label =..count..), vjust = -0.2),
             ggplot(car, aes(x = engine.type, y = price)) + geom_boxplot(), ncol = 2)
# most car sold was of engine type = ohc and in term of car_price car with engine type = ohcv is costly. 

# num.of.cylinders vs price
summary(car$num.of.cylinders)
# There are 2 levels having value = 1 and it does not mean much and it will cause a prob in train-test split. Hence we will remove it.
cylinders <- car %>% count(num.of.cylinders) %>% filter(n > 3)
car <- car[car$num.of.cylinders %in% cylinders$num.of.cylinders, ]
car$num.of.cylinders <- factor(car$num.of.cylinders, unique(car$num.of.cylinders))
summary(car$num.of.cylinders)
# Visualization of the data
ggplot(car, aes(x = num.of.cylinders)) + geom_bar() +
  geom_text(stat = 'count',aes(label =..count..), vjust = -0.2)
grid.arrange(ggplot(car, aes(x = num.of.cylinders)) + geom_bar() +
               geom_text(stat = 'count',aes(label =..count..), vjust = -0.2),
             ggplot(car, aes(x = num.of.cylinders, y = price)) + geom_boxplot(), ncol = 2)
# Most of the car sold was having 4 cylinder - 159 and car with 8cylinder is costly.

# fuel.system vs price
summary(car$fuel.system)
# There are 2 levels mfi and spfi having only 1 value. either we can remove it or we can replace it with mpfi and spdi.
# Whenever you receive this type of dta where there is only one value for a factor it's better to ask client what should be done either to remove it or replace it. Because the reason might be because of the human mistake.
levels(car$fuel.system)[levels(car$fuel.system) == "mfi"] <- "mpfi"
levels(car$fuel.system)[levels(car$fuel.system) == "spfi"] <- "spdi"
summary(car$fuel.system)
ggplot(car, aes(x = fuel.system)) + geom_bar() +
  geom_text(stat = 'count',aes(label =..count..), vjust = -0.2)
grid.arrange(ggplot(car, aes(x = fuel.system)) + geom_bar() +
               geom_text(stat = 'count',aes(label =..count..), vjust = -0.2),
             ggplot(car, aes(x = fuel.system, y = price)) + geom_boxplot(), ncol = 2)
# IDI and MPFI fuel system have higher price.

ggcorr(car, label = TRUE, label_size = 2.9, hjust = 1, layout.exp = 2)
# We can find that enigne.size and curb.weight are highly correlated and hence we will remove it.
# We will also remove make column because of having many levels and normalized.losses.

car_mod <- subset(car, select = c(normalized.losses, make, 
                                  engine.size, curb.weight))
names(car_mod)

car$normalized.losses <- NULL
car$make <- NULL
car$engine.size <- NULL
car$curb.weight <- NULL

dim(car)

# Splitting the Dataset
set.seed(123)
car_mod1 <- sample.split(car, SplitRatio = 0.7)
train <- subset(car, car_mod1 == TRUE)
dim(train)
test <- subset(car, car_mod1 == FALSE)
dim(test)

# Run Linear Regression on Full Model
model <- lm(price ~ ., data = train)
summary(model)

# There are lot of insignificant variable and hence we will only consider those variable having value less than 0.05
model1 <- lm(price ~ fuel.type + body.style + drive.wheels + engine.location + 
               engine.type + num.of.cylinders + stroke + compression.ratio, data = train)
summary(model1)

# Prediction of the train model.
prediction <- predict(model1, type = "response", se.fit = FALSE)
length(prediction)
actual <- train$price
train_pred <- prediction
difference <- actual - train_pred
df <- data.frame(actual, train_pred, difference)
head(df, 10)

# RMSE value for the Train data
rmse(actual, train_pred)

# Prediction on Test data
test_pred <- predict(model1, newdata = test)
length(test_pred)

# Calculate RMSE for Test model
rmse(test$price, test_pred)

cat("RMSE for TRAIN data on Full Model is: ", rmse(actual, train_pred))
cat("RMSE for TEST data on Full Model is: ", rmse(test$price, test_pred))

# Performing BAckward stepwise regression on the data
model2 <- lm(price ~ ., data = train)
summary(model2)

model3 <- stepAIC(model2, direction = "backward")

model4 <- lm(price ~ symboling + body.style + drive.wheels + engine.location + 
               wheel.base + width + engine.type + num.of.cylinders + fuel.system + 
               stroke + compression.ratio + city.mpg + highway.mpg, data = train)
summary(model4)

# Prediction of the train model.
prediction1 <- predict(model4, type = "response", se.fit = FALSE)
length(prediction1)
actual1 <- train$price
train_pred1 <- prediction1
difference1 <- actual1 - train_pred1
df1 <- data.frame(actual1, train_pred1, difference1)
head(df1, 10)

# RMSE value for the Train data
rmse(actual1, train_pred1)

# Prediction on Test data
test_pred1 <- predict(model4, newdata = test)
length(test_pred1)

# Calculate RMSE for Test model
rmse(test$price, test_pred1)

cat("RMSE for TRAIN data on Full Model is: ", rmse(actual, train_pred))
cat("RMSE for TEST data on Full Model is: ", rmse(test$price, test_pred))

cat("RMSE for TRAIN data on Full Model is: ", rmse(actual1, train_pred))
cat("RMSE for TEST data on Full Model is: ", rmse(test$price, test_pred1))

# Check for Heterocedasticity
bptest(model4)
# Since p-value is greater than 0.05 we fail to reject the null hypothesis and our data is Homocedasticity

# Check for AutoCorrelation
durbinWatsonTest(model4)
# With p-value > 0.05, we can conclude that autocorrelation is not present.


# Check for Normality test
shapiro.test(model1$residuals)
# With p-value > 0.05, we can conclude that our residuals are normally distributed.

hist(model4$residuals)

plot(model4)

difference2 <- test$price - test_pred1
Final_Full_Model <- data.frame(test$price, test_pred1, difference2)

# Check whether Y is normally distributed or not.
hist(car$price)
# Let's make Y normally distributed and at same time transform all the X variable.

dim(car)

car_price <- car
dim(car_price)

range(car_price$price)
car_price$price <- log(car_price$price)
range(car_price$price)
hist(car_price$price)

numeric_Cols <- which(sapply(car_price, is.numeric))
names(numeric_Cols)

# wheel.base, length, width, height are calculated in same unit but have different range and hence we will do scalling on them.
range(car_price$wheel.base)
hist(car_price$wheel.base)
car_price$wheel.base <- scale(car_price$wheel.base, center = TRUE, scale = TRUE)
range(car_price$wheel.base)
hist(car_price$wheel.base)

range(car_price$length)
hist(car_price$length)
car_price$length <- scale(car_price$length, center = TRUE, scale = TRUE)
range(car_price$length)
hist(car_price$length)

range(car_price$width)
hist(car_price$width)
car_price$width <- scale(car_price$width, center = TRUE, scale = TRUE)
range(car_price$width)
hist(car_price$width)

range(car_price$height)
hist(car_price$height)
car_price$height <- scale(car_price$height, center = TRUE, scale = TRUE)
range(car_price$height)
hist(car_price$height)

range(car_price$compression.ratio)
hist(car_price$compression.ratio)
car_price$compression.ratio <- scale(car_price$compression.ratio, center = TRUE, scale = TRUE)
range(car_price$compression.ratio)
hist(car_price$compression.ratio)

range(car_price$horsepower)
hist(car_price$horsepower)
car_price$horsepower <- scale(car_price$horsepower, center = TRUE, scale = TRUE)
range(car_price$horsepower)
hist(car_price$horsepower)

range(car_price$peak.rpm)
hist(car_price$peak.rpm)
car_price$peak.rpm <- scale(car_price$peak.rpm, center = TRUE, scale = TRUE)
range(car_price$peak.rpm)
hist(car_price$peak.rpm)

range(car_price$city.mpg)
hist(car_price$city.mpg)
car_price$city.mpg <- scale(car_price$city.mpg, center = TRUE, scale = TRUE)
range(car_price$city.mpg)
hist(car_price$city.mpg)

range(car_price$highway.mpg)
hist(car_price$highway.mpg)
car_price$highway.mpg <- scale(car_price$highway.mpg, center = TRUE, scale = TRUE)
range(car_price$highway.mpg)
hist(car_price$highway.mpg)

# Split the Data into Train and Test.
set.seed(123)
car_mod2 <- sample.split(car_price, SplitRatio = 0.7)
train1 <- subset(car_price, car_mod2 == TRUE)
dim(train1)
test1 <- subset(car_price, car_mod2 == FALSE)
dim(test1)

# Run the full model on Train data
model5 <- lm(price ~ ., data = train1)
summary(model5)

# There are lot of insignificant variable and hence we will only consider those variable having value less than 0.05
model6 <- lm(price ~ symboling + body.style + engine.location + length +
               width + engine.type + num.of.cylinders + stroke + horsepower, 
             data = train1)
summary(model6)

# Prediction of the train model.
predict1 <- predict(model6, type = "response", se.fit = FALSE)
length(predict1)
actual <- train1$price
train_pred <- predict1
difference <- actual - train_pred
df <- data.frame(actual, train_pred, difference)
head(df, 10)

# RMSE value for the Train data
rmse(actual, train_pred)

# Prediction on Test data
test_pred <- predict(model6, newdata = test1)
length(test_pred)

# Calculate RMSE for Test model
rmse(test1$price, test_pred)

cat("RMSE for TRAIN data on Full Model is: ", rmse(actual, train_pred))
cat("RMSE for TEST data on Full Model is: ", rmse(test1$price, test_pred))

# Check for Heterocedasticity
bptest(model6)
# Since p-value is greater than 0.05 we fail to reject the null hypothesis and our data is Homocedasticity

# Check for AutoCorrelation
durbinWatsonTest(model6)
# With p-value > 0.05, we can conclude that autocorrelation is not present.

# Check for Normality test
shapiro.test(model6$residuals)
# With p-value > 0.05, we can conclude that our residuals are normally distributed.

hist(model6$residuals)

qqnorm(model6$residuals) ; qqline(model6$residuals)

plot(model6)

# Actual vs Predicted value for Train data
difference <- exp(train1$price) - exp(train_pred)
final_train_model <- data.frame(exp(train1$price), exp(train_pred), difference)
head(final_train_model)

# Actual vs Predicted value for Test data
difference1 <- exp(test1$price) - exp(test_pred)
final_test_model <- data.frame(exp(test1$price), exp(test_pred), difference1)
head(final_test_model)
