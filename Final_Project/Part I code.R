data = read.csv("C:/Users/ae7le/OneDrive/Documents/Sara Schenirer/Intro to Data Science/Final_Project/auto-mpg.csv", header=TRUE)

# Check if numeric fields are all numeric
is.numeric(data$mpg)
is.numeric(data$cylinder)
is.numeric(data$displacement)
is.numeric(data$horsepower)           
is.numeric(data$weight)
is.numeric(data$model.year)
is.numeric(data$origin)

# Delete rows with non-numeric data in horsepower and convert data from string to int
data <- data[!is.na(as.numeric(data$horsepower)), ]
data$horsepower <- as.numeric(data$horsepower)
is.numeric(data$horsepower)

# Split data into train and test
nrow(data)
data_train <- head(data, 300)
data_test <- tail(data, 92)

# Create a full, forwards and backwards models
library(MASS)
full_model <- lm(mpg ~ . - car.name, data=data_train)
summary(full_model)

forward_model <- stepAIC(full_model, direction = "forward", scope = formula(~ .))
summary(forward_model)

backwards_model <- stepAIC(full_model, direction = "backward")
summary(backwards_model)

# Other models based on what was significant in the previous ones
model1 <- lm(mpg ~ weight + model.year, data=data_train)
summary(model1)

model2 <- lm(mpg ~ weight + model.year + origin,  data=data_train)
summary(model2)

model3 <- lm(mpg ~ weight, data=data_train)
summary(model3)

model4 <- lm(mpg ~ model.year, data=data_train)
summary(model4)

# Make histogram of actual test data
hist(data_test$mpg, main = 'Actual Test Data')

# Test model2 and backwards model on test data by making a histogram of predicted data and a residual plot
res_model2 <- data_test$mpg - predict(model2, data_test)
plot(predict(model2, data_test), res_model2)
abline(0,0)
hist(predict(model2, data_test), main = "Model 2 on Test Data")

res_backwardsModel <- data_test$mpg - predict(backwards_model, data_test)
plot(predict(backwards_model, data_test), res_backwardsModel)
abline(0,0)
hist(predict(backwards_model, data_test), main = "Backwards Model on Test Data")

# Test model1 also because other two tested models were very similar
res_model1 <- data_test$mpg - predict(model1, data_test)
plot(predict(model1, data_test), res_model2)
abline(0,0)
hist(predict(model1, data_test), main = "Model 1 on Test Data")
