library(e1071)
library(rpart)
library(tidyr)
library(rpart.plot)



train_data <- data.frame(train_data) # convert to data frame if not already done
train_data <- separate(train_data, train_data, sep = ",")

set.seed(123) # for reproducibility

play_tennis <- read.delim("D:/2CS/BDM/play_tennis.csv")
play_tennis

# Split the single column into separate columns using "," as the separator
play_tennis <- separate(play_tennis, col = "day.outlook.temp.humidity.wind.play",
                        into = c("Day", "Outlook", "Temp", "Humidity", "Wind", "Play"),
                        sep = ",")

# Convert relevant columns to factors
play_tennis[, 2:6] <- lapply(play_tennis[, 2:6], as.factor)

play_tennis
str(play_tennis)


train <- sample(nrow(play_tennis), 0.7*nrow(play_tennis))
train_data <- play_tennis[train,]
train_data
test_data <- play_tennis[-train,]
test_data
str(train_data)

train_data$Outlook <- as.factor(train_data$Outlook)
train_data$Temp <- as.factor(train_data$Temp)
train_data$Humidity <- as.factor(train_data$Humidity)
train_data$Wind <- as.factor(train_data$Wind)
train_data$Play <- as.factor(train_data$Play)

test_data$Outlook <- as.factor(test_data$Outlook)
test_data$Temp <- as.factor(test_data$Temp)
test_data$Humidity <- as.factor(test_data$Humidity)
test_data$Wind <- as.factor(test_data$Wind)
test_data$Play <- as.factor(test_data$Play)

#building the decision tree 
play_tennis_model <- rpart(Play ~ ., data=train_data, method="class")

#visualize the decision tree using the rpart.plot package:

rpart.plot(play_tennis_model)

#
predictions <- predict(play_tennis_model, newdata=test_data, type="class")

