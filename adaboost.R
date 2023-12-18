install.packages('adabag')                    # for fitting the adaboost model

install.packages('caret')                    # for general data preparation and model fitting

library(adabag)
library(caret)  
library(mlbench)
library(e1071)


data("Glass")               # reads the dataset
data<- Glass
head(data)           # head() returns the top 6 rows of the dataframe

summary(data)       # returns the statistical summary of the data columns

dim(data)

# createDataPartition() function from the caret package to split the original dataset into a training and testing set and split data into training (80%) and testing set (20%)
parts = createDataPartition(data$Type, p = 0.8, list = F)
train = data[parts, ]
test = data[-parts, ]

help(boosting)
model_adaboost <- boosting(Type~., data=train, boos=TRUE, mfinal=50)

model_adaboost
