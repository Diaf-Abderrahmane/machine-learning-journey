library(mlbench)
library(factoextra)
library(FactoMineR)
library(glmnet)
data(BostonHousing)
data = BostonHousing
summary(data)
dim(data)
names(data)

t1 <- Sys.time()
model <- lm(medv ~ ., data = BostonHousing)
predictions <- predict(model, BostonHousing)
print(predictions)
mse <- mean((BostonHousing$medv - predictions)^2)
cat("MSE:", mse, "\n")
#MSE: 21.89483

# calculate RMSE
rmse <- sqrt(mse)
cat("RMSE:", rmse, "\n")
#RMSE: 4.679191

t2 <- Sys.time()
t3 <- t2 - t1 
print(t3)

#------------------------------------------- reg + acp -------------------

names(data)
data <- data[, -which(names(data) == "chas")]

#Perform PCA
pca <- PCA(data, scale=TRUE, graph =T)

#see contribution of each component
pca$eig
pca$ind

#pick 5 components that represent over 80% of the information
data1 = pca$ind$coord[,1:5]
data1 <- as.data.frame(data1)

#add the "medv" column ( target column )
data1$medv <- data$medv

#Perform a linear regression of the resulting data of the acp
model <- lm(medv ~ ., data = data1 )
predictions <- predict(model, data1)
print(predictions)

#Calculating MSE
mse <- mean((data1$medv - predictions)^2)
cat("MSE:", mse, "\n")
#MSE: 11.77573 

# calculate RMSE
rmse <- sqrt(mse)
cat("RMSE:", rmse, "\n")
#RMSE: 3.431579 

# ------------------------------------- Reg + Ridge -----------------------

train_index <- sample(nrow(data), nrow(data)*0.8)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

X=BostonHousing[,1:13]
X

X <- X[, -which(names(BostonHousing) == "chas")]
y <- BostonHousing$medv

# Fit a Ridge regression model
fit.ridge <- glmnet(X, y, alpha = 0, lambda = seq(0, 20, 0.1))

# Calculate the cross-validated mean squared error for each lambda
cv.ridge <- cv.glmnet(X, y, alpha = 0, lambda = seq(0, 20, 0.1), nfolds = 10, type.measure = "mse")

#cor_mat <- cor(data)
#cor_mat

#--------------------------------- Ridge Regression ---------------------------

# Perform a Ridge Regrssion on our data ( Boston Housing )
# Lambda ranges from 0 to 20, with a step size of 0.1
mod.ridge = lm.ridge(medv~.,data,lambda = seq(0,20,0.1))

# sets the layout of the plotting region to a single row and single column, with
# only one plot displayed at a time.
par(mfrow = c(1,1))

#plot result of ridge regression
plot(mod.ridge)

# line plot of the coefficient estimates for each predictor variable as
# a function of the logarithm of the lambda values used in Ridge regression
matplot(t(mod.ridge$coef),lty=1:3,type="l",col = 1:10)

# adds a legend to the plot
legend("top", legend = rownames(mod.ridge$coef), col = 1:10, lty = 1:3)

# prints the weights related to each variable or feature 
mod.ridge$coef

# creates a plot of the Generalized Cross-Validation (GCV) statistic as a function
# of the lambda values used in Ridge regression.
plot(mod.ridge$lambda,mod.ridge$GCV)

# select the value of lambda related the smalled value of the GCV
select(mod.ridge)

# Perform a ridge regression with lambda = "selected lambda"
mod.ridge = lm.ridge(medv~.,data,lambda = 4.3)
mod.ridge$coef


# compariason avec mse
mod.ridge = lm.ridge(medv~crim+zn+nox+rm+dis+rad+tax+ptratio+b+lstat,data, lambda = 4.3)

# This line of code creates a matrix X.matrix that combines the columns of the 
# predictor variables used in Ridge regression with an additional column of 1's for the intercept.
X.matrix = cbind(rep(1,length = length(data$medv)),data$crim,data$zn,data$nox,data$rm,data$dis, data$rad,data$tax,data$ptratio,data$b,data$lstat)

X.matrix1 = as.matrix(data[,-9])
fitted.vals = X.matrix %*%c(34.918471472,-0.104315656, 0.043678727,-16.683126171, 3.85174685, -1.426638997, 0.272261020, -0.010629921, -.0935344659, 0.009278324, -0.516975577)
sse.ridge = sum((data$medv-fitted.vals)^2)
sse.ridge
msse = sse/(506)
msse

#----------------------------- LASSO Reg ---------------------------------

set.seed(123)


x <- data.matrix(data[, -14])
y <- data$medv

# we can split the data into training and testing sets:
train_index <- sample(nrow(data), 0.7*nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

x_train <- apply(x_train, 2, function(x) ifelse(is.na(as.numeric(x)), 0, as.numeric(x)))


x_train <- as.matrix(train_data[, -14])
y_train <- train_data$medv

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)


#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda
#[1] 0.03039722

#produce plot of test MSE by lambda value
plot(cv_model) 

x_test <- as.matrix(test_data[, -14])
y_test <- test_data$medv

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#use lasso regression model to predict response value
pred <- predict(best_model, s = best_lambda, newx = x_test)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((pred - y)^2)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

rsq <- 1 - sse/sst
rsq
#[1] 0.9991502
# That is, the best model was able to explain 99.91% of the variation in 
# the response values of the training data.


