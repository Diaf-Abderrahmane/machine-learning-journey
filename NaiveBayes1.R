library(e1071)

data = Glass

n=dim(data)[1]

# split the data set
index = sample(n, 0.7 * n)

Appren = data[index, ]
Test = data[-index, ]

#naiveBayes
nb.model<- naiveBayes(Type ~ ., data = Appren)
nb.model

# predict class value of the test set using the model
Pred=predict(object = nb.model, newdata = Test)
Test.mod <- cbind(Test, Pred)
head(Test.mod, 5)

# the confusion matrix
Confusion = table(Test.mod$Type, Test.mod$Pred)
Confusion

# the error 0.5846
err<- 1-sum(diag(Confusion))/sum(Confusion)
err
