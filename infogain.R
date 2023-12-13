library(FSelector)
library(mlbench)
data(Glass)
data <- Glass
data
# nombre de valeurs manquantes dans chacune des colonnes
sapply(data,function(x) sum(is.na(x)))
weights <- information.gain(Type~., data)
weights
subset1 <- cutoff.k.percent(weights,0.5)
subset1
#weights <- gain.ratio(Type~., data)
#weights
data
f <- as.simple.formula(subset1, "Type")
print(f)


