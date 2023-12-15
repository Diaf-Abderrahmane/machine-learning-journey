library(mlbench)
library( randomForest)
library(e1071)

data("Glass")
summary(Glass)
Glass

#random forest on Glass dataset
t1=Sys.time()
RF=randomForest(Type~., data=Glass)
t2=Sys.time()
t3=t2 - t1
print(t3)
# time = 0.1870999
RF # nous donne l'erreur par classe 
# model error : 20.09% // No. of variables tried at each split: 3 // Trees : 500


h <- data.frame( RI="1.52101", Na="13.64", Mg="4.49",Al="1.10", Si="71.78", K="0.06", Ca="8.75", Ba="0.00", Fe="0.00")
obv_pred <- predict(RF, newdata=h)
obv_pred

t1 = tune.randomForest(Type~., data=Glass, ntree=c(10,100,200,500))
# retourner celui qui a retourner l'erreur la plus petite. ( avec validation croisée )
t1
plot(t1)

t1=Sys.time()
RF1=randomForest(Type~., data=Glass, ntree=200)
t2=Sys.time()
t3=t2 - t1
print(t3)
# time = 0.07993793
RF1



#nombre de variable dans chaque arbre
t2 = tune.randomForest(Type~.,data = Glass,ntree = 200 , mtry=c(3,9,4))
plot(t2)
t2
#mtry = 3





# my comments : 

#RF1=randomForest(Type~., data=Glass, ntree=500, mtry=c(racine(p), p, p/2))


# le nombre d'attribut pour chaque split ( mtry = c ( racine(variable)))


# Prédire la sortie d’une nouvelle entrée, à choisir parmi les instances existantes

# hyperparamètres.
# nombre d'arbre
# nombre d'attribut selectionnés pour chaque arbre
# le nombre maximale de noeuds ??

# peu de donnée -> decision tree
# nombre d'arbre optimal pour un dataset -> faire un tuning des hyperparamètres
# l'avantage de random forest = parralellisable ( arbre indep // faire plusieurs noeuds )

#Optimisation des hyperparamètres : grid search : optiman ( toutes les combi) / random search ( random, peut rater la solution optimal mais plus rapide)
# on favorise les arbres de petites tailles