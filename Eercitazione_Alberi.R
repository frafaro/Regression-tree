
#### ESERCITAZIONE ALBERI ####

#Si consideri il dataset Carseats nel pacchetto ISLR2. L’obiettivo è quello di prevedere la variabile Sales
#sulla base di tutte le altre informazioni presenti nel dataset. Svolgere un’analisi che risponda ai seguenti
#quesiti.

# install.packages("ISLR2")
library(ISLR2)

data("Carseats")
attach(Carseats)

#1) Dividere il dataset in una parte di training (70% delle osservazioni) e una parte di test.

ntr <- round(nrow(Carseats)*0.70)  # numero unita nel training sample

set.seed(123456) 

indice <- sample(1:nrow(Carseats),ntr)

train <- Carseats[indice,]  # training sample
test <- Carseats[-indice,]  # test sample

#2) Stimare un albero di regressione basato sul training set. Calcolare l’MSE delle previsioni sul test set.

library(rpart)
library(rpart.plot)

tree<-rpart(formula = Sales~. , 
            data= train,
            method = "anova")

pred_val<-predict(tree, newdata = test)
mean((pred_val-test$Sales)^2) #MSE 4.912664

#3) Potare l’albero scegliendo il parametro di costo/complessità sulla base della cross-validation. Calcolare
#nuovamente l’MSE sulle previsioni sul test set e commentare.

plotcp(tree) # il grafico ci dice di potare ball'altezza del nodo numero 8 #cp 0.025

tree_pruned<-prune(tree, cp= 0.025)

pred_val.pruned<-predict(tree_pruned, newdata = test)
mean((pred_val.pruned-test$Sales)^2) #MSE 4.982664  questo modello è meno preciso del primo infatti l'MSE è più elevata

#la differenza però è molto piccola quindi a livello di costo complessità si può
#usare anche il modello pruned

#4) Usare il metodo del bagging, calcolando l’MSE sul test set. Commentare il risultato e l’importanza dei
#regressori.

library(randomForest)
set.seed(1)

bagging<-randomForest( formula= Sales~ . ,
                       data= train,
                       mtry= ncol(train)-1,
                       importance= T)

varImpPlot(bagging) #covariata più imp. sheiveloc, price 

pred_val.bagging<-predict(bagging, newdata = test)
mean((pred_val.bagging-test$Sales)^2) #2.476998 ridotto rispetto ai modelli precedenti


#5) Usare una random forest per prevedere Sales. Calcolare MSE sul test set e commentare l’importanza
#delle variabili.

set.seed(1)

RF<-randomForest(formula= Sales~ . , 
                 data= train,
                 mtry= sqrt(ncol(train)-1) ,
                 importance= T)

varImpPlot(RF)

pred_val_RF<-predict(RF, newdata = test)
mean((pred_val_RF-test$Sales)^2) #2.649523 più elevato del modello bagging

#6) Descrivere il significato dell’argomento mtry nella funzione randomForest.


