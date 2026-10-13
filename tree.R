install.packages("ISLR")
library(tidyverse)
library(ISLR)
library(rpart)
library(rpart.plot)

# carichiamo i dati
data("Hitters")

# togliamo NA
Hitters <- Hitters %>% 
  as_tibble() %>% 
  drop_na()

# aggiungiamo colonna di id
Hitters <- Hitters %>%
  mutate(id = row_number())

# estraiamo il 70% delle righe
set.seed(123)
training <- Hitters %>% 
  sample_frac(.7)
test <- anti_join(Hitters, training, by = "id") %>% 
  mutate(id = NULL)
training<-training %>% 
  mutate(id = NULL)

# stimiamo l'albero
tree <- rpart(formula = log(Salary)~.,
              data = training, 
              method = "anova")
rpart.plot(tree)
plotcp(tree)

# pruning

tree_pruned<-prune(tree, cp= 0.035)
rpart.plot(tree_pruned)

#previsione

pred_tree<-predict(tree_pruned, newdata = test)

#valutazione previsione

plot(pred_tree, log(test$Salary))
abline(0,1)
mean((pred_tree-log(test$Salary))^2)

####
#bagging
install.packages("randomForest")
library(randomForest)

set.seed(123)

begging<-randomForest(formula= log(Salary)~. ,
             data=training,
             mtry=ncol(training)-1,
             importance=T)


varImpPlot(begging) #predittori più importanti nello spiegare il fenomeno
