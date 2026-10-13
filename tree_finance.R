library(tidyverse)
library(tidyquant)

#scarichiamo i dati

prices<-tq_get(x="DIS", get= "stock.prices",
               from="2016-01-01",
               to= "2023-03-31")

#selezioniamo variabili che ci interessano

prices<-prices %>% 
  select(date, volume, adjusted)

#creiamo variabile in modo che adjusted e volume siano al tempo precedente

prices<-prices %>% 
  mutate(lag_adjusted= lag(adjusted),
         lag_volume=lag(volume))

#calcoliamo le medie mobili a 2,7,14,21

prices_rolling<- prices %>% 
  tq_mutate(select=lag_adjusted, #crea nuove variabile su funz legate alla finanza
            mutate_fun = rollapply, #funz per le medie mobili
            width=2,
            align="right", #allineato al dato più recente dei due di cui fa la media
            FUN=mean,
            col_rename = "RM2") %>% 
  tq_mutate(select=lag_adjusted, 
            mutate_fun = rollapply, 
            width=7,
            align="right", 
            FUN=mean,
            col_rename = "RM7") %>% 
  tq_mutate(select=lag_adjusted, 
            mutate_fun = rollapply, 
            width=14,
            align="right", 
            FUN=mean,
            col_rename = "RM14") %>% 
  tq_mutate(select=lag_adjusted, 
            mutate_fun = rollapply,
            width=21,
            align="right", 
            FUN=mean,
            col_rename = "RM21") %>% 
  tq_mutate(select=lag_adjusted,         #ora mettiamo la volatilità a 2 e a 7 periodi
            mutate_fun = rollapply, 
            width=2,
            align="right", 
            FUN=sd,                     #funz della voltilità
            col_rename = "sd2") %>% 
  tq_mutate(select=lag_adjusted, 
            mutate_fun = rollapply, 
            width=7,
            align="right", 
            FUN=sd,
            col_rename = "sd7") 

#togliamo righe con osservazioni mancanti

prices_rolling<-prices_rolling %>% 
  na.omit()

#grafico serie storiche

prices_rolling %>% 
  filter(date>"2020-12-31") %>% 
  ggplot(aes(x= date))+
  geom_line(aes(y=adjusted, color= "Price"))+
  geom_line(aes(y=RM2, color="Roll2"))+
  geom_line(aes(y=RM7, color="Roll7"))+
  geom_line(aes(y=RM14, color="Roll14"))+
  geom_line(aes(y=RM21, color="Roll21"))

#analisi corr preliminare

library(corrplot)

corrplot(cor(prices_rolling[,-c(1,2,4)]))

#standardizziamo le variabili per evitare instabilità numerica

prices_rolling<-prices_rolling %>% 
  mutate(across(volume:sd7, ~ scale(.x)[,])) #.x per applicarla ogni volta ad una colonna diversa tra quelle selezionate

#creazione training set

training<-prices_rolling %>% 
  filter(date<"2022-01-01") %>% #prendiamo come training tutte le oss fino al primo gennaio 2022
  select(-date,-volume,-lag_adjusted)

#creazione test set

test<-prices_rolling %>% 
  filter(date>="2022-01-01")
  

#### albero di regressione ####

library(rpart)
library(rpart.plot)

tree<-rpart(formula= adjusted ~ . , 
            data=training, 
            method = "anova")

rpart.plot(tree)

plotcp(tree) #l'impressione è che in questo caso non ci sia da potare l'albero

#valutazionje sul test set

pred_val<- predict(tree, newdata = test)
plot(pred_val, test$adjusted)
abline(0,1)

plot(1:nrow(test), test$adjusted, type="l")
lines(1:nrow(test), pred_val, lty=2, col=2)

test$pred_tree<-pred_val

mean((pred_val - test$adjusted)^2) #MSE

#bagging

library(randomForest)
set.seed(1)

bagging<-randomForest(formula= adjusted~ . , 
                      data= training,
                      mtry= ncol(training)-1,
                      importance= T)

varImpPlot(bagging)

pred_val_bag<-predict(bagging, newdata = test) #non sono discreti ma continui quindi il grafico verrà diverso 
#                                               da quello tratteggiato rosso di prima

plot(1:nrow(test), test$adjusted, type="l")
lines(1:nrow(test), pred_val_bag, lty=2, col=2)

test$pred_tree<-pred_val_bag

mean((pred_val_bag - test$adjusted)^2) #MSE miglioramento nel bagging mse si è ridotto

# random forest

set.seed(1)

RF<-randomForest(formula= adjusted~ . , 
                      data= training,
                      mtry= sqrt(ncol(training)-1) ,
                      importance= T)

varImpPlot(RF)

pred_val_RF<-predict(RF, newdata = test) #non sono discreti ma continui quindi il grafico verrà diverso 
#                                               da quello tratteggiato rosso di prima

plot(1:nrow(test), test$adjusted, type="l")
lines(1:nrow(test), pred_val_RF, lty=2, col=2)

test$pred_tree<-pred_val_RF

mean((pred_val_RF - test$adjusted)^2) #MSE miglioramento nel RF rispetto all'albero ma peggioramento rispetto al bagging

#grafico previsioni

library(tidyverse)

test %>% 
  ggplot(aes(x=date))+
  geom_line(aes(y=adjusted, color= "Observed"))+
  geom_line(aes(y = pred_tree, color= "Pred-Tree"), lty=2)+.       #in questo caso devo correggere le y pk le ho segnate tutte come pred tree quindi si sono sovrascritte ogni volta 
  geom_line(aes(y = pred_bag, color= "Pred-Bagging"), lty=2)+
  geom_line(aes(y = pred_RF, color= "Pred-RF"), lty=2)+
    scale_color_manual("Linea", values= c("Observed"="black",
                                          "Pred-Tree"="red",
                                          "Pred-Bagging"="blue",
                                          "Pred-RF"="green"))
  






