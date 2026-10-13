
library(tidyquant)
library(tidyverse)

#indici disponibili su tidyquant

tq_index_options()       # indici
tq_exchange_options()    # azioni

#### scarichiamo info titoli DOW ####

DOW_index<-tq_index("DOW") 

#### scarichiamo i prezzi ####

prices<- tq_get(x= DOW_index$symbol,    #singola azione o tibble, in quesro caso colonna con i nomi delle azioni
                get = "stock.prices",   #get per decidere cosa prendere
                from= "2010-01-01", 
                to= "2022-01-31")       


#### analisi su dati mancanti####

prices %>% 
  group_by(symbol) %>% 
  summarise(n_NA = sum(is.na(adjusted)),        #vediamo se ci sono NA
            n= n(),                             #osservazioni tot per ogni titolo
            n_NaN= sum(!is.finite(adjusted)))   #oss strane

#rimuovioamo il titolo DOW

prices<- prices %>% 
  filter(symbol!="DOW")

#### dividiamo dataset in test e training set ####

# 80% come training set

length(unique(prices$date))*0.8 #è l'80% delle date

split_date<-unique(prices$date)[length(unique(prices$date))*0.8] #30 agosto 2019

#training set
prices_train<- prices %>% 
  filter(date<split_date)

#test set
prices_test<-prices %>% 
  filter(date>=split_date)

#esplorazione grafica per compagnie settore finanziario

symbols_Financial<-DOW_index %>% 
  filter(sector == "Financials") %>% 
  select(symbol) %>% 
  unlist() #per avere vettore e non tibble

prices %>% 
  filter(symbol %in% symbols_Financial ) %>% 
  group_by(symbol) %>% 
  ggplot(aes(x=date, y= adjusted, color=symbol))+
  geom_line()+
  scale_y_continuous(labels=scales::dollar)+
  labs(title="Daily stock prices",
       x="",
       y="adjusted prices", color="")+
  theme_tq()+
  scale_color_tq()

#calcolo rendimenti giornalieri

returns<- prices %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted, 
               mutate_fun = periodReturn,
               period="daily", 
               type = "arithmetic")


returns %>% 
  filter(symbol %in% symbols_Financial ) %>% 
  group_by(symbol) %>% 
  ggplot(aes(x=date, y= daily.returns, color=symbol))+
  geom_line()+
  labs(title="Daily stock returns",
       x="",
       y="returns", color="")+
  theme_tq()+
  scale_color_tq()+
  facet_wrap(~symbol)

#organizzare dati rendimenti in modo che siano utilizzabili per la pca

#training set
returns_train<- returns %>% 
  filter(date<split_date)

#test set
returns_test<-returns %>% 
  filter(date>=split_date)

#riordino dati

returns_matrix<-returns_train %>% 
  pivot_wider(names_from = symbol, values_from = daily.returns) %>% 
  column_to_rownames("date")

#facciamo PCA

library(FactoMineR)
library(factoextra)

pca_result<-PCA(returns_matrix, scale.unit = T, ncp = 15)

summary(pca_result)
fviz_eig(pca_result) #prendiamo le prime 2

corrplot(cor(returns_matrix))

corrplot(t(pca_result$var$coord), is.corr = F) #la prima tutta positiva è l'andamento del mercato
#la seconda sappiamo per costruizione che è indipendente

#il passo successivo è trovare i pesi

# matrice di pesi
matrix_weights <- pca_result$var$coord
str(matrix_weights)
# standardizziamo le colonne
for(i in 1:ncol(matrix_weights)){
  matrix_weights[,i]<-matrix_weights[,i]/
    sum(matrix_weights[,i])
}
colSums(matrix_weights) #controllo
matrix_weights <- cbind(matrix_weights, 
                        1/nrow(matrix_weights)) 
############################
## Calcolo portafoglio

port_returns_test <- returns_test %>% 
  tq_portfolio(assets_col = symbol,
               returns_col = daily.returns,
               weights = matrix_weights[,1]
  )
port_returns_test %>% 
  tq_performance(Ra = portfolio.returns,
                 Rb = NULL, 
                 performance_fun = table.AnnualizedReturns)

## più portafogli insieme

returns_test_multi <- returns_test %>% 
  tq_repeat_df(n = ncol(matrix_weights))

weights_table <- tibble(symbol = rownames(matrix_weights)) %>% 
  tq_repeat_df(n = ncol(matrix_weights)) %>% 
  bind_cols(wts = c(matrix_weights))

port_returns_test_multi <- returns_test_multi %>% 
  tq_portfolio(assets_col = symbol, 
               returns_col = daily.returns,
               weights = weights_table
  )

port_returns_test_multi %>% 
  tq_performance(Ra = portfolio.returns,
                 Rb = NULL, 
                 performance_fun = table.AnnualizedReturns,
                 geometric = FALSE)

# back-testing

port_backtest_test_multi <- returns_test_multi %>% 
  tq_portfolio(assets_col = symbol, 
               returns_col = daily.returns,
               weights = weights_table,
               wealth.index = T
  )

port_backtest_test_multi %>% 
  filter(portfolio %in% c(1,2)) %>% 
  ggplot(aes(x = date, 
             y = portfolio.wealthindex, 
             color = factor(portfolio)))+
  geom_line()+
  theme_tq()+
  scale_color_tq()















