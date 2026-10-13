## ESERCITAZIONE 2 28_02_2023

#### ES 1 ####

#Si carichi il dataset contenuto nel file “climate_carbon.csv”
library(tidyverse)
climate_carbon<-read_csv("data/climate_carbon.csv")


#### punto 1 ####

#Esplorare graficamente la correlazione tra le anomalie della temperatura (temp_anomaly) e le emissioni
#di carbonio (carbon_emissions).

climate_carbon %>% 
  ggplot(aes(x=carbon_emissions, y=temp_anomaly))+
  geom_point() +
  geom_smooth(se=F)+
  geom_smooth(method = "lm", color= "red", se= F) #applichiamo una retta che segue la regressione lineare, sembra ci sia una relazione lineare

#### punto 2 ####

#Visualizzare la serie storica delle emissioni che abbia sull’asse delle x l’etichetta “Anno” e sull’asse delle
#y “Emissioni di carbonio”. Crearne un grafico interattivo.

climate_to_plotly<-climate_carbon %>% 
  ggplot(aes(x=year, y = carbon_emissions))+
  geom_line()+
  labs(
    x="Anno",
    y="Emissioni Carbonio"
  )

library(plotly)
ggplotly(climate_to_plotly)

# oppure

(climate_carbon %>% 
  ggplot(aes(x=year, y = carbon_emissions))+
  geom_line()+
  labs(
    x="Anno",
    y="Emissioni Carbonio"
  )) %>% 
  ggplotly()

#### punto 3 ####

#Aggiungere una nuova variabile "post60", che assuma valore "Si" se year>1960 e "No" altrimenti. Si
#noti che la funzione cut() permette di discretizzare un vettore numerico.


climate_carbon_new<-climate_carbon %>% 
  mutate(post60 = cut(year, breaks = c(-Inf,1960,Inf), #attenzione a quando devi comprendere certi valori
                      labels = c("no", "si")))


#### punto 4 ####

#Confrontare graficamente la distribuzione delle anomalie di temperatura globale (temp_anomaly) negli
#anni precedenti e seguenti al 1960 sfruttando la variabile appena creata.


climate_carbon_new %>%
  ggplot(aes(x=temp_anomaly, color=post60))+
  geom_density()

#oppure

climate_carbon_new %>% 
  ggplot(aes(x=post60, y=temp_anomaly))+
  geom_boxplot(aes(fill=post60))

#### ES 2 ####

#Caricare il dataset mpg contenuto nel pacchetto ggplot2.

data("mpg")

#### punto 1 ####

#Visualizzare la distribuzione delle frequenze della variabile class.
# in questo caso bar e non density pk la variabile è categorica

mpg %>% 
  ggplot(aes(x=class))+
  geom_bar()

#### punto 2 ####

#Visualizzare la distribuzione delle frequenze della variabile manufacturer. Mettere i nomi delle case
#automobilistiche in verticale per facilitare la lettura del grafico.

mpg %>% 
  ggplot(aes(x=manufacturer))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90))
  

#### punto 3 ####

#Investigare la correlazione tra i consumi in città (cty) ed i consumi in autostrada (hwy)

mpg %>% 
  ggplot(aes(x=cty, y=hwy))+
  geom_point() +
  geom_smooth(se=F)+
  geom_smooth(method = "lm", color= "red", se= F)

#### punto 4 ####

#vedere come la cilindrata (displ) impatta nel confronto precedente.

mpg %>% 
  ggplot(aes(x=cty, y=hwy))+
  geom_point(aes(size= displ, color = displ), alpha=0.4) +
  geom_smooth(se=F)+
  geom_smooth(method = "lm", color= "red", se= F)

#### punto 5 ####

#Confrontare le distribuzioni dei consumi in città (cty) rispetto al tipo di vettura (class), escludendo i
#tipi "2seater" e "minivan"


mpg %>% 
  filter(class!="2seater" & class!="minivan") %>% 
  ggplot(aes(y=cty, x= class))+
  geom_boxplot()

mpg %>% 
  filter(!class %in% c("2seater", "minivan") ) %>% 
  ggplot(aes(y=cty, x= class))+
  geom_violin()+
  geom_point(position = position_jitter(width = 0.2))


#### punto 6 ####

#Riproporre il grafico del punto 4), rappresentando un grafico per ogni tipo di vettura (escludendo gli
#stessi due tipi del punto precedente)

mpg %>% 
  filter(class!="2seater" & class!="minivan") %>% 
  ggplot(aes(x=cty, y=displ))+
  geom_point(aes(size=displ, color = displ), alpha=0.4) +
  geom_smooth(se=F)+
  geom_smooth(method = "lm", color= "red", se= F)+
  facet_grid(~ class)



