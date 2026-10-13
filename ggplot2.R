# GGPLOT 2

library(tidyverse)

data("diamonds")

#### grafico a barre ####

#frequenza del colore
#variabili qualitative

diamonds %>% 
  group_by(color) %>% 
  summarise(n=n())       #questa è la tabella che raggruppa i diamanti per colore

diamonds %>%              #questo è il grafico a barre della tabella precedente
  ggplot(aes(x = color)) +
  geom_bar()

#### grafico a barre raggruppato ####

diamonds %>% 
  group_by(color, cut) %>% 
  summarise(n=n())

diamonds %>%                       #per ogni colore mi da la frequenza della qualità del taglio nella barra
  ggplot(aes(x=color, fill=cut))+
  geom_bar()

diamonds %>%                       #per ogni colore mi dice la freq della qualità del taglio ma su più barre 
  ggplot(aes(x=color, fill=cut))+
  geom_bar(position = "dodge")

diamonds %>%                       #frequenza percentuale  
  ggplot(aes(x=color, fill=cut))+
  geom_bar(position = "fill")

#### cambiare nomi nel grafico ####

#tengo il grafico precedente

#metodo 1

diamonds %>%                       
  ggplot(aes(x=color, fill=cut))+
  geom_bar(position = "dodge")+
  ylab("Frequenza")+
  xlab("Colore")+
  scale_fill_discrete("Taglio") #con scale si accede alla legenda, con fill pk il nome usato in aes era fill, 
                                #discrete pk i colori sono discreti

#metodo 2

diamonds %>%                       
  ggplot(aes(x=color, fill=cut))+
  geom_bar(position = "dodge")+
  labs(
    x= "Colore",
    y= "Frequenza",
    fill= "Taglio"
  )
  
#### istogramma ####
#variabili quantitative


diamonds %>%            #bins è la suddivisione dei dati in gruppi, di base è 30 ma si può cambiare
ggplot(aes(x=price))+
  geom_histogram( bins = 10)

#### kernel density ####

diamonds %>%              #è una distr di probabilità
  ggplot(aes(x= price)) +
  geom_density() 

diamonds %>%              #distribuzione dei sottogruppi rispetto al prezzo
  ggplot(aes(x= price, color=cut)) +
  geom_density() 

#### unione grafici ####

diamonds %>%              
  ggplot(aes(x= price)) +
  geom_density()+
  geom_histogram(aes(y= after_stat(density))) #density dietro e hist davanti pk c'è gerarchia nella sovrapposizione
                                              #after stat ci permette di unire due grafici con scale diverse
diamonds %>%              
  ggplot(aes(x= price)) +
  geom_histogram(aes(y= after_stat(density)))+
  geom_density(color= "red")  #density davanti

#### box plot ####

diamonds %>% 
  ggplot(aes(x=price))+         #linea centrale mediana
  geom_boxplot()                #inizio e fine del box sono primo e terzo quartile
                                #quarti quartile lungo max una volta e mezzo la grandezza del box, dopo outliers

diamonds %>%                    #y quantitativa, x qualitativa
  ggplot(aes(y=price, x= cut, fill=cut))+         
  geom_boxplot() 


#### scatter plot ####
#due variabili quantitative

diamonds %>% 
  ggplot(aes(x=carat, y=price))+
  geom_point() +                
  geom_smooth() # linea che richiama il concetto di media mobile, per studiare la forma della relazione tra le due variabili

#aggiungere una terza variabile allo scatter plot

#scatter plot con terza variabile qualitativa

diamonds %>% 
  ggplot(aes(x=carat, y=price))+
  geom_point(aes(color=color, shape=cut))      #color colora le osservazioni in base alla v. color,
                                               # shape cambia la forma delle oss. in base alla v. cut
  

diamonds %>%                                  #crea diversi grafici in base alle varie combinazione dei caratteri in cut e color
  ggplot(aes(x=carat, y=price))+
  geom_point()+
  facet_grid(color~cut)                       #per solo una var, "var~."



#### serie storica ####

# install.packages("tidyquant")
library(tidyquant)

prices<- tq_get(c("MSFT", "AAPL"),
                get = "stock.prices",
                from= "2005-01-01")           #anno mese giorno

#grafico prezzi microsoft

prices %>% 
  filter(symbol=="MSFT") %>% 
  ggplot(aes(x=date, y = adjusted))+
  geom_line()+
  scale_x_date(date_labels = "%m-%Y") #cambia nome asse x con mese e anno di riferimento

#i due grafici assieme, e cambiare colore

prices %>% 
  ggplot(aes(x=date, y = adjusted))+
  geom_line(aes(color=symbol))+
  scale_x_date(date_labels = "%m-%Y")+
  scale_color_manual(values = c("AAPL"= "gray30", "MSFT"= "blue"))+ #cambia colore time series
  theme_bw(base_size = 15)+ #cambia colore sfondo
  labs(
    x="Data",
    y="Prezzo Aggiustato",
    color= "Titolo"
  )

#### ggplotly ####
#con la f ggsave() salva l'ultimo grafico creato nel formato che uno vuole 
# con ggplotly() il grafico di ggplot diventa interattivo, e si puo usare non solo su R

plot_to_plotly<-prices %>% 
  ggplot(aes(x=date, y = adjusted))+
  geom_line(aes(color=symbol))+
  scale_x_date(date_labels = "%m-%Y")+
  scale_color_manual(values = c("AAPL"= "gray30", "MSFT"= "blue"))+ #cambia colore time series
  theme_bw(base_size = 15)+ #cambia colore sfondo
  labs(
    x="Data",
    y="Prezzo Aggiustato",
    color= "Titolo"
  )


library(plotly)

ggplotly(plot_to_plotly)

#bar plot con ggplotly

diamonds %>% 
  count(color) %>% 
  plot_ly(x= ~color, y=~n, type = "bar")

#scatter plot 3d

diamonds %>% 
  plot_ly(x= ~carat, y=~price, z=~depth) 

#### ggsave ####
  
prices %>% 
  ggplot(aes(x=date, y = adjusted))+
  geom_line(aes(color=symbol))+
  scale_x_date(date_labels = "%m-%Y")+
  scale_color_manual(values = c("AAPL"= "gray30", "MSFT"= "blue"))+ #cambia colore time series
  theme_bw(base_size = 15)+ #cambia colore sfondo
  labs(
    x="Data",
    y="Prezzo Aggiustato",
    color= "Titolo"
  )

ggsave(filename = "plot_azioni.pdf", device = "pdf", width = 30, height = 20, units = "cm", dpi = 150)

#### più grafici nello stesso file ####

install.packages("gridExtra")
library(gridExtra)

grid.arrange(plot_to_plotly, plot_to_plotly, ncol=1)

