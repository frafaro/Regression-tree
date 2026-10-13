# ESERCITAZIONE PERSONALE

library(tidyverse)


#### ES 1 ####

#### punto 1 ####

# Renderlo un dataset tidy creando le variabili year e GDP. Salvarlo come nuovo file CSV denominato
# “GDP_USA_tidy.csv”

GDP_USA<-read_csv(file = "data/GDP_USA.csv")

GDP_USA_1<-GDP_USA %>% 
  pivot_longer(-State, names_to = "year", values_to = "GDP")

write_csv2(GDP_USA_1, file = "data/GDP_USA_tidy.csv")

#### punto 2 ####

# Calcolare il GDP annuale degli Stati Uniti come somma dei GDP dei singoli stati. Salvarlo in un nuovo
# oggetto chiamato GDP_tot.


GDP_tot<-GDP_USA_1 %>% 
  group_by(year) %>% 
  summarise(GDP_state = sum(GDP))

#### punto 3 ####

#Calcolare il GDP medio per gli stati e salvarlo in Average_GDP.

Average_GDP<-GDP_USA_1 %>% 
  group_by(State) %>% 
  summarise(Average_GDP = mean(GDP))

#### punto 4 ####

#Individuare gli stati con GDP medio più alto e più basso

Average_GDP %>% 
  filter( Average_GDP==min(Average_GDP, na.rm=T) | Average_GDP==max(Average_GDP, na.rm = T))



#### ES 2 ####

library(nycflights13)
data("flights")  
flights
data("airports")
airports

#### punto 1 ####

#Creare un dataset flights_JFK che contenga:
#a) Le osservazioni che hanno partenza da "JFK"
#b) Le variabili flight, tailnum, dest, time_hour e distance

flights_JFK <- flights %>% 
  filter( origin == "JFK") %>% 
  select(flight, tailnum, dest, time_hour, distance)

#### punto 2 ####

#Stabilire le prime 10 destinazioni più frequenti da JFK.
 flights_JFK %>% 
  count(dest) %>% 
  arrange(desc(n)) %>% 
  print(n=10)

#### punto 3 ####

#Stabilire le prime 10 destinazioni più lontane da JFK.

flights_JFK_new<-flights_JFK[!duplicated(flights_JFK$dest),] 

flights_JFK_new %>% 
  arrange(desc(distance))


#### punto 4 ####

#Aggiungere al dataset le informazioni sull’aeroporto di destinazione, presenti in airports:
# a) Si individuino le chiavi adatte, verificando ne esista una primaria.
# b) Scegliere la funzione di join più adatta, motivando.

#chiave primaria faa in airports

flights_JFK<-flights_JFK%>% 
  left_join(airports, by= c("dest"="faa"))



