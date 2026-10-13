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

#### ES 3 ####

#### punto 1 ####

#1) Si passi ad un dataset in cui l’unità statistica è l’atleta con le seguenti variabili: nome atleta, numero di
#partecipazioni alle gare di olimpiadi invernali, numero partecipazioni alle gare di olimpiadi estive, anno
#prima partecipazione, anno ultima partecipazione, numero di medaglie d’oro, argento e bronzo, genere.

athlete_events<-read_csv("data/athlete_events.csv")

#nuova tabella

athlet_new<- athlete_events %>% 
  group_by(ID,Name, Sex) %>% 
  summarise( prima_partecipazione = min(Year), ultima_partecipazione = max(Year), part_inv= sum(Season=="Winter"),
             part_estive= sum(Season=="Summer"), oro= sum(Medal=="Gold", na.rm = T),
             argento= sum(Medal=="Silver", na.rm = T), bronzo= sum(Medal=="Bronze", na.rm = T))


#### punto 2 ####

#Stabilire se ci sono atleti che hanno partecipato sia ad olimpiadi estive che invernali.

athlet_new %>% 
  filter(part_inv>0 & part_estive>0)

#### punto 3 ####

#Creare una variabile con la somma del numero di medaglie e una con il range temporale delle participazioni.

athlet_new<-athlet_new %>% 
  mutate( Tot_medal = sum(oro, argento, bronzo), range= ultima_partecipazione-prima_partecipazione )

#### punto 4 ####

#Mostrare i 5 atleti con il numero più alto di medaglie vinte complessivamente.

athlet_new %>% 
  arrange(desc(Tot_medal)) %>% 
  print(n=5)

#### punto 5 ####

#Mostrare le 10 atlete di genere femminile che hanno partecipato più a lungo alle olimpiadi.

athlet_new %>% 
  filter(Sex== "F") %>% 
  arrange(desc(range)) %>% 
  print(n=10)

#### punto 6 ####


#Pensare alla costruzione di un dataset che abbia come unità statistica lo Sport. Costruirlo inserendo
#variabili ottenute da sintesi del dataset originario che siano ragionevoli.

sport<- athlete_events %>% 
  group_by(Sport) %>% 
  summarise(oro= sum(Medal=="Gold", na.rm = T),
            argento= sum(Medal=="Silver", na.rm = T),
            bronzo= sum(Medal=="Bronze", na.rm = T),
            partecipanti_tot= sum(n=n()),
            età_media= mean(Age, na.rm=T),
            peso_max = max(Weight, na.rm = T))















