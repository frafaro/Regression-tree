#ESERCITAZIONE CORRETTA

#### ES 1 ####

library(tidyverse)

#punto 1

GDP_USA<- read_csv("data/GDP_USA.csv")

GDP_USA_tidy<-GDP_USA %>% 
  pivot_longer(-State, names_to = "year", values_to = "GDP")

write_csv2(GDP_USA_tidy, file = "data/GDP_USA_tidy.csv")

#punto 2

GDP_tot<-GDP_USA_tidy %>% 
  group_by(year) %>% 
  summarise(GDP_state = sum(GDP))

#punto 3

Average_GDP<-GDP_USA_tidy %>% 
  group_by(State) %>% 
  summarise(Average_GDP = mean(GDP))

#punto 4

Average_GDP %>% 
  filter( Average_GDP==min(Average_GDP, na.rm=T) | Average_GDP==max(Average_GDP, na.rm = T))



#### ES 2 ####

#punto 1

library(nycflights13)
data("flights")  
data("airports")

str(flights)

flights_JFK <- flights %>% 
  filter( origin == "JFK") %>% 
  select(flight, tailnum, dest, time_hour, distance)

# punto 2

flights_JFK %>%           #metodo 1 
  count(dest) %>% 
  arrange(desc(n)) %>% 
  print(n=10)

flights_JFK %>%          #metodo 2
  group_by(dest) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  print(n=10)

#punto 3

flights_JFK %>%         #metodo 1
  group_by(dest) %>% 
  summarise(dist= distance[1]) %>% 
  arrange(desc(dist)) %>% 
  print(n=10)

flights_JFK %>%         #metodo 2
  select(dest, distance) %>% 
  distinct() %>% 
  arrange(desc(distance)) %>% 
  print(n=10)

#punto 4

airports %>% 
  count(faa) %>% 
  filter(n>1) 

#chiave primaria faa in airports infatti non ci sono valori che si ripetono

flights_JFK<-flights_JFK%>% 
  left_join(airports, by= c("dest"="faa")) #nome col primo data set = nome col secondo data set

#### ES 3 #####

# punto 1 

athlet_new<- athlete_events %>% 
  group_by(ID,Name) %>% 
  summarise( prima_partecipazione = min(Year), 
             ultima_partecipazione = max(Year), 
             part_inv= sum(Season=="Winter"),
             part_estive= sum(Season=="Summer"), 
             oro= sum(Medal=="Gold", na.rm = T),
             argento= sum(Medal=="Silver", na.rm = T), 
             bronzo= sum(Medal=="Bronze", na.rm = T),
             Sex= Sex[1])

#poteva anche essere messo in group_by visto che cmq è univoco
#ID ci vuole pk ci sono degli omonimi

# punto 2 

athlet_new %>% 
  filter(part_inv>0 & part_estive>0)

#punto 3

athlet_new<-athlet_new %>% 
  mutate( Tot_medal = sum(oro, argento, bronzo), range= ultima_partecipazione-prima_partecipazione )

#punto 4 

athlet_new %>% 
  arrange(desc(Tot_medal)) %>% 
  print(n=5)

#punto 5

athlet_new %>% 
  filter(Sex== "F") %>% 
  arrange(desc(range)) %>% 
  print(n=10)

