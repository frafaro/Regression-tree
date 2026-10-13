# ESERCITAZIONE ESAME

#### Esercizio 1 (16 punti) ####

#Il file "seasonal_temp.csv" contiene le temperature medie stagionali (in °F) registrate in una città americana.

#1) Caricare il dataset creando un oggetto tibble. [1 pt.]

library(tidyverse)
seasonal_temp<- read_csv("data/seasonal_temp.csv")

#2) Spiegare cosa si intende per dataset tidy. Motivare se il dataset caricato risponde ai requisiti. [2 pt.]

# le colonne sono variabili e le righe sono le unità statistiche, in questo caso non
# corrisponde ai requisiti pk la variabile è la temperatura  che è presente in più colonne

#3) Trasformare il dataset in un nuovo dataset con 3 colonne : Year, Season, Temp. [3 pt.]

seasonal_temp2 <- seasonal_temp %>% 
  pivot_longer( c("Winter", "Spring", "Summer", "Fall"), names_to = "Season", #al posto di c(...) anche -Years
                values_to = "Temp")

#Per chi non riesce: si carchi il dataset "seasonal_temp2.csv" per le prossime domande.

#4) Si mostrino solo le osservazioni relative all’autunno (Fall). [1 pt.]

seasonal_temp2 %>% 
  filter(Season=="Fall")

#5) Si rappresenti in un grafico l’andamento della temperatura in autunno nel corso degli anni. [3 pt.]

seasonal_temp2 %>% 
  filter(Season=="Fall") %>% 
  ggplot(aes(x=Year, y = Temp))+
  geom_line()

#6) Si mostrino ora gli andamenti relativi a tutte le stagioni in un unico grafico, assumendo un colore
#diverso per ogni stagione. [3 pt.]

seasonal_temp2 %>% 
  ggplot(aes(x=Year, y = Temp))+
  geom_line(aes(color=Season))

#7) Calcolare le temperature medie e le relative deviazioni standard per ogni stagione.[3 pt.]

seasonal_temp2 %>% 
  group_by(Season) %>% 
  summarise(Average_Temp = mean(Temp),
            dev_temp = sqrt(var(Temp))) # o con formula sd


#### Esercizio 2 (17 punti) ####

#Il file "decathlon_data.csv" contiene osservazioni circa prestazioni di atleti nelle varie discipline del decathlon.

#1) Caricare il dataset creando un oggetto tibble. [1 pt.]

decathlon<- read_csv("data/decathlon_data.csv")

#2) Impostare la colonna Name a nominativi delle righe del dataset. [1 pt.]

decathlon<- decathlon %>% 
  column_to_rownames("Name")

#3) Selezionare solo le osservazioni relativi alla Competition uguale a "OlympicG". [1 pt.]

decathlon %>% 
  filter(Competition== "OlympicG")

#4) Rimuovere le variabili Competition,Points e Rank [1 pt.]

decathlon2<-decathlon %>% 
  select(-c("Competition", "Points", "Rank"))

#5) Visualizzare le correlazioni tra le variabili tramite un grafico opportuno. Visto il risultato, cosa si può
#dire circa l’utilità di applicare l’analisi delle componenti principali ai dati? [2 pt.]

library(corrplot)
cor<-cor(decathlon2, use= "complete.obs")

corrplot(cor)

#utile applicare la PCA pk le variabili mostrano correlazione elevata

#6) Effettuare l’analisi delle componenti principali, tenendo tutte le componenti. Scegliere e motivare se
#l’analisi va preferibilmente fatta sulla matrice di correlazione o di covarianza. [2 pt.]

library(FactoMineR)
library(factoextra)

pca_result<-PCA(X=decathlon2[complete.cases(decathlon2),], scale.unit = T, ncp=10)
summary(pca_result)

#l'analisi in questo caso è preferibile farla sulla matrice di correlazione in quanto le variabili hanno
# unità di misuara differenti
#mat var-cov usata solo se le varisbili sono in %

#7) Individuare il numero corretto di componenti secondo le 3 regole note. Sceglerne poi un numero
#definitivo, motivando. [4 pt.]

#regola varianza cumulata scelgo le prime 4 che spiegano il 74,708%
pca_result$eig[,3]

#regola di kaiser scelgo le prime 4 pk hanno varianza > di 1
pca_result$eig[,1]

#con regola grafica scelgo le prime 4 in quanto dalla 5 la curva si inizia ad appiattire
fviz_eig(pca_result)

#8) Riportare λ2 e a1, spiegando di che quantità si trattano. [2 pt.]

pca_result$eig[2] #autovalore della seconda CP (varianza della seconda componente principale)

pca_result$var$coord[,1] #autovettore della prima CP

#9) Commentare i significati che possono essere dati alle prime due componenti principali. [3 pt.]

pca_result$var$cor[, c(1,2)]

corrplot(t(pca_result$var$cor), is.cor= T)
fviz_pca_biplot(pca_result, repel=T,
                col.var = "blue",
                col.ind= "red")

#la prima CP sembra essere negativamente correlata agli sport di corsa veloce e positivamente correlata
# alle altre discipline

#dal biplot possiamo osservare che le oss più estreme sono gli atleti con i punteggi migliori in quelle discipline

#con la seconda componente pirncipale invcece possiamo vedere le discipline dove gli atleti sono andati peggio



