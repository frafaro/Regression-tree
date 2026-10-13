#esercitazione pca


#### es 1 ####

library(tidyverse)
dati<-read_csv("data/dati_energia.csv")

####punto 1 ####

#Spostare la variabile Territorio come nome delle righe del tibble

dati <-dati %>% 
  column_to_rownames("Territorio")

####punto 2 ####

#Indagare preliminarmente le correlazioni esistenti tra le variabili del dataset. Appare utile impiegare la
#PCA?

cor<-cor(dati, use= "complete.obs")
library(corrplot)

corrplot(cor)

#abbiamo molte variabili tra loro correlate quindi la PCA è utile quindi possono essere semplificate
#all'interno di nuove CP che porteranno la stessa informazione

####punto 3 ####

#Effettuare la PCA. Individuare il numero di componenti appropriato secondo le 3 regole note. Scegliere,
#motivando, il numero di componenti con cui si decide di portare avanti l’analisi.

library(FactoMineR)
library(factoextra)

pca_result<-PCA(X=dati[complete.cases(dati),], scale.unit = T, ncp=10)
summary(pca_result)

#regola varianza cumulata scelgo le prime 3 che spiegano il 77,10%
pca_result$eig
#regola di kaiser scelgo le prime 4 pk hanno varianza > di 1
pca_result$eig[,1]
#con regola grafica scelgo le prime 3

fviz_eig(pca_result)

#quindi direi che scelgo le prime 3 variabili


####punto 4 ####

#Riportare λ4, a2 e le coordinate necessarie a proiettare l’osservazione 10 nel nuovo sistema di riferimento.

pca_result$eig[4] #autovalore 4 CP

pca_result$var$coord[,2] #autovettore della seconda CP

#### prova

pca_result$var$coord
pca_result$ind$coord

#oss n10 

pca_result$ind$coord[10,] #brescia


#### punto 5 ####

#Verificare che le componenti principali trovate sono tra loro incorrelate


corrplot(cor(pca_result$ind$coord))

#### punto 6 ####

#Interpretare i coefficienti degli autovettori delle prime due componenti principali.

pca_result$var$coord

corrplot(t(pca_result$var$cor), is.cor= T)

#nella prima CP i valori sono tutti positivi, questo potrebbe identificare la domanda di energia richiesta 
#la seconda CP anche se non in maniera netta inizia a suddividere i tipi di energia richiesta

#### punto 7 ####

#È possibile trarre qualche conclusione dalle coordinate delle osservazioni nel nuovo sistema di riferimento?
fviz_pca_biplot(pca_result, repel=F,
                col.var = "blue",
                col.ind= "red")

#le osservazioni che si trovano in basso a destra sono più collegate al consumo di enrgia infatti
#sono presenti le città più grandi

#le oss che si trovano in altro a destra sono più collegate alla produzione di rinnovabili e idroellettrrico
#infatti sono più presenti città di montagna








