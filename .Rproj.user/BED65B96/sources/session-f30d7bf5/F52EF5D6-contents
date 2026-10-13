#carico library

library(tidyverse)
library(FactoMineR)

#install.packages("factoextra")
library(factoextra)

#carico dati

data<-read_csv("data/dati_turismo.csv")

#mettiamo colonna territori come nome righe del dataset

data<-data %>% 
  column_to_rownames("Territorio") 

## matrice di correlazione

cor<-cor(data) #non va pk ci sono dati NA

cor<-cor(data, use="complete.obs") #considera solo le righe piene senza gli NA
#ancora difficile da leggere, possiamo usare un grafico

library(corrplot)

corrplot(cor) #attraverso dimensione e colore ci riporta intensità e segno della correlazione

## PCA

pca_result<-PCA(X=data[complete.cases(data),], scale.unit = T, ncp=10) #deselezioniamo le righe con NA

summary(pca_result)

## regole di selezione PC

#eigenvalues = gli autovalori sono le varianze del nuovo sistema di riferimento
#con le prime due CP spieghiamo il 82.53%
#con la regola di kaiser prime 3 CP (autovalore > media varianza (che è 1 in sto caso pk abbiamo fatto la corr))

fviz_eig(pca_result) #dal grafico ne prendiamo 3 


## che significato diamo alle componenti?

fviz_pca_var(pca_result, axes = c(1,2), repel = T) #repel separa le etichette, axes combina le componenti
#non arriva a uno pk le freccie sono proiettate su uno spaziuo bidimensionale da uno spazio a 14 dimensioni

pca_result$var$coord #coordinate sul grafico per le prime due CP delle variabili
                     #coincidono con correlazione tra nuove variabili e vechhie variabili, questo pk stiamo ragionando su una mat di corr


## altro grafico su variabili

corrplot(t(pca_result$var$cor), is.cor= T) #ogni CP come correla con le variabili


## grafico su indivisui

fviz_pca_ind(pca_result, axes = c(1,2), repel = F)

## grafico individui e variabili insieme

fviz_pca_biplot(pca_result, repel=F,
                col.var = "blue",
                col.ind= "red")

## verifichiamo incorrelazione tra le componenti

corrplot(cor(pca_result$ind$coord)) #questo pk le comp principlai sono indip. pk gli autovettori sono ortogonali








