## PCA

library(tidyverse)
install.packages("FactoMineR")
library(FactoMineR)

#carico dati
size<-read_csv("data/size.csv")

#scatter plot
size %>% 
  ggplot(aes(x=weights, y = heights))+
  geom_point()

#matrice varianza covarianza
mat_var<- var(size)

#matirce correlazione
mat_cor<- cor(size)

#### PCA ####

pca_result <- PCA( X = size, scale.unit = T) #dim 1 e dim 2 percentuale di varianza spiegata dalle due componenti principali

pca_result$eig #autovalore è anche la varianza delle nuove componenti quindi dagli eig si ricava la proporzione di varianza spiegata
pca_result$var

#coord c'è vettori con valori componenti principali a1 e a2 (colonne)

#cor quanto è correlata la nuova componente principale alla vecchia variabile

pca_result$ind

#info che riguardano le nuove coordinate degli individui rispetto alle componenti principali
#coord coordinate che hanno i punti nel nuovo sistema di riferimento

#### plot nuova nuvola dei punti ####

pca_result$ind$coord %>% 
  as_tibble() %>% 
  ggplot(aes(x= Dim.1, y= Dim.2))+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)


