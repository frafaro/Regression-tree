## SIMULAZIONE ESAME ##

#dopo che lo sposto nella cartella dell'uni, non andrà più ma lo tengo solo come appunti

####Esercizio 1 (18 punti) ####

#1) Caricare il dataset contenuto nel file “data_stud.csv”, creando un oggetto tibble. [1 pt.]

library(tidyverse)
data_stud<-read_csv("data_stud.csv")

#2) Mostrare le distribuzioni delle frequenze assolute per l’appartenza ad una minoranza etnica (Minority)
#e genere (Sex). [2 pt.]

#NB sta parlando di mostrare una freq quindi il numero non il grafico

data_stud %>% 
  count(Minority)

data_stud %>% 
  count(Sex)


#3) Esplorare graficamente (riportando anche una linea di smoothing) la correlazione tra le variabili SES
#(stato socio-economico) e MathAch (punteggio test di matematica). Inoltre, si mettano come titoli degli
#assi: “Status Socio-Economico” e “Punteggio Matematica”. [4 pt.]

data_stud %>%
  ggplot(aes(x = SES, y = MathAch))+
  geom_point()+
  geom_smooth()+
  labs(x = "Status Socio-Economico",
       y="Punteggio Matematica")

#4) Ripetere il grafico precedente, ottenendo però due finestre grafiche separate per maschi e femmine. [2PT] 

data_stud %>%
  ggplot(aes(x = SES, y = MathAch))+
  geom_point()+
  geom_smooth()+
  labs(x = "Status Socio-Economico",
       y="Punteggio Matematica")+
  facet_grid(.~Sex)

#5) Confrontare la distribuzione della variabile MathAch tra i due generi. Scegliere un grafico opportuno.
#Commentare brevemente il grafico e il risultato. [3 pt.]

#faccio il box plot pk voglio osservare la distribuzione che una variable
#quantitativa assume all’interno di sottopopolazioni diverse, individuate da
#una variabile categorica

#da qui osserviamo come la sottopopolazione dei maschi ha una mediana più alta rispetto alla sottopopolazione delle femmine
#anche primo e terzo quartile risultano più alti rispeto alla sottopopolazione femminile

data_stud %>% 
  ggplot(aes(y = MathAch, x = Sex, fill = Sex))+
  geom_boxplot()



#6) Creare un tibble con 3 colonne: scuola (“School”), punteggio SES medio (“Mean_SES”) e punteggio in
#matematica medio (“Mean_MathAch”). [3 pt.]

data_stud2<-data_stud %>% 
  select(-"Minority", -"Sex") %>% 
  group_by(School) %>% 
  summarise(Mean_SES= mean(SES), Mean_MathAch= mean(MathAch))

#7) Aggiungere il tibble creato al punto precedente a quello ottenuto caricando il dataset “data_school.csv”.
#Quale variabile costituisce una chiave ragionevole? Motivare. [3 pt.]

data_school<-read_csv("data_school.csv")

#come chiave utilizzo la variabile school in quanto è presente in entrambi i dataset per avere la prova faccio:

sum(table(data_school$School)!=1) #non ci son o valori che si ripetono quindi la posso usare come chiave primaria

data_school_stud <- data_school %>%
  left_join(data_stud2, by = ("School"="School"))

#### Esercizio 2 (15 pt.) ####

#1) Dividere il dataset ottenuto all’ultimo punto dell’esercizio precedente in un training set con l’80% delle
#unità e il restante test set. Selezionare le unità casualmente, fissando il seme 178. [2 pt.]

set.seed(178) 

training <- data_school_stud %>%
  sample_frac(.8)

test <- anti_join(data_school_stud, training)

#2) Con il training set, stimare un albero con “Mean_MathAch” come responso e tutte le altre variabili a
#parte “School” come regressori. Spiegare brevemente il criterio di ottimizzazione adottato per ottenere
#le divisioni dell’albero. [2 pt.]

#gli split ottimali dell'albero si ottengondo 
#minimizzando la somma dei residui al quadrato (RSS), cercando di ottenere gruppi il più
#omogrnri possibile al loro interno

library(rpart)
library(rpart.plot)

tree <- rpart(formula = Mean_MathAch~Size+Sector+PRACAD+DISCLIM+Mean_SES,
              data=training,
              method = "anova")


rpart.plot(tree)

#3) Che procedura viene adottata per determinare una versione più efficiente dell’albero di regressione?
#  Effettuarla, ottenendo quindi un albero definitivo. [3 pt.]


#il cosiddetto pruning, consiste nella potatura dell'albero effettuata mediante un parametro alpha di complessità,
#più alpha è elevato e più l'albero risulterà sintetizzato,
#più alpha è basso e più lalbero è complesso


plotcp(tree) #come valore di alpha devo usare 0.034

tree_pruned <- prune(tree, cp = 0.034)

#4) Mostrare graficamente l’albero ottenuto e dire quali variabili sono ritenute importanti nella procedura.
# Quali sono i valori assunti dalle previsioni fatte con l’albero? [3 pt.]


rpart.plot(tree_pruned) #le variabili ritenute importanti sono Mean_SES e PRACAD

#i valori possibili sono le medie dei gruppi individuati rispettivamente:
# 6.6 9.9 12 15 18

#5) Calcolare l’MSE sul test set utilizzando l’albero ottimale determinato al punto precedente

pred_val<-predict(tree_pruned, newdata = test)

MSE<- mean((pred_val-test$Mean_MathAch)^2)

#6 Stimare un bagging, impiegando le stesse variabili usate per l’albero di regressione. [2 pt.]

library(randomForest)

rf1<-randomForest(formula = training$Mean_MathAch~Size+Sector+PRACAD+DISCLIM+Mean_SES,
                  data=training, mtry = 5, #pk già c'era una colonna in meno e ora devo fare anche -1 quindi 7-2 = 5
                  importance = T)

#7) Calcolare l’MSE sul test set e commentare il risultato. [2 pt.]

pred_val2<-predict(rf1, newdata = test)

MSE2<- mean((pred_val2-test$Mean_MathAch)^2)

#si ha una riduzione dell'errore quadratico medio




