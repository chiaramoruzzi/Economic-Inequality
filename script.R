#import packages----
library(corrplot)
library(tidyr)
library(plyr)
library(dplyr)
library(psych)
library(ggplot2)
library(GPArotation)

#import original dataset----
setwd("/Users/trava/OneDrive/Desktop/")
data <-rio::import( "ZA7600_v3-0-0.dta")

#creazione dataframe per analisi fattoriale----
data_fa <- data.frame(data$v30, data$v31, data$v50, data$v32, data$v22, data$v23, data$v24, data$v28, data$v34)
colnames(data_fa) <- c("v30", "v31", "v50", "v32", "v22", "v23", "v24", "v28", "v34")

#vedere distribuzione di frequenze----
#fattore 1
descr::freq(data_fa$v30)
descr::freq(data_fa$v31)
descr::freq(data_fa$v50)
descr::freq(data_fa$v32)
#fattore 2
descr::freq(data_fa$v22)
descr::freq(data_fa$v23)
descr::freq(data_fa$v24)
descr::freq(data_fa$v28)
descr::freq(data_fa$v34)


#ricodifica delle variabili----
data_fa <- data_fa %>%
  mutate(v30=mapvalues(v30, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v31=mapvalues(v31, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v50=mapvalues(v50, from=c(-9,-8,1,2,3,4),
                       to=c(NA,NA,4,3,2,1)))%>%
  mutate(v32=mapvalues(v32, from=c(-9,-8,0,1,2,3,4,5,6,7,8,9,10),
                       to=c(NA,NA,1,1,2,2,3,3,3,4,4,5,5)))%>%
  mutate(v22=mapvalues(v22, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v23=mapvalues(v23, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v24=mapvalues(v24, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v28=mapvalues(v28, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v34=mapvalues(v34, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))


#correlation matrix e grafico di correlazione----
cor(data_fa, use = "complete.obs")
corrplot(cor(data_fa, use = "complete.obs"),method = "number")


#analisi fattoriale----

#1. controllare il numero di fattori sia con il metodo pca che paf
fa.parallel(data_fa,fa = "both")#2: utilizzeremo il metodo pca, poiché gli eigenvalue prodotti sono ottimali nel nostro caso


#2. metodo di estrazione PAC
fa <- principal(data1, nfactors = 2, threshold = 0.3)
fa


# Imposta la soglia desiderata
soglia <- 0.4  # Sostituisci con la soglia desiderata

# Visualizza solo i valori superiori alla soglia nella matrice dei risultati
matrmatrice_risultati[matrice_risultati > soglia]
#grafici
#1
variabili_fattori <- fa.diagram(fa)

print(variabili_fattori)
#2
# Estrai carichi dei fattori
loadings <- fa$loadings
soglia <- 0.4
# Elimina i carichi dei fattori inferiori alla soglia
loadings[abs(loadings) < soglia] <- 0


# Creazione del data frame per il grafico
df <- data.frame(
  Factor1 = loadings[, 1],
  Factor2 = loadings[, 2],
  Variable = rownames(loadings)
)
# Creazione del grafico della factor analysis
ggplot(df, aes(x = Factor1, y = Factor2, label = Variable)) +
  geom_point() +
  geom_text(hjust = 1.2) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  xlab("Factor 1") +
  ylab("Factor 2") +
  ggtitle("Factor Analysis Plot")

#last step: variables creation
variabili_fattori <- as.matrix(data1) %*% loadings

# Visualizza le variabili dei fattori
head(variabili_fattori, 10)

data1 <- cbind(data1, variabili_fattori)

hist(dat1$RC1)
hist(dat1$RC2)

plot(data1$v21, data1$RC1)

#F1 from "unfairness beliefs" to "personal beliefs"


setwd("C:/Users/chiar/Desktop/Università/DAPS&CO/SOCIAL AND POLITICAL ATTITUDES/project/Economic-Inequality")
data <-rio::import( "ZA7600_v3-0-0.dta")


data_pr <- data.frame(data$v1, data$v30, data$v31, data$v33, data$v50, data$v32, data$v22, data$v23,
                      data$v24, data$v28, data$v34, data$v21, data$v56, data$v62, data$v63, data$v64, data$v65)
colnames(data_pr) <- c("v1", "v30", "v31", "v33", "v50", "v32", "v22", "v23", "v24", "v28", "v34", "v21", "v56", "v62", "v63", "v64", "v65")

descr::freq(data_pr$v1)
descr::freq(data_pr$v56)
descr::freq(data_pr$v62)
descr::freq(data_pr$v63)
descr::freq(data_pr$v64)              
descr::freq(data_pr$v65)              

# -9 -1 NA
# -8 messi a 3 in scale likert a 5 

data_pr <- data_pr %>%
  mutate(v1=mapvalues(v1, from=c(-9,-8,1,2,3,4,5),
                      to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v56=mapvalues(v56, from=c(-9,-8,1,2,3,4),
                       to=c(NA,NA,4,3,2,1)))%>%
  mutate(v30=mapvalues(v30, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v31=mapvalues(v31, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v22=mapvalues(v22, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v23=mapvalues(v23, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v24=mapvalues(v24, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v28=mapvalues(v28, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v33=mapvalues(v33, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v50=mapvalues(v50, from=c(-9,-8,1,2,3,4),
                       to=c(NA,NA,4,3,2,1)))%>%
  mutate(v34=mapvalues(v34, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v32=mapvalues(v32, from=c(-9,-8,0,1,2,3,4,5,6,7,8,9,10),
                       to=c(NA,NA,1,1,2,2,3,3,3,4,4,5,5))) %>% 
  mutate(v62=mapvalues(v62, from=c(-9,-8,-1,1,2,3,4,5),
                       to=c(NA,3,NA,5,4,3,2,1)))%>%
  mutate(v63=mapvalues(v63, from=c(-9,-8,-1,1,2,3,4,5),
                       to=c(NA,3,NA,5,4,3,2,1)))%>%
  mutate(v64=mapvalues(v64, from=c(-9,-8,-1,1,2,3,4,5),
                       to=c(NA,3,NA,5,4,3,2,1)))%>%
  mutate(v65=mapvalues(v65, from=c(-9,-8,-1,1,2,3,4,5),
                       to=c(NA,3,NA,5,4,3,2,1)))


data_pr <- na.omit(data_pr)

fa1 <- fa.parallel(data_pr, fa="fa")

factanal(~data_pr$v30+data_pr$v31+data_pr$v33+data_pr$v1+data_pr$v50+data_pr$v56+data_pr$v62+data_pr$v63+
           data_pr$v64+data_pr$v65+data_pr$v22+data_pr$v23+data_pr$v24, factors = 2)

fa <- factanal(~data_pr$v62+data_pr$v63+data_pr$v64+data_pr$v65+data_pr$v22+data_pr$v23+data_pr$v24, factors = 2)
fa.plot(fa)
fa.diagram(fa)


#install.packages("GPArotation")
library(GPArotation)

oblimin(fa)


