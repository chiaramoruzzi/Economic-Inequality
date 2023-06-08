##---------import packages----
library(corrplot)
library(tidyr)
library(plyr)
library(dplyr)
library(psych)
library(ggplot2)
library(GPArotation)
library(ggrepel)
install.packages("Rgraphviz")

##---------import original dataset----
setwd("/Users/trava/OneDrive/Desktop/")
setwd("C:/Users/chiar/Desktop/Università/DAPS&CO/SOCIAL AND POLITICAL ATTITUDES/project/Economic-Inequality")

data <-rio::import( "ZA7600_v3-0-0.dta")


##---------creazione dataframe per analisi fattoriale----
data_fa <- data.frame(data$v30, data$v31, data$v32, data$v22, data$v23, data$v24, data$v28, data$v34)
colnames(data_fa) <- c("v30", "v31", "v32", "v22", "v23", "v24", "v28", "v34")


##---------vedere distribuzione di frequenze----
#fattore 1
descr::freq(data_fa$v30)
descr::freq(data_fa$v31)
descr::freq(data_fa$v21)
descr::freq(data_fa$v32)
#fattore 2
descr::freq(data_fa$v22)
descr::freq(data_fa$v23)
descr::freq(data_fa$v24)
descr::freq(data_fa$v28)
descr::freq(data_fa$v34)


##---------ricodifica delle variabili----
data_fa <- data_fa %>%
  mutate(v30=mapvalues(v30, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,1,2,3,4,5)))%>%
  mutate(v31=mapvalues(v31, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,1,2,3,4,5)))%>%
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
#mutate(v21=mapvalues(v21, from=c(-9,-8,1,2,3,4,5),
#to=c(NA,3,5,4,3,2,1)))%>%

##---------NA remove-----
data_fa<-na.omit(data_fa)

##---------correlation matrix e grafico di correlazione----
cor(data_fa, use = "complete.obs")
corrplot(cor(data_fa, use = "complete.obs"),method = "number")



##---------analisi fattoriale----
#1. controllare il numero di fattori sia con il metodo pca che paf
fa.parallel(data_fa,fa = "pc")#2: utilizzeremo il metodo pca, poiché gli eigenvalue prodotti sono ottimali nel nostro caso
#2. metodo di estrazione PAC
fa <- principal(r=data_fa, nfactors = 2, rotate = "promax", missing = TRUE)
print.psych(fa)


##---------grafici----
#1 diagramma
fa.diagram(fa, cut = 0.4)
#2 piano cartesiano
# Estrai carichi dei fattori
loadings <- fa$loadings
#Creazione del data frame per il grafico
df <- data.frame(
  Factor1 = loadings[, 1],
  Factor2 = loadings[, 2],
  Variable = rownames(loadings))
# Creazione del grafico della factor analysis
ggplot(df, aes(x = Factor1, y = Factor2, label = Variable)) +
  geom_point() +
  geom_label_repel(hjust = 0.5, position = "identity", box.padding = 0.16, label.size = 0.1) +
  geom_vline(xintercept = 0, lty=2) +
  geom_hline(yintercept = 0, lty=2) +
  xlab("Factor 1") +
  ylab("Factor 2") +
  ggtitle("Factor Analysis Plot with promax rotation") +
  theme_minimal()
#è utile per vedere la vicinanza dei punti delle variabili rispetto agli assi dei fattori
#inoltre, se la plotti prima e dopo aver impostato rotate, si vede che i punti si spostano dal punto originale!


##---------creazione delle variabili----
variabili_fattori <- as.matrix(data_fa) %*% loadings
#Visualizza le variabili dei fattori
head(variabili_fattori, 10)
#aggiungere le nuove variabili al dataset
data_fa <- cbind(data_fa, variabili_fattori)

rc2 <- (data_fa$v30 + data_fa$v31)
rc1 <- (data_fa$v23 + data_fa$v22 + data_fa$v34 + data_fa$v24)

##----------subset del dataframe eliminando variabili superflue----
data_fa1 <- data.frame(data_fa$v30, data_fa$v31, data_fa$v32, data_fa$v22, data_fa$v23, data_fa$v24, data_fa$v34)
#factor analysis
fa1 <- principal(r=data_fa1, nfactors = 2, rotate = "varimax")
print.psych(fa1, cut = 0.4)
#diagramma
fa.diagram(fa1, digits = 2)
# Estrai carichi dei fattori
loadings1 <- fa1$loadings
variabili_fattori1 <- as.matrix(data_fa1) %*% loadings1
#Visualizza le variabili dei fattori
head(variabili_fattori1, 10)
#aggiungere le nuove variabili al dataset
data_fa1 <- cbind(data_fa1, variabili_fattori1)

print(fa1)

## indice composito: attitude towards economic inequality
# range 1-5= 1-> sfavore ridurre disuguaglianze
#            5-> favore ridurre disuguaglianze

plot(density(data_fa1$RC1, na.rm = T))
plot(density(data_fa1$RC2, na.rm = T))

att_eco <- (data_fa1$RC1 + data_fa1$RC2)

plot(density(att_eco))
range(att_eco)

att_eco <- mean(data_fa1$RC2 + data_fa1$RC1)


# import Gini index data

library(readr)
gini <- read_csv("Gini index 2018 2019.csv")
View(gini)






#FINE----






