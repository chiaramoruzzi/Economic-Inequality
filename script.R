##---------import packages----
library(corrplot)
library(tidyr)
library(plyr)
library(dplyr)
library(psych)
library(ggplot2)
library(GPArotation)
library(ggrepel)
library(ltm)
library(readr)
library(ggeffects)



##---------import original dataset----
#setwd("/Users/trava/OneDrive/Desktop/")
#setwd("C:/Users/chiar/Desktop/Università/DAPS&CO/SOCIAL AND POLITICAL ATTITUDES/project/Economic-Inequality")

data <-rio::import( "ZA7600_v3-0-0.dta")


##---------creazione dataframe per analisi fattoriale----
#data_fa <- data.frame(data$v30, data$v31, data$v32, data$v22, data$v23, data$v24, data$v28, data$v34)
#colnames(data_fa) <- c("v30", "v31", "v32", "v22", "v23", "v24", "v28", "v34")
data_fa <- data.frame(data$country, data$v21,data$v30, data$v31, data$v32, data$v22, data$v23, data$v24, data$v28, data$v34)
colnames(data_fa) <- c("country", "v21", "v30", "v31", "v32", "v22", "v23", "v24", "v28", "v34")

##---------vedere distribuzione di frequenze----
#fattore 1
descr::freq(data_fa$v30)
descr::freq(data_fa$v31)
descr::freq(data$v50)
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
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v21d=mapvalues(v21, from=c(-9,-8,1,2,3,4,5),
                        to=c(NA,0,1,1,0,0,0)))%>%
  mutate(v21=mapvalues(v21, from=c(-9,-8,1,2,3,4,5),
                        to=c(NA,3,5,4,3,2,1)))
  

##---------NA remove-----
data_fa<-na.omit(data_fa)

##---------correlation matrix e grafico di correlazione----
#cor(data_fa, use = "complete.obs")
#corrplot(cor(data_fa, use = "complete.obs"),method = "number", type = c("lower"))

###---------analisi fattoriale----
##1. controllare il numero di fattori sia con il metodo pca che paf
#fa.parallel(data_fa,fa = "pc")#2: utilizzeremo il metodo pca, poiché gli eigenvalue prodotti sono ottimali nel nostro caso
##2. metodo di estrazione PAC
#fa <- principal(r=data_fa, nfactors = 2, rotate = "promax", missing = TRUE)
#print.psych(fa)
#
#
###---------grafici----
##1 diagramma
#fa.diagram(fa, cut = 0.4, digits = 2)
#
##2 piano cartesiano
## Estrai carichi dei fattori
#loadings <- fa$loadings
##Creazione del data frame per il grafico
#df <- data.frame(
#  Factor1 = loadings[, 1],
#  Factor2 = loadings[, 2],
#  Variable = rownames(loadings))
## Creazione del grafico della factor analysis
#ggplot(df, aes(x = Factor1, y = Factor2, label = Variable)) +
#  geom_point() +
#  geom_label_repel(hjust = 0.5, position = "identity", box.padding = 0.16, label.size = 0.1) +
#  geom_vline(xintercept = 0, lty=2) +
#  geom_hline(yintercept = 0, lty=2) +
#  xlab("Factor 1") +
#  ylab("Factor 2") +
#  ggtitle("Factor Analysis Plot with promax rotation") +
#  theme_minimal()
##è utile per vedere la vicinanza dei punti delle variabili rispetto agli assi dei fattori
##inoltre, se la plotti prima e dopo aver impostato rotate, si vede che i punti si spostano dal punto originale!
#
#
###---------creazione delle variabili----
#variabili_fattori <- as.matrix(data_fa) %*% loadings
##Visualizza le variabili dei fattori
#head(variabili_fattori, 10)
##aggiungere le nuove variabili al dataset
#data_fa <- cbind(data_fa, variabili_fattori)


##----------subset del dataframe eliminando variabili superflue----
#data_fa1 <- data.frame(data_fa$v30, data_fa$v31, data_fa$v32, data_fa$v22, data_fa$v23, data_fa$v24, data_fa$v34)
#colnames(data_fa1) <- c("v30", "v31", "v32", "v22", "v23", "v24", "v34")
##----------subset 2
country_e_v50 <- subset(data_fa[,c(1,2,11)])
data_fa1 <- subset(data_fa[,c(3:8,10)])

#factor analysis
fa.parallel(data_fa1, fa="pc")

fa1 <- principal(r=data_fa1, nfactors = 2, rotate = "varimax")
print.psych(fa1, cut = 0.4)
###---------grafici----
##1 diagramma
fa.diagram(fa1, cut = 0.4)

#2 piano cartesiano
## Estrai carichi dei fattori
loadings <- fa1$loadings
##Creazione del data frame per il grafico
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
##è utile per vedere la vicinanza dei punti delle variabili rispetto agli assi dei fattori
##inoltre, se la plotti prima e dopo aver impostato rotate, si vede che i punti si spostano dal punto originale!


# Creazione variabili fattori
variabili_fattori1 <- as.matrix(data_fa1) %*% loadings
#Visualizza le variabili dei fattori
head(variabili_fattori1, 10)
#aggiungere le nuove variabili al dataset
data_fa1 <- cbind(data_fa1, variabili_fattori1)


##---------metodo alternativo per la creazione delle variabili fattori-----

# indici non pesati
# rc2 <- (data_fa1$v30 + data_fa1$v31 + data_fa1$v32)
# rc1 <- (data_fa1$v23 + data_fa1$v22 + data_fa1$v34 + data_fa1$v24)

# indici pesati per factor loadings - controllo incrociato
# F2 <- ((0.91*data_fa1$v30) + (0.91*data_fa1$v31) + (0.51*data_fa1$v32))
# F1 <- ((0.74*data_fa1$v23) + (0.68*data_fa1$v22) + (0.64*data_fa1$v34) + (0.61*data_fa1$v24))

## indice composito: attitude towards economic inequality
# range 1-5= 1-> sfavore ridurre disuguaglianze
#            5-> favore ridurre disuguaglianze


# indici creati con metodo standard
plot(density(data_fa1$RC1, na.rm = T))
plot(density(data_fa1$RC2, na.rm = T))

# indici creati manualmente senza pesi
#plot(density(F1, na.rm = T))
#plot(density(F2, na.rm = T))

# indici creati manualmente pesati
#plot(density(rc1, na.rm = T))
#plot(density(rc2, na.rm = T))

#--------confronto tra indici-----

#ind <- data.frame(data_fa1$RC1,F1,rc1,data_fa1$RC2,F2,rc2)
#head(ind)

##-------variabile finale---------

att_eco <- (data_fa1$RC1 + data_fa1$RC2)/7

p <- ggplot(data=NULL, aes(x=att_eco))+
  geom_density()+
  xlim(0,30)+
  xlab("Attitude towards economic inequality")+
  ylab("Density")+
  ggtitle("Distribution of the dependent variable")+
  theme_minimal()

p + theme(plot.title = element_text(hjust = 0.5))
hist(att_eco)
range(att_eco)

##-----RELIABILITY TEST-----------

#omega(m = data_fa1) # CON FATTORI


rel_test <- data.frame(data_fa$v30, data_fa$v31, data_fa$v32, data_fa$v22, data_fa$v23, data_fa$v24, data_fa$v34)
colnames(rel_test) <- c("v30", "v31", "v32", "v22", "v23", "v24", "v34")

#om <- omega(m = rel_test, nfactors = 2) # SENZA FATTORI
#print.psych(om, cut=0.3)

# metodo alternativo grazie chatgpt

#alpha_fattori <- alpha(data_fa1[, fa1$loadings[,2] != 0])
cronbach.alpha(rel_test)


##-----VALIDITY TEST--------------


##-----RELATION TEST--------------
# import Gini index data

gini <- read_csv("Gini index 2018 2019.csv")
View(gini)
##-----subset gini index data---------------
total <- data.frame(att_eco, data_fa1, country_e_v50)

gini2018 <- subset(gini, TIME == 2018)

gini2018 <- subset(gini2018[c(1, 3:7, 11, 18, 20, 22, 23),])
gini2018 <- subset(gini2018[,c(1,6,7)])

head(gini2018,11)

hist(gini2018$Value)

##----insert the gini index in the dataset
unique(total$country)
match_country <- data.frame(
  num = c(36,40,100,152,158,191,203,208,246,250,276,352,376,380,392,440,554,578,608,643,705,710,
          740,752,756,764,826,840,862),  
  country = c("Australia", "Austria", "Bulgaria", "Chile", "Taiwan", "Croatia", "Czech Republic", "Denmark",
              "Finland", "France", "Germany", "Iceland", "Israel", "Italy", "Japan", "Lithuania", "New Zealand",
              "Norway", "Philippines", "Russia", "Slovenia", "South Africa", "Suriname", "Sweden", "Switzerland",
              "Thailand", "Great Britain", "United States", "Venezuela") 
)
length(match_country$num)
length(unique(total$country))

# Trova gli indici dei numeri delle nazioni nella tabella di corrispondenza
indici <- match(total$country, match_country$num)
total$country <- match_country$country[indici]
#match gini-nation
gini <- c(gini2018$Value)

 gini2018 <-gini2018%>%
   mutate(LOCATION=mapvalues(LOCATION, from=c("AUT", "CZE", "DNK", "FIN", "FRA",
                                              "DEU", "ITA", "SWE", "SVN", "LTU", "BGR"),
                        to=c("Austria", "Czech Republic", "Denmark", "Finland",
                             "France", "Germany", "Italy", "Sweden", "Slovenia", 
                             "Lithuania", "Bulgaria")))
 
 
total1 <- merge(total, gini2018, by.x = "country", by.y = "LOCATION")



##----subset data for total analysis----

total <- data.frame(att_eco, data_fa1, country_e_v50)

rm(list = "df","data","country_e_v50", "rel_test", "data_fa", "fa", "variabili_fattori1", "att_eco", "loadings",
   "fa1", "data_fa1", "gini", "p")

#FINE----

##------------test---------------------
#t test between attitude towards eco_ineq e dummy of v21
t.test(total$att_eco[total$v21d==0], total$att_eco[total$v21d==1])

boxplot(total$att_eco[total$v21==1], total$att_eco[total$v21==2], total$att_eco[total$v21==3],
        total$att_eco[total$v21==4], total$att_eco[total$v21==5])
mean(total$att_eco[total$v21d==1])

lm1 <- lm(total$att_eco ~ total$v21)
stargazer::stargazer(lm1, type = "text")

ggplot(data = total, aes(y=RC1, x=v21)) +
  geom_smooth(method = "lm",
              formula = y~x)+ 
  ylim(0,14)

ggplot(data = total, aes(y=RC2, x=v21)) +
  geom_smooth(method = "lm",
              formula = y~x)+ 
  ylim(0,15)


range(total$RC2)
## INTERACTIVE MODEL 
# Y = a + b1x + b2z + b3(x*z) + u
# 
# x è dicotomica: ineguaglianza percepita sì/no v 50
# z è continua: indice di gini
# y è continua

descr::freq(data$v50)

#make x dichotomous
data %>%
mutate(v50d=mapvalues(v50, from=c(-9,-8,1,2,3,4),
                       to=c(NA,0,0,0,1,1)))
hist(total$v50)

# v50: do you believe there are economic inequalities in your country?
descr::freq(data$v50) # 0 = no ; 1 = yes
x <- data$v50 #dummy x

# per inserire valori di zeta bisogna fare merge con usando country come variabile
# comune; 

#lm <- lm(att_eco ~ x + z + x*z)




