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
library(descr)
library(stargazer)


##---------import original dataset----
#setwd("/Users/trava/OneDrive/Desktop/")
#setwd("C:/Users/chiar/Desktop/Università/DAPS&CO/SOCIAL AND POLITICAL ATTITUDES/project/Economic-Inequality")

data <-rio::import( "ZA7600_v3-0-0.dta")


##---------creazione dataframe per analisi fattoriale----
#data_fa <- data.frame(data$v30, data$v31, data$v32, data$v22, data$v23, data$v24, data$v34)
#colnames(data_fa) <- c("v30", "v31", "v32", "v22", "v23", "v24", "v34")
data_fa <- data.frame(data$country, data$v21,data$v30, data$v31, data$v22, data$v23, data$v24, data$v34)
colnames(data_fa) <- c("country", "v21", "v30", "v31", "v22", "v23", "v24", "v34")

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
  mutate(v22=mapvalues(v22, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v23=mapvalues(v23, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v24=mapvalues(v24, from=c(-9,-8,1,2,3,4,5),
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
fa <- principal(r=data_fa, nfactors = 2, rotate="varimax", missing = TRUE)
print.psych(fa, cut = 0.4)
#
#
###---------grafici----
##1 diagramma
#fa.diagram(fa, cut = 0.4, digits = 2)
#
##2 piano cartesiano
## Estrai carichi dei fattori
#loadings <- fa$loadings
###Creazione del data frame per il grafico
#df <- data.frame(
#  Factor1 = loadings[, 1],
#  Factor2 = loadings[, 2],
#  Variable = rownames(loadings))
### Creazione del grafico della factor analysis
#ggplot(df, aes(x = Factor1, y = Factor2, label = Variable)) +
#  geom_point() +
#  geom_label_repel(hjust = 0.5, position = "identity", box.padding = 0.16, label.size = 0.1) +
#  geom_vline(xintercept = 0, lty=2) +
#  geom_hline(yintercept = 0, lty=2) +
#  xlab("Factor 1") +
#  ylab("Factor 2") +
#  ggtitle("Factor Analysis Plot") +
#  theme_minimal()
##è utile per vedere la vicinanza dei punti delle variabili rispetto agli assi dei fattori
##inoltre, se la plotti prima e dopo aver impostato rotate, si vede che i punti si spostano dal punto originale!
#
#
###---------creazione delle variabili----
variabili_fattori <- as.matrix(data_fa1) %*% loadings
#Visualizza le variabili dei fattori
head(variabili_fattori, 10)
##aggiungere le nuove variabili al dataset
data_fa1 <- cbind(data_fa1, variabili_fattori)


##----------subset del dataframe eliminando variabili superflue----
#data_fa1 <- data.frame(data_fa$v30, data_fa$v31, data_fa$v32, data_fa$v22, data_fa$v23, data_fa$v24, data_fa$v34)
#colnames(data_fa1) <- c("v30", "v31", "v32", "v22", "v23", "v24", "v34")
##----------subset 2
country_e_v21 <- subset(data_fa[,c(1,2,9)])
data_fa1 <- subset(data_fa[,c(3:8)])

#factor analysis
fa.parallel(data_fa1, fa="pc")

fa1 <- principal(r=data_fa1, nfactors = 2, rotate = "promax")
print.psych(fa1, cut = 0.4)
###---------grafici----
##1 diagramma
fa.diagram(fa1, cut = 0.4)

#2 piano cartesiano
## Estrai carichi dei fattori
loadings <- fa1$loadings
##Creazione del data frame per il grafico
df <- data.frame(
  Factor1 = loadings[, 2],
  Factor2 = loadings[, 1],
  Variable = rownames(loadings))
# Creazione del grafico della factor analysis
ggplot(df, aes(x = Factor1, y = Factor2, label = Variable)) +
  geom_point() +
  geom_label_repel(hjust = 0.5, position = "identity", box.padding = 0.16, label.size = 0.1) +
  geom_vline(xintercept = 0, lty=2) +
  geom_hline(yintercept = 0, lty=2) +
  xlab("Factor 1") +
  ylab("Factor 2") +
  ggtitle("Factor Analysis Plot with Promax rotation") +
  theme_minimal()
##è utile per vedere la vicinanza dei punti delle variabili rispetto agli assi dei fattori
##inoltre, se la plotti prima e dopo aver impostato rotate, si vede che i punti si spostano dal punto originale!

print(fa1$loadings)

# Creazione variabili fattori
#variabili_fattori1 <- as.matrix(data_fa1) %*% loadings
#Visualizza le variabili dei fattori
#head(variabili_fattori1, 10)
#aggiungere le nuove variabili al dataset
data_fa1 <- cbind(data_fa1, variabili_fattori1)


##---------metodo alternativo per la creazione delle variabili fattori-----

# indici non pesati
# rc2 <- (data_fa1$v30 + data_fa1$v31 + data_fa1$v32)
# rc1 <- (data_fa1$v23 + data_fa1$v22 + data_fa1$v34 + data_fa1$v24)

# indici pesati per factor loadings - controllo incrociato
F1 <- ((0.93*data_fa1$v30) + (0.93*data_fa1$v31))
F2 <- ((0.76*data_fa1$v23) + (0.69*data_fa1$v22) + (0.65*data_fa1$v34) + (0.62*data_fa1$v24))
print(fa1)

data_fa1 <- cbind(data_fa1, F1, F2)

data_fa1$att_eco <- (data_fa1$RC1 + data_fa1$RC2)

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

att_eco <- (data_fa1$RC1 + data_fa1$RC2)/6

range(data_fa1$RC2)

p <- ggplot(data=data_fa1, aes(x=att_eco))+
  geom_histogram()+
  xlim(0,25)+
  xlab("Attitude towards economic inequality")+
  ylab("Density")+
  ggtitle("Distribution of the dependent variable")+
  theme_minimal()

p + theme(plot.title = element_text(hjust = 0.5))
hist(att_eco)
range(data_fa1$att_eco)

##-----RELIABILITY TEST-----------

#omega(m = data_fa1) # CON FATTORI


rel_test <- data.frame(data_fa$v30, data_fa$v31, data_fa$v22, data_fa$v23, data_fa$v24, data_fa$v34)
colnames(rel_test) <- c("v30", "v31", "v22", "v23", "v24", "v34")

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
total <- data.frame(data_fa1, country_e_v21)

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

total1 <- subset(total1[,(1,3:15)])
total1 <-total1 %>%
  rename(gini_index=Value)
##----subset data for total analysis----

total <- data.frame(att_eco, data_fa1, country_e_v50)

rm(list = ls())

#FINE----
write.csv(total1, "C:\\Users\\trava\\OneDrive\\Desktop\\università\\Daps&co\\8-Social and political attitude\\Economic-Inequality\\total1.csv", row.names=FALSE)
##------------test---------------------

setwd("C:/Users/chiar/Desktop/Università/DAPS&CO/SOCIAL AND POLITICAL ATTITUDES/project/Economic-Inequality")

total1 <- rio::import("total1.csv")

total1$att_eco <- total1$att_eco/7
plot(density(total1$att_eco))

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

## MULTIVARIATE MODEL

# Y = a + b1x + b2z + u


freq(total1$v21d)
freq(total1$v21)

freq(total1$Value)

y <- total1$att_eco
x <- total1$v21d
z <- total1$gini_index

lm1 <- rlm(y ~ x)
lm2 <- rlm(y ~ z)
lm3 <- rlm(y ~ x+z)
lm_int <- rlm(y ~ x*z)
stargazer::stargazer(dep.var.caption = "Attitude towards economic inequality", column.labels = c("Model 1", "Model 2", "Interactive model"),
                      lm1,lm2,lm_int, type = "text", notes = "robust standard errors in parentheses", out = "filename.html")

summary(lm_int)

sd(data_fa1$RC1)
sd(data_fa1$RC2)
# adj R^2 = 0.204

ggplot(data = total1, mapping = aes(x=v21d))+
  geom_bar()+
  xlab("Inequality perception (No/Yes)")+
  ylab("Count")+
  theme_minimal()

sd(data_fa1$att_eco)


ggplot(data = total1, aes(x=gini_index,y=att_eco))+
  geom_smooth(aes(color = factor(v21d)), method = "lm", se = F)+
  ylim (0,23)+
  theme_minimal()

ggplot(data = total1, aes(x=v21d,y=att_eco))+
  geom_smooth(aes(color = factor(v21d)), method = "lm", se = F)+
  theme_minimal()

plot_data <- ggpredict(lm_int, terms = c("x"))
plot(plot_data, 
     add.data = TRUE,
     ci.style = "ribbon",  
     dot
     use.theme = TRUE)


plot_data <- ggpredict(lm, terms = c("z", "x"))
plot(plot_data, 
     add.data = TRUE,
     ci.style = "ribbon", 
     dot.size = 1.5,
     use.theme = TRUE,)







