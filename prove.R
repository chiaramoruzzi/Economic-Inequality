### prove (finite male)

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

#-----------------------------------------------------------

#install.packages("psychTools")
library(psychTools)

.First <- function(x) {library(psych)}

describe(data)
pairs.panels(data_fa)  
