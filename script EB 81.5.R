library(corrplot)
library(tidyr)
library(plyr)
library(dplyr)
library(psych)
rm(list = ls())

#import data
setwd("C:/Users/chiar/Desktop/Università/DAPS&CO/SOCIAL AND POLITICAL ATTITUDES/project/Economic-Inequality")
eb <-rio::import( "EB 81.5.dta")

View(eb)

eb_pr <- data.frame(eb$qa2_4, eb$qa2_5, eb$qa2_7, eb$qa2_8, eb$qa2_9, eb$qa2_14,
                    eb$qa9, eb$qa10, eb$qa11, eb$qa12, eb$qb1_8, eb$qb2_8)

descr::freq(eb$qa2_4) 
descr::freq(eb$qa24) 
# 4 cat -> 4 cat
descr::freq(eb$qa2_5) # 4 cat
descr::freq(eb$qa2_7) # 4 cat
descr::freq(eb$qa2_8) # 4 cat
descr::freq(eb$qa2_9) # 4 cat
descr::freq(eb$qa2_14) # 4 car
descr::freq(eb$qa9) # 3 cat
descr::freq(eb$qa10) # 3 cat 
descr::freq(eb$qa11) # 3 cat
descr::freq(eb$qa12) # 3 cat
descr::freq(eb$qb1_8) # 3 cat 
descr::freq(eb$qb2_8) # 3 cat

--------------------------------------------------------------------
  
# data frame with recoded variables

eb_rec <- data.frame(eb$qa24, eb$qa25, eb$qa27, eb$qa28, eb$qa29, eb$qa214,
                      eb$qa5_d, eb$qa6_d, eb$qa10_d)
colnames(eb_rec) <- c("qa24", "qa25", "qa27", "qa28", "qa29", "qa214",
                      "qa5_d", "qa6_d", "qa10_d")



#na remove
eb_rec <- na.omit(eb_rec)

#correlation matrix
cor(eb_rec, use = "complete.obs")
corrplot(cor(eb_rec, use = "complete.obs"),method = "number")

#factor analysis

#1 check the number of possible factors
fa.parallel(eb_rec, fa="fa")#2

factanal(~eb_rec$qa24+eb_rec$qa25+eb_rec$qa27+eb_rec$qa28+eb_rec$qa29+eb_rec$qa214+
           eb_rec$qa5_d+eb_rec$qa6_d+eb_rec$qa10_d, factors = 2, rotation = "varimax")









