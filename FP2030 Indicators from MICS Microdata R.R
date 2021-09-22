# Fp2030 Survey Based Indicators from a MICS

# Kristin Bietsch, PhD
# Track20 Project, Avenir Health

# Please note that variable names change between rounds of MICS surveys.  
# This code is written for MICS 6.  
# The dataframe "mics1" created below will show names and labels for variables and can be used to edit the code for other datasets. 


library(dplyr)
library(haven)
library(survey)
require(xlsx)
library(tibble)
library(questionr)

options(scipen = 999)

setwd("C:/Users/KristinBietsch/Files/MICS Data")
mics <- read_sav("DRCongo MICS6 SPSS Datafiles/DRCongo MICS6 SPSS Datafiles/wm.sav")

library(foreign)
mics1 <-  read.spss("DRCongo MICS6 SPSS Datafiles/DRCongo MICS6 SPSS Datafiles/wm.sav", to.data.frame= TRUE)
mics1 <- as.data.frame(attr(mics1, "variable.labels")) %>% rownames_to_column()



attr(mics$CP4A,"labels")
attr(mics$CP4B,"labels")
attr(mics$CP4C,"labels")
attr(mics$CP4D,"labels")
attr(mics$CP4E,"labels")
attr(mics$CP4F,"labels")
attr(mics$CP4G,"labels")
attr(mics$CP4H,"labels")
attr(mics$CP4I,"labels")
attr(mics$CP4J,"labels")
attr(mics$CP4K,"labels")
attr(mics$CP4L,"labels")
attr(mics$CP4M,"labels")
attr(mics$CP4X,"labels")

mics <- mics %>% mutate(method=case_when(CP4A=="A" ~ "F_Ster", 
                                         CP4B=="B" ~ "M_Ster", 
                                         CP4C=="C" ~ "IUD",
                                         CP4D=="D" ~ "Injectable",
                                         CP4E=="E" ~ "Implant",
                                         CP4F=="F" ~ "Pill",
                                         CP4G=="G" ~ "M_Condom",
                                         CP4H=="H" ~ "F_Condom",
                                         CP4I=="I" ~ "Diaphragm",
                                         CP4J=="J" ~ "Foam/Jelly",
                                         CP4K=="K" ~ "LAM",
                                         CP4L=="L" ~ "Periodic Abstinence/Rhythm",
                                         CP4M=="M" ~ "Withdrawal",
                                         CP4X=="X" ~ "Other", 
                                         TRUE ~  "None"))
  


mics$modern <- ifelse(mics$method=="F_Ster" | 
                        mics$method=="M_Ster" |
                        mics$method=="IUD" |
                        mics$method=="Injectable" |
                        mics$method=="Implant" |
                        mics$method=="Pill" |
                        mics$method=="M_Condom" |
                        mics$method=="F_Condom" |
                        mics$method=="Diaphragm" |
                        mics$method=="Foam/Jelly" |
                        mics$method=="LAM", 1, 
                      ifelse(mics$method=="Periodic Abstinence/Rhythm" |
                               mics$method=="Withdrawal" |
                               mics$method=="Other" |
                               mics$method=="Necklace"|
                               mics$method=="None" , 0, NA))

mics$traditional <- ifelse(mics$method=="F_Ster" | 
                             mics$method=="M_Ster" |
                             mics$method=="IUD" |
                             mics$method=="Injectable" |
                             mics$method=="Implant" |
                             mics$method=="Pill" |
                             mics$method=="M_Condom" |
                             mics$method=="F_Condom" |
                             mics$method=="Diaphragm" |
                             mics$method=="Foam/Jelly" |
                             mics$method=="LAM" |
                             mics$method=="None" , 0, 
                           ifelse(mics$method=="Periodic Abstinence/Rhythm" |
                                    mics$method=="Withdrawal" |
                                    mics$method=="Necklace" |
                                    mics$method=="Other"  , 1, NA))




# Only asks about most recent birth
mics$intention <- ifelse(mics$DB2==1, "Then",
                         ifelse(mics$DB2==2 & mics$DB4==1, "Later", 
                                ifelse(mics$DB2==2 & mics$DB4==2, "Not at All", NA)))


married <- filter(mics, MSTATUS==1)
unmarried <- filter(mics, MSTATUS!=1)

##############################################################################

# mCP All women
prop.table(wtd.table(x= mics$modern, weights = mics$wmweight))
# mCP Married women
prop.table(wtd.table(x= married$modern, weights = married$wmweight))
# mCP Unmarried women
prop.table(wtd.table(x= unmarried$modern, weights = unmarried$wmweight))

# tCP All women
prop.table(wtd.table(x= mics$traditional, weights = mics$wmweight))
# tCP Married women
prop.table(wtd.table(x= married$traditional, weights = married$wmweight))
# tCP Unmarried women
prop.table(wtd.table(x= unmarried$traditional, weights = unmarried$wmweight))
