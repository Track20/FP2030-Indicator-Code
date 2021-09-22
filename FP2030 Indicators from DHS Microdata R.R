# Fp2030 Survey Based Indicators from a DHS

# Kristin Bietsch, PhD
# Track20 Project, Avenir Health

library(dplyr)
library(tidyr)
library(haven)
library(sjlabelled)
library(DHS.rates)
library(questionr)

setwd("C:/Users/KristinBietsch/files/DHSLoop")
women <- read_dta("NGIR7AFL.DTA")




# Setup data
women_clean <- women %>% mutate(sampleweights=v005/100000,
                                married=case_when(v502==1 ~ 1, v502!=1 ~ 0),
                                unmarried=case_when(v502!=1 ~ 1, v502==1 ~ 0))

# mCP, tCP, Unmet Modern, and Demand Satisfied Modern
women_clean <- women_clean %>% mutate(mcp=case_when(v313==3 ~ 1, v313!=3 ~ 0),
                                      tcp=case_when(v313==1 | v313==2 ~ 1, v313==0 | v313==3 ~ 0),
                                      unmet_mod=case_when(v626a==1 | v626a==2 ~ 1,
                                                          v313==1 | v313==2 ~ 1,
                                                          TRUE ~ 0), 
                                      ds=case_when(v313==3 ~ 1, v626a==1 | v626a==2 | v626a==3 | v626a==4 ~ 0))


# Method Prevelance (used to calculte method mix)
# want to check variable coding for v312 (method)
Labels=get_labels(women$v312)
Var1=get_values(women$v312)
Methods=as.data.frame(cbind(Labels, Var1))
#pill, injectable, IUD, implant, condom (male), condom (female), LAM, sterilization (male), sterilization (female), and the Standard Days Method. Other modern methods, including emergency contraception (EC)/diaphragm/foam/jelly, are grouped into an 'other' category

women_clean <- women_clean %>% mutate(method=case_when(v312==1 ~ "Pill",
                                                       v312==3 ~ "Injectable",
                                                       v312==2 ~ "IUD",
                                                       v312==11 ~ "Implant",
                                                       v312==5 ~ "MCondom",
                                                       v312==14 ~ "FCondom",
                                                       v312==13 ~ "LAM",
                                                       v312==6 ~ "FSter",
                                                       v312==7 ~ "MSter",
                                                       v312==18 ~ "SDM",
                                                       v312==16 | v312==17 ~ "OMM"))


# Method Information Index (MII+ not available for this survey)
#v3a02 v3a04 (only if v3a02 is yes) v3a05
women_clean <- women_clean %>% mutate(told_se=case_when(v3a02==1 ~ 1, v3a02!=1 ~ 0),
                                      told_todo_se=case_when(v3a02==1 & v3a04==1 ~ 1, 
                                                             v3a02==1 & v3a04==0 ~ 0,
                                                             v3a02!=1 ~ 0),
                                      told_om = case_when(v3a05==1 ~ 1, v3a05!=1 ~ 0),
                                      mii=case_when(told_se==1 & told_todo_se==1 & told_om==1 ~ 1,
                                                    told_se!=1 | told_todo_se!=1 | told_om!=1 ~ 0))


# Percentage of women who received family planning information during a contact with a health service provider
#v393 v393a v394 v395
women_clean <- women_clean %>% mutate(fp_info=case_when(v393a==1  ~ 1,
                                                        v395==1 ~ 1,
                                                        !is.na(v393) | !is.na(v394)  ~ 0))

#FP Source
Source_Labels=get_labels(women$v326)
Source_Var1=get_values(women$v326)
Source=as.data.frame(cbind(Source_Labels, Source_Var1))

women_clean <- women_clean %>% mutate(Source=case_when(v326==11  | v326==12 | v326==13 | v326==14 | v326==15 | v326==16 ~ "Public",
                                                       v326==25  | v326==26 | v326==32 | v326==34 ~ "NGO/FBO",
                                                       v326==30  | v326==33 | v326==96 | v326==98 ~ "Other",
                                                       v326==21 | v326==24 ~ "Private clinical",
                                                       v326==22 | v326==23 ~ "Private pharmacy or drug shop",
                                                       v326==27 | v326==31 ~ "Private shop or market"),
                                      Source_simple=case_when(v326==11  | v326==12 | v326==13 | v326==14 | v326==15 | v326==16 ~ "Public",
                                                              v326==30  | v326==33 | v326==96 | v326==98 ~ "Other",
                                                              v326==25  | v326==26 | v326==32 | v326==34 |   v326==21 | v326==24 |  v326==22 | v326==23 | v326==27 | v326==31 ~ "Private" ))

# Percentage of women who decided to use family planning alone or jointly with their husbands/partners
women_clean <- women_clean %>% mutate(decision=case_when(v632==1 | v632==3 ~ 1,
                                                         v632!=1 & v632!=3 ~ 0))

# Adolescent birth rate can be calcualted using the rates package by the DHS
ASFR <- fert(women_clean,Indicator="asfr")
ABR <- ASFR %>% filter(AGE=="15-19")

# Percent of births that are unintended
women_long <- women_clean %>% select(v005, v225, m10_1, m10_2, m10_3, m10_4, m10_5, m10_6) %>% gather(Variable, Value, v225:m10_6) %>% filter(!is.na(Value)) %>% mutate(sampleweights=v005/100000)

unintended <- as.data.frame(prop.table(wtd.table(x= as.factor(women_long$Value), weights = women_long$sampleweights))) %>% mutate(Var1=case_when(Var1==1 ~ "Then", Var1==2 ~ "Later", Var1==3 ~ "No More")) 

################################################
# Calculate Results

married <- women_clean %>% filter(married==1)
unmarried <- women_clean %>% filter(unmarried==1)

# mCP All women
prop.table(wtd.table(x= women_clean$mcp, weights = women_clean$sampleweights))
# mCP Married women
prop.table(wtd.table(x= married$mcp, weights = married$sampleweights))
# mCP Unmarried women
prop.table(wtd.table(x= unmarried$mcp, weights = unmarried$sampleweights))

# tCP All women
prop.table(wtd.table(x= women_clean$tcp, weights = women_clean$sampleweights))
# tCP Married women
prop.table(wtd.table(x= married$tcp, weights = married$sampleweights))
# tCP Unmarried women
prop.table(wtd.table(x= unmarried$tcp, weights = unmarried$sampleweights))

# Unmet Modern All women
prop.table(wtd.table(x= women_clean$unmet_mod, weights = women_clean$sampleweights))
# Unmet Modern Married women
prop.table(wtd.table(x= married$unmet_mod, weights = married$sampleweights))
# Unmet Modern Unmarried women
prop.table(wtd.table(x= unmarried$unmet_mod, weights = unmarried$sampleweights))

# Demand Satisfied Modern All women
prop.table(wtd.table(x= women_clean$ds, weights = women_clean$sampleweights))
# Demand Satisfied Married women
prop.table(wtd.table(x= married$ds, weights = married$sampleweights))
# Demand Satisfied Unmarried women
prop.table(wtd.table(x= unmarried$ds, weights = unmarried$sampleweights))

# Method Mix All Women
prop.table(wtd.table(x= women_clean$method, weights = women_clean$sampleweights))

# Method Information Index
prop.table(wtd.table(x= women_clean$mii, weights = women_clean$sampleweights))
# Method Information Index by Method (Note, only some methods are asked about MII)
prop.table(wtd.table(x= women_clean$mii, y=women_clean$method, weights = women_clean$sampleweights),2)
# Told About Side Effects
prop.table(wtd.table(x= women_clean$told_se, weights = women_clean$sampleweights))
# Told What to Do About Side Effects
prop.table(wtd.table(x= women_clean$told_todo_se, weights = women_clean$sampleweights))
# Told About Other Methods
prop.table(wtd.table(x= women_clean$told_om, weights = women_clean$sampleweights))

# Family Planning Information in Last 12 Months
prop.table(wtd.table(x= women_clean$fp_info, weights = women_clean$sampleweights))

# Family Planning Source
prop.table(wtd.table(x= women_clean$Source, weights = women_clean$sampleweights))
prop.table(wtd.table(x= women_clean$Source_simple, weights = women_clean$sampleweights))
# Family Planning Source by Method
prop.table(wtd.table(x= women_clean$Source,  y=women_clean$method, weights = women_clean$sampleweights),2)
prop.table(wtd.table(x= women_clean$Source_simple,  y=women_clean$method, weights = women_clean$sampleweights),2)

# Percentage of women who decided to use family planning alone or jointly with their husbands/partners
prop.table(wtd.table(x= women_clean$decision, weights = women_clean$sampleweights))
