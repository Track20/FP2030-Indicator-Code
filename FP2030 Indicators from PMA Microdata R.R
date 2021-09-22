# Fp2030 Survey Based Indicators from a PMA

# Kristin Bietsch, PhD
# Track20 Project, Avenir Health

library(dplyr)
library(tidyr)
library(haven)
library(sjlabelled)
library(DHS.rates)
library(questionr)

women <- read_dta("C:/Users/KristinBietsch/files/PMA2020 Data/Uganda/PMA2020_UGP1_HQFQ_v2.0_21Jul2021/PMA2020_UGP1_HQFQ_v2.0_21Jul2021.dta")

women_clean <- women %>% filter(FRS_result==1 & HHQ_result==1 &  usually_live==1 )

# Setup data
women_clean <- women_clean %>% mutate(sampleweights= FQweight,
                                      married=case_when(FQmarital_status==1 | FQmarital_status==2 ~ 1, FQmarital_status!=1 & FQmarital_status!=2 ~ 0),
                                      unmarried=case_when(FQmarital_status==3 | FQmarital_status==4 | FQmarital_status==5 ~ 1, FQmarital_status!=3 & FQmarital_status!=4 & FQmarital_status!=5  ~ 0))

# mCP (do not need to recode), tCP (do not need to recode), Unmet Modern, and Demand Satisfied Modern
women_clean <- women_clean %>% mutate(unmet_mod=case_when(unmet==1 | unmet==2 ~ 1,
                                                          tcp==1  ~ 1,
                                                          TRUE ~ 0), 
                                      ds=case_when(mcp==1 ~ 1, unmet==1 | unmet==2 | unmet==3 | unmet==4 ~ 0))

# Method Prevelance (used to calculte method mix)
# want to check variable coding for v312 (method)
Labels=get_labels(women_clean$current_methodnum_rc)
Var1=get_values(women_clean$current_methodnum_rc)
Methods=as.data.frame(cbind(Labels, Var1))
#pill, injectable, IUD, implant, condom (male), condom (female), LAM, sterilization (male), sterilization (female), and the Standard Days Method. Other modern methods, including emergency contraception (EC)/diaphragm/foam/jelly, are grouped into an 'other' category

women_clean <- women_clean %>% mutate(method=case_when(current_methodnum_rc==7 ~ "Pill",
                                                       current_methodnum_rc==5 |  current_methodnum_rc==16 ~ "Injectable",
                                                       current_methodnum_rc==4 ~ "IUD",
                                                       current_methodnum_rc==3 ~ "Implant",
                                                       current_methodnum_rc==9 ~ "MCondom",
                                                       current_methodnum_rc==10 ~ "FCondom",
                                                       current_methodnum_rc==14 ~ "LAM",
                                                       current_methodnum_rc==1 ~ "FSter",
                                                       current_methodnum_rc==2 ~ "MSter",
                                                       current_methodnum_rc==13 ~ "SDM",
                                                       current_methodnum_rc==8 | current_methodnum_rc==11 | current_methodnum_rc==12  ~ "OMM"))

# Method Information Index Plus
# fp_side_effects, fp_side_effects_instructions (only if fp_side_effects is yes), fp_told_other_methods, fp_told_switch
women_clean <- women_clean %>% mutate(told_se=case_when(fp_side_effects==1 ~ 1, fp_side_effects!=1 ~ 0),
                                      told_todo_se=case_when(fp_side_effects==1 & fp_side_effects_instructions==1 ~ 1, 
                                                             fp_side_effects==1 & fp_side_effects_instructions==0 ~ 0,
                                                             fp_side_effects!=1 ~ 0),
                                      told_om = case_when(fp_told_other_methods==1 ~ 1, fp_told_other_methods!=1 ~ 0),
                                      told_switch=case_when(fp_told_switch==1 ~ 1, fp_told_switch!=1 ~ 0),
                                      mii=case_when(told_se==1 & told_todo_se==1 & told_om==1 ~ 1,
                                                    told_se!=1 | told_todo_se!=1 | told_om!=1 ~ 0),
                                      mii_plus= case_when(told_se==1 & told_todo_se==1 & told_om==1 & told_switch==1 ~ 1 ,
                                                          told_se!=1 | told_todo_se!=1 | told_om!=1 | told_switch!=1 ~ 0))


# Percentage of women who received family planning information during a contact with a health service provider
# visited_by_health_worker visited_a_facility facility_fp_discussion
women_clean <- women_clean %>% mutate(fp_info=case_when(visited_by_health_worker==1  ~ 1,
                                                        facility_fp_discussion==1 ~ 1,
                                                       !is.na(visited_by_health_worker) | is.na(visited_a_facility)  ~ 0))

sel <- select(women_clean, visited_by_health_worker, visited_a_facility, facility_fp_discussion, fp_info)

#FP Source
Source_Labels=get_labels(women_clean$fp_provider_rw)
Source_Var1=get_values(women_clean$fp_provider_rw)
Source=as.data.frame(cbind(Source_Labels, Source_Var1))

women_clean <- women_clean %>% mutate(Source=case_when(fp_provider_rw== 11 | fp_provider_rw== 12 | fp_provider_rw== 13 | fp_provider_rw== 14 | fp_provider_rw== 15 | fp_provider_rw== 16     ~ "Public",
                                                       fp_provider_rw== 24 | fp_provider_rw== 25  | fp_provider_rw== 32   ~ "NGO/FBO",
                                                       fp_provider_rw== -88 | fp_provider_rw== 27 | fp_provider_rw== 33 | fp_provider_rw== 96  ~ "Other",
                                                       fp_provider_rw==21 | fp_provider_rw== 23     ~ "Private clinical",
                                                       fp_provider_rw==22  ~ "Private pharmacy or drug shop",
                                                       fp_provider_rw== 31  ~ "Private shop or market"),
                                      Source_simple=case_when(fp_provider_rw== 11 | fp_provider_rw== 12 | fp_provider_rw== 13 | fp_provider_rw== 14 | fp_provider_rw== 15 | fp_provider_rw== 16     ~ "Public",
                                                              fp_provider_rw== -88 | fp_provider_rw== 27 | fp_provider_rw== 33 | fp_provider_rw== 96  ~ "Other",
                                                              fp_provider_rw== 24 | fp_provider_rw== 25  | fp_provider_rw== 32  | fp_provider_rw==21 | fp_provider_rw== 23 |  fp_provider_rw==22 |  fp_provider_rw== 31 ~ "Private"  ))

# Percentage of women who decided to use family planning alone or jointly with their husbands/partners
women_clean <- women_clean %>% mutate(decision=case_when(partner_overall==1 | partner_overall==3 ~ 1,
                                                         partner_overall!=1 & partner_overall!=3 ~ 0))


################################################
# Calculate Results

married <- women_clean %>% filter(married==1)
unmarried <- women_clean %>% filter(unmarried==1)

# mCP All women
prop.table(wtd.table(x= as.factor(women_clean$mcp), weights = women_clean$sampleweights))
# mCP Married women
prop.table(wtd.table(x= as.factor(married$mcp), weights = married$sampleweights))
# mCP Unmarried women
prop.table(wtd.table(x= as.factor(unmarried$mcp), weights = unmarried$sampleweights))

# tCP All women
prop.table(wtd.table(x= as.factor(women_clean$tcp), weights = women_clean$sampleweights))
# tCP Married women
prop.table(wtd.table(x= as.factor(married$tcp), weights = married$sampleweights))
# tCP Unmarried women
prop.table(wtd.table(x= as.factor(unmarried$tcp), weights = unmarried$sampleweights))

# Unmet Modern All women
prop.table(wtd.table(x= as.factor(women_clean$unmet_mod), weights = women_clean$sampleweights))
# Unmet Modern Married women
prop.table(wtd.table(x= as.factor(married$unmet_mod), weights = married$sampleweights))
# Unmet Modern Unmarried women
prop.table(wtd.table(x= as.factor(unmarried$unmet_mod), weights = unmarried$sampleweights))

# Demand Satisfied Modern All women
prop.table(wtd.table(x= as.factor(women_clean$ds), weights = women_clean$sampleweights))
# Demand Satisfied Married women
prop.table(wtd.table(x= as.factor(married$ds), weights = married$sampleweights))
# Demand Satisfied Unmarried women
prop.table(wtd.table(x= as.factor(unmarried$ds), weights = unmarried$sampleweights))

# Method Mix All Women
prop.table(wtd.table(x= as.factor(women_clean$method), weights = women_clean$sampleweights))


# Method Information Index Plus
prop.table(wtd.table(x= women_clean$mii_plus, weights = women_clean$sampleweights))
# Method Information Index
prop.table(wtd.table(x= women_clean$mii, weights = women_clean$sampleweights))
# Method Information Index Plus by Method (Note, only some methods are asked about MII)
prop.table(wtd.table(x= women_clean$mii_plus, y=women_clean$method, weights = women_clean$sampleweights),2)
# Method Information Index by Method (Note, only some methods are asked about MII)
prop.table(wtd.table(x= women_clean$mii, y=women_clean$method, weights = women_clean$sampleweights),2)
# Told About Side Effects
prop.table(wtd.table(x= women_clean$told_se, weights = women_clean$sampleweights))
# Told What to Do About Side Effects
prop.table(wtd.table(x= women_clean$told_todo_se, weights = women_clean$sampleweights))
# Told About Other Methods
prop.table(wtd.table(x= women_clean$told_om, weights = women_clean$sampleweights))
# Told About Switching to Other Methods
prop.table(wtd.table(x= women_clean$told_switch, weights = women_clean$sampleweights))


# Family Planning Information in Last 12 Months
#prop.table(wtd.table(x= women_clean$fp_info, weights = women_clean$sampleweights))

# Family Planning Source
prop.table(wtd.table(x= women_clean$Source, weights = women_clean$sampleweights))
prop.table(wtd.table(x= women_clean$Source_simple, weights = women_clean$sampleweights))
# Family Planning Source by Method
prop.table(wtd.table(x= women_clean$Source,  y=women_clean$method, weights = women_clean$sampleweights),2)
prop.table(wtd.table(x= women_clean$Source_simple,  y=women_clean$method, weights = women_clean$sampleweights),2)

# Percentage of women who decided to use family planning alone or jointly with their husbands/partners
prop.table(wtd.table(x= women_clean$decision, weights = women_clean$sampleweights))


