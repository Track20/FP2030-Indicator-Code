# Using the DHS API to Access FP2030 Indicators

# Kristin Bietsch, PhD
# Track20 Project, Avenir Health

# The DHS API is a quick and useful way to access many DHS indicators.  It contains similar information as StatCompiler (https://www.statcompiler.com/en/)  


###########################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# For this exercise, we will be requesting a large amount of information.  
# You are going to request a large amount of data, 
# you can register for a "key" by emailing api@dhsprogram.com
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
###########################################################


# To access data, you will need an indicator ID (https://api.dhsprogram.com/rest/dhs/indicators?returnFields=IndicatorId,Label,Definition&f=html)

# We use 4 libraries to request and clean data
library(jsonlite) 
library(data.table)
library(dplyr)
library(tidyr)

# First, we set up an object called "url"
# For a specific indicator, we are changing the indicatorID, in the example below "FP_CUSA_W_MOD"
# You will need to add your key in the blank space after "APIkey="
# We are requesting all surveys and all breakdowns 
url<-("http://api.dhsprogram.com/rest/dhs/data?f=json&indicatorIds=FP_CUSA_W_MOD&surveyid=all&breakdown=all&perpage=20000&APIkey=AVEHTH-279664")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$Data)

# Now in our dataframe "dta" we can select the columns of interest.  For this example, we want mCP by wealth quintile
dta_small<- dta %>% select( SurveyId, CountryName, SurveyYear, Value,   CharacteristicCategory, CharacteristicLabel)  %>% # Keep the columns of interest
  filter(CharacteristicCategory=="Wealth quintile")

# If you want your data to be in wide format (each wealth quintile as its own column)
dta_wide <- dta_small %>% spread(CharacteristicLabel, Value)

#############################################################################################################
# Modern contraceptive prevalence (MCP)
# For married women, the IndicatorID is FP_CUSM_W_MOD
url<-("http://api.dhsprogram.com/rest/dhs/data?f=json&indicatorIds=FP_CUSA_W_MOD&surveyid=all&breakdown=all&perpage=20000&APIkey=AVEHTH-279664")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$Data)
mcp_all<- dta %>% select( SurveyId, CountryName, SurveyYear, Value,   CharacteristicCategory, CharacteristicLabel) 

# tCP all women (Note, the API indicator does not include folk methods.  See Track20's microdata files for information on tCP including folk methods)

# Percentage of women estimated to have an unmet need for modern methods of contraception
url<-("http://api.dhsprogram.com/rest/dhs/data?f=json&indicatorIds=FP_NADA_W_UMT&surveyid=all&breakdown=all&perpage=20000&APIkey=AVEHTH-279664")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$Data)
unmod_all<- dta %>% select( SurveyId, CountryName, SurveyYear, Value,   CharacteristicCategory, CharacteristicLabel) 

# Percentage of women estimated to have their demand for family planning met with a modern method of contraception 
url<-("http://api.dhsprogram.com/rest/dhs/data?f=json&indicatorIds=FP_NADA_W_PDM&surveyid=all&breakdown=all&perpage=20000&APIkey=AVEHTH-279664")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$Data)
ds_all<- dta %>% select( SurveyId, CountryName, SurveyYear, Value,   CharacteristicCategory, CharacteristicLabel) 

# Contraceptive Method Mix
# To find a method's share of the method mix, we take the prevalence of the method divided by mCP
# For example, Pills (the cade is also requesting mCP):
url<-("http://api.dhsprogram.com/rest/dhs/data?f=json&indicatorIds=FP_CUSA_W_PIL,FP_CUSA_W_MOD&surveyid=all&breakdown=all&perpage=30000&APIkey=AVEHTH-279664")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$Data)
pill_mm_all<- dta %>% select( SurveyId, CountryName, SurveyYear, SDRID, Value,   CharacteristicCategory, CharacteristicLabel) %>%
  filter(CharacteristicCategory=="Total") %>%
  spread(SDRID, Value) %>% 
  mutate(pill_mm=FPCUSAWPIL/FPCUSAWMOD) %>%
  select( SurveyId, CountryName, SurveyYear, pill_mm)


# Method Source
#Private	Private clinical	Private hospitals, clinics, doctors, nurses, midwives, health centers, maternity homes, other private medical
#FP_SRCM_W_PHS	Current users most recent supply or information from a private hospital/clinic
#FP_SRCM_W_PDR	Current users most recent supply or information from a private doctor	
#FP_SRCM_W_POT	Current users most recent supply or information from an other private medical source	

#Private pharmacy or drug shop	Pharmacy, drug shop, dispensary, chemist
#FP_SRCM_W_PPH	Current users most recent supply or information from a pharmacy	

#Private shop or market	Shop, market, bar, disco, vending machine, gas station, grocery store, guest house/hotel, warehouse, other private
#FP_SRCM_W_OSR	Current users most recent supply or information from other non-medical sources	
#FP_SRCM_W_SHP	Current users most recent supply or information from a shop	

#NGO/FBO	Mission hospital, mission health center/clinic, church, mosque, religious institution, NGO health facility, mobile clinics, fieldworkers 
#FP_SRCM_W_PMB	Current users most recent supply or information from a mobile clinic	
#FP_SRCM_W_PFW	Current users most recent supply or information from a fieldworker	
#FP_SRCM_W_CHH	Current users most recent supply or information from a church	

#Public	Public	Government hospitals, health centers, clinics, and CHWs, can be disaggregated into public hospitals vs. public other.
#FP_SRCM_W_PUB	Current users most recent supply or information from a public source	

#Other	Other/Don't know/missing	Friend, relative, partner, parent, traditional healer, traditional birth attendant, school, the respondent herself, "other," don't know, missing data
#FP_SRCM_W_FRR	Current users most recent supply or information from a friend/relative
#FP_SRCM_W_OTH	Current users most recent supply or information from an other unspecified source		
#FP_SRCM_W_DKM	Current users most recent supply or information from an unknown source	
#FP_SRCM_W_MIS	Current users most recent supply or information: Missing	

url<-("http://api.dhsprogram.com/rest/dhs/data?f=json&indicatorIds=FP_SRCM_W_PHS,FP_SRCM_W_PDR,FP_SRCM_W_POT,FP_SRCM_W_PPH,FP_SRCM_W_OSR,FP_SRCM_W_SHP,FP_SRCM_W_PMB,FP_SRCM_W_PFW,FP_SRCM_W_CHH,FP_SRCM_W_PUB,FP_SRCM_W_FRR,FP_SRCM_W_OTH,FP_SRCM_W_DKM,FP_SRCM_W_MIS&surveyid=all&breakdown=all&perpage=20000&APIkey=AVEHTH-279664")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$Data)
source <- dta %>%  select( SurveyId, CountryName, SurveyYear, SDRID, Value,   CharacteristicCategory, CharacteristicLabel) %>%
  filter(CharacteristicCategory=="Total") %>%
  spread(SDRID, Value) %>% gather(Variable, Value, FPSRCMWCHH:FPSRCMWSHP) %>%
  mutate(Value = replace_na(Value, 0)) %>% spread(Variable, Value) %>% # filling in missing variables
  mutate(PrivateClinical= FPSRCMWPHS +  FPSRCMWPDR + FPSRCMWPOT, 
         PrivatePharmacy= FPSRCMWPPH,
         PrivateShop= FPSRCMWOSR + FPSRCMWSHP,
         NGO_FBO= FPSRCMWPMB + FPSRCMWPFW + FPSRCMWCHH ,
         Public= FPSRCMWPUB,
         Other_DK_Missing= FPSRCMWFRR + FPSRCMWOTH + FPSRCMWDKM + FPSRCMWMIS,
         PrivateTotal=PrivateClinical + PrivatePharmacy + PrivateShop + NGO_FBO ) %>% 
  select( SurveyId, CountryName, SurveyYear, PrivateTotal, PrivateClinical, PrivatePharmacy, PrivateShop, NGO_FBO, Public, Other_DK_Missing) 
  

# Adolescent Birth Rate
url<-("http://api.dhsprogram.com/rest/dhs/data?f=json&indicatorIds=FE_FRTR_W_A15&surveyid=all&breakdown=all&perpage=20000&APIkey=AVEHTH-279664")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$Data)
ABR<- dta %>% select( SurveyId, CountryName, SurveyYear, Value,   CharacteristicCategory, CharacteristicLabel) 


# Percent of births that are unintended
url<-("http://api.dhsprogram.com/rest/dhs/data?f=json&indicatorIds=PR_PLST_W_LAT,PR_PLST_W_NOM&surveyid=all&breakdown=all&perpage=30000&APIkey=AVEHTH-279664")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$Data)
unintended<- dta %>% select( SurveyId, CountryName, SurveyYear, SDRID, Value, ByVariableLabel,   CharacteristicCategory, CharacteristicLabel) %>%
  filter(CharacteristicCategory=="Total") %>%
  filter(ByVariableLabel=="Five years preceding the survey") %>%
  spread(SDRID, Value) %>% 
  mutate(unintended=PRPLSTWLAT + PRPLSTWNOM) %>%
  select( SurveyId, CountryName, SurveyYear, unintended)


# Percentage of women who decided to use family planning alone or jointly with their husbands/partners
url<-("http://api.dhsprogram.com/rest/dhs/data?f=json&indicatorIds=FP_DMKF_W_UWF,FP_DMKF_W_UJN&surveyid=all&breakdown=all&perpage=30000&APIkey=AVEHTH-279664")
jsondata<-fromJSON(url) 
dta<-data.table(jsondata$Data)
decision<- dta %>% select( SurveyId, CountryName, SurveyYear, SDRID, Value,    CharacteristicCategory, CharacteristicLabel) %>%
  filter(CharacteristicCategory=="Total") %>%
  spread(SDRID, Value) %>% 
  mutate(decision=FPDMKFWUJN + FPDMKFWUWF) %>%
  select( SurveyId, CountryName, SurveyYear, decision)

