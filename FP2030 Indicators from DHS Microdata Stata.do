* FP2030 Survey Based Indicators from a DHS

* Kristin Bietsch, PhD
* Track20 Project, Avenir Health

clear all
set maxvar 10000
use "C:\Users\KristinBietsch\files\DHS Data\Nigeria\NGIR7AFL.DTA"

#*Setup data
gen sampleweights=v005/100000

gen married=1 if v502==1 
replace married=0 if v502!=1 


* mCP, tCP, Unmet Modern, and Demand Satisfied Modern
gen mcp=1 if v313==3 
replace mcp=0 if v313!=3

gen tcp=1 if v313==1 | v313==2 
replace tcp= 0 if v313==0 | v313==3 
                                  
gen unmet_mod=0 if v626a!=.
replace unmet_mod= 1 if v626a==1 | v626a==2
replace unmet_mod=1 if v313==1 | v313==2 
                                  
gen ds_mod= 0 if v626a==1 | v626a==2 | v626a==3 | v626a==4 
replace ds_mod=1 if v313==3								  
                                    

* Method Prevelance (used to calculte method mix)
* want to check variable coding for v312 (method)
fre v312

*pill, injectable, IUD, implant, condom (male), condom (female), LAM, sterilization (male), sterilization (female), and the Standard Days Method. Other modern methods, including emergency contraception (EC)/diaphragm/foam/jelly, are grouped into an 'other' category
gen method="Pill" if v312==1
replace method = "Injectable" if v312==3 
replace method = "IUD" if  v312==2
replace method = "Implant" if v312==11
replace method = "MCondom" if v312==5
replace method = "FCondom" if  v312==14 
replace method = "LAM" if v312==13
replace method = "FSter" if v312==6 
replace method =  "MSter" if  v312==7
replace method = "SDM" if v312==18 
replace method = "OMM" if   v312==16 | v312==17 


* Method Information Index (MII+ not available for this survey)
*v3a02 v3a04 (only if v3a02 is yes) v3a05

gen told_se=1 if v3a02==1 
replace told_se=0 if v3a02!=1 

gen told_todo_se=1 if v3a02==1 & v3a04==1
replace told_todo_se=0 if  v3a02==1 & v3a04==0
replace told_todo_se=0 if  v3a02!=1 
                                  
gen told_om= 1 if 	v3a05==1
replace told_om=0 if v3a05!=1					  
                                      
gen mii=1 if told_se==1 & told_todo_se==1 & told_om==1			
replace mii=0 if told_se!=1 | told_todo_se!=1 | told_om!=1			  
                                      

* Percentage of women who received family planning information during a contact with a health service provider
*v393 v393a v394 v395
gen fp_info= 1 if v393a==1 
replace fp_info= 1 if v395==1  
replace  fp_info=0 if (v393==1 & v393a==0) | (v394==1 & v395==0)                                           
                                                         
* FP Source
fre v326

gen Source= "Public" if  v326==11  | v326==12 | v326==13 | v326==14 | v326==15 | v326==16
replace Source="NGO/FBO" if  v326==25  | v326==26 | v326==32 | v326==34
replace Source="Other" if  v326==30  | v326==33 | v326==96 | v326==98
replace Source="Private clinical" if  v326==21 | v326==24 
replace Source="Private pharmacy or drug shop" if v326==22 | v326==23
replace Source="Private shop or market" if v326==27 | v326==31 

gen Source_Simple= "Public" if  v326==11  | v326==12 | v326==13 | v326==14 | v326==15 | v326==16
replace Source_Simple="Private" if  v326==25  | v326==26 | v326==32 | v326==34 | 326==21 | v326==24 |  v326==22 | v326==23 | v326==27 | v326==31 
replace Source_Simple="Other" if  v326==30  | v326==33 | v326==96 | v326==98

* Percentage of women who decided to use family planning alone or jointly with their husbands/partners
gen decision= 1 if v632==1 | v632==3
replace decision= 0 if v632!=1 & v632!=3


* Adolescent birth rate can be calcualted using the rates package TFR2 (https://www.demographic-research.org/volumes/vol28/38/28-38.pdf)
tfr2 [pweight=v005], len(3) ageg(5) bvar(b3_*) dates(v008) wbirth(v011)

*********************************************

* mCP All women
tab mcp [aw=sampleweights]
* mCP Married women
tab mcp if married==1 [aw=sampleweights]
* mCP Unmarried women
tab mcp if married==0 [aw=sampleweights]

* tCP All women
tab tcp [aw=sampleweights]
* tCP Married women
tab tcp if married==1 [aw=sampleweights]
* tCP Unmarried women
tab tcp if married==0 [aw=sampleweights]

* Unmet Modern All women
tab unmet_mod [aw=sampleweights]
* Unmet Modern Married women
tab unmet_mod if married==1 [aw=sampleweights]
* Unmet Modern Unmarried women
tab unmet_mod if married==0 [aw=sampleweights]

* Demand Satisfied Modern All women
tab ds_mod [aw=sampleweights]
* Demand Satisfied Married women
tab ds_mod if married==1 [aw=sampleweights]
* Demand Satisfied Unmarried women
tab ds_mod if married==0 [aw=sampleweights]

* Method Mix All Women
tab method [aw=sampleweights]


* Method Information Index
tab mii [aw=sampleweights]
* Method Information Index by Method (Note, only some methods are asked about MII)
tab method mii  [aw=sampleweights], row
* Told About Side Effects
tab told_se [aw=sampleweights]
* Told What to Do About Side Effects
tab told_todo_se [aw=sampleweights]
* Told About Other Methods
tab told_om [aw=sampleweights]


* Family Planning Information in Last 12 Months
*prop.table(wtd.table(x= women_clean$fp_info, weights = women_clean$sampleweights))

* Family Planning Source
tab Source [aw=sampleweights]
tab Source_Simple [aw=sampleweights]
* Family Planning Source by Method
tab method Source  [aw=sampleweights], row
tab method Source_Simple  [aw=sampleweights], row

* Percentage of women who decided to use family planning alone or jointly with their husbands/partners
tab decision [aw=sampleweights]




*********************************************

* Percent of births that are unintended

clear all
set maxvar 10000
use "C:\Users\KristinBietsch\files\DHS Data\Nigeria\NGIR7AFL.DTA"

* classifying pregnancy as 0 in birth order
gen m10_0=v225

* pretending pregnancie is birth order 0 
gen bidx_00=v213
replace bidx_00=. if bidx_00==0

keep  v008 v005 v021 v022 m10_0 m10_1 m10_2 m10_3 m10_4 m10_5 m10_6 bidx_00 bidx_01 bidx_02 bidx_03 bidx_04 bidx_05 bidx_06 b3_01 b3_02 b3_03 b3_04 b3_05 b3_06 

* Pretending pregnancy's date of birth is month of interview (just so pregnancies are not dropped)
gen b3_00=v008
replace b3_00=. if bidx_00==.

* creating women id needed to reshape file
gen id=_n
reshape long m10_ bidx_0 b3_0, i(id v005 v021 v022) j(var)

* generating age of child (only want to keep those under 5, but dont want to drop those missing m10_ information)
gen age=.
replace age=v008-b3_0
drop if age >=60

* making the remaining missings into their own category
replace m10_=4 if m10_==.

gen sampleweights=v005/100000

tab m10_ [aw=sampleweights]
