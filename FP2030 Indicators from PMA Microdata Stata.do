* FP2030 Survey Based Indicators from a PMA

* Kristin Bietsch, PhD
* Track20 Project, Avenir Health

clear all
set more off

use "C:\Users\KristinBietsch\files\PMA2020 Data\Uganda\PMA2020_UGP1_HQFQ_v2.0_21Jul2021\PMA2020_UGP1_HQFQ_v2.0_21Jul2021.dta"

keep if FQweight!=.

gen married=1 if FQmarital_status==1 | FQmarital_status==2
replace married=0 if FQmarital_status!=1 & FQmarital_status!=2

gen unmet_mod = unmettot
replace unmet_mod=1 if tcp==1

gen ds_mod= 0 if unmet==1 | unmet==2 | unmet==3 | unmet==4 
replace ds_mod=1 if mcp==1


gen method= "Pill" if current_methodnum_rc==7
replace method= "Injectable" if   current_methodnum_rc==5 |  current_methodnum_rc==16 
replace method="IUD"    if  current_methodnum_rc==4
replace method= "Implant"   if current_methodnum_rc==3 
replace method= "MCondom"   if  current_methodnum_rc==9 
replace method=  "FCondom"   if current_methodnum_rc==10
replace method=   "LAM"  if   current_methodnum_rc==14
replace method=  "FSter"   if   current_methodnum_rc==1 
replace method=  "MSter"  if current_methodnum_rc==2 
replace method= "SDM"   if  current_methodnum_rc==13 
replace method= "OMM"   if  current_methodnum_rc==8 | current_methodnum_rc==11 | current_methodnum_rc==12  
                                                     
                                                      
gen told_se=1 if fp_side_effects==1 
replace told_se=0 if fp_side_effects!=1 & fp_side_effects!=.

gen told_todo_se=1 if fp_side_effects==1 & fp_side_effects_instructions==1
replace told_todo_se =0 if   fp_side_effects==1 & fp_side_effects_instructions==0
replace told_todo_se =0 if  fp_side_effects!=1 & fp_side_effects!=.
 
gen told_om = 1 if fp_told_other_methods==1
replace  told_om = 0 if fp_told_other_methods!=1 & fp_told_other_methods!=.

gen told_switch=1 if fp_told_switch==1 
replace told_switch=0 if fp_told_switch!=1 &  fp_told_switch!=.

gen mii=1 if told_se==1 & told_todo_se==1 & told_om==1
replace mii=0 if told_se==0 | told_todo_se==0 | told_om==0 

gen mii_plus=1 if told_se==1 & told_todo_se==1 & told_om==1 & told_switch==1 
replace mii_plus =0 if told_se==0 | told_todo_se==0 | told_om==0 | told_switch==0 


gen source= "Public" if fp_provider_rw== 11 | fp_provider_rw== 12 | fp_provider_rw== 13 | fp_provider_rw== 14 | fp_provider_rw== 15 | fp_provider_rw== 16  
replace source="NGO/FBO" if  fp_provider_rw== 24 | fp_provider_rw== 25  | fp_provider_rw== 32 
replace source= "Other" if fp_provider_rw== -88 | fp_provider_rw== 27 | fp_provider_rw== 33 | fp_provider_rw== 96 
replace source= "Private clinical" if fp_provider_rw==21 | fp_provider_rw== 23   
replace source= "Private pharmacy or drug shop" if  fp_provider_rw==22
replace source= "Private shop or market" if  fp_provider_rw== 31 

gen source_simple= "Public" if fp_provider_rw== 11 | fp_provider_rw== 12 | fp_provider_rw== 13 | fp_provider_rw== 14 | fp_provider_rw== 15 | fp_provider_rw== 16  
replace source_simple="Private" if  fp_provider_rw== 24 | fp_provider_rw== 25  | fp_provider_rw== 32 | fp_provider_rw==21 | fp_provider_rw== 23  | fp_provider_rw==22 | fp_provider_rw== 31 
replace source_simple= "Other" if fp_provider_rw== -88 | fp_provider_rw== 27 | fp_provider_rw== 33 | fp_provider_rw== 96 



gen decision=1 if partner_overall==1 | partner_overall==3
replace decision=0 if   partner_overall!=1 & partner_overall!=3 & partner_overall!=.

*****************************************************
* mCP All women
tab mcp [aw=FQweight]
* mCP Married women
tab mcp if married==1 [aw=FQweight]
* mCP Unmarried women
tab mcp if married==0 [aw=FQweight]

* tCP All women
tab tcp [aw=FQweight]
* tCP Married women
tab tcp if married==1 [aw=FQweight]
* tCP Unmarried women
tab tcp if married==0 [aw=FQweight]

* Unmet Modern All women
tab unmet_mod [aw=FQweight]
* Unmet Modern Married women
tab unmet_mod if married==1 [aw=FQweight]
* Unmet Modern Unmarried women
tab unmet_mod if married==0 [aw=FQweight]

* Demand Satisfied Modern All women
tab ds_mod [aw=FQweight]
* Demand Satisfied Married women
tab ds_mod if married==1 [aw=FQweight]
* Demand Satisfied Unmarried women
tab ds_mod if married==0 [aw=FQweight]

* Method Mix All Women
tab method [aw=FQweight]

* Method Information Index Plus
tab mii_plus [aw=FQweight]
* Method Information Index
tab mii [aw=FQweight]
* Method Information Index Plus by Method (Note, only some methods are asked about MII)
tab method mii_plus  [aw=FQweight], row
* Method Information Index by Method (Note, only some methods are asked about MII)
tab method mii  [aw=FQweight], row
* Told About Side Effects
tab told_se [aw=FQweight]
* Told What to Do About Side Effects
tab told_todo_se [aw=FQweight]
* Told About Other Methods
tab told_om [aw=FQweight]
* Told About Switching to Other Methods
tab told_switch [aw=FQweight]


* Family Planning Information in Last 12 Months
*prop.table(wtd.table(x= women_clean$fp_info, weights = women_clean$sampleweights))

* Family Planning Source
tab source_simple [aw=FQweight]
tab source_simple [aw=FQweight]
* Family Planning Source by Method
tab method source_simple  [aw=FQweight], row
tab method source_simple  [aw=FQweight], row

* Percentage of women who decided to use family planning alone or jointly with their husbands/partners
tab decision [aw=FQweight]



