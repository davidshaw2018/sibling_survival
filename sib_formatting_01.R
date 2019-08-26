############################################################################################################################################
# Formatting for sibling survival
# Hopefully you don't need to do much, but we need standard columns: 

# Necessary Variables:	Type:	    Description:
# svy	                  [string]	identify the survey, country, and year i.e. “DHS_BFA_1998”
# 
# id	                  [string]	uniquely identify the family unit within the svy i.e. “1 36 1”
# 
# sibid	                [numeric]	uniquely identify the sibling within the family unit. The respondent should have sibid=0.
# 
# v005	                [numeric]	population weight. May need to look in documentation from the survey to identify which variable contains this information.
# 
# v008	                [numeric]	cmc date of interview. See this document for how to create cmc dates from month and year info.
# 
# v021	                [numeric]	primary sampling unit. May need to look in documentation from the survey to identify which variable is the primary sampling unit.
# 
# yr_interview	        [numeric]	year in which each respondent’s individual interview took place. Sometimes, although a svy is called the BFA 1998 DHS, some interviews will have taken place in 1997 or 1999. There should be a variable for year or date of interview.
# 
# sex	                  [numeric]	2=female 1=male
# 
# yob	                  [numeric]	year of birth of each sibling. Minimize missingness by using all information available to calculate this field.
# 
# yod                 	[numeric]	year of death of sibling. Respondents should always be missing this field.
# 
# alive               	[numeric]	0=dead 1=alive

############################################################################################################################################

library(data.table)
library(haven)
library(readr)
rm(list=ls())

user <- Sys.getenv("USER")
if (Sys.info()[1]=="Windows") {
  root <- "J:"
  h_root <- "H:/mort-data"
} else {
  root <- "/home/j"
  h_root <- paste0("/homes/", user, "/mort-data")
}
args <- commandArgs(trailingOnly = T)
filepath <- args[1]
input_svy <- args[2]
output_dir <- paste0(root, '/WORK/02_mortality/02_inputs/04_45q15/sibling/',input_svy, '/new_outputs')
rawdata <- setDT(read_stata(filepath))

# test <- rawdata[, sapply(.SD, attr, 'label')]
# deaths <- grep("death|yod", test, value=T)

#rename columns
drop.cols <- grep("^mm(.)_0", colnames(rawdata))
colnames(rawdata)[drop.cols] = gsub("_0","_",colnames(rawdata)[drop.cols])

#rawdata <- rawdata[!is.na(mmc1),]	# Drop females who did not respond to sibling history module. 
rawdata <- rawdata[v007>99, v007:=v007-1900]
  
# // The following applies to the respondent - since they are not reported in the sib history, 
# // their information is generated using the information they provide about themselves (year of birth, etc.).
rawdata[,mmidx_21 := 0]
rawdata[,mm1_21 := 2] #sex (in raw dta file, 1 = male 2=female)
rawdata[,mm2_21 := 1] #alive
rawdata[,mm3_21 := v012] #current age
rawdata[,mm4_21 := v011] # cmc dob
rawdata[,mm6_21 := NA] #years ago died
rawdata[,mm7_21 := NA] # age at death
rawdata[,mm8_21 := NA] #cmc dod
rawdata[,mm15_21 := NA] # year of death

# Reshape so that every observation is a sibling
reshaped = melt(rawdata,id.vars = c("caseid", "v000","v002","v005","v006","v007","v008","v009","v010","v011","v012","v013","v014","v021"),
                measure=patterns("mmidx_","mm1_", "mm2_", "mm3_", "mm4_", "mm6_", "mm7_", "mm8_", "mm15_"),
                variable.name = "sibid", variable.factor = F, value.name = c("mmid","sex","alive","age_sib_ifalive","cmc_dob_sib","years_since_dead","age_sib_ifdead","cmc_dod","yod_sib")
)
reshaped[,sibid:=as.numeric(sibid)]
reshaped[sibid==21, sibid:=0]
reshaped <- reshaped[!is.na(mmid),]




setnames(reshaped,c("caseid","v000","v002","v007","v009","v010","v011", "v012", "v013", "v014"),
         c("id","wbcode","hhid","yr_interview","month_of_birth","yob_resp","cmc_dob","ageresp","ageresp_cat","data_completeness"))
reshaped[,v005 := v005 / 1000000] # Per DHS manual, divide sampling weight by 1000000 before applying. 


# ** CALCULATE BASIC INPUTS 
# // Year of death
# ** Primary input: mm8_ : CMC date of death of sibling
# ** Secondary input: v007 - mm6_ : year of interview - number of years ago respondent's sibling died
# ** Tertiary input: mm15_: year of death of sibling (not in all surveys)
reshaped[,yod := as.integer((cmc_dod-1)/12)]
reshaped[is.na(yod)|(wbcode == "NP3" & yr_interview == 1996),yod := as.integer(yr_interview-years_since_dead)]
reshaped[is.na(yod),yod := as.integer(yod_sib)]

# // Year of birth
# ** Primary input: mm4_: CMC date of birth of sibling
# ** Secondary input: year of interview - sibling's current age [if alive]
# ** Tertiary input: year of interview - (age at death of sibling + years ago sibling died) [if dead]
reshaped[, yob:=as.integer((cmc_dob_sib-1)/12)]
reshaped[(alive==1 & is.na(yob))|(wbcode == "NP3" & yr_interview == 1996), yob:=as.integer(yr_interview-age_sib_ifalive)]
reshaped[(alive==0 & is.na(yob))|(wbcode == "NP3" & yr_interview == 1996), yob:=as.integer(yr_interview-(age_sib_ifdead+years_since_dead))]
reshaped[yob<0, yob:=0]


# ** *****************************************************************************************
#   ** Convert Ethiopia dates to standard calendar
# ** *****************************************************************************************
#   // ANYTHING BEFORE MAY: +7 YEARS, AFTER MAY: + 8 YEARS
# // Ethiopia's 
		if(substr(input_svy,1,3) == "ETH") {
		  reshaped[, yr_interview := 8 + yr_interview]

			max_yr_interview = max(reshaped$yr_interview)
      max_yod = max(reshaped$yod)

			if(max_yod < max_yr_interview) {
			  reshaped[v006 < 5 & yr_interview%in%c(2000,2005,2010), yob := yob + 7]
			  reshaped[v006 < 5 & yr_interview%in%c(2000,2005,2010), yod := yod + 7] 

			  reshaped[v006 >= 5 & yr_interview%in%c(2000,2005,2010), yob := yob + 8] 
			  reshaped[v006 >= 5 & yr_interview%in%c(2000,2005,2010), yod := yod + 8] 
			}
		}

# // Alive or dead status:
reshaped[alive == 8, alive:=NA] # Don't know -- recode to missing.
		

reshaped[,svy := input_svy]
reshaped[,hhid:=as.character(hhid)]
samplesize <- sum(reshaped$v005[reshaped$sibid==0])
reshaped[,samplesize := samplesize]
keepcols <- c("id", "sibid", "hhid","v005", "v006", "yr_interview", "v008", "ageresp", "v021", "sex", "alive", "yod", "yob", "svy", "samplesize")
input <- reshaped[,.SD, .SDcols = keepcols]
setorder(input, "hhid","sibid")
# write_dta(input, paste0(output_dir,"/allsibhistories.dta"),version = 13)

# As before, hopefully there's not much work. Sometimes years have to be recoded, and a few surveys have different alive/dead codes. Column names
# may not be standardized well
# Check each survey year, before running ZSC/regression/everything else - those depend on having the right column names and such

# ex. AGO_2015_2016

# input <- setDT(read_stata('/home/j/WORK/02_mortality/02_inputs/04_45q15/sibling/AGO_2015_2016/allsibhistories.dta'))

# do your work here -----------------------------

# Years are coded oddly
unique(input$yr_interview)
unique(input$yob)
unique(input$yod)

# Using 115==2015 as reference, adjust years (by adding 1900)
cols <- c('yob','yr_interview','yod')
input[, (cols):=lapply(.SD, function(x) {x+1900}), .SDcols=cols]

# sex almost always coded as 0=female, 1=male; change to match GBD sex ids
input[sex==0, sex:=2]

# check that ageresp looks OK (if provided, you can always calculate with yr_interview-yob)
unique(input$ageresp)

# --------------------------------------------

# write formatted data to output director
fwrite(input, paste0(output_dir,'/allsibhistories_formatted.csv'))






















