# Recall bias correction and regression

rm(list=ls())
library(readr)
library(data.table)
library(mortdb, lib='/ihme/mortality/shared/r')
library(haven)

args <- commandArgs(trailingOnly = T)
filepath <- args[1]
input <- fread(filepath)

# Data processing

# gen period
input[, year := as.integer(regmatches(svy, regexpr(".{4}$", svy)))]
# input[, suryear := year]
input[svy != "IRQ_2007", year := year-1L]

input[, year := (year-period) + 1.5] # So that the first period starts in 2013.5 if the survey was in 2014


# get loc ids
input[, ihme_loc_id := gsub("_.*$",'',svy)]

# STATA script drops some poorly formatted locs; like Bohol, Pemba, UP, AP, and sexual_behavior_zmb
input <- input[!(ihme_loc_id %in% c('AP02009','AP12006','Bohol02009','Bohol12006','Pemba12006','UP12007')),]

# if location_id longer than 3, add an underscore
input[nchar(ihme_loc_id)>3, ihme_loc_id := gsub('^([A-Z]{3})([0-9]+)$', '\\1_\\2', ihme_loc_id)]

input[,sex:=ifelse(female==1,"female","male")]

# merge onto locations
locs <- get_locations()
locs <- locs[,.(ihme_loc_id, location_name, location_id)]
alldata <- merge(input, locs, by='ihme_loc_id')

# generate duplicates for country year sex

alldata[, dup := 1:.N, by=c('ihme_loc_id','year','sex')]
alldata <- alldata[,.SD,.SDcols = c('svy', 'q45q15', 'year', 'ihme_loc_id', 'sex', 'NID')]
alldata[, suryear:= as.integer(substr(svy, nchar(svy)-3,nchar(svy)))]

# Drop subnationals
subnats <- alldata[grepl("BRA[0-9]+|ETH_[0-9]{5}|IDN[0-9]+|KEN_[0-9]{5}|ZAF[0-9]{3}", svy)]
alldata <- alldata[!(svy %in% unique(subnats$svy))]

# not a big dataset, try using a for loop
# If it takes too long, we can parallelize

# Goal: get survey year pairs
paired <- NULL
for (v in unique(alldata$svy)) {
  subs <- alldata[svy==v, c('svy','q45q15','suryear','ihme_loc_id','year','sex')]
  setnames(subs, old=c("svy","q45q15","suryear"), new=c("basesvy", "base45q15","base_suryear"))
  to_add <- merge(subs, alldata, by=c('ihme_loc_id','year','sex'))
  
  setnames(to_add, old=c("svy","q45q15","suryear"), new=c("compsvy","comp45q15","comp_suryear"))
  to_add <- to_add[base_suryear>comp_suryear,]
  paired[[v]] <-  to_add
}

paired <- do.call("rbind", paired)
paired[, diff := comp45q15-base45q15]
paired[, year_diff := as.integer(base_suryear)-as.integer(comp_suryear)]


# generate the log difference
paired[, lnbase45q15:=log(base45q15)]
paired[, lncomp45q15:=log(comp45q15)]
paired[, lndiff:=lncomp45q15-lnbase45q15]

# Output pairs
fwrite(paired, paste0('/home/j/WORK/02_mortality/02_inputs/04_45q15/sibling/recall_bias_input/paired_data_', format(Sys.Date(), "%d %b %Y"), ".csv"))

# Run the regression - STATA does regression separately for males and females
male_model <- lm(formula=diff ~ year_diff+0, data=paired[sex=="male",])
fem_model <- lm(formula=diff ~ year_diff+0, data=paired[sex=="female",])

reg_results <- data.table('coeffs'=c(male_model$coefficients, fem_model$coefficients), 
                          'se'=c(coef(summary(male_model))[, "Std. Error"], coef(summary(fem_model))[, "Std. Error"]), 
                          'sex'=c(1,2))
reg_results[, `:=`(lb45q15=coeffs-1.96*se, ub45q15=coeffs+1.96*se)]

# output
fwrite(reg_results, paste0('/home/j/WORK/02_mortality/02_inputs/04_45q15/sibling/FINAL_ESTIMATES/reg_coefficients_',format(Sys.Date(), "%d %b %Y"), ".csv"))

# ------------------------------
# Apply recall bias correction to estimates


# going to use inputs set from before
adj_results <- copy(input)
adj_results[, c('adj45q15','adj45q15_lower','adj45q15_upper'):=q45q15]


#add a current toggle, for now use T
current=T
for(s in c(1,2)) {
  gender <- ifelse(s==1, 'male', 'female')
  if(current) {
    est <- reg_results[sex==s, c('sex','coeffs','lb45q15','ub45q15')]
  } else {
    est <- setDT(read_stata('/home/j/Project/Mortality/GBD Envelopes/envelope2011/data/Corrected sibling survival for GBD/Recall Bias/Sibs Results/regression_coefficients/allcoefficients_ 1 Oct 2015.dta'))
    est <- est[sex==s,]
    est <- dcast(est, sex ~ coefficient, value.var='yeardiff')
    setnames(est, old=c('b','lb','ub'), new=c('coeffs','lb45q15','ub45q15'))
  }
  
  for (i in seq(1,15)) {
    adj_results[period==i & sex==gender, `:=`(adj45q15=adj45q15+est$coeffs*i,
                                           adj45q15_lower=adj45q15_lower+est$lb45q15*i,
                                           adj45q15_upper=adj45q15_upper+est$ub45q15*i)]
  }
}

# Add a NID map - does this still exist? / is it up to date? STILL NEED TO MERGE THIS ONTO OLD DATA
nid_map <- setDT(readr::read_csv('/home/j/Project/Mortality/GBD Envelopes/envelope2011/data/Corrected sibling survival for GBD/nid_map_gbd2016.csv'))
nid_map[, svy_type:=NULL]

est_global_sib <- merge(adj_results, nid_map, by='svy', all.x=T)
est_global_sib[,NID:=as.integer(NID)]
est_global_sib[is.na(nid),nid:=NID]
assertable::assert_values(est_global_sib, 'nid', test = "not_na")

# Subset to necessary cols
est_global_sib <- est_global_sib[, .(svy, deaths_source, year, ihme_loc_id, sex, adj45q15, adj45q15_lower, adj45q15_upper, nid)]

# Generate sourcedate
est_global_sib[, source_date := as.integer(regmatches(svy, regexpr(".{4}$", svy)))]



# write output
# fwrite(est_global_sib, paste0('/home/j/WORK/02_mortality/02_inputs/04_45q15/sibling/recall_bias_output_',date, '.csv'))
# save results to ESTIMATE folder where it will be compiled in 45q15_data_prep
write_dta(est_global_sib, "/home/j/WORK/02_mortality/02_inputs/04_45q15/sibling/FINAL_ESTIMATES/EST_GLOBAL_SIB_45q15s.dta",version = 13)
write_dta(est_global_sib, paste0("/home/j/WORK/02_mortality/02_inputs/04_45q15/sibling/FINAL_ESTIMATES/EST_GLOBAL_SIB_45q15s_",format(Sys.Date(), "%d %b %Y"), ".dta"),version = 13)