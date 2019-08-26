#####################################################################
# Run all for Sibling Survival Analysis
# Steps: 1 - format script, run that independently
# 2 - Zero Survivor Correction
# 3 - Data prep for regression
# 4 - Sparse Data Correction

# Supplementary: 5 - recall bias correction
# Can turn this into Jobmon later

#####################################################################

# Configure R
library(assertable)
library(readr)
library(haven)
library(mortcore, lib='/ihme/mortality/shared/r')
library(data.table)

rm(list=ls())
user <- Sys.getenv("USER")

if (Sys.info()[1]=="Windows") {
  root <- "J:"
  h_root <- "H:/mort-data"
} else {
  root <- "/home/j"
  h_root <- paste0("/homes/", user, "/mort-data")
}

extraction_sheet <- fread(paste0(root, '/WORK/02_mortality/02_inputs/04_45q15/sibling/_input/new_sib_his_sources.csv'))
rownum <- 22  # change to the row number you'd like to prep
survey <- as.list(extraction_sheet[rownum,])
filepath <- gsub("\\\\", "/", survey$filename)
filepath <- gsub("J:", root, filepath)
input_svy <- paste0(survey$iso3,"_",survey$year)  #"AGO_2015_2016" # change this to survey of interest
deaths_source <- tolower(paste0("DHS","_",survey$iso3)) # change this if deaths_source = DHS, CDC-RHS, etc.

# Set or create input directory
input_dir <- paste0(root, '/WORK/02_mortality/02_inputs/04_45q15/sibling/',input_svy, '/new_outputs')
if (!dir.exists(input_dir)) {
  dir.create(file.path(input_dir))
}

code_dir <- paste0(h_root, '/45q15/raw/sibs/SKELETON/Code/R/')
r_shell <- "/share/singularity-images/rstudio/shells/r_shell_singularity_3530.sh"

##########################################
# Before doing this step, make sure there's an appropriate output directory


#note: this 01 formatting script is for DHS. If you are prepping other surveys that have different column names than DHS, you will need to format the survey interactively
qsub(
  jobname= "sibling_prep_01",
  code = paste0(code_dir, 'sib_formatting_01.R'),
  pass = list(filepath, input_svy),
  mem = 20,
  cores=1,
  proj = 'proj_mort_prep',
  wallclock = "00:15:00",
  shell = r_shell,
  archive_node = TRUE,
  submit = T
)

qsub(
  jobname= "sibling_prep_02",
  code = paste0(code_dir, 'zero_survivor_correction_02.R'),
  pass = list(input_dir),
  mem = 2,
  cores=1,
  proj = 'proj_mort_prep',
  wallclock = "00:15:00",
  shell = r_shell,
  archive_node = TRUE,
  hold = "sibling_prep_01",
  submit = T
)

qsub(
  jobname = "sibling_prep_03",
  code = paste0(code_dir, 'data_prep_for_analysis_03.R'),
  pass = list(input_dir),
  cores = 1,
  mem=2,
  proj = 'proj_mort_prep',
  wallclock = "00:10:00",
  shell=r_shell,
  hold = "sibling_prep_02",
  archive_node = TRUE,
  submit=T
)

qsub(
  jobname = "sibling_prep_04",
  code = paste0(code_dir, 'regression_sv_04.R'),
  pass = list(input_dir, input_svy, deaths_source, survey$nid),
  cores = 2,
  mem = 10,
  proj = 'proj_mort_prep',
  wallclock = "00:40:00",
  shell=r_shell,
  hold = "sibling_prep_03",
  archive_node = TRUE,
  submit=T
)


########################################################################################

# Check error files, then check for files existing

# 01 formatted - look for allsibhistories_formated.csv
assertable::check_files(paste0(input_dir, '/allsibhistories_formatted.csv'))

# 02 ZSC - look for sibhistories_with_missing_sibs.csv
assertable::check_files(paste0(input_dir, '/sibhistories_with_missing_sibs.csv'), continual=T, sleep_end = 20, sleep_time=30)

# 03 data prep - look for allcountrysurveys.csv
assertable::check_files(paste0(input_dir, '/allcountrysurveys.csv'), continual=T, sleep_end = 20, sleep_time=15)

# 04 regression - look for both_sexes_45q15
assertable::check_files(paste0(input_dir, '/both_sexes_45q15.csv'), continual=T, sleep_end = 20, sleep_time=15)



########################################################################################

# Recall bias correction

# Append all both_sexes results, from regression, into one set
# Please do this every time you want to run recall bias, just for ease of date formatting

file_list <- Sys.glob('/home/j/WORK/02_mortality/02_inputs/04_45q15/sibling/*/new_outputs/fullmodel_svy.csv')

master <- NULL
for (f in file_list) {
  infile <- fread(f)

  master <- rbind(master, infile)
}

#append old prepped dataset from GBD2017
old_data <- setDT(read_dta('/home/j/WORK/02_mortality/02_inputs/04_45q15/sibling/recall_bias_input/compiled_data_ 8 May 2019.dta')) 
master <- rbind(old_data,master)
master_filepath <- paste0('/home/j/WORK/02_mortality/02_inputs/04_45q15/sibling/recall_bias_input/compiled_data_',
                          Sys.Date(),'.csv')
fwrite(master, master_filepath)

qsub(
  jobname = "recall_bias_05",
  code= paste0(code_dir, "Recall_bias_correction_05.R"),
  pass=list(master_filepath),
  cores=4,
  mem = 20,
  wallclock="00:45:00",
  shell = r_shell, 
  archive_node = TRUE,
  submit=T
)



















