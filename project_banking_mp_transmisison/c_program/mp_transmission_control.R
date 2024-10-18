# TARGET: Creating a Main dataset for all control variables from different sources
# INDATA: banks_sod, pop_cnty, ur_cnty, qwi_earnings, controls_sod
# OUTDATA/ OUTPUT: MAINNAME 

################################################################################################################+
# INTRO	script-specific ####

#clear gobal environment of all but uppercase objects (globals, myfunctions, scalars)
CLEARCOND()

#get scriptname
MAINNAME <- current_filename()#returns path+name of sourced script (from currently executed master script)
if(is.null(MAINNAME)){
  MAINNAME <- rstudioapi::getActiveDocumentContext()$path #dto. of currently executed script
}
MAINNAME <- sub(".*/|^[^/]*$", "", MAINNAME)
MAINNAME <- substr(MAINNAME,1,nchar(MAINNAME)-2) #cut off .R
######################+
#release unused memory 
gc()

################################################################################################################+
# MAIN PART ####

# 1. Summary of Deposits - Control Variables ===================================

# Create control variables based on variables available in the SOD

# Load the Summary of Deposits for the period 1994 to 2020
sod <- LOAD(dfinput = "banks_sod", dfextension = ".rda")
setDT(sod)

# Restrict to the relevant variables
sod <- sod[, .(year, fips, sims_acquired_date, msabr, bkmo)]

# 
# Create turnover rate of branches for each year (sims_fips# Create turnover rate of branches for each year (sims_aquired_date)
# sod <- sod[, year := as.integer(year)]
# sod <- sod[, sims_acquired_date := 
#              substr(sims_acquired_date, 
#                     nchar(sims_acquired_date) - 3, 
#                     nchar(sims_acquired_date))
#         ]
# sod <- sod[, sims_acquired_date := as.integer(sims_acquired_date)]
# 
# sod <-  sod[, d_turnover := ifelse(year == sims_acquired_date, 1, NA)]
# sod <-  sod[, cnty_turnover := sum(d_turnover), by = .(fips, year)]

# Number of Main offices in a county:
# Create dummy whether county is has the main office or not (bkmo)
sod <-  sod[, cnty_main_office := sum(bkmo), by = .(fips, year)]

# Collapse by year and fips 
sod <- unique(sod, by = c("year", "fips"))

# Indicatir whether a county lays in a Metropolitan Statistical Area (MSA) or not:
# MSA tells whether an areas has than 50000 inhabitants
# Could account for the fact that a different economic dynamic is present compared
# to non-MSA and the amount of loans handed-out is different compared to a areas
# with less than 50000.
sod <- sod[, d_msa := as.integer(ifelse(msabr > 0, 1, 0))]

# Select relevant variables
sod <-  sod[, c("year", "fips", "cnty_main_office", "d_msa")]

# SAVE
SAVE(dfx = sod, name = "controls_sod")

# clear global environment
rm(list = c("sod"))

# 2. Creating Control Dataset ==================================================

# Import Population County dataset
pop_cnty <- LOAD(dfinput = "pop_cnty")

# Import Population State dataset
pop_state <- LOAD(dfinput = "pop_state")

# Import Unemployment Rate
ur_cny <- LOAD(dfinput = "ur_cnty")

# Import Average Earnings Data
qwi_earnings <- LOAD(dfinput = "qwi_earnings")

# Import Controls from sod
controls_sod <- LOAD(dfinput = "controls_sod")

# # Define as data.table
# datasets <- ls()[sapply(ls(), function(x) is.data.frame(get(x)) || inherits(get(x), "data.table"))]
# print(datasets)
# 
# for (dataset in datasets) {
#   assign(dataset, setDT(get(dataset)))
# }

# Comining datsets by county and year
merged_data <- left_join(pop_cnty, controls_sod, by = c("fips", "year"))
merged_data <- left_join(merged_data, qwi_earnings, by = c("fips", "year"))
merged_data <- left_join(merged_data, ur_cny, by = c("fips", "year"))

# SAVE
SAVE(dfx = merged_data)
