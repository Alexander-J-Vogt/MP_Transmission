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
sod <- sod[, .(year, fips, msabr, bkmo)]

# Number of Main offices in a county:
# Create dummy whether county is has the main office or not (bkmo)
sod <-  sod[, cnty_main_office := sum(bkmo), by = .(fips, year)]

# Collapse by year and fips 
sod <- unique(sod, by = c("year", "fips"))

# Indicator whether a county lays in a Metropolitan Statistical Area (MSA) or not:
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
ur_cnty <- LOAD(dfinput = "ur_cnty")

# Import Average Earnings Data
qwi_earnings <- LOAD(dfinput = "qwi_earnings")

# Import Controls from sod
controls_sod <- LOAD(dfinput = "controls_sod")

# Population density
pop_density <- LOAD(dfinput = "landarea_data")

# Combining datasets by county and year
merged_data <- left_join(pop_cnty, controls_sod, by = c("fips", "year"))
merged_data <- left_join(merged_data, qwi_earnings, by = c("fips", "year"))
merged_data <- left_join(merged_data, ur_cnty, by = c("fips", "year"))
merged_data <- left_join(merged_data, pop_density, by = "fips")

# Creating share of employment in each county and year
setDT(merged_data)
merged_data[, emp_rate := mean_emp / cnty_pop]

# Calculate population desnity of a county
merged_data <- merged_data[, pop_density := cnty_pop / landarea_sqkm]

# SAVE
SAVE(dfx = merged_data)

########################## ENDE ###############################################+