# TARGET: 
# INDATA: 
# OUTDATA/ OUTPUT:

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

# 1. Merging the dataset from outcome, treatment and control script ============

# Loading Mortgage Data from Outcome Script
# a) Dataset that only includes Commercial Banks and their Mortgage Subdivisions
# that lended mortgages
outcome_banks_data <- LOAD(dfinput = "hmda_banks")
setDT(outcome_banks_data)

# b) Dataset that includes all financial institutions that lend mortgage
outcome_all_data <- LOAD(dfinput = "hmda_all")
setDT(outcome_all_data)
# loading treatment data (important as this contains counties, which are observed over all time periods in the SOD)
treatment_data <- LOAD(dfinput = "mp_transmission_treatment")
setDT(treatment_data)

# Loading control data
controls_data <- LOAD(dfinput = "mp_transmission_control")
setDT(controls_data)

# 2. Commerical Banks ==========================================================

# Perform Full Join between Mortgage Data and treatment (SOD + FFR) ------------
banks_data <- full_join(outcome_banks_data, treatment_data, by = c("fips", "year"))
banks_data <- full_join(banks_data, controls_data, by = c("fips", "year"))

SAVE(dfx = banks_data, namex = "merged_banks_data")

# 3. All Financial Institutions ================================================

allfin_data <- full_join(outcome_all_data, treatment_data, by = c("fips", "year"))
allfin_data <- full_join(allfin_data, controls_data, by = c("fips", "year"))

SAVE(dfx = allfin_data, namex = "merged_allfin_data")


