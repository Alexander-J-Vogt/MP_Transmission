# TARGET: Creating two county-level datasets, where one only contains mortgages of commercial banks and one from all financial institutions
# INDATA: hmda_merge files for all years
# OUTDATA/ OUTPUT: hmda_banks, hmda_all

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

# 1. Data Cleaning and Collapsing Data to County-level ========================= 

#' This section performs a standardized way to do some basic cleaning of data
#' and collapses the data to county-level with the help of the COUNTYLEVEL function.
#'
#' What does COUNTYLEVEL-function do?
#' i) It allows to filter either for Commercial Banks and their subdivisions,
#' which lend mortgages or to exclude the filter. I
#' -> instfilter: institution filter for commercial banks (TRUE: Only Commercial banks)
#' ii) It excludes all observation if either the state-code or county-code is missing.
#' iii) It creates the fips-code. The unique identifier for each county in the U.S.
#' iv) It makes sure to only include US states and District of Columbia.
#' v) It collapses the data to county-year-level.

# List all merged files
hmda_files <- list.files(paste0(TEMP, "/") , pattern = "merge")


# Filter for all Commercial Banks
# Core Problem less and less CB or their mortgage subdivisions are handing out 
# mortgage after 2009, which is trend observed in the mortgage lending business.
mortgages_banks <- lapply(hmda_files, COUNTYLEVEL, instfilter = TRUE)

# Create the actual datasets
hmda_banks <- bind_rows(mortgages_banks)

# 2. Create weights for counties
pop_cnty <- LOAD(dfinput = "pop_cnty")
setDT(pop_cnty)

# Merge hmda dataset with population dataset
hmda_banks <- merge(hmda_banks, pop_cnty, by = c("fips", "year"), all.x = TRUE)

# Create several variables with logs and leads, weighted and not weighted outcome variables
hmda_banks[, wtd_loan_amount := total_amount_loan / cnty_pop]
hmda_banks[, ln_loan_amount := log(total_amount_loan)]
hmda_banks[, ln_wtd_loan_amount := log(total_amount_loan)]
hmda_banks[, lead_wtd_loan_amount := shift(wtd_loan_amount, type = "lead"), by = fips]
hmda_banks[, lead_ln_loan_amount := shift(ln_loan_amount, type = "lead"), by = fips]
hmda_banks[, lead_ln_wtd_loan_amount := shift(ln_wtd_loan_amount, type = "lead"), by = fips]

# Select relevant variables
hmda_banks[, c("cnty_pop", "us_pop", "wt_cnty_pop") := NULL]
setcolorder(hmda_banks, c("year", "fips", "state"))

# Drop observation in the year 2017, due to lead-lag
# hmda_banks[year != 2017]


# Exclude all missing observation from hmda_all
# hmda_all <- hmda_all[!is.na(total_amount_loan)]

# Save
SAVE(dfx = hmda_banks, namex = "hmda_banks")

########################## ENDE ###############################################+
