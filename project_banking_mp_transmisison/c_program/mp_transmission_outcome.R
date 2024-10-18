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

# 1. Data CLeaning and Collapsing Data to County-level ========================= 

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
start_time <- Sys.time()
# Basic Data Cleaning for two possible filters
# a) Filter for all Commercial Banks
# Core Problem less and less CB or their mortgage subdivisions are handing out 
# mortgage after 2009, which is trend observed in the mortgage lending business.
mortgages_banks <- lapply(hmda_files, COUNTYLEVEL, instfilter = TRUE)

# b) Includes all Financial Institutions
# This dataset is collapses all loans within a county independently of the 
# of financial institution. 
mortgages_all <- lapply(hmda_files, COUNTYLEVEL, instfilter = FALSE)
end_time <- Sys.time()
print(end_time - start_time)

# Create the actual datasets
hmda_banks <- bind_rows(mortgages_banks)
hmda_all <- bind_rows(mortgages_all)

# Exclude all missing observation from hmda_all
hmda_all <- hmda_all[!is.na(total_amount_loan)]

# Save
SAVE(dfx = hmda_banks, namex = "hmda_banks")
SAVE(dfx = hmda_all, namex = "hmda_all")


########################## ENDE ################################################



