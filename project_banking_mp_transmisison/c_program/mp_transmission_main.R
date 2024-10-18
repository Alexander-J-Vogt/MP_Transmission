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


# Load merged data
banks_data <- LOAD(dfinput = "merged_banks_data")
allfin_data <- LOAD(dfinput = "merged_allfin_data")
# 

# 1. only banks
setDT(banks_data)
banks_data <- banks_data[inrange(year, 2004, 2017)]

# Only allow for observation with no missings
complete_banks_data <- banks_data[complete.cases(banks_data),]

# Get counties, which are observed over the whole time period
check_cnty <- complete_banks_data[, c("year", "fips")]
# check_control <- check_cnty[year == 2010]

# filter <- complete_data[year == 2010]
check_cnty <- check_cnty[, ones := 1]
check_cnty <- dcast(check_cnty, fips ~ year, value.var = "ones", fill = 0)
counties_full_obs <- check_cnty[rowSums(check_cnty[ , 2:ncol(check_cnty), with = FALSE] > 0) == 14]

complete_banks_data <- complete_banks_data[fips %in% counties_full_obs$fips]






# 2. All fin
setDT(allfin_data)
allfin_data <- allfin_data[inrange(year, 2004, 2017)]

# Only allow for observation with no missings
complete_allfin_data <- allfin_data[complete.cases(allfin_data),]

# Get counties, which are observed over the whole time period
check_cnty <- complete_allfin_data[, c("year", "fips")]
# check_control <- check_cnty[year == 2010]

# filter <- complete_data[year == 2010]
check_cnty <- check_cnty[, ones := 1]
check_cnty <- dcast(check_cnty, fips ~ year, value.var = "ones", fill = 0)
counties_full_obs <- check_cnty[rowSums(check_cnty[ , 2:ncol(check_cnty), with = FALSE] > 0) == 14]

complete_allfin_data <- complete_allfin_data[fips %in% counties_full_obs$fips]

uniqueN(complete_allfin_data$fips)

# 4. Save both datasets
SAVE(dfx = complete_allfin_data, namex = "main_allfin_data")

SAVE(dfx = complete_banks_data, namex = "main_banks_data")