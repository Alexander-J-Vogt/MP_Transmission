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

# Determine the period
start <- 2002
end <- 2016
diff_year <- end - start + 1


# 1. Dataset with Mortgages of Commercial Banks ================================

# Load merged data
banks_data <- LOAD(dfinput = "banks_data_joined")
setDT(banks_data)

# Subset for the relevant period
banks_data <- banks_data[inrange(year, start, end)]

# Only keep counties that are observed over the whole period
complete_banks_data <- COMPLETEOBS(data = banks_data, rowx = "fips", colx = "year")

# Implement First Differencing for all relevant variables
# Define the key variables to demean
key_vars <- c("ln_loan_amount", "ln_wtd_loan_amount", "lead_ln_loan_amount", "lead_ln_wtd_loan_amount","mean_hhi", "cnty_pop", "mean_earning", "mean_emp", "ur")

# Calculate the year-specific mean for each variable and subtract it from the individual observation
for (var in key_vars) {
  # Calculate the mean for each year
  complete_banks_data[, paste0("demeaned_", var) := get(var) - mean(get(var), na.rm = TRUE), by = year]
}

# Create lagged variables
lagged_var <- c("cnty_pop", "mean_earning", "mean_emp", "ur")

for (i in lagged_var) {
  complete_banks_data[, paste0("lag_", i) := shift(get(i), type = "lag"), by = fips]
}

# Calculate the change in earnings
complete_banks_data[, delta_earnings := mean_earning - shift(mean_earning, type = "lag")]

# Creating log mean_earnings in order to get normally distributed variables ====
complete_banks_data[, log_earnings := log(mean_earning)]
complete_banks_data[, lag_log_earnings := shift(log_earnings, type = "lag"), by = fips]


# 2. Dataset with Mortgages of all Financial Institutions ======================

allfin_data <- LOAD(dfinput = "allfin_data_joined")
# 2. All fin
setDT(allfin_data)

# Subset for the relevant period
allfin_data <- allfin_data[inrange(year, start, end)]

# # Only allow for observation with no missings
# complete_allfin_data <- allfin_data[complete.cases(allfin_data),]
# 
# # Get counties, which are observed over the whole time period
# check_cnty <- complete_allfin_data[, c("year", "fips")]
# # check_control <- check_cnty[year == 2010]
# 
# # filter <- complete_data[year == 2010]
# check_cnty <- check_cnty[, ones := 1]
# check_cnty <- dcast(check_cnty, fips ~ year, value.var = "ones", fill = 0)
# counties_full_obs <- check_cnty[rowSums(check_cnty[ , 2:ncol(check_cnty), with = FALSE]) == diff_year]
# 
# complete_allfin_data <- complete_allfin_data[fips %in% counties_full_obs$fips]

# uniqueN(complete_allfin_data$fips)

complete_allfin_data <- COMPLETEOBS(allfin_data, rowx = "fips", colx = "year")


# 4. Save both datasets
SAVE(dfx = complete_allfin_data, namex = "main_allfin_data")

SAVE(dfx = complete_banks_data, namex = "main_banks_data")


#### Extra 





########################## ENDE ###############################################+