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

# 0. Determine Periods of Dataset ==============================================

start <- 2002
end <- 2016
# diff_year <- end - start + 1


# 1. Dataset with Mortgages of Commercial Banks ================================

# Load merged data
banks_data <- LOAD(dfinput = "mp_transmission_merge")
setDT(banks_data)

# Subset for the relevant period
banks_data <- banks_data[inrange(year, start, end)]

# names(banks_data)
# 
# banks_data <- banks_data[, .(year, fips, state, lead_ln_loan_amount, d_median_all_pre,
#                              d_ffr_indicator, d_placebo_2004, d_placebo_2014, log_earnings,
#                              ur, d_top_bank, d_msa, total_amount_loan, ln_loan_amount,
#                              hhi, mean_hhi, d_hhi_max, d_hhi_max_pre, d_median_all,
#                              d_median_all_pre, d_marketdef_all, d_q70_all, ffr_mean, d_ffr_mean,
#                              d_ffr_last, cnty_pop, cnty_main_office, mean_earning, pop_density, mean_earning, mean_emp,
#                              share_emp, landarea_sqkm, emp_rate, delta_earnings)]

# Only keep counties that are observed over the whole period
complete_banks_data <- COMPLETEOBS(data = banks_data, rowx = "fips", colx = "year")

# 2. Implement First Differencing for all relevant variables ===================

# # Define the key variables to demean
# key_vars <- c("ln_loan_amount", "ln_wtd_loan_amount", "lead_ln_loan_amount", "lead_ln_wtd_loan_amount","mean_hhi", "cnty_pop", "mean_earning", "mean_emp", "ur")
# 
# # Calculate the year-specific mean for each variable and subtract it from the individual observation
# for (var in key_vars) {
#   # Calculate the mean for each year
#   complete_banks_data[, paste0("demeaned_", var) := get(var) - mean(get(var), na.rm = TRUE), by = year]
# }
uniqueN(banks_data$fips)
uniqueN(complete_banks_data$fips)

# 2. Save dataset ==============================================================

# Save dataset
# SAVE(dfx = complete_banks_data, namex = "main_banks_data")
SAVE(dfx = complete_banks_data, namex = MAINNAME)

########################## ENDE ###############################################+