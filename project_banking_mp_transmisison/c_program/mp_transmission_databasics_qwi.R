# TARGET: Import QWI 
# INDATA: qwi_e14db0de913c427aa12de971a73eb389.csv
# OUTDATA/ OUTPUT: mp_transmission_databasics_qwi

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


# 1. Import of Average Earning per county ======================================

# Import data on average earnings per county from the Quarterly Workforce Indicator
earnings_data <- read_csv(paste0(A, "g_qwi/", "qwi_e14db0de913c427aa12de971a73eb389.csv"), col_types = cols(.default = "c"))
pop_cnty_data <- LOAD(dfinput = "mp_transmission_databasics_pop")
setDT(earnings_data)
setDT(pop_cnty_data)

# Select relevant variables
earnings_data <- earnings_data[, c("geography", "year", "quarter", "EarnBeg", "Emp")]

# Rename variables
setnames(earnings_data, old = names(earnings_data), new = c("fips", "year", "quarter", "avg_monthly_earn", "tot_emp"))

# Filter for only county observation and excluding average earnings for the US
# and state totals
earnings_data <- earnings_data[nchar(fips) == 5]

# Extract state code and remove Puerto Rico from the dataset
earnings_data <- earnings_data[, state := substr(fips, 1, 2)]
earnings_data <- earnings_data[!(state %in% c("72", "66", "60", "69", "74", "78"))]

# Calculate the mean of the avergae monthly earning of a quarter in order to 
# an annual measure for the average monthly earnings within a county and a year
earnings_data <- earnings_data[, avg_monthly_earn := as.integer(avg_monthly_earn)]
earnings_data <- earnings_data[, mean_earning := base::mean(avg_monthly_earn, na.rm = TRUE), by = .(fips, year)]

# Calculate the mean of the total employment of a quarter in order to 
# an annual measure for the total employment  within a county and a year
earnings_data <- earnings_data[, tot_emp := as.integer(tot_emp)]
earnings_data <- earnings_data[, mean_emp := base::mean(tot_emp, na.rm = TRUE), by = .(fips, year)]

# Collapse the data to county and year level
earnings_data <- unique(earnings_data, by = c("fips", "year"))

# Select relevant variables
earnings_data <- earnings_data[, c("fips", "year", "mean_earning", "mean_emp")]
earnings_data <- earnings_data[, year := as.integer(year)]

# Merge Population Data to earnings_data
earnings_data <- merge(earnings_data, pop_cnty_data, by = c("fips", "year"))

# Calculate the share of emplyoment over total county population
earnings_data <- earnings_data[, share_emp := mean_emp / cnty_pop]
earnings_data <- earnings_data[, c("state", "cnty_pop") := NULL]

# SAVE
SAVE(dfx = earnings_data, namex = MAINNAME)

####################################### END ###################################+