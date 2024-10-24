# TARGET: Determine Market Concentration in counties + Create treatment/Control group + Create annual data for FFR
# INDATA: sod_banks
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

# 1. Summary of Deposits =======================================================

## 1.1 Load the Dataset ---------------------------------------------------------

# Load the Summary of Deposits for the period 1994 to 2020
sod <- LOAD(dfinput = "banks_sod", dfextension = ".rda")
setDT(sod)

DEBUBG <- F
if (DEBUG) {
  sod <- sod[specdesc == "mortgage_lending"]
}

# Select the relevant variables for creating HHI by county-level
sod <- sod[, .(year, fips, state, county, depsumbr, rssdid)]
setorder(sod, year, fips, rssdid)

# Exclude all irrelevant time periods
sod <-  sod[inrange(year, 2004 , 2017)]



## 1.2 Create HHI by county -----------------------------------------------------

# Calculate the sum of deposit by year, financial institution and fips
sod <- sod[, bank_cnty_dep := sum(depsumbr), by = .(rssdid, fips, year)]

# Calculate the sum of deposits by year and fips-code
sod <- sod[, cnty_tot_dep := sum(depsumbr), by = .(fips, year)]

# Calculate the market share and square the value of it. Additionally, substitute
# all NaN with 0, which were produced when only one bank is active in the whole county.
sod <- sod[, bank_market_share := bank_cnty_dep / cnty_tot_dep * 100]
sod <- sod[, bank_market_share := ifelse(is.nan(bank_market_share), 0, bank_market_share)]
sod <- sod[, bank_market_share_sq := bank_market_share^2]

# Drop all duplicates in order to create a dataset on bank-cnty-year level:
# One observation is equal to the deposits of one bank in one county in one year.
sod <- unique(sod, by = c("year", "fips", "rssdid"))

# Calculate the HHI for each county based on the deposits of one banks in one county in one year.
sod <- sod[, .(hhi = sum(bank_market_share_sq)), by = .(fips, year)]

# 1.3 Mean HHI for each county ---------------------------------------

# This section does two things:
# i) Creates and indicator for each county with an HHI of 10000
# ii) Creates a dataset with the mean HHI of each county over all periods

# Create dummy variable for counties with HHI of 10000. Counties with HHI 10000
# are associated with rural areas like counties in Alaska, Nebraska, South Dakota etc,
# There are 96 counties, which have this high market concentration over the whole period.

# Create vector with all counties of a HHI of 10000
sod_10000 <- sod[hhi == 10000]
sod_10000 <- unique(sod_10000, by = c("fips", "year"))

# Check, which counties have HHI of 1000 over the whole period
sod_10000 <- sod_10000[, ones := 1]
sod_matrix <- dcast(sod_10000, fips ~ year, value.var = "ones", fill = 0)
conc_10000 <- sod_matrix[rowSums(sod_matrix[ , 2:ncol(sod_matrix), with = FALSE] > 0) == 21]
cnty_10000 <- sod[sod$fips %in% conc_10000$fips]

# Create dummy variable d_hhi_10000, which contains all counties with a HHI of 
# 10000 over the whole period
sod <- sod[, d_hhi_10000 := ifelse(sod$fips %in% cnty_10000$fips, 1, 0)]

# Calculate the mean HHI for each county over the period 2004 to 2021
# Contains observations on county-level (no annual data!)
sod_hhi <- sod[, .(mean_hhi = mean(hhi)), by = fips]

# Create a dummy variable for each county that has a HHI of 10000 over all periods
sod_hhi <- sod_hhi[, d_hhi_10000 := ifelse(sod_hhi$fips %in% cnty_10000$fips, 1, 0)]

# Create a dataset equal to sod_hhi but with counties with HHI = 10000 on county-level
sod_hhi_rest <- sod_hhi[d_hhi_10000 == 0]

# Calculate the mean HHI for each county in the main SOD dataset
sod <- sod[, mean_hhi := mean(hhi), by = fips]

# 1.4 Create Treatment/Control Dummy Variables ----------------------------------

# Create different dummy variables that divide the dataset into treatment and 
# control group in the main SOD datase. Thereby, sod_hhi (mean hhi on county-level)
# is used to determine median, mean and q70 of the mean_hhi.

# Datasets with the following are resticted:
# all: Includes counties with hhi == 10000
# rest: Exldues counties with hhi == 10000

# a) Threshold: Median
# All Counties
median_hhi_all <- median(sod_hhi$mean_hhi)
sod <- sod[, d_median_all := ifelse(fips %in% sod_hhi[mean_hhi > median_hhi_all, fips], 1, 0)]

# Exclude Counties with HHI = 10000 over all periods
median_hhi_rest <- median(sod_hhi_rest$mean_hhi)
sod <- sod[, d_median_rest := ifelse(fips %in% sod_hhi_rest[mean_hhi > median_hhi_rest], 1, 0)]

# b) Threshold: Mean
# All Counties
mean_hhi_all <- mean(sod_hhi$mean_hhi)
sod <- sod[, d_mean_all := ifelse(fips %in% sod_hhi[mean_hhi > mean_hhi_all, fips], 1, 0)]

# Exclude Counties with HHI = 10000 over all periods
mean_hhi_rest <- mean(sod_hhi_rest$mean_hhi)
sod <- sod[, d_mean_rest := ifelse(fips %in% sod_hhi_rest[mean_hhi > mean_hhi_rest], 1, 0)]

# c) Threshold: Market Defintion of a highly-concentrated market (HHI > 2500)
# All counties
sod <- sod[, d_marketdef_all := ifelse(mean_hhi > 2500, 1, 0)]

# Exclude counties with HHI = 10000 over all periods
sod <-  sod[, d_marketdef_rest := ifelse((mean_hhi > 2500) & (d_hhi_10000 == 0), 1, 0)]

# d) Threshold: 70 percentile 
q70_hhi_all <- quantile(sod_hhi$mean_hhi, probs = 0.70)
sod <- sod[, d_q70_all := ifelse(fips %in% sod_hhi[mean_hhi > q70_hhi_all, fips], 1, 0)]

# Exclude Counties with HHI = 10000 over all periods
q70_hhi_rest <- quantile(sod_hhi$mean_hhi, probs = 0.70)
sod <- sod[, d_q70_rest := ifelse(fips %in% sod_hhi_rest[mean_hhi > q70_hhi_rest], 1, 0)]

# Save dataset
if (DEBUG) {
  SAVE(dfx = sod, name = "sod_mortgage")
}


if (!DEBUG) {
SAVE(dfx = sod, name = "SOD_final")
}

# 2. Federal Funds Rate ========================================================

# Load raw dataset of the federal funds rate
ffr_data <- LOAD(dfinput = "ffr")
setDT(ffr_data)

# Creating measures of the federal funds rate
# Method 1: Average FFR of each year
ffr_data <- ffr_data[, ffr_mean := mean(ffr), by = year]
ffr_data <- ffr_data[, d_ffr_mean := as.integer(ifelse( ffr_mean < 2, 1,0))]

# Method 2: Last FFR of each year
ffr_data <- ffr_data[, ffr_last := ffr[.N], by = .(year)]
ffr_data <- ffr_data[, d_ffr_last := as.integer(ifelse(ffr_last < 2, 1, 0))]

# Dummy Variable for before and after 2008:
# From the year 2007 to 8 the US experienced a drop in the FFR by 4.08 as the 
# Great Recession unfolded.
ffr_data <- ffr_data[, d_ffr_indicator := as.integer(ifelse(year >= 2008, 1, 0))]

# Restrict the data to the period 
ffr_data <- ffr_data[inrange(year, 2004, 2020)]

# Reduce dataset to yearly dataset
ffr_data <- unique(ffr_data, by = c("year"))

# Save dataset
SAVE(dfx = ffr_data, namex = "ffr_annual")

# 3. Combine Federal Funds Rate & SOD ==========================================

# Combine SOD dataset and FFR dataset by year
treatment_data <- left_join(sod, ffr_data, by = c("year"))

# Save dataset
if (!DEBUG) {
SAVE(treatment_data, namex = MAINNAME)
}

if (DEBUG) {
  SAVE(treatment_data, namex = paste0(MAINNAME, "_mortgage"))
}
########################## ENDE ###############################################+



