# TARGET: Creating HHI and dummy variable for dividing counties into high-market concentration and low-market concentratoin 
# INDATA: sod_banks
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

# Load the Summary of Deposits for the period 1994 to 2020
sod <- LOAD(dfinput = "banks_sod", dfextension = ".rda")
setDT(sod)

# Select the relevant variables for creating HHI by county-level
sod <- sod[, .(year, fips, depsumbr, rssdid)]
setorder(sod, year, fips, rssdid)

# Create HHI by county
# Calculate the sum of deposit by year, financial instiution and fips
sod <- sod[, bank_cnty_dep := sum(depsumbr), by = .(rssdid, fips, year)]

# Calculate the sum of deposits by year and fips-code
sod <- sod[, cnty_tot_dep := sum(depsumbr), by = .(fips, year)]

# Calculate the market share and sqaure the value of it
sod <- sod[, bank_market_share := bank_cnty_dep / cnty_tot_dep * 100]
sod <- sod[, bank_market_share := ifelse(is.nan(bank_market_share), 0, bank_market_share)]
sod <- sod[, bank_market_share_sq := bank_market_share^2]

# Drop all duplicates
sod <- unique(sod, by = c("year", "fips", "rssdid"))
# test <- test[, check := sum(bank_market_share), by = .(fips, year)]
# duplicate_rows <- sod[duplicated(sod, by = c("year", "fips", "rssdid"))]

# Calculate the HHI
sod <- sod[, .(hhi = sum(bank_market_share_sq)), by = .(fips, year)]

sod_hhi <- sod[, .(mean_hhi = mean(hhi)), by = fips]

# Follow the strateg

# Calculate the market-share of one 
# Create turnover rate of branches for each year (sims_aquired_date)
# Create dummy variable for whether county lays in a Metropolitan Statistical Area or not (msabr)
# Create dummy whether county is has the main office or not (bkmo)


sod <- sod[, stcntybr := ifelse(nchar(stcntybr) == 4, paste0("0", stcntybr), "stcntybr") ]

# Restrict to the period 2000 to 2020
sod <- 
sod <- sod[year >= 2000]

 
test <- table(nchar(sod$stcntybr))



sod$stcntybr