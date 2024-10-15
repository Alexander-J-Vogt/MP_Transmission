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

# Check identical observation
sod <- sod[, check_identical := bank_cnty_dep == cnty_tot_dep]
sod[check_identical == T]
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
# sod <- sod[, check := hhi == 10000]

# Create dummy variable for counties with HHI of 10000. Counties with HHI 10000
# are associated with rural areas like counties in Alaska, Nebraska, SOuth Dakota etc,
# There are 96 counties, which have this high market concentration over the whole period.
sod_10000 <- sod[hhi == 10000]
sod_10000 <- unique(sod_10000, by = c("fips", "year"))
sod_10000 <- sod_10000[, ones := 1]
sod_matrix <- dcast(sod_10000, fips ~ year, value.var = "ones", fill = 0)
conc_10000 <- sod_matrix[rowSums(sod_matrix[ , 2:ncol(sod_matrix), with = FALSE] > 0) == 21]
cnty_10000 <- sod[sod$fips %in% conc_10000$fips]
sod <- sod[, d_hhi_10000 := ifelse(sod$fips %in% cnty_10000$fips, 1, 0)]

# Calculate the mean HHI for each county for the period 2000 to 2021; based
# on this mean the counties are divided into treatment and control group
sod_hhi <- sod[, .(mean_hhi = mean(hhi)), by = fips]
sod_hhi <- sod[, d_hhi_10000 := ifelse(sod$fips %in% cnty_10000$fips, 1, 0)]

# Create different dummy variables for treatment and control group in the main
# dataset "sod" with the help of sod_hhi
# all: Includes counties with hhi == 10000
# rest: Exldues counties with hhi == 10000
# 1. Dummy variable with median as threshold
median_hhi_all <- median(sod_hhi$mean_hhi)
sod <- sod[, d_median_all := ifelse(fips %in% sod_hhi[mean_hhi > median_hhi_all, fips], 1, 0)]

median_hhi_rest <- median(sod_hhi[fips conc_10000$fips ]$mean_hhi)
median_hhi_rest <- median(sod_hhi[sod_10000 == 0])

cnty_median_all <- sod_hhi[, d_median_all := ifelse(mean_hhi > median(mean_hhi), 1, 0)]
cnty_median_all <- cnty_median_all[d_median_all == 1]
tset <- sod[, d_median_all := ifelse(fips %in% cnty_median_all$fips, 1, 0) ]
sod <-  d
sod <- sod[, d_median_rest := ifelse((hhi > median(hhi)) & (d_hhi_10000 == 0), 1, 0 )]

# 2. Dummy variable with mean as threshold
# Calculate the mean of hhi over time in order to identify treatment and control group
graph_sod <- sod_hhi
ggplot(data = graph_sod, aes(x = mean_hhi)) +
  geom_density(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Mean HHI", x = "Mean HHI", y = "Frequency") +
  theme_minimal()

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