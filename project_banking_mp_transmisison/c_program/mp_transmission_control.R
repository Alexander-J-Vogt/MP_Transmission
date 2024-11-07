# TARGET: Creating a Main dataset for all control variables from different sources
# INDATA: banks_sod, pop_cnty, ur_cnty, qwi_earnings, controls_sod
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

# 1. Summary of Deposits - Control Variables ===================================

# Create control variables based on variables available in the SOD

# Load the Summary of Deposits for the period 1994 to 2020
# sod <- LOAD(dfinput = "banks_sod", dfextension = ".rda")
sod <- LOAD(dfinput = "mp_transmission_databasics_sod", dfextension = ".rda")
setDT(sod)

# Restrict to the relevant variables
sod <- sod[, .(year, fips, msabr, bkmo)]

# Number of Main offices in a county:
# Create dummy whether county is has the main office or not (bkmo)
sod <-  sod[, cnty_main_office := sum(bkmo), by = .(fips, year)]

# Collapse by year and fips 
sod <- unique(sod, by = c("year", "fips"))

# Indicator whether a county lays in a Metropolitan Statistical Area (MSA) or not:
# MSA tells whether an areas has than 50000 inhabitants
# Could account for the fact that a different economic dynamic is present compared
# to non-MSA and the amount of loans handed-out is different compared to a areas
# with less than 50000.
sod <- sod[, d_msa := as.integer(ifelse(msabr > 0, 1, 0))]

# Select relevant variables
sod <-  sod[, c("year", "fips", "cnty_main_office", "d_msa")]

# SAVE
SAVE(dfx = sod, name = "controls_sod")

# clear global environment
rm(list = c("sod"))

# 2. Creating a variable for firmsize

# load dataset
raw_sod <- LOAD(dfinput = "raw_sod")
setDT(raw_sod)

# Market power of commercial banks
top_banks <- raw_sod[, .(depsumbank = sum(depsumcnty)), by = .(year, rssdid)]
top_banks <- top_banks[, tot_marketvalue_yearly  := as.numeric(sum(depsumbank)), by = year]
top_banks <- top_banks[, marketshare_yearly := depsumbank / tot_marketvalue_yearly]

# Filter data
# raw_sod <- raw_sod[marketshare_yearly > 0.01]

top_banks <- top_banks |> 
  group_by(year) |>
  arrange(desc(marketshare_yearly)) |> 
  slice_head(n = 5) |>
  mutate(d_top_bank = 1) |>
  ungroup() |> 
  select(year, rssdid, d_top_bank)

# marketshare <- top_banks |>
#   group_by(year) |> 
#   mutate(tot_market_share = sum(marketshare_yearly))

raw_sod <- raw_sod |> 
  left_join(top_banks, by = c("year", "rssdid")) |>
  mutate(d_top_bank = ifelse(is.na(d_top_bank), 0, d_top_bank))

raw_sod <- raw_sod |>
  group_by(year, fips) |>
  mutate(nr_top_bank = sum(d_top_bank)) |> 
  ungroup() |> 
  distinct(year, fips, .keep_all = TRUE)
  


# 2. Creating Control Dataset ==================================================

# Import Population County dataset
# pop_cnty <- LOAD(dfinput = "pop_cnty")
pop_cnty <- LOAD(dfinput = "mp_transmission_databasics_pop")

# Import Population State dataset
pop_state <- LOAD(dfinput = "pop_state")

# Import Unemployment Rate
# ur_cnty <- LOAD(dfinput = "ur_cnty")
ur_cnty <- LOAD(dfinput = "mp_transmission_databasics_ur")

# Import Average Earnings Data
# qwi_earnings <- LOAD(dfinput = "qwi_earnings")
qwi_earnings <- LOAD(dfinput = "mp_transmission_databasics_qwi")

# Import Controls from sod
controls_sod <- LOAD(dfinput = "controls_sod")

# Population density
# pop_density <- LOAD(dfinput = "landarea_data")
pop_density <- LOAD(dfinput = "mp_transmission_databasics_landarea")

# Combining datasets by county and year
merged_data <- left_join(pop_cnty, controls_sod, by = c("fips", "year"))
merged_data <- left_join(merged_data, qwi_earnings, by = c("fips", "year"))
merged_data <- left_join(merged_data, ur_cnty, by = c("fips", "year"))
merged_data <- left_join(merged_data, pop_density, by = "fips")

# 3. Variable Creation =========================================================

# Creating share of employment in each county and year
setDT(merged_data)
merged_data[, emp_rate := mean_emp / cnty_pop]

# Calculate population desnity of a county
merged_data <- merged_data[, pop_density := cnty_pop / landarea_sqkm]

# Creating log emp
merged_data <- merged_data[, log_emp := log(mean_emp)]

# Create lagged variables
lagged_var <- c("cnty_pop", "mean_earning", "mean_emp", "ur")
for (i in lagged_var) {
  merged_data[, paste0("lag_", i) := shift(get(i), type = "lag"), by = fips]
}

# Calculate the change in earnings
merged_data[, delta_earnings := mean_earning - shift(mean_earning, type = "lag")]

# Creating log mean_earnings in order to get normally distributed variables
merged_data[, log_earnings := log(mean_earning)]
merged_data[, lag_log_earnings := shift(log_earnings, type = "lag"), by = fips]

# 4. Saving ====================================================================

# save dataset
SAVE(dfx = merged_data)

########################## ENDE ###############################################+