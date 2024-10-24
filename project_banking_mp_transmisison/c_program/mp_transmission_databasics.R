# TARGET: Reading-In all Raw Datasets  & Perform basic data cleaning
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

# 1. Import US FIPS County Codes ===============================================
# FIPS Code is the unique identifier of a county and consist of a 5-digit number.
# The first two digits is the state code, while the last three digits are county-code.

# This data is used to verify fips code in the Summary of Deposits
# SOD contians fips code that do not exist.
fips_data <- read_xls(paste0(A, "c_census/", "US_FIPS_Codes.xls"), skip = 1)
setDT(fips_data)
colnames(fips_data) <-  c("state_name", "county_name", "state_code", "county_code")

# Creates fips code by combining state and county code
fips_data <- fips_data[, fips := paste0(state_code, county_code)]
SAVE(dfx = fips_data, namex = "fips_data")

rm(fips_data)

# 2. Importing Summary of Deposits (by FDIC) for the years 2018 to 2024 ========
## 2.1 Import Files ------------------------------------------------------------

# list all raw sod files in h_sod_direct
files_sod <- list.files(paste0(A, "h_sod_direct/"))
files_sod <- files_sod[grepl("\\csv$",files_sod)]
years <- as.numeric(str_extract(files_sod, "\\d{4}"))
files_sod <- files_sod[years >= 1994 & years <= 2017]

# files_sod <-  files_sod[26]

# Loop for importing all sod datasets from 1994 to 2024 
for (i in files_sod) {
  print(paste0("Iteration Status: ", i))
  file <- suppressMessages(read_csv(paste0(A, "h_sod_direct/", i), , col_types = cols(.default = "c")))
  year <- str_sub(i, start = 5, end = 8)
  SAVE(dfx = file, namex = paste0("sod_", year))
  rm(file, year)
}

# test <- fread(paste0(A, "h_sod_direct/", files_sod))

## 2.2 Append all raw SOD files ------------------------------------------------

# Loop over all SOD datasets in order to create one large data frame for 
# the years 1994 to 2024

# Create vector with all names of SOD datasets
sod_temp <- list.files(paste0(TEMP))
sod_temp <- sod_temp[str_detect(sod_temp, "^sod")]
years <- as.numeric(str_extract(sod_temp, "\\d{4}"))
sod_temp <- sod_temp[years >= 1994 & years <= 2017]

DEBUG <- F
if (DEBUG) {
  sod_temp <- sod_temp[years >= 2004 & years <= 2014] 
}

# Create empty list in which all SOD datasets will be saved
combined_sod <- list()

# Save all datasets within a list object
for (file in sod_temp) {
  # Read the .rda file
  load(paste0(TEMP, "/", file))
  
  # Extract df name of file
  df_name <- str_sub(file, end = -5)
  
  # Retrieve the df by string
  loaded_data <- get(df_name)
  
  # Combine Data
  combined_sod <- append(combined_sod, list(loaded_data))
  
  # Avoid littering the global environment
  rm(loaded_data)
}

# Remove all single datasets in order to avoid littering the global enivronment
rm(list = str_sub(sod_temp, end = -5))

# Rename the variables to lower case variables
names_upper <- names(combined_sod[[1]])
names_lower <- str_to_lower(names_upper)
combined_sod <-  LOWERCASEVAR(combined_sod, names_lower)

# Combine all data frames within the list to one large data frame
combined_sod <-  bind_rows(combined_sod)

# Change object from data.frame to data.tabel for efficieny reasons
setDT(combined_sod)

# Label all variables with their original variables in upper case
for (i in seq_along(combined_sod)) {
  attr(combined_sod[[i]], "label") <- names_upper[i]
}

## 2.3 Basic Data Cleaning -----------------------------------------------------

# Select the variables of interest
combined_sod <- combined_sod[, .(year, stcntybr, uninumbr, depsumbr, insured, 
                                 specdesc, msabr, bkmo, stnumbr, cntynumb, rssdid)]

# Clean variables depsumbr and sims_aquired_date from all special characters (This part can be potentially deleted)
combined_sod <- combined_sod[, depsumbr := gsub(",", "", depsumbr)]
# combined_sod <- combined_sod[, sims_acquired_date := 
#                                ifelse(nchar(sims_acquired_date) > 1,
#                                       substr(sims_acquired_date, nchar(sims_acquired_date) - 3, 
#                                              nchar(sims_acquired_date)), 
#                                       sims_acquired_date)]

# Format the relevant variables to integers
columns_to_convert <- c("year", "depsumbr", "msabr", "bkmo")
combined_sod <- combined_sod[, (columns_to_convert) := lapply(.SD, as.integer), .SDcols = columns_to_convert]

# Cleaning the variable specdesc from special characters
combined_sod <- combined_sod[, specdesc := str_to_lower(specdesc)]
combined_sod <- combined_sod[, specdesc := gsub(" ", "_", specdesc)]
combined_sod <- combined_sod[, specdesc := gsub(">", "greater", specdesc)]
combined_sod <- combined_sod[, specdesc := gsub("<", "lower", specdesc)]
combined_sod <- combined_sod[, specdesc := gsub("-", "_", specdesc)]
combined_sod <- combined_sod[, specdesc := gsub("\\$", "", specdesc)]

# Restrict the dataset the year 2000 to 2020
# combined_sod <- combined_sod[between(year, 2000, 2017)]

# Create fips-code by combining the state and county code (USE FIPSCREATOR!)
combined_sod <- combined_sod[stnumbr != "" & cntynumb != ""]
combined_sod <- combined_sod[, stnumbr := ifelse(nchar(stnumbr) == 1, paste0("0", stnumbr), stnumbr)]
combined_sod <- combined_sod[, cntynumb := ifelse(nchar(cntynumb) == 2, paste0("0", cntynumb), cntynumb)]
combined_sod <- combined_sod[, cntynumb := ifelse(nchar(cntynumb) == 1, paste0("00", cntynumb), cntynumb)]
combined_sod <- combined_sod[, fips := paste0(stnumbr, cntynumb)]

test <- combined_sod
combined_sod <- test

# Excluding the following US territories as they are not relevant for the analysis: 
# Puerto Rico (72), US Virgin Islands (78), American Samoa (60), 
# Northern Marian Islands (69), U.S. Minor Outlying Islands (74), Guam (66)
combined_sod <- combined_sod[!stnumbr %in% c("72", "66", "60", "69", "74", "78")]

# Validate the quality of the fips code by comparing the available fips code in
# the SOD with a list of all fips code in the US from the US Census Bureau (MDR Education)
# -> The SOD contains fips-codes that are not existing. The observations with the invalid fips-codes are excluded. 
load(paste0(TEMP, "/", "fips_data.rda"))
notvalid <- setdiff(fips_data$fips, combined_sod$fips)
combined_sod <- combined_sod[!(fips %in% notvalid)]

# Restrict the dataset the year 2000 to 2017
combined_sod <- combined_sod[year >= 2000 & year <= 2017]

# Only fips-codes, which are observed over the period of 2000 to 2020 are included
# in the dataset.
# Collapse data to county-year level
check_obs <- combined_sod[, .(fips, year, rssdid)]
check_obs <- check_obs |> distinct(fips, year, rssdid)
check_obs <- check_obs |> distinct(fips, year)

# Irrelevant warning that is supressed. Warning is related to the data.table package.
check_obs <- suppressWarnings(check_obs[, ones := 1])

# Check if rows have duplicates 
duplicated_rows <- any(duplicated(check_obs[, .(fips, year)]))
check_obs[!duplicated(check_obs, by = c("year", "fips"))]

# Determine the counties that are observed over all periods and filter for those counties (USE COMPLETEOBS)
county_matrix <- dcast(check_obs, fips ~ year, value.var = "ones", fill = 0)
setDT(county_matrix)
counties_full_obs <- county_matrix[rowSums(county_matrix[ , 2:ncol(county_matrix), with = FALSE] > 0) == 18]
uniqueN(combined_sod)
test1 <- combined_sod[fips %in% counties_full_obs$fips]
uniqueN(combined_sod)
test2 <- COMPLETEOBS(data = combined_sod, rowx = "fips", colx = "year")

identical(test1, test2)
# Rename variables and sort columns
setnames(combined_sod, old = c("stnumbr", "cntynumb"), new = c("state", "county"))
setcolorder(combined_sod,c("year", "fips", "state"))
combined_sod[, stcntybr := NULL]

# 
# if (DEBUG) {
#   setDT(combined_sod)
#   sod_mortgage <- combined_sod[SPECDESC == "MORTGAGE LENDING"]
#   sod_mortgage <- sod_mortgage["YEAR", ]
# }
## 2.4 Save datasets  ----------------------------------------------------------

# Create two different datasets
# i. Only Commercial banks
sod_banks <- combined_sod[insured == "CB"]
sod_banks <- sod_banks[, insured := NULL]

# ii. All available financial institutions
sod_all <- combined_sod
sod_all[, insured := NULL]

# Save Combined (raw)NULL# Save Combined (raw) SOD dataset
SAVE(dfx = sod_banks, namex = "banks_sod")
SAVE(dfx = sod_all, namex =  "all_sod")

# Clear Global Environment
rm(list = c("combined_sod", "sod_banks", "sod_all", "fips_data"))

# Clear unused memory
gc()

# 3. Import Data from the Housing Mortgage Disclosure Act ======================

# The Home Mortgage Disclosure Act (HMDA) contains information on every originated 
# mortgage in the United States. This allows to later create a dataset on mortgage
# loan amount for each county. Thereby, each financial institution is obligated to 
# hand-in information on the mortgage loan, which can be identified by the 
# respondent ID.

##  3.1 List Panel and Loan Application Records Files (LRA) --------------------   
lra_files <- list.files(paste0(A, "a_hdma_lra/"))
lra_files <- lra_files[gsub("[^0-9]", "", lra_files) %in% c(2000:2017)]
panel_files <- list.files(paste0(A, "b_hdma_panel/"))
panel_files <- panel_files[gsub("[^0-9]", "", panel_files) %in% c(2000:2017)]
if (DEBUG) {
lra_files <- lra_files[1]
panel_files <- panel_files[1]
}

start_time <- Sys.time()


# # 3.2 Import LRA files -------------------------------------------------------

# This loop import all LRA files
lapply(lra_files, function(file) {
  
  # Column name depend on the years of the submission of the LRA as the program
  # has undergone several changes over time.
  if (as.integer(gsub("[^0-9]", "", file) %in% c(2000:2006))) {
    lra_columns <- c("activity_year", "respondent_id", "agency_code", "loan_amount", "state_code", "county_code")
  } else if (as.integer(gsub("[^0-9]", "", file) %in% c(2007:2017))) {
    lra_columns <- c("as_of_year", "respondent_id", "agency_code", "loan_amount_000s", "state_code", "county_code")
  } 
  
  # Load all the raw LRA data on respondent-ID level (contains the information 
  # on each handed out loan). In order to reduce processing time, only the 
  # relevant variables in lra_columns are imported.
  data <- fread(paste0(A, "a_hdma_lra/", file), colClasses = "character", select = lra_columns)
  
  # Standardize the column names
  if (as.integer(gsub("[^0-9]", "", file) %in% c(2007:2017))){
    setnames(data,
             old = c("as_of_year", "respondent_id", "agency_code", "loan_amount_000s", "state_code", "county_code"),
             new = c("activity_year", "respondent_id", "agency_code", "loan_amount", "state_code", "county_code"))
  }
  
  # Save the raw lra dataset
  SAVE(dfx = data, namex = paste0("hmda_lra_", gsub("[^0-9]", "", file)), pattdir = TEMP)
  print(paste0("LRA: Successful import of the year ", gsub("[^0-9]", "", file)))
  
  # Free unused memory and clear object from the  global environment.
  gc()
  rm(data)
})

## 3.3 Import Panel files ------------------------------------------------------

# Import Panel data and retrieve all unique observation in order to
# get the all unique respondent_id and agency_code combinations.
# This is later needed to identify Commercial Banks that hand out Mortgages with 
# the help of the variable "other_lender_code".

# This loop imports panel data 
purrr::walk(panel_files, function(file) {
   # file <- panel_files[12]
  year <- as.integer(gsub("[^0-9]", "", file))
  
  # Check which year of the data is imported and adjust the column names.
  if (year == 2007) {
    # Reading in the file from 2009 needed a manual fix by skipping the last observation.
    # Otherwise, it was not possible to read the file independently of the import 
    # package used. 
    data <- fread(paste0(A, "b_hdma_panel/", file), nrows = 8608, colClasses = "character")
  } else {
    data <- fread(paste0(A, "b_hdma_panel/", file), colClasses = "character")
  }
  
  # Standardize the column names of the panel
  if (year %in% c(2007:2009)) {
    setnames(data, 
             old = c("Respondent Identification Number", "Agency Code", "Other Lender Code"), 
             new = c("respondent_id", "agency_code", "other_lender_code"))
  } else if (year %in% c(2010:2017)) {
    setnames(data, 
             old = c("Respondent ID", "Agency Code", "Other Lender Code"), 
             new = c("respondent_id", "agency_code", "other_lender_code"))
  }
  
  # Select the relevant variables
  data <- data[, c("respondent_id", "agency_code", "other_lender_code")]
  # get rid off any duplicants
  data <- unique(data, by = c("respondent_id", "agency_code"))
  
  # Save the panel dataset
  SAVE(dfx = data, namex = paste0("hmda_panel_", year), pattdir = TEMP)
  
  # Update on iteration process
  print(paste0("Panel: Successful import of the year ", year))
  
  # Remove objects from the global environment and clean memory
  rm(data)
  gc()
})

## 3.4 Merging the Panel and LRA dataset with each other -----------------------

#' In the next step, the final HMDA dataset for each year is produced. 
#' These datasets are all on respondent-ID level, where each respondent can have
#' multiple observations as they have to report every single originated loan.

# Merge both Panel and LRA based on year
purrr::walk(2000:2017, function(i) {
  
  # Determine the imported LRA and Panel dependent on the year
  lra <- paste0("hmda_lra_", i)
  panel <- paste0("hmda_panel_", i)
  
  # Import the HMDA datasets of the current iteration
  load(file = paste0(TEMP, "/", lra, ".rda"))
  load(file = paste0(TEMP, "/", panel, ".rda"))
  
  # Retrieve the object based on the vector lra and panel
  dflra <- get(lra)
  dfpanel <- get(panel)
  
  # Performs a left_join as we want to keep the observation level of the LRA
  # and want to be able to identify the lender code of each respondents.  
  main <- left_join(dflra, dfpanel, by = c("respondent_id", "agency_code"))
  
  # Check if LRA and main dataset have same length: Test to make sure that 
  # no additional observations are added. (Double Check)
  if (nrow(main) == nrow(dflra)) {
    message("Merge successful for year: ", i)
  } else {
    warning("Merging issues for year ", i, ": LRA observations: ", nrow(get("lra")),
            ", Merged observations: ", nrow(main))
  }
  
  # Save the merged file
  SAVE(dfx = main, namex = paste0("hmda_merge_", i))
  
  # Clean up memory and Free unused memory
  rm(list = c(paste0("hmda_lra_", i), paste0("hmda_panel_", i), "main", "dflra", "dfpanel"))
  gc()
})

end_time <- Sys.time()
print(end_time - start_time)

# 4. Federal Funds Rate ========================================================

# Import raw monthly data on the Federal Funds Rate
ffr_data <- read.csv(paste0(A, "d_fred/", "FEDFUNDS.csv"))
setDT(ffr_data)

# Rename columns
setnames(ffr_data, old = c("date", "ffr"))

# Create year variable
ffr_data <- ffr_data[, year := as.integer(substr(date, 1, 4))]

# Format date variable
ffr_data <- ffr_data[, date := as.Date(date)]

# Save Data
SAVE(dfx = ffr_data, name = "ffr")

# Remove from global environment
rm(list = c("ffr_data"))

# 5. County-Level Population ===================================================

## 5.1 Import Raw Datasets from U.S. Census ------------------------------------

# Population Estimates for the years 2000 to 2010
pop_00 <- read_csv(paste0(A, "f_us_census_bureau/", "co-est00int-tot.csv"), col_types = cols(.default = "c"))
setDT(pop_00)

# Population Estinates for the years 2010 t0 2019
pop_10 <- read_csv(paste0(A, "f_us_census_bureau/", "co-est2019-alldata.csv"), col_types = cols(.default = "c"))
setDT(pop_10)

# Population Estimates for the years 2020to 2023
pop_20 <- read_csv(paste0(A, "f_us_census_bureau/", "co-est2023-alldata.csv"), col_types = cols(.default = "c"))
setDT(pop_20)

## 5.2 Basic Data Manipulation of the Population Datasets ----------------------

# Create list for the population datasets
pop_list <- list(pop_00 = pop_00, pop_10 = pop_10, pop_20 = pop_20)

# Apply the same operations to each element in the list
pop_list <- lapply(pop_list, function(data) {
  
  # Create fips code
  data <- FIPSCREATOR(data, state_col = "STATE", county_col = "COUNTY")
  
  # Select relevant variables
  data <- EXTRACTVAR(data = data, state_col = "STATE", county_col = "fips", indicator = "POPESTIMATE")
  
  # Reshape into long format
  data <- RESHAPEPOP(data = data)
  
  # All variables to small letter
  colnames(data) <- str_to_lower(names(data))
  
  return(data)
})

# Delete the year 2010 in the population datset for the period 2000 to 2010
pop_list[[1]] <- subset(pop_list[[1]], year != "2010")

# Create population dataset over all periods for county and states
pop_data <- bind_rows(pop_list)
setDT(pop_data)

# Format as integer
pop_data <- pop_data[, year := as.integer(year)]
pop_data <- pop_data[, population := as.integer(population)]

# Restrict to the necessary year
pop_data <- pop_data[inrange(year, 2000, 2020)]

# Extract all state observations
pop_state_data <- pop_data[
  substr(get("fips"), nchar(get("fips")) - 2, nchar(get("fips"))) == "000", 
  .(fips, state, year, population)
]
pop_state_data <- pop_state_data[, c("state", "year", "population")]

# Extract all county population observatinons
pop_cnty_data <- pop_data[
  substr(get("fips"), nchar(get("fips")) - 2, nchar(get("fips"))) != "000", 
  .(fips, state, year, population)
]

# Adjust names for clarification
setnames(pop_cnty_data, old = c("population"), new = c("cnty_pop"))
setnames(pop_state_data, old = c("population"), new = c("state_pop"))

# SAVE
SAVE(dfx = pop_cnty_data, namex = "pop_cnty")
SAVE(dfx = pop_state_data, namex = "pop_state")

# 6. Import Unemployment Data ==================================================

# Import unemployment data on county level from the economic research service
unemp_data <- read_xlsx(paste0(A, "e_economic_research_service/", "Unemployment.xlsx"), skip = 4, col_types = "text")
setDT(unemp_data)

# Extract Unemployment Rates for all counties
unemp_data <- EXTRACTVAR(data = unemp_data, state_col = "State", county_col = "FIPS_Code", indicator = "Unemployment_rate")

# Standardize variable names
colnames(unemp_data) <- str_to_lower(names(unemp_data))
setnames(unemp_data, old = c("fips_code"), new = c("fips"))

# Shift data from wide to long format
unemp_data <- melt(
  unemp_data,
  measure.vars = patterns("unemployment_rate_"),
  variable.name = "year",
  value.name = "ur"
)

# Clean the year variable
unemp_data <- unemp_data[, year := sub("unemployment_rate_", "", year)]

# Delete observation for states and US Totals
unemp_data <- unemp_data[fips != "00000"]
unemp_data <- unemp_data[
  substr(get("fips"), nchar(get("fips")) - 2, nchar(get("fips"))) != "000", 
  .(fips, state, year, ur)
]

# format variables
unemp_data <- unemp_data[, year := as.integer(year)]
unemp_data <- unemp_data[, ur := round(as.double(ur), 2)]

# Select relevant variable
unemp_data <- unemp_data[, c("fips", "year", "ur")]

# Save
SAVE(dfx = unemp_data, namex = "ur_cnty")


# 7. Import of Average Earning per county ======================================

# Import data on average earnings per county from the Quarterly Workforce Indicator
earnings_data <- read_csv(paste0(A, "g_qwi/", "qwi_e14db0de913c427aa12de971a73eb389.csv"), col_types = cols(.default = "c"))
setDT(earnings_data)

# Select relevant variables
earnings_data <- earnings_data[, c("geography", "year", "quarter", "EarnBeg", "Emp")]

# rename variables
setnames(earnings_data, old = names(earnings_data), new = c("fips", "year", "quarter", "avg_monthly_earn", "tot_emp"))

# Filter for only county observation and excluding average earnings for the US
# and state totals
earnings_data <- earnings_data[nchar(fips) == 5]

# Extract state code and remove Puerto Rico from the dataset
earnings_data <- earnings_data[, state := substr(fips, 1, 2)]
earnings_data <- earnings_data[!(state %in% c("72", "66", "60", "69", "74", "78"))]

# earnings_data <- earnings_data[, mean_earnings := base::mean(avg_monthly_earn), by = .(fips, year)]
# earnings_data <- earnings_data[, quarter := as.integer(quarter)]
# earnings_data <- earnings_data[, quarter_date := as.Date(paste0(year, "-", (quarter - 1) * 3 + 1, "-01"), format = "%Y-%m-%d")]

# Calculate the mean of the avergae monthly earning of a quarter in order to 
# an annual measure for the average monthly earnings within a county and a year
earnings_data <-  earnings_data[, avg_monthly_earn := as.integer(avg_monthly_earn)]
earnings_data <-  earnings_data[, mean_earning := base::mean(avg_monthly_earn, na.rm = TRUE), by = .(fips, year)]

# Calculate the mean of the total employment of a quarter in order to 
# an annual measure for the total employment  within a county and a year
earnings_data <-  earnings_data[, tot_emp := as.integer(tot_emp)]
earnings_data <-  earnings_data[, mean_emp := base::mean(tot_emp, na.rm = TRUE), by = .(fips, year)]
# test <- earnings_data[, mean_emp := zoo::na.approx(mean_emp, x = year, na.rm = TRUE), by = fips]

# Collapse the data to county and year level
earnings_data <- unique(earnings_data, by = c("fips", "year"))

# Select relevant variables
earnings_data <- earnings_data[, c("fips", "year", "mean_earning", "mean_emp")]
earnings_data <- earnings_data[, year := as.integer(year)]

# SAVE
SAVE(dfx = earnings_data, namex = "qwi_earnings")


########################## ENDE ###############################################+