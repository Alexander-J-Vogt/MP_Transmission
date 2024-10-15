# TARGET: Reading 
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
# The first two digits is the state code, while the last three digist are county-code.

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

# Loop for importing all sod datasets from 1994 to 2024 
for (i in files_sod) {
  print(paste0("Iteration Status: ", i))
  file <- suppressMessages(read_csv(paste0(A, "h_sod_direct/", i), , col_types = cols(.default = "c")))
  year <- str_sub(i, start = 5, end = 8)
  SAVE(dfx = file, namex = paste0("sod_", year))
  rm(file, year)
}

## 2.2 Append all raw SOD files ------------------------------------------------

# Loop over all SOD datasets in order to create one large data frame for 
# the years 1994 to 2024

# Create vector with all names of SOD datasets
sod_temp <- list.files(paste0(TEMP))
sod_temp <- sod_temp[str_detect(sod_temp, "^sod")]

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
                                 specdesc, sims_acquired_date, msabr, bkmo,
                                 stnumbr, cntynumb, rssdid)]

# Clean variables depsumbr and sims_aquired_date from all special characters
combined_sod <- combined_sod[, depsumbr := gsub(",", "", depsumbr)]
combined_sod <- combined_sod[, sims_acquired_date := 
                               ifelse(nchar(sims_acquired_date) > 1,
                                      substr(sims_acquired_date, nchar(sims_acquired_date) - 3, 
                                             nchar(sims_acquired_date)), 
                                      sims_acquired_date)]

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
combined_sod <- combined_sod[between(year, 2000, 2020)]

# Create fips-code by combining the state and county code
combined_sod <- combined_sod[stnumbr != "" & cntynumb != ""]
combined_sod <- combined_sod[, stnumbr := ifelse(nchar(stnumbr) == 1, paste0("0", stnumbr), stnumbr)]
combined_sod <- combined_sod[, cntynumb := ifelse(nchar(cntynumb) == 2, paste0("0", cntynumb), cntynumb)]
combined_sod <- combined_sod[, cntynumb := ifelse(nchar(cntynumb) == 1, paste0("00", cntynumb), cntynumb)]
combined_sod <- combined_sod[, fips := paste0(stnumbr, cntynumb)]

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

# Only fips-codes, which are observed over the period of 2000 to 2020 are included
# in the dataset.
# Collapse data to county-year level
check_obs <- combined_sod[, .(fips, year, rssdid)]
check_obs <- check_obs |> distinct(fips, year, rssdid)
check_obs <- check_obs |> distinct(fips, year)

# Irrelevant warning that is supresed. Warning is related to the data.table package.
check_obs <- suppressWarnings(check_obs[, ones := 1])

# Check if rows have duplicates 
# duplicated_rows <- any(duplicated(check_obs[, .(fips, year)]))
# check_obs[!duplicated(check_obs, by = c("year", "fips"))]
# Determine the counties that are observed over all periods and filter for those counties
county_matrix <- dcast(check_obs, fips ~ year, value.var = "ones", fill = 0)
setDT(county_matrix)
counties_full_obs <- county_matrix[rowSums(county_matrix[ , 2:ncol(county_matrix), with = FALSE] > 0) == 21]
combined_sod <- combined_sod[fips %in% counties_full_obs$fips]

## 2.4 Save datasets  ----------------------------------------------------------

# Create two different datasets
# i. Only Commercial banks
sod_banks <- combined_sod[insured == "CB"]
sod_banks <- sod_banks[, insured := NULL]

# ii. All available financial institutions
sod_all <- combined_sod

# Save Combined (raw) SOD dataset
SAVE(dfx = sod_banks, namex = "banks_sod")
SAVE(dfx = sod_all, namex =  "all_sod")

# Clear Global Environment
rm(list = c("combined_sod", "sod_banks", "sod_all", "fips_data"))

# Clear unused memory
gc()

### 3. Import: U.S.Population Data ---------------------------------------------

# Import U.S.Population for the years 2010 to 2020
df_pop_us_1 <- read_csv(paste0(A, "population_us/", "nst-est2020-popchg2010-2020.csv"))

# Import U.S.Population for the years 2000 to 2010
df_pop_us_2 <- read_excel(paste0(A, "population_us/", "nst-est2010-01.xls"), 
                          sheet = "NST01", 
                          range = "A4:L60")

## 3.1 Manipulate Data for the years 2010 t0 2020 ------------------------------

# Select relevant variables
select_columns <- grep("POPESTIMATE", names(df_pop_us_1), value = TRUE)
select_columns <- c("STATE", "NAME", select_columns)
df_pop_us_1 <- df_pop_us_1[, select_columns]

# Clean name variables
df_pop_us_1$NAME <- gsub(" ", "_", df_pop_us_1$NAME)
df_pop_us_1$NAME <- str_to_lower(df_pop_us_1$NAME)

# Variable names to only lower case letters
names_low <- str_to_lower(names(df_pop_us_1))
colnames(df_pop_us_1) <- names_low



# Adjust census regions by substituting "_region" with ""
df_pop_us_1$name <- gsub("_region", "", df_pop_us_1$name)

# Exclude u.s. territory "Puerto Rico" 
df_pop_us_1 <-  df_pop_us_1 |> filter(name != "puerto_rico") 

## 3.2 Manipulate Data for the years 2010 to 2020 ------------------------------

# Create a vector with variable names as imported excel does not include any
# column names. -> Needs to be coded by hand
column_names <- c()

# Create popestimate_year variable names
for (i in 1:11) {
  # Create the variable name by concatenating "POPESTIMATE" with the year
  variablename <- paste0("popestimate", 2011 - i )
  
  # Append the variable name to the vector
  column_names <- c(column_names, variablename)
}

# Rename variables with the newly created variable names vector
column_names <- c("name", column_names)
colnames(df_pop_us_2) <- column_names 

# Clean the state variable
df_pop_us_2$name <- gsub(" ", "_", df_pop_us_2$name)
df_pop_us_2$name <- gsub("^\\.", "", df_pop_us_2$name)
df_pop_us_2$name <- str_to_lower(df_pop_us_2$name)

# Exclude the estimate as it is already available in the other dataset
df_pop_us_2$popestimate2010 <- NULL

## 3.3 Merge both datasets in order to create a U.S. population dataset for the 
# period 2000 to 2020
# !!! Check if states is chr or dbl
df_pop_us_complete <- merge(df_pop_us_1, df_pop_us_2, by = "name")
df_pop_us_complete <- df_pop_us_complete |>
  pivot_longer(cols = starts_with("popestimate"),
               names_to = "year",
               values_to = "population_total") |>
  mutate(year = str_remove(year, "popestimate")) |>
  mutate(year = as.double(year)) 

# Save the merged dataset as pop_us
SAVE(dfx = df_pop_us_complete, namex = "pop_us")

# remove all created objects
rm(list = ls(pattern = "df_pop_us"))

### Home Mortgage Disclosure Act (HMDA) ---- 

## HMDA Data for the years 2000 to 2006 ----
# Read all lra and panel data files in their respective file
lra_txt_files <- list.files(paste0(A, "a_hdma_lra/"), pattern = ".txt")
lra_txt_files <- lra_txt_files[1]
panel_txt_files <- list.files(paste0(A, "b_hdma_panel/"), pattern = ".txt")
panel_txt_files <- panel_txt_files[1]

# Check if file years are aligning in lra and panel and have the same length
if (length(lra_txt_files) != length(panel_txt_files)) {
  stop("The LRA and Panel files lists must be of the same length")
}

if (!identical(gsub("[^0-9]", "", lra_txt_files), gsub("[^0-9]", "", panel_txt_files))) {
  stop("LRA and PANEL years are not aligning.")
}

# Setting up a parallel processing plan with two levels of parallel execution using 
# multisession
plan(list(multisession, multisession), workers = availableCores() - 1)

# Import all HMDA dataset for the years 2000 to 2006 with the help of the 
# MERGEHMDATXT function (Import the panel and LRA dataset + merges them by 
# respondent_id and agency_code)
col_names <- c("activity_year", "respondent_id", "agency_code", "loan_amount", "state_code", "county_code")
future_lapply(seq_along(lra_txt_files), function(i) {
  # Import and merge HDMA Panel and LRA files
  MERGEHMDATXT(lrafilex = lra_txt_files[i], panelfilex = panel_txt_files[i], varnamex = col_names)
  # Print that merge for certain year has been completed
  print(paste0("HDMA is succesfully saved for the year: ", gsub("[^0-9]", "", lra_txt_files[i])))
})

# Basic Data Cleaning as describes in the function COUNTYLEVEL
# i) Filters for commerical banks or mortgage-subsidaries of banks
# ii) Filters all observation with missing state & county code
# iii) Filters all US-Territories
# iv) Creates fips-code (unique identifier for each county)
# v) Collapses the data by fips and sums up the loan amount within each county
hmda_files <- list.files(TEMP, pattern = "raw")
hmda_files <- hmda_files[, gsub("[^0-9]", "", hmda_files) %in% c(2000:2006)]

result_hmda <- future_lapply(seq_along(hmda_files), function(i) {
  # Merge HDMA Panel and LRA files
  data <- COUNTYLEVEL(hmda_files[i], bank_code = c(1)) 
  # assign(paste0("hdma_county_", gsub("[^0-9]", "", hmda_files[i])), data)
  return(data)
})

# Assign the results to the global environment
for (i in seq_along(result_hmda)) {
  assign(paste0("hmda_county_", result_hmda[[i]]$activity_year), result_hmda[[i]], envir = .GlobalEnv)
}

hmda_files <- hmda_files[3]
main_2007 <- COUNTYLEVEL(hmda_files[3], bank_code = c(1, 2))


## HMDA Data for the years 2007 to 2017 ---- -----------------------------------



# Check if file years are aligning in lra and panel and have the same length
if (length(lra_csv_files) != length(panel_csv_files)) {
  stop("The LRA and Panel files lists must be of the same length")
}

if (!identical(gsub("[^0-9]", "", lra_csv_files), gsub("[^0-9]", "", panel_csv_files))) {
  stop("LRA and PANEL years are not aligning.")
}

select_columns <- c("as_of_year", "respondent_id", "agency_code", "loan_amount_000s", "county_code", "state_code")

# Import all HMDA datasets for the period 2007 to 2017
plan(list(multisession, multisession), workers = availableCores() - 4)
future_lapply(seq_along(lra_csv_files), function(i) {
  # Merge HDMA Panel and LRA files
  MERGEHMDA(lrafilex = lra_csv_files[i], panelfilex = panel_csv_files[i])
  # Print after 
  print(paste0("HDMA is succesfully saved for the year: ", gsub("[^0-9]", "", lra_csv_files[i])))
  gc()
})


# Import of HMDA files -----
lra_files <- list.files(paste0(A, "a_hdma_lra/"))
lra_files <- lra_files[gsub("[^0-9]", "", lra_files) %in% c(2000:2017)]
# lra_files <- lra_files[1]
panel_files <- list.files(paste0(A, "b_hdma_panel/"))
panel_files <- panel_files[gsub("[^0-9]", "", panel_files) %in% c(2000:2017)]
panel_files <- panel_files[1]

start_time <- Sys.time()

lapply(lra_files, function(file) {
  # Account for different column names
  if (as.integer(gsub("[^0-9]", "", file) %in% c(2000:2006))) {
    lra_columns <- c("activity_year", "respondent_id", "agency_code", "loan_amount", "state_code", "county_code")
  } else if (as.integer(gsub("[^0-9]", "", file) %in% c(2007:2017))) {
    lra_columns <- c("as_of_year", "respondent_id", "agency_code", "loan_amount_000s", "state_code", "county_code")
  } 
  
  # Load all the raw LRA data on respondent-ID level (contains the information on each handed out loan)
  data <- fread(paste0(A, "a_hdma_lra/", file), colClasses = "character", select = lra_columns)
  
  # Adjust Column names
  if (as.integer(gsub("[^0-9]", "", file) %in% c(2007:2017))){
    setnames(data,
             old = c("as_of_year", "respondent_id", "agency_code", "loan_amount_000s", "state_code", "county_code"),
             new = c("activity_year", "respondent_id", "agency_code", "loan_amount", "state_code", "county_code"))
  }
  
  
  
  # Save the raw lra dataset
  SAVE(dfx = data, namex = paste0("hmda_lra_", gsub("[^0-9]", "", file)), pattdir = TEMP)
  print(paste0("LRA: Successful import of the year ", gsub("[^0-9]", "", file)))
  
  # Free unused memorey
  gc()
  rm(data)
})

end_time <- Sys.time()
print(end_time - start_time)



  
purrr::walk(panel_files, function(file) {
  # Import Panel data and retrieve all unique observation in order to
  # get the all unique respondent_id and agency_code combinations.
  # This is later needed to identify Commercial Banks that hand out Mortgages.
  # Check which year of the data is imported and adjust the column names.
  file <- panel_files[12]
  year <- as.integer(gsub("[^0-9]", "", file))
  
  if (year == 2007) {
    # Reading in the file from 2009 needed a manual fix by skiping the last observation
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
  
  # select the relevant variables
  data <- data[, c("respondent_id", "agency_code", "other_lender_code")]
  # get rid off any duplicants
  data <- unique(data, by = c("respondent_id", "agency_code"))
  
  SAVE(dfx = data, namex = paste0("hmda_panel_", year), pattdir = TEMP)
  print(paste0("Panel: Successful import of the year ", year))
  gc()
  rm(data)
})
  

purrr::walk(2000:2017, function(i) {
  lra <- paste0("hmda_lra_", i)
  panel <- paste0("hmda_panel_", i)
  
  load(file = paste0(TEMP, "/", lra, ".rda"))
  load(file = paste0(TEMP, "/", panel, ".rda"))
  
  dflra <- get(lra)
  dfpanel <- get(panel)
  
  # Performs a left_join
  main <- left_join(dflra, dfpanel, by = c("respondent_id", "agency_code"))
  
  # Check if LRA and main dataset have same length
  
  if (nrow(main) == nrow(dflra)) {
    message("Merge successful for year: ", i)
  } else {
    warning("Merging issues for year ", i, ": LRA observations: ", nrow(get("lra")),
            ", Merged observations: ", nrow(main))
  }
  
  # Save merged file
  SAVE(dfx = main, namex = paste0("hmda_merge_", i))
  
  # Clean up memory space
  rm(list = c(paste0("hmda_lra_", i), paste0("hmda_panel_", i), "main", "dflra", "dfpanel"))
  
  # Free unused memory
  gc()
})





# Basic data cleaning for the periods 2007 to 2017 -----------------------------
hmda_files <- list.files(TEMP, pattern = "raw")
hmda_files <- hmda_files[, gsub("[^0-9]", "", hmda_files) %in% c(2007:2017)]

result_hmda <- future_lapply(seq_along(hmda_files), function(i) {
  # Merge HDMA Panel and LRA files
  data <- COUNTYLEVEL(hmda_files[i], bank_code = c(1,2)) 
  # assign(paste0("hdma_county_", gsub("[^0-9]", "", hmda_files[i])), data)
  return(data)
  gc()
})




csv_lra <- list.files(paste0(A, "a_hdma_lra/"), pattern = "originated")
csv_panel <- list.files(paste0(A, "b_hdma_panel/"))
csv_panel <- csv_panel[as.integer(gsub("[^0-9]", "", csv_panel)) %in% c(2007:2017)]
csv_lra <- csv_lra[1]
csv_panel <- csv_panel[1]



plan(list(multisession, multisession), workers = availableCores() - 1)
future_lapply(seq_along(lra_csv_files), function(i) {
  # Merge HDMA Panel and LRA files
  MERGEHMDATXT(lrafilex = lra_csv_files[i], panelfilex = panel_csv_files[i])
  # Print after 
  print(paste0("HDMA is succesfully saved for the year: ", gsub("[^0-9]", "", lra_txt_files[i])))
})

hmda_2000 <- lra_txt_files




test <- READBIGDATA(file = "hmda_2016_nationwide_originated-records_labels.csv", path = "a_hdma_lra")
test2 <- READBIGDATA(file = panel_csv_files[1], path = "b_hdma_panel/")

merge_test <- MERGEHMDACSV(lrafilex = csv_lra, panelfilex = csv_panel, varnamex = select_columns)

test <- test[state_code != "" | county_code != ""]
test1 <- fread(file = paste0(A,"b_hdma_panel/", panel_csv_files[1]), colClasses = "character", skip = 8609)
test2 <- fread(file = paste0(A,"b_hdma_panel/", "hmda_2007_panel - Kopie.csv"), colClasses = "character", nrows = 8700)


test  fread()




dt_csv_lra <- parLapply(cl, csv_lra, READBIGDATA, path = "a_hdma_lra/")
dt_csv_lra <- dt_csv_lra[[1]]


dt_txt_panel <- parLapply(cl, txt_panel, READBIGDATA, path = "b_hdma_panel/")
dt_txt_panel <- dt_txt_panel[[1]]
dt_txt_panel <- dt_txt_panel[, c("respondent_id", "agency_code", "other_lender_code")]
dt_txt_panel <- unique(dt_txt_panel, by = c("respondent_id", "agency_code"))




csv_data <- csv_data[, c("activity_year", "respondent_id", "agency_code", "loan_amount_000s", "state_code", "county_code")]







Viewdf_callreport <- read.delim(paste0())
anyDuplicated(df$rssdid, df$date)
df %>%
  select(rssdid, date) %>%
  distinct() %>%
  nrow() == nrow(df)

excel <- read.csv((paste0(A, "._ZIP_COUNTY_122017.csv")))

list.files(path = paste0(A, "mortgage_data/"), all.files = TRUE)
