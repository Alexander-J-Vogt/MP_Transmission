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

### Importing Call Report Data from Wang et al. (2022) -------------------------
# Call Rreport: Information on bank-level data
# df_callreport <- read_dta(paste0(A,"wang_22_data/", "callreport_ffiec_merged.dta"))

### Importing Summary of Deposits (SOD) (by FDIC) ------------------------------
## Importing Summary of Deposits (by FDIC) from Wang et al. (2022) -------------
df_sod <- read_dta(paste0(A,"wang_22_data/", "FDIC_SOD.dta"))

### 1. Importing Summary of Deposits (by FDIC) for the years 2018 to 2024 ------
## 1.1 Import Files ----
files_sod <- list.files(paste0(A, "sod_direct/"))
files_sod <- files_sod[grepl("\\csv$",files_sod)]

# Loop for importing all sod datasets from 2018 - 
for (i in files_sod) {
  print(paste0("Iteration Status: ", i))
  file <- suppressMessages(read_csv(paste0(A, "sod_direct/", i), , col_types = cols(.default = "c")))
  year <- str_sub(i, start = 5, end = 8)
  SAVE(dfx = file, namex = paste0("sod_", year))
  rm(file, year)
}

# Loop over all SOD datasets in order to create one large data frame for 
# the years 2018 to 2014

# Create vector with all names of SOD datasets
sod_temp <- list.files(paste0(TEMP))
sod_temp <- sod_temp[str_detect(sod_temp, "^sod")]

# Create empty list in which all SOD datasets will be saved
combined_sod <- list()

# Save all data frames within alist
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
names_capslock <- names(combined_sod[[1]])
names_low <- str_to_lower(names_capslock)
combined_sod <-  LOWERCASEVAR(combined_sod, names_low)

# Combine all data frames within the list to one large data frame
combined_sod <-  bind_rows(combined_sod)

# Label all variables with their original variables in upper case
for (i in seq_along(combined_sod)) {
  attr(combined_sod[[i]], "label") <- names_capslock[i]
}

# Save Combined (raw) SOD dataset
SAVE(dfx = combined_sod, namex = "combined_sod")

# Clear Global Environment
rm(list = "combined_sod")

# Clear unused memory
gc()

LOAD(paste0(TEMP, "/combined_sod"))
### 2. Import: Mortgages Data  -------------------------------------------------
    
df_new_mortgage <- read.csv(paste0(A, "mortgage_data/", "nmdb-new-mortgage-statistics-state-annual.csv"))
# df_mortgage_performance <- read.csv(paste0(A, "mortgage_data/", "nmdb-mortgage-performance-statistics-states-quarterly.csv"))
# df_new_mortgage <- read.csv(paste0(A, "mortgage_data/", "nmdb-new-mortgage-statistics-national-census-areas-annual.csv"))

# Start to 
column_vector <- colnames(df_new_mortgage)
column_vector <- str_to_lower(column_vector)
colnames(df_new_mortgage) <- column_vector

# Clean name variables
df_new_mortgage <- LOWERUNDERSCORE(data = df_new_mortgage, varname = "geoname")

LOWERUNDERSCORE <-  function(data, varname) {
  data[[varname]] <- gsub(" ", "_", data[[varname]])
  data[[varname]] <- str_to_lower(data[[varname]])
  return(data)
}
df_new_mortgage <- LOWERUNDERSCORE(data = df_new_mortgage, varname = "market")
df_new_mortgage <- gsub("[()]", "", df_new_mortgage)
df_new_mortgage <- df_new_mortgage |>
  rename(nr_orginations = value1,
         value_originations = value2)

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


## Test ----

setwd(paste0(A, "county_level_data/"))
county_level <- fromJSON("counties.json")

# Read the JSON file as a raw text
json_raw <- readLines("counties.json", warn = FALSE)

# Replace NaN with null (or another placeholder)
json_cleaned <- gsub("NaN", "null", json_raw)

json_cleaned_string <- paste(json_cleaned, collapse = "\n")

data <- fromJSON(json_cleaned_string)
json_raw <- readLines("path_to_your_file.json", warn = FALSE)
if (!validate(paste(json_raw, collapse = "\n"))) {
  cat("The JSON file is not valid!\n")
} else {
  data <- fromJSON(paste(json_raw, collapse = "\n"))
}

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


COUNTYLEVEL <- function (filesnamex , bank_code) {
  
  # load raw hmda files
  load(paste0(TEMP, "/",filesnamex))
  
  # retrieve current loaded hmda file and save into standardized name
  files_list <- ls(pattern = "raw")
  main <- get(files_list)
  
  # Check if vector is only 1 element
  if (length(files_list) != 1) {
    stop("STOP: Only one dataset at a time!")
  }
  
  print(paste0("Current Dataset: ", gsub("[^0-9]", "", files_list)))
  
  # Filter for only commercial banks, who distributed mortgages + for available to 
  # state_code and county_code
  main <- main[other_lender_code %in% bank_code]
  main <- main[state_code != "" & county_code != ""]
  
  # Excluding: Puerto Rico (72), US Virgin Islands (78), American Samoa (60), 
  # Northern Marian Islands (69), U.S. Minor Outlying Islands (74), Guam (66)
  main <- main[!state_code %in% c("72", "66", "60", "69", "74", "78")]
  
  # Create fips-code (unique identifier for each county; state_code + county_code)
  main <- main[, fips := paste0(state_code, county_code, sep = "")]
  # main <- main[, fips := as.integer(fips)]
  
  # Clean loan amount variable is available in thousands
  main <- main[, loan_amount := as.integer(gsub("0", "", loan_amount))]
  
  # Keep only YEAR, fips-code and the loan amount in 000s
  main <- main[, c("activity_year", "fips", "loan_amount")]
  main <- main[, activity_year := as.integer(activity_year)]
  
  # Exclude all values where the fips code is incomplete # only 3134 counties instead of 3142
  main <- main[nchar(fips) == 5]
  
  # Extract the unique year
  year <- unique(main$activity_year)
  
  # Collapse by county and sum up all loan amounts within a county
  main <- main[, .(total_amount_loan = sum(loan_amount)), by = fips]
  
  # Add year back to data.table
  main <- main[, activity_year := year]
  
  #SAVE and assign n
  SAVE(dfx = main, namex = paste0("hdma_county_", gsub("[^0-9]", "", files_list)))
  
  # Save county-level dataset in global environment
  return(main)
  
  # Remover raw dataset from global enviroment
  rm(list = c(files_list))
}



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

lra_columns <- c("as_of_year", "respondent_id", "agency_code", "loan_amount_000s", "county_code", "state_code")

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



  
lapply(panel_files, function(file) {
  # Import Panel data and retrieve all unique observation in order to
  # get the all unique respondent_id and agency_code combinations.
  # This is later needed to identify Commercial Banks that hand out Mortgages.
  # Check which year of the data is imported and adjust the column names.
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

  data <- data[, c("respondent_id", "agency_code", "other_lender_code")]
  data <- unique(data, by = c("respondent_id", "agency_code"))
  
  SAVE(dfx = data, namex = paste0("hmda_panel_", year), pattdir = TEMP)
  print(paste0("Panel: Successful import of the year ", year))
  gc()
  rm(data)
})
  

lapply(, function(i) {
  lra <- 

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
