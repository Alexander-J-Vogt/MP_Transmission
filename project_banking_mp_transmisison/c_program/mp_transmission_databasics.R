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

### Importing Summary of Deposits (SOD) (by FDIC) ------------------------------------
## Importing Summary of Deposits (by FDIC) from Wang et al. (2022) -------------
df_sod <- read_dta(paste0(A,"wang_22_data/", "FDIC_SOD.dta"))

### 1. Importing Summary of Deposits (by FDIC) for the years 2018 to 2024 ---------
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



Viewdf_callreport <- read.delim(paste0())
anyDuplicated(df$rssdid, df$date)
df %>%
  select(rssdid, date) %>%
  distinct() %>%
  nrow() == nrow(df)

excel <- read.csv((paste0(A, "._ZIP_COUNTY_122017.csv")))

list.files(path = paste0(A, "mortgage_data/"), all.files = TRUE)
