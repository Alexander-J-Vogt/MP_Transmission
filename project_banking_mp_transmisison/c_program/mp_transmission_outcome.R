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

########################

# 1. Basic data cleaning 


hmda_files <- list.files(paste0(TEMP, "/") , pattern = "merge")
start_time <- Sys.time()
list_merge <- lapply(hmda_files, COUNTYLEVEL)
end_time <- Sys.time()
print(end_time - start_time)

COUNTYLEVEL <- function (filenamex) {
  # filenamex <- hmda_files[8]
  year <- as.integer(gsub("[^0-9]", "", filenamex))
  # load raw hmda files
  load(paste0(TEMP, "/",filenamex))
  
  # retrieve current loaded hmda file and save into standardized name
  file_list <- ls(pattern = "merge")
  main <- get(file_list)
  
  # Check if vector is only 1 element
  if (length(file_list) != 1) {
    stop("STOP: Only one dataset at a time!")
  }
  
  # Determine based on year, which bank code needs to be used
  # Over the years the HMDA has changed their codes, when it comes to indicating 
  # who the lender is. Until 2006, mortgage-subdivisions of banks are indicated 
  # as 1 in the variable other_lender_code. From 2007 on, additionally all
  # depository institions were included and are listed with the numeber 0.
  # 2007 until 2017, 
  if (year %in% c(2000:2006)) {
    bank_code <- c(1) # All mortgage-subdivisions of banks
  } else if (year %in% c(2007:2017)) {
    bank_code <- c(1, 2) # All depository instiutions and their mortgage-subdivisions
  }
  
  # Filter for only commercial banks, who distributed mortgages + for available to 
  # state_code and county_code
  main <- main[other_lender_code %in% bank_code]
  main <- main[state_code != "" & county_code != ""]
  
  # Check if there any missing in the columns
  message(paste0("Current Dataset: ", year))
  for (colname in colnames(main)) {
    if (any(main[[colname]] == "")) {
      message(paste("Missing values in column:", colname))
    } else {
      message(paste("No missing values in column:", colname))
    }
  }
  
  # Standardize state_code and county code such that state_code consist always
  # 2 numbers (e.g., 01 and not 1) and county_code consists of 3 numbers (e.g., 
  # 001 and not 1)
  main <- main[, state_code := ifelse(nchar(state_code) == 1, paste0("0", state_code), state_code)]
  main <- main[, county_code := ifelse(nchar(county_code) == 1, paste0("00", county_code), county_code)]
  main <- main[, county_code := ifelse(nchar(county_code) == 2, paste0("0", county_code), county_code)]
  
  # Excluding: Puerto Rico (72), US Virgin Islands (78), American Samoa (60), 
  # Northern Marian Islands (69), U.S. Minor Outlying Islands (74), Guam (66)
  main <- main[!state_code %in% c("72", "66", "60", "69", "74", "78")]
  
  # Create fips-code (unique identifier for each county; state_code + county_code)
  main <- main[, fips := paste0(state_code, county_code, sep = "")]
  # main <- main[, fips := as.integer(fips)]
  
  # Clean loan amount variable is available in thousands
  main <- main[, loan_amount := as.integer(gsub("^0{1,3}", "", loan_amount))]
  
  # Keep only YEAR, fips-code and the loan amount in 000s
  main <- main[, year := as.integer(activity_year)]
  main <- main[, c("year", "fips", "loan_amount")]
  
  # Be sure that only 5 digit log fips code are included
  main <- main[nchar(fips) == 5]
  
  # Extract the unique year
  # year <- unique(main$activity_year)
  
  # Collapse by county and sum up all loan amounts within a county
  main <- main[, .(total_amount_loan = sum(loan_amount)), by = fips]
  
  # Add year back to data.table
  main <- main[, year := year]
  
  # Order the columns
  main <- main[, .(year, fips, total_amount_loan)]
  
  #SAVE and assign n
  SAVE(dfx = main, namex = paste0("hdma_county_", year))
  
  # Print to console the the process has been finished for the currrent dataset
  message(paste("Basic Data Cleaning is finished for: ", year))
  
  # Save county-level dataset in global environment
  return(main)
  
  # Remover raw dataset from global environment
  rm(list = c(file_list, "main"))
}
