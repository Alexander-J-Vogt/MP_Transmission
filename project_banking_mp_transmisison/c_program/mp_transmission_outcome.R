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

lapply


COUNTYLEVEL <- function (filesnamex , bank_code) {
  filenamex <- hmda_files[1]
  bank_code <- 1 
  year <- gsub("[^0-9]", "", filenamex)
  # load raw hmda files
  load(paste0(TEMP, "/",filenamex))
  
  # retrieve current loaded hmda file and save into standardized name
  file_list <- ls(pattern = "merge")
  main <- get(file_list)
  
  # Check if vector is only 1 element
  if (length(file_list) != 1) {
    stop("STOP: Only one dataset at a time!")
  }
  
  print(paste0("Current Dataset: ", year))
  
  # Determine based on year, which bank code needs to be used
  if (year )
  
  
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
