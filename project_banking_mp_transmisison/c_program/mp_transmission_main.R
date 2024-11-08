# TARGET: Get dataset with no missing variables
# INDATA: mp_transmission_merge
# OUTDATA/ OUTPUT: mp_transmission_main

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

# 0. Determine Periods of Dataset ==============================================

start <- 2002
end <- 2016


# 1. Dataset with Mortgages of Commercial Banks ================================

# Load merged data
banks_data <- LOAD(dfinput = "mp_transmission_merge")
setDT(banks_data)

# Subset for the relevant period
banks_data <- banks_data[inrange(year, start, end)]

# Only keep counties that are observed over the whole period
complete_banks_data <- COMPLETEOBS(data = banks_data, rowx = "fips", colx = "year")

# 2. Save dataset ==============================================================

# Save dataset
SAVE(dfx = complete_banks_data, namex = MAINNAME)

########################## ENDE ###############################################+