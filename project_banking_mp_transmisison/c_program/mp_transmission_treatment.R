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
sod <- LOAD(dfinput = "sod_banks", dfextension = ".rda")
setDT(sod)

# Select the relevant variables for creating HHI by county-level
sod <- sod[, .(year, stcntybr, depsumbr)]


# Create HHI by county


# Create turnover rate of branches for each year (sims_aquired_date)
# Create dummy variable for whether county lays in a Metropolitan Statistical Area or not (msabr)
# Create dummy whether county is has the main office or not (bkmo)


sod <- sod[, stcntybr := ifelse(nchar(stcntybr) == 4, paste0("0", stcntybr), "stcntybr") ]

# Restrict to the period 2000 to 2020
sod <- 
sod <- sod[year >= 2000]

 
test <- table(nchar(sod$stcntybr))



sod$stcntybr