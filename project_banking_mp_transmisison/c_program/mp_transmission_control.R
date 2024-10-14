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

# Load the Summary of Deposits for the period 1994 to 2020
sod <- LOAD(dfinput = "combined_sod", dfextension = ".rda")

# Restrict to the relevant variables
sod <- sod[, .(year, stcntybr,  insured, specdesc, sims_acquired_date, msabr, bkmo)]