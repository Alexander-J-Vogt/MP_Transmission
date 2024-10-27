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

main <- LOAD(dfinput = "main_banks_data")
setDT(main)
main <- main[inrange(year, 2006, 2010)]
# main <-main[, state := as.factor(state)]
# main <-main[, year := as.factor(year)]

outcome_var <- c("ln_loan_amount", "ln_wtd_loan_amount", "lead_ln_loan_amount", "lead_ln_wtd_loan_amount")

# Dummy: Median_pre | state FE and clustered SE ================================

lapply(outcome_var, function (x) {
  # x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | 0 | 0 | state")), data = main)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | state | 0 | state")), data = main)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop | state | 0 | state")), data = main)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_earning | state | 0 | state")), data = main)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur | state | 0 | state")), data = main)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_emp | state | 0 | state")), data = main)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + d_msa | state | 0 | state")), data = main)
  did8 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp + d_msa | state | 0 | state")), data = main)
  did9 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator +  mean_earning + ur + d_msa | state | 0 | state")), data = main)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, did8, did9,
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "FE" , "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6", "Pref."),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "County Pop", "Earnings", "UR", "Employment", "Dummy: MSA", "DiD Estimator"),
            dep.var.labels = c(rep(gsub("_", " ", x),9)),
            out = paste0(LATEX, x, ".html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
            )
  
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))

  })

# Dummy: Mean_pre | state FE and clustered SE ==================================

lapply(outcome_var, function (x) {
  # x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_mean_all_pre + d_ffr_indicator + d_mean_all_pre:d_ffr_indicator | 0 | 0 | state")), data = main)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_mean_all_pre + d_ffr_indicator + d_mean_all_pre:d_ffr_indicator | state | 0 | state")), data = main)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_mean_all_pre + d_ffr_indicator + d_mean_all_pre:d_ffr_indicator + cnty_pop | state | 0 | state")), data = main)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_mean_all_pre + d_ffr_indicator + d_mean_all_pre:d_ffr_indicator + mean_earning | state | 0 | state")), data = main)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_mean_all_pre + d_ffr_indicator + d_mean_all_pre:d_ffr_indicator + ur | state | 0 | state")), data = main)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_mean_all_pre + d_ffr_indicator + d_mean_all_pre:d_ffr_indicator + mean_emp | state | 0 | state")), data = main)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_mean_all_pre + d_ffr_indicator + d_mean_all_pre:d_ffr_indicator + d_msa | state | 0 | state")), data = main)
  did8 <- felm(as.formula(paste0(x,  "~" , "d_mean_all_pre + d_ffr_indicator + d_mean_all_pre:d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp + d_msa | state | 0 | state")), data = main)
  did9 <- felm(as.formula(paste0(x,  "~" , "d_mean_all_pre + d_ffr_indicator + d_mean_all_pre:d_ffr_indicator +  mean_earning + ur + d_msa | state | 0 | state")), data = main)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, did8, did9,
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "FE" , "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6", "Pref."),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "County Pop", "Earnings", "UR", "Employment", "Dummy: MSA", "DiD Estimator"),
            dep.var.labels = c(rep(gsub("_", " ", x),9)),
            out = paste0(LATEX, x, "_mean.html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
  )
  
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))
  
})


# Dummy: Market_def | state FE and clustered SE ================================

lapply(outcome_var, function (x) {
  # x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator | 0 | 0 | state")), data = main)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator | state | 0 | state")), data = main)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + cnty_pop | state | 0 | state")), data = main)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + mean_earning | state | 0 | state")), data = main)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + ur | state | 0 | state")), data = main)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + mean_emp | state | 0 | state")), data = main)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + d_msa | state | 0 | state")), data = main)
  did8 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp + d_msa | state | 0 | state")), data = main)
  did9 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator +  mean_earning + ur + d_msa | state | 0 | state")), data = main)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, did8, did9,
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "FE" , "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6", "Pref."),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "County Pop", "Earnings", "UR", "Employment", "Dummy: MSA", "DiD Estimator"),
            dep.var.labels = c(rep(gsub("_", " ", x),9)),
            out = paste0(LATEX, x, "_market_df.html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
  )
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))
  
})

# Version with demeaned variables in order to get all time-variant variables ====
demeand_var <- c("demeaned_ln_loan_amount", "demeaned_ln_wtd_loan_amount", "demeaned_lead_ln_loan_amount", "demeaned_lead_ln_wtd_loan_amount")
demeand_var <- demeand_var[3:4]
lapply(demeand_var, function (x) {
  did1 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator | 0 | 0 | state")), data = main)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator | state | 0 | state")), data = main)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + demeaned_cnty_pop | state | 0 | state")), data = main)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + demeaned_mean_earning | state | 0 | state")), data = main)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + demeaned_ur | state | 0 | state")), data = main)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + demeaned_mean_emp | state | 0 | state")), data = main)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + d_msa | state | 0 | state")), data = main)
  did8 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + demeaned_cnty_pop + demeaned_mean_earning + demeaned_ur + demeaned_mean_emp + d_msa | state | 0 | state")), data = main)
  did9 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator +  demeaned_mean_earning + demeaned_ur + d_msa | state | 0 | state")), data = main)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, did8, did9,
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "FE" , "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6", "Pref."),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "County Pop", "Earnings", "UR", "Employment", "Dummy: MSA", "DiD Estimator"),
            dep.var.labels = c(rep(gsub("_", " ", x),9)),
            out = paste0(LATEX, x, "_demeaned.html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
  )
}
)
