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
main <-main[, state := as.factor(state)]
# main <-main[, year := as.factor(year)]

outcome_var <- c("lead_ln_loan_amount", "lead_ln_wtd_loan_amount")

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
            out = paste0(LATEX, x, "_median.html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
            )
  
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))

  })

# Same as above but without state fixed effects -

lapply(outcome_var, function (x) {
  # x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | 0 | 0 | state")), data = main)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | 0 | 0 | state")), data = main)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop | 0 | 0 | state")), data = main)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_earning | 0 | 0 | state")), data = main)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur | state | 0 | 0")), data = main)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_emp | 0 | 0 | state")), data = main)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + d_msa | 0 | 0 | state")), data = main)
  did8 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp + d_msa | 0 | 0 | state")), data = main)
  did9 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator +  mean_earning + ur + d_msa | 0 | 0 | state")), data = main)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, did8, did9,
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "FE" , "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6", "Pref."),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "County Pop", "Earnings", "UR", "Employment", "Dummy: MSA", "DiD Estimator"),
            dep.var.labels = c(rep(gsub("_", " ", x),9)),
            out = paste0(LATEX, x, "_median_no_fe.html")
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

# Same as with state fe model bit without clusters on the state level
lapply(outcome_var, function (x) {
  # x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | 0 | 0 | 0")), data = main)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | state | 0 | 0")), data = main)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop | state | 0 | 0")), data = main)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_earning | state | 0 | 0")), data = main)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur | state | 0 | 0")), data = main)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_emp | state | 0 | 0")), data = main)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + d_msa | state | 0 | 0")), data = main)
  did8 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp + d_msa | state | 0 | 0")), data = main)
  did9 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator +  mean_earning + ur + d_msa | state | 0 | 0")), data = main)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, did8, did9,
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "FE" , "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6", "Pref."),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "County Pop", "Earnings", "UR", "Employment", "Dummy: MSA", "DiD Estimator"),
            dep.var.labels = c(rep(gsub("_", " ", x),9)),
            out = paste0(LATEX, x, "_median_no_cluster.html")
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
  did1 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | 0 | 0 | state")), data = main)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | state | 0 | state")), data = main)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + demeaned_cnty_pop | state | 0 | state")), data = main)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + demeaned_mean_earning | state | 0 | state")), data = main)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + demeaned_ur | state | 0 | state")), data = main)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + demeaned_mean_emp | state | 0 | state")), data = main)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + d_msa | state | 0 | state")), data = main)
  did8 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + demeaned_cnty_pop + demeaned_mean_earning + demeaned_ur + demeaned_mean_emp + d_msa | state | 0 | state")), data = main)
  did9 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator +  demeaned_mean_earning + demeaned_ur + d_msa | state | 0 | state")), data = main)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, did8, did9,
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "FE" , "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6", "Pref."),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "County Pop", "Earnings", "UR", "Employment", "Dummy: MSA", "DiD Estimator"),
            dep.var.labels = c(rep(gsub("_", " ", x),9)),
            out = paste0(LATEX, x, "_median.html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
  )
}
)



lapply(demeand_var, function (x) {
  did1 <- felm(as.formula(paste0(x,  "~" , "d_mean_all_pre + d_ffr_indicator + d_mean_all_pre:d_ffr_indicator | 0 | 0 | state")), data = main)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_mean_all_pre + d_ffr_indicator + d_mean_all_pre:d_ffr_indicator | state | 0 | state")), data = main)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_mean_all_pre + d_ffr_indicator + d_mean_all_pre:d_ffr_indicator + demeaned_cnty_pop | state | 0 | state")), data = main)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_mean_all_pre + d_ffr_indicator + d_mean_all_pre:d_ffr_indicator + demeaned_mean_earning | state | 0 | state")), data = main)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_mean_all_pre + d_ffr_indicator + d_mean_all_pre:d_ffr_indicator + demeaned_ur | state | 0 | state")), data = main)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_mean_all_pre + d_ffr_indicator + d_mean_all_pre:d_ffr_indicator + demeaned_mean_emp | state | 0 | state")), data = main)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_mean_all_pre + d_ffr_indicator + d_mean_all_pre:d_ffr_indicator + d_msa | state | 0 | state")), data = main)
  did8 <- felm(as.formula(paste0(x,  "~" , "d_mean_all_pre + d_ffr_indicator + d_mean_all_pre:d_ffr_indicator + demeaned_cnty_pop + demeaned_mean_earning + demeaned_ur + demeaned_mean_emp + d_msa | state | 0 | state")), data = main)
  did9 <- felm(as.formula(paste0(x,  "~" , "d_mean_all_pre + d_ffr_indicator + d_mean_all_pre:d_ffr_indicator +  demeaned_mean_earning + demeaned_ur + d_msa | state | 0 | state")), data = main)
  
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
}
)



lapply(demeand_var, function (x) {
  # x <-  demeand_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator | 0 | 0 | state")), data = main, weights = main$county_pop)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator | state | 0 | state")), data = main)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + demeaned_cnty_pop | state | 0 | state")), data = main)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + demeaned_mean_earning | state | 0 | state")), data = main)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + demeaned_ur | state | 0 | state")), data = main)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + demeaned_mean_emp | state | 0 | state")), data = main)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + d_msa | state | 0 | state")), data = main)
  did8 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + demeaned_cnty_pop + demeaned_mean_earning + demeaned_ur + demeaned_mean_emp + d_msa | state | 0 | state")), data = main)
  did9 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all:d_ffr_indicator + demeaned_mean_earning + demeaned_ur + d_msa | state | 0 | state")), data = main)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, did8, did9,
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "FE" , "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6", "Pref."),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "County Pop", "Earnings", "UR", "Employment", "Dummy: MSA", "DiD Estimator"),
            dep.var.labels = c(rep(gsub("_", " ", x),9)),
            out = paste0(LATEX, x, "_md.html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
  )
}
)


# Include lagged variables and share of employed individuals ===================


lapply(outcome_var, function (x) {
  # x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | 0 | 0 | state")), data = main)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop| 0 | 0 | state")), data = main)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_cnty_pop| 0 | 0 | state")), data = main)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_earning | 0 | 0 | state")), data = main)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_mean_earning | 0 | 0 | state")), data = main)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur | 0 | 0 | state")), data = main)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_ur | 0 | 0 | state")), data = main)
  did8 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_emp | 0 | 0 | state")), data = main)
  did9 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_mean_emp | 0 | 0 | state")), data = main) 
  did10 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + emp_rate | 0 | 0 | state")), data = main)
  # did10 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp + emp_rate + d_msa | 0 | 0 | state")), data = main)
  # did11<- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator +  mean_earning + ur + d_msa | 0 | 0 | state")), data = main)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, did8, did9, did10, 
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6", "Cntrl 7", "cntrl 8", "Cntrl 9"),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "County Pop", "lag County Pop", "Earnings", "lag Earnings", "UR", "lag UR", "Employment", "lag Employment","Employment Share", "DiD Estimator"),
            dep.var.labels = c(rep(gsub("_", " ", ""),10))
            ,
            out = paste0(LATEX, x, "_lagvar.html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
  )
  
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))
  
})


# Implement new variables ===
lapply(outcome_var, function (x) {
  # x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | 0 | 0 | state")), data = main)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + delta_earnings | 0 | 0 | state")), data = main)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag(delta_earnings) | 0 | 0 | state")), data = main)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_earning | 0 | 0 | state")), data = main)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_mean_earning | 0 | 0 | state")), data = main)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur | 0 | 0 | state")), data = main)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_ur | 0 | 0 | state")), data = main)
  did8 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_emp | 0 | 0 | state")), data = main)
  did9 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_mean_emp | 0 | 0 | state")), data = main) 
  did10 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + emp_rate | 0 | 0 | state")), data = main)
  # did10 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp + emp_rate + d_msa | 0 | 0 | state")), data = main)
  # did11<- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator +  mean_earning + ur + d_msa | 0 | 0 | state")), data = main)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, did8, did9, did10, 
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6", "Cntrl 7", "cntrl 8", "Cntrl 9"),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "Net Earnings", "lag Earnings", "Earnings", "lag Earnings", "UR", "lag UR", "Employment", "lag Employment","Employment Share", "DiD Estimator"),
            dep.var.labels = c(rep(gsub("_", " ", ""),10))
            ,
            out = paste0(LATEX, x, "_delta_earnings.html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
  )
  
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))
  
})


# Earnings variable ============================================================

lapply(outcome_var, function (x) {
  # x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | 0 | 0 | state")), data = main)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + delta_earnings | 0 | 0 | state")), data = main)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag(delta_earnings) | 0 | 0 | state")), data = main)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_earning | 0 | 0 | state")), data = main)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_mean_earning | 0 | 0 | state")), data = main)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + log_earnings | 0 | 0 | state")), data = main)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_log_earnings | 0 | 0 | state")), data = main)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, 
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6"),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "Delta Earnings", "lag Delta Earnings", "Earnings", "lag Earnings", "log Earnings", "log lag earnings", "DiD Estimator"),
            dep.var.labels = c(rep(gsub("_", " ", ""),7))
            ,
            out = paste0(LATEX, x, "_earnings.html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
  )
  
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))
  
})

# Testing the need for clustered-se ============================================
library(sandwich)
library(lmtest)

formula1 <- as.formula(lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator)
formula2 <- as.formula(lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop)
formula3 <- as.formula(lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_ur)
formula4 <- as.formula(lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + log_earnings)
formula5 <- as.formula(lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + d_msa)
formula6 <- as.formula(lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_emp)
formula7 <- as.formula(lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur)
formula8 <- as.formula(lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop + lag_ur + ur + log_earnings + d_msa + mean_emp)

formula <- c("formula1", "formula2", "formula3", "formula4", "formula5", "formula6", "formula7", "formula8" )

# Hausman Test for Fixed vs Random Fixed Effects 
pmain <- pdata.frame(main, index = c("fips", "year"))

for (i in formula) {
  fixed_model <- plm(get(i), data = pmain, model = "within")
  random_model <- plm(get(i), data = pmain, model = "random")
  test <- phtest(fixed_model, random_model) 
  print(paste0("Test Results: ", i))
  print(test)
  # Standard errors without clustering
  # non_clustered_se <- coeftest(fixed_model, vcov = vcovHC(fixed_model, type = "HC1"))
  # 
  # # Clustered standard errors at the state level
  # clustered_se <- coeftest(fixed_model, vcov = vcovHC(fixed_model, cluster = "group"))
  # 
  # print(paste0("Analysis of ", i))
  # print(non_clustered_se)
  # print(clustered_se)

}
