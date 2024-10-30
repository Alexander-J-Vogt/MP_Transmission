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

df_base <- LOAD(dfinput = "main_banks_data")
setDT(main)
main <- df_base[inrange(year, 2006, 2010)]
# main <- main[, id := 1:nrow(main)]

main <-main[, state := as.factor(state)]
# main <-main[, year := as.factor(year)]

outcome_var <- c("lead_ln_loan_amount")

# 1. Dummy: Median_pre =========================================================
 
## 1.1 Same as with state FE model but no clusters on the state level -----
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

## 1.2 State FE & Clusters on State-level --------------------------------------
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
  did9 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_earning + ur + d_msa | state | 0 | state")), data = main)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, did8, did9,
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "FE" , "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6", "Pref."),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "County Pop", "Earnings", "UR", "Employment", "Dummy: MSA", "DiD Estimator"),
            dep.var.labels = c(rep(gsub("_", " ", x),9)),
            out = paste0(LATEX, x, "_median_FE_cluster.html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
            )
  
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))

  })

## 1.3 No State FE & including State Clustered Standard Errors  -----------------

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

## 1.4 State FE & Clustered SE & Weightes by county population ------------------
lapply(outcome_var, function (x) {
  # x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | 0 | 0 | state")), data = main, weights = main$cnty_pop)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | state | 0 | state")), data = main, weights = main$cnty_pop)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop | state | 0 | state")), data = main, weights = main$cnty_pop)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_earning | state | 0 | state")), data = main, weights = main$cnty_pop)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur | state | 0 | state")), data = main, weights = main$cnty_pop)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_emp | state | 0 | state")), data = main, weights = main$cnty_pop)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + d_msa | state | 0 | state")), data = main, weights = main$cnty_pop)
  did8 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp + d_msa | state | 0 | state")), data = main, weights = main$cnty_pop)
  did9 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_earning + ur + d_msa | state | 0 | state")), data = main, weights = main$cnty_pop)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, did8, did9,
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "FE" , "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6", "Pref."),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "County Pop", "Earnings", "UR", "Employment", "Dummy: MSA", "DiD Estimator"),
            
            dep.var.labels = c(rep(gsub("_", " ", x),9)),
            out = paste0(LATEX, x, "_median_weights_pop.html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
  )
  
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))
  
})

## 1.5 State FE & Clustered SE & Weighted by county population density ----------
lapply(outcome_var, function (x) {
  # x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | 0 | 0 | state")), data = main, weights = main$pop_density)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | state | 0 | state")), data = main, weights = main$pop_density)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop | state | 0 | state")), data = main, weights = main$pop_density)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_earning | state | 0 | state")), data = main, weights = main$pop_density)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur | state | 0 | state")), data = main, weights = main$pop_density)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_emp | state | 0 | state")), data = main, weights = main$pop_density)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + d_msa | state | 0 | state")), data = main, weights = main$pop_density)
  did8 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp + d_msa | state | 0 | state")), data = main, weights = main$pop_density)
  did9 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_earning + ur + d_msa | state | 0 | state")), data = main, weights = main$pop_density)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, did8, did9,
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "FE" , "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6", "Pref."),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "County Pop", "Earnings", "UR", "Employment", "Dummy: MSA", "DiD Estimator"),
            
            dep.var.labels = c(rep(gsub("_", " ", x),9)),
            out = paste0(LATEX, x, "_median_weights_popdensity.html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
  )
  
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))
  
})

## 1.6 State FE & CLustered SE & Weighted by Inverse County Population ---------
lapply(outcome_var, function (x) {
  # x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | 0 | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_earning | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_emp | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + d_msa | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did8 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp + d_msa | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did9 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_earning + ur + d_msa | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, did8, did9,
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "FE" , "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6", "Pref."),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "County Pop", "Earnings", "UR", "Employment", "Dummy: MSA", "DiD Estimator"),
            
            dep.var.labels = c(rep(gsub("_", " ", x),9)),
            out = paste0(LATEX, x, "_median_weights_popinv.html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
  )
  
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))
  
})

# 2. Dummy: Mean_pre | State FE and Clustered SE ===============================

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


# 3. Dummy: Market_def | State FE and Clustered SE =============================

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

# 4. Demeaned variables - with State FE & Clustered Standard Errors ============

## 4.1 Dummy: Median
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


## 4.2 Dummy: Mean -------------------------------------------------------------
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

## 4.3 Dummy: Market Definition ------------------------------------------------

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


# 5. Lagged Variables and Share of Employed Individuals ========================


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


# 6. Evaluation of New Earnings and Employment Variables =======================
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


# 7. New Earnings variable =====================================================

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

# 8. Employment & Population density ===========================================

lapply(outcome_var, function (x) {
  # x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | state | 0 | state")), data = main)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + mean_emp | state | 0 | state")), data = main)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag(mean_emp) | state | 0 | state")), data = main)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + log_emp | state | 0 | state")), data = main)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_log_emp | state | 0 | state")), data = main)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop | state | 0 | state")), data = main)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + pop_density | state | 0 | state")), data = main)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, 
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6"),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "Employment", "lag Employment", "log Employment", "lag log Emploment", "County Population", "Population Density in County",  "DiD Estimator"),
            dep.var.labels = c(rep(gsub("_", " ", ""),7))
            ,
            out = paste0(LATEX, x, "_emp_pop.html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
  )
  
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))
  
})



# 9. Testing the need for clustered-se =========================================

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
formula9 <- as.formula(lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + log_emp)


formula <- c("formula1", "formula2", "formula3", "formula4", "formula5", "formula6", "formula7", "formula8", "formula9")

# Hausman Test for Fixed vs Random Fixed Effects 
pmain <- pdata.frame(main, index = c("fips", "year"))

for (i in formula) {
  fixed_model <- plm(get(i), data = pmain, model = "within")
  random_model <- plm(get(i), data = pmain, model = "random")
  # test <- phtest(fixed_model, random_model) 
  test <- plmtest(fixed_model, effect = "individual", type = "bp") 
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

ggplot() +
  geom_density(data = main, aes(x = log(mean_emp)))

ggplot() +
  geom_density(data = main, aes(x = mean_emp))

# 10. Final Assessment of the Control Variables ================================

## 10.1 Final Assesment w/o weighted regression --------------------------------
lapply(outcome_var, function (x) {
  # x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | state | 0 | state")), data = main)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop | state | 0 | state")), data = main)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + pop_density | state | 0 | state")), data = main)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur | state | 0 | state")), data = main)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_ur | state | 0 | state")), data = main)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + log_earnings | state | 0 | state")), data = main)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + log_emp | state | 0 | state")), data = main)
  did8 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_log_emp | state | 0 | state")), data = main)
  did9 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + d_msa | state | 0 | state")), data = main)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, did8, did9,
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6", "Cntrl8", "Cntrl9"),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "Total Population", "Poplation Density", 
                                 "Unemployment Rate", "Lagged Unemployment Rate", "Log Earnings", "Log Employment", 
                                 "Lagged Log Employment", "MSA Indicator", "DiD Estimator"),
            dep.var.labels = c(rep(gsub("_", " ", ""),9))
            ,
            out = paste0(LATEX, x, "_final_var.html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
  )
  
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))
  
})

## 10.2 Final Assesment with weighted regression --------------------------------

lapply(outcome_var, function (x) {
  # x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + cnty_pop | state | 0 | state")), data = main, weights =  1/main$cnty_pop)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + pop_density | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_ur | state | 0 | state")), data = main, weights =  1/main$cnty_pop)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + log_earnings | state | 0 | state")), data = main, weights =  1/main$cnty_pop)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + log_emp | state | 0 | state")), data = main, weights =  1/main$cnty_pop)
  did8 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_log_emp | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did9 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + d_msa | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, did8, did9,
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6", "Cntrl8", "Cntrl9"),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "Total Population", "Poplation Density", 
                                 "Unemployment Rate", "Lagged Unemployment Rate", "Log Earnings", "Log Employment", 
                                 "Lagged Log Employment", "MSA Indicator", "DiD Estimator"),
            dep.var.labels = c(rep(gsub("_", " ", ""),9))
            ,
            out = paste0(LATEX, x, "_final_var_weighted.html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
  )
  
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))
  
})

## 10.3 Assesing different specifications of weighted regressions --------------

# Setting 8 & 9 seem to be good
lapply(outcome_var, function (x) {
  # x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur + log_earnings + log_emp| state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur + log_earnings + log_emp + d_msa| state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_ur + log_earnings + log_emp | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_ur + log_earnings + log_emp + d_msa| state | 0 | state")), data = main, weights =  1/main$cnty_pop)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur + log_emp| state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur + log_emp + d_msa| state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did8 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_ur + log_earnings | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did9 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_ur + log_earnings + d_msa| state | 0 | state")), data = main, weights =  1/main$cnty_pop)
  did10 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur + lag_ur + log_earnings + log_emp + d_msa| state | 0 | state")), data = main, weights =  1/main$cnty_pop)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, did8, did9, did10,
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6", "Cntrl8", "Cntrl9", "Cntrl10"),
            # covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "Unemployment Rate", "Lagged Unemployment Rate", "Log Earnings", "Log Employment", 
            #                       "MSA Indicator", "DiD Estimator"),
            dep.var.labels = c(rep(gsub("_", " ", ""),10))
            ,
            out = paste0(LATEX, x, "_final_combintations.html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
  )
  
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))
  
})

# Try a few more specification and add 8 & 9 from above
lapply(outcome_var, function (x) {
  x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur + log_earnings | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur + log_earnings + d_msa| state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_ur + log_earnings  | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_ur + log_earnings + d_msa| state | 0 | state")), data = main, weights =  1/main$cnty_pop)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + log_emp + log_earnings | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did7 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + log_emp + log_earnings + d_msa| state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did8 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_ur + log_earnings | state | 0 | state")), data = main, weights = 1/main$cnty_pop)
  did9 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + lag_ur + log_earnings + d_msa| state | 0 | state")), data = main, weights =  1/main$cnty_pop)
  did10 <- felm(as.formula(paste0(x,  "~" , "d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur + lag_ur + log_earnings + log_emp + d_msa| state | 0 | state")), data = main, weights =  1/main$cnty_pop)
  
  stargazer(did1, did2, did3, did4, did5, did6, did7, did8, did9, did10,
            type = "text",
            title = paste0("Results: ", gsub("_", " ", x)),
            column.labels = c("Base", "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5", "Cntrl 6", "Cntrl8", "Cntrl9", "Cntrl10"),
            # covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "Unemployment Rate", "Lagged Unemployment Rate", "Log Earnings", "Log Employment", 
            #                      "MSA Indicator", "DiD Estimator"),
            dep.var.labels = c(rep(gsub("_", " ", ""),10))
            ,
            out = paste0(LATEX, x, "_final_combset.html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
  )
  
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))
  
})

confint(did1)

# 11. Calculate the ATT ========================================================

formula_base <- as.formula(lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | state | 0 | state)
formula_ur <- as.formula(lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator  + ur + log_earnings| state | 0 | state)
formula_ur_msa <- as.formula(lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator  + ur + log_earnings + d_msa| state | 0 | state)
formula_lagur <- as.formula(lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator  + lag_ur + log_earnings + d_msa| state | 0 | state)
formula_lagur_msa <- as.formula(lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator  + lag_ur + log_earnings + d_msa| state | 0 | state)

# Change in stratgy: two functions
# One for which the isolated effects are calculated for the period after the Great Recession
# One for which I calculate the effects on after one 
# in the end I only have to append them correctly




results <- SPECIFICATE(data = df_base, reg = formula_ur, reference_yr = 2007, max_yr = 2015, anticipation = 1)


# Function to calculate post-treatment effects 
data <- df_base
reference_yr <- 2007
anticipation <- 0
i <- 2009
min_yr <- 2004
max_yr <- 2007
# period <- 2005
covx <-  c("ur + log_earnings")



preresults <- PRETREATMENTATE(data = df_base, min_yr = 2004, max_yr = 2007, covx = c("ur + log_earnings"))

