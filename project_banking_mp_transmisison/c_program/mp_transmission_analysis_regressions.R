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
setDT(df_base)
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

# Different formulas
formula_base <- as.formula(lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator | state | 0 | state)
formula_ur <- as.formula(lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator + ur + log_earnings| state | 0 | state)
formula_ur_msa <- as.formula(lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator  + ur + log_earnings + d_msa| state | 0 | state)

formula_list <- list(formula_base, formula_ur, formula_ur_msa)

names(formula_list) <- c("formula_base", "formula_ur", "formula_ur_msa")

# Different additional covariate settings for pre-treatment effects
cov_base <- c("")
cov_ur <- c("+ ur + log_earnings")
cov_ur_msa <- c("+ ur + log_earnings + d_msa")

# Combine all covariate settings into a list
covariate_list <- list(cov_base, cov_ur, cov_ur_msa)

# Assign names for readability in results
names(covariate_list) <- c("cov_base", "cov_ur", "cov_ur_msa")

# Define the range of anticipation values
anticipation_values <- 0:2

# Loop over anticipation values and store combined results with names
combined_results_all <- setNames(
  lapply(anticipation_values, function(anticipation) {
    
    # Run pre-treatment analysis
    results_pre <- lapply(covariate_list, PRETREATMENTATE, data = df_base, 
                          min_yr = 2005, max_yr = 2007, anticipation = anticipation)
    
    # Run post-treatment analysis
    results_post <- lapply(formula_list, SPECIFICATE, data = df_base, 
                           reference_yr = 2007, max_yr = 2012, anticipation = anticipation)
    
    # Combine pre and post results for each covariate/formula setting
    combined_results <- map2(results_pre, results_post, bind_rows)
    
    # Return combined results for this anticipation level
    combined_results
  }),
  paste0("anticipation_", anticipation_values)
)

# Saving list element into more accessible object
anticipation_0 <- combined_results_all$anticipation_0
anticipation_1 <- combined_results_all$anticipation_1
anticipation_2 <- combined_results_all$anticipation_2

ggdid <- function (dfx) {

  # Formatting a Graph for the Average Treatment Effect by Period
  plot <- ggplot(data = dfx, aes(x = year, y = att)) +
    geom_point(color = "blue") +                         
    geom_line(color = "blue", size = 0.7) +                          
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 0.2, color = "red") +
    labs(x = "Year",
        y = "Average Treatment Effect",
        title = "Average Treatment Effect",
        subtitle = paste0("Anticipation of Event: ", unique(na.omit(dfx$anticipation)) ," years")
        ) +
    scale_x_continuous(breaks = seq(min(dfx$year), max(dfx$year), by = 1)) +
    scale_y_continuous(breaks = seq(-0.25, 0.25, by = .05), limits = c(-0.25, 0.25)) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(size = 12, face = "bold"),
      legend.position = "bottom",
      plot.title.position = "plot" 
    ) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
    geom_vline(xintercept = 2007.5, linetype = "dashed", color = "black") +
    theme_minimal()
  
  # Return plot
  return(plot)
}

# Creating Graphs
gg_anticp0 <- ggdid(dfx = anticipation_0$cov_base)
gg_anticp1 <- ggdid(dfx = anticipation_1$cov_ur)

# Saving as pdf
pdf(paste0(FIGURE, "did_graph_full_specifciation.pdf"), width = 14, height = 7)
grid.arrange(gg_anticp0, gg_anticp1, ncol = 2)
dev.off()

## 12. Final specification with a formatted stargazer table ====================

df_antcp0 <- df_base[inrange(year, 2007, 2010)]
df_antcp1 <- df_base[inrange(year, 2006, 2010)]

base_formel <- c("lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator")

did1 <- felm(as.formula(paste0(base_formel, " | state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)
did2 <- felm(as.formula(paste0(base_formel, " + ur + log_earnings | state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)
did3 <- felm(as.formula(paste0(base_formel, " + ur + log_earnings + d_msa| state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)

did4 <- felm(as.formula(paste0(base_formel, " | state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)
did5 <- felm(as.formula(paste0(base_formel, " + ur + log_earnings | state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)
did6 <- felm(as.formula(paste0(base_formel, " + ur + log_earnings + d_msa| state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)

# Stargazer table
stargazer(did1, did2, did3, did4, did5, did6,
          type = "text",
          digits = 3,
          title = "Regression Results: Leading Log Loan Amount",
          column.labels = c("Anticipation: 0 Years", "Anticipation: 1 Year"),  # Headers for the first and last three columns
          column.separate = c(3, 3), 
          dep.var.labels.include = FALSE,        # Exclude automatic dependent variable label
          model.names = FALSE,                   # Exclude model names
          covariate.labels = c("Dummy: Market Concentration", "Dummy: Great Recession",
                               "Unemployment Rate", "Log Earnings", "Dummy: MSA", "DiD Estimator"),
          add.lines = list(
            c("State FE:", "True", "True", "True", "True", "True", "True"),
            c("Clustered SE on State-Level:", "True", "True", "True", "True", "True", "True")# Custom row for dependent variable
          ),               # Exclude model names
          omit.stat = c("LL", "ser", "f", "rsq"),
          no.space = FALSE,
          out = paste0(LATEX, "regression_main_results.tex") 
          )

# 13. Placebo in Post-Treatment Period =========================================

# Restrict Period according to anticipation period & three periods after treatment
df_antcp0 <- df_base[inrange(year, 2013, 2016)]
df_antcp1 <- df_base[inrange(year, 2012, 2016)]

# Placebo Formula
placebo_formel <- c("lead_ln_loan_amount ~ d_median_all_pre + d_placebo_2014 + d_median_all_pre:d_placebo_2014")

# Regression for no anticipation
did7 <- felm(as.formula(paste0(placebo_formel, " | state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)
did8 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings | state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)
did9 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings + d_msa| state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)

# Regression for one year of anticipation
did10 <- felm(as.formula(paste0(placebo_formel, " | state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)
did11 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings | state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)
did12 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings + d_msa| state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)

# Stargazer table
stargazer(did7, did8, did9, did10, did11, did12,
          type = "text",
          digits = 3,
          title = "Placebo Results for Post Treatment Period: Leading Log Loan Amount",
          column.labels = c("Anticipation: 0 Years", "Anticipation: 1 Year"),  # Headers for the first and last three columns
          column.separate = c(3, 3), 
          dep.var.labels.include = FALSE,        # Exclude automatic dependent variable label
          model.names = FALSE,                   # Exclude model names
          covariate.labels = c("Dummy: Market Concentration", "Dummy: Placebo Treatment 2014",
                               "Unemployment Rate", "Log Earnings", "Dummy: MSA", "DiD Estimator"),
          add.lines = list(
            c("State FE:", "True", "True", "True", "True", "True", "True"),
            c("Clustered SE on State-Level:", "True", "True", "True", "True", "True", "True")# Custom row for dependent variable
          ),               # Exclude model names
          omit.stat = c("LL", "ser", "f", "rsq"),
          no.space = FALSE,
          out = paste0(LATEX, "regression_placebo_post.tex") 
)


# 14. Placebo in Pre-Treatment Period ==========================================

# Restrict Period according to anticipation period & three periods after treatment
df_antcp0 <- df_base[inrange(year, 2003, 2006)]
df_antcp1 <- df_base[inrange(year, 2002, 2006)]

# Placebo Formula
placebo_formel <- c("lead_ln_loan_amount ~ d_median_all_pre + d_placebo_2004 + d_median_all_pre:d_placebo_2004")

# Regression for no anticipation
did13 <- felm(as.formula(paste0(placebo_formel, " | state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)
did14 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings | state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)
did15 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings + d_msa| state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)

# Regression for one year of anticipation
did16 <- felm(as.formula(paste0(placebo_formel, " | state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)
did17 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings | state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)
did18 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings + d_msa| state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)

# Stargazer table
stargazer(did13, did14, did15, did16, did17, did18,
          type = "text",
          digits = 3,
          title = "Placebo Results for Pre Treatment Period: Leading Log Loan Amount",
          column.labels = c("Anticipation: 0 Years", "Anticipation: 1 Year"),  # Headers for the first and last three columns
          column.separate = c(3, 3), 
          dep.var.labels.include = FALSE,        # Exclude automatic dependent variable label
          model.names = FALSE,                   # Exclude model names
          covariate.labels = c("Dummy: Market Concentration", "Dummy: Placebo Treatment 2004",
                               "Unemployment Rate", "Log Earnings", "Dummy: MSA", "DiD Estimator"),
          add.lines = list(
            c("State FE:", "True", "True", "True", "True", "True", "True"),
            c("Clustered SE on State-Level:", "True", "True", "True", "True", "True", "True")# Custom row for dependent variable
          ),               # Exclude model names
          omit.stat = c("LL", "ser", "f", "rsq"),
          no.space = FALSE,
          out = paste0(LATEX, "regression_placebo_pre.tex")
)



