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

outcome_var <- c("ln_loan_amount", "ln_wtd_loan_amount", "lead_ln_loan_amount", "lead_ln_wtd_loan_amount")

lapply(outcome_var, function (x) {
  # x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator | 0 | 0 | 0 ")), data = main)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator + cnty_pop | 0 | 0 | 0")), data = main)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator + cnty_pop + mean_earning | 0 | 0 | 0")), data = main)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator + cnty_pop + mean_earning + ur | 0 | 0 | 0")), data = main)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp | 0 | 0 | 0")), data = main)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp + d_msa | 0 | 0 | 0")), data = main)
  
  stargazer(did1, did2, did3, did4, did5, did6,
            type = "html",
            title = paste0(x),
            column.labels = c("Base", "Base + FE", "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5"),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "County Pop", "Earnings", "UR", "Employment", "Dummy: MSA", "DiD Estimator"),
            # dep.var.labels = paste0(x),
            out = paste0(LATEX, x, ".html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
            )
  
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))

  })


lapply(outcome_var, function (x) {
  # x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all * d_ffr_indicator | 0 | 0 | 0 ")), data = main)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all * d_ffr_indicator + cnty_pop | 0 | 0 | 0")), data = main)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all * d_ffr_indicator + cnty_pop + mean_earning | 0 | 0 | 0")), data = main)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all * d_ffr_indicator + cnty_pop + mean_earning + ur | 0 | 0 | 0")), data = main)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all * d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp | 0 | 0 | 0")), data = main)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_marketdef_all + d_ffr_indicator + d_marketdef_all * d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp + d_msa | 0 | 0 | 0")), data = main)
  
  stargazer(did1, did2, did3, did4, did5, did6,
            type = "html",
            title = paste0(x),
            column.labels = c("Base", "Base + FE", "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5"),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "County Pop", "Earnings", "UR", "Employment", "Dummy: MSA", "DiD Estimator"),
            # dep.var.labels = paste0(x),
            out = paste0(LATEX, x, "_marketdef", ".html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
  )
  
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))
  
})

lapply(outcome_var, function (x) {
  # x <- outcome_var[1]
  did1 <- felm(as.formula(paste0(x,  "~" , "d_mean_all + d_ffr_indicator + d_mean_all * d_ffr_indicator | 0 | 0 | 0 ")), data = main)
  did2 <- felm(as.formula(paste0(x,  "~" , "d_mean_all + d_ffr_indicator + d_mean_all * d_ffr_indicator + cnty_pop | 0 | 0 | 0")), data = main)
  did3 <- felm(as.formula(paste0(x,  "~" , "d_mean_all + d_ffr_indicator + d_mean_all * d_ffr_indicator + cnty_pop + mean_earning | 0 | 0 | 0")), data = main)
  did4 <- felm(as.formula(paste0(x,  "~" , "d_mean_all + d_ffr_indicator + d_mean_all * d_ffr_indicator + cnty_pop + mean_earning + ur | 0 | 0 | 0")), data = main)
  did5 <- felm(as.formula(paste0(x,  "~" , "d_mean_all + d_ffr_indicator + d_mean_all * d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp | 0 | 0 | 0")), data = main)
  did6 <- felm(as.formula(paste0(x,  "~" , "d_mean_all + d_ffr_indicator + d_mean_all * d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp + d_msa | 0 | 0 | 0")), data = main)
  
  stargazer(did1, did2, did3, did4, did5, did6,
            type = "html",
            title = paste0(x),
            column.labels = c("Base", "Base + FE", "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5"),
            covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "County Pop", "Earnings", "UR", "Employment", "Dummy: MSA", "DiD Estimator"),
            # dep.var.labels = paste0(x),
            out = paste0(LATEX, x, "_mean", ".html")
            # no.space = TRUE,  # Removes extra spaces for better formatting
            # digits = 2       # Rounds coefficients to 2 decimal places
  )
  
  # tinytex::latexmk(paste0(LATEX, x, ".tex"))
  
})


plm(ln_loan_amount ~  d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator, data = main, model = "fd")

did1 <- felm(ln_loan_amount ~  d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator | 0 | 0 | 0, data = main)
did2 <- felm(ln_loan_amount ~  d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator + cnty_pop | 0 | 0 | 0, data = main)
did3 <- felm(ln_loan_amount ~  d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator + cnty_pop + mean_earning | 0 | 0 | 0, data = main)
did4 <- felm(ln_loan_amount ~  d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator + cnty_pop + mean_earning + ur| 0 | 0 | 0, data = main)
did5 <- felm(ln_loan_amount ~  d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp | 0 | 0 | 0, data = main)
did6 <- felm(ln_loan_amount ~  d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp + d_msa| 0 | 0 | 0, data = main)

stargazer(did1, did2, did3, did4, did5, did6,
          type = "text",
          title = "Regression Results",
          column.labels = c("Base", "Base + FE", "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5"),
          covariate.labels = c("Dummy: HHI MC", "Dummy: Before GR", "County Pop", "Earnings", "UR", "Employment", "Dummy: MSA", "DiD Estimator")
          # dep.var.labels = "log loan amount"
          # out = "regression_results.txt",
          # no.space = TRUE,  # Removes extra spaces for better formatting
          # digits = 2       # Rounds coefficients to 2 decimal places
)

stargazer(did_base_fe_ctrl3, type = "text")
summary(did_base_fe_ctrl3)

tdid1 <- felm(log(total_amount_loan) ~ hhi + d_median_all + d_ffr_indicator + d_median_all * hhi + d_ffr_indicator * hhi + d_median_all * d_ffr_indicator * hhi | 0 | 0 | 0, data = main)
tdid2 <- felm(log(total_amount_loan) ~  hhi + d_median_all + d_ffr_indicator + d_median_all * hhi + d_ffr_indicator * hhi + d_median_all * d_ffr_indicator * hhi | fips + year | 0 | 0, data = main)
tdid3 <- felm(log(total_amount_loan) ~  hhi + d_median_all + d_ffr_indicator + d_median_all * hhi + d_ffr_indicator * hhi + d_median_all * d_ffr_indicator * hhi + cnty_pop | fips + year | 0 | 0, data = main)
tdid4 <- felm(log(total_amount_loan) ~  hhi + d_median_all + d_ffr_indicator + d_median_all * hhi + d_ffr_indicator * hhi + d_median_all * d_ffr_indicator * hhi + cnty_pop + mean_earning | fips + year | 0 | 0, data = main)
tdid5 <- felm(log(total_amount_loan) ~  hhi + d_median_all + d_ffr_indicator + d_median_all * hhi + d_ffr_indicator * hhi + d_median_all * d_ffr_indicator * hhi + cnty_pop + mean_earning + ur| fips + year | 0 | 0, data = main)
tdid6 <- felm(log(total_amount_loan) ~  hhi + d_median_all + d_ffr_indicator + d_median_all * hhi + d_ffr_indicator * hhi + d_median_all * d_ffr_indicator * hhi + cnty_pop + mean_earning + ur + mean_emp | fips + year | 0 | 0, data = main)
tdid7 <- felm(log(total_amount_loan) ~  hhi + d_median_all + d_ffr_indicator + d_median_all * hhi + d_ffr_indicator * hhi + d_median_all * d_ffr_indicator * hhi + cnty_pop + mean_earning + ur + mean_emp + d_msa| fips + year | 0 | 0, data = main)

lm(d_median_all ~ factor(year), data = main)
lm(d_median_all ~ factor(fips), data = main)
stargazer(tdid1, tdid2, tdid3, tdid4, tdid5, tdid6, tdid7,
          type = "text",
          title = "Regression Results",
          column.labels = c("Base", "Base + FE", "Cntrl 1",  "Cntrl 2",  "Cntrl 3", "Cntrl 4", "Cntrl 5"),
          covariate.labels = c( "HHI",,"Dummy: HHI MC", "Dummy: Before GR", "County Pop", "Earnings", "UR", "Employment", "Dummy: MSA", "I:Median, HHI", "I:HHI, Before GR", "I: Median, Before GR", "Triple DiD Estimator")
          # dep.var.labels = "log loan amount"
          # out = "regression_results.txt",
          # no.space = TRUE,  # Removes extra spaces for better formatting
          # digits = 2       # Rounds coefficients to 2 decimal places
)



summary()
summary(res)
