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


did_base <- felm(log(total_amount_loan) ~  d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator | 0 | 0 | 0, data = main)
did_base_fe <- felm(log(total_amount_loan) ~  1 + d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator | state | 0 | 0, data = main)
did_base_fe_ctrl1 <- felm(log(total_amount_loan) ~  d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator + cnty_pop | state | 0 | 0, data = main)
did_base_fe_ctrl2 <- felm(log(total_amount_loan) ~  d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator + cnty_pop + mean_earning | state | 0 | 0, data = main)
did_base_fe_ctrl3 <- felm(log(total_amount_loan) ~  d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator + cnty_pop + mean_earning + ur| state | 0 | 0, data = main)
did_base_fe_ctrl4 <- felm(log(total_amount_loan) ~  d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp | state | 0 | 0, data = main)
did_base_fe_ctrl5 <- felm(log(total_amount_loan) ~  d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator + cnty_pop + mean_earning + ur + mean_emp + d_msa| state | 0 | 0, data = main)

stargazer(did_base, did_base_fe, did_base_fe_ctrl1, did_base_fe_ctrl2, did_base_fe_ctrl3, type = "text")

stargazer(did_base, did_base_fe, did_base_fe_ctrl1, did_base_fe_ctrl2, did_base_fe_ctrl3,
          did_base_fe_ctrl4, did_base_fe_ctrl5,
          type = "text",
          title = "Regression Results",
          column.labels = c("Base", "Base + FE", "Base Cntrl 1",  "Base Cntrl 2",  "Base Cntrl 3", "Base Cntrl 4", "Base Cntrl 5"),
          dep.var.labels = "log loan amount",
          # out = "regression_results.txt",
          # no.space = TRUE,  # Removes extra spaces for better formatting
          # digits = 2       # Rounds coefficients to 2 decimal places
          )

stargazer(did_base_fe_ctrl3, type = "text")
summary(did_base_fe_ctrl3)

tripledid_base <- felm(log(total_amount_loan) ~ d_median_all + d_ffr_indicator + d_median_all * hhi + d_ffr_indicator * hhi + d_median_all * d_ffr_indicator * hhi | 0 | 0 | 0, data = main)
tripledid_base_fe <- felm(log(total_amount_loan) ~  d_median_all + d_ffr_indicator + d_median_all * d_ffr_indicator | fips + year | 0 | 0, data = main)
tripledid_base_fe_ctrl1 <- felm(log(total_amount_loan) ~  d_median_all + d_ffr_indicator + d_median_all * hhi + d_ffr_indicator * hhi + d_median_all * d_ffr_indicator * hhi + cnty_pop | fips + year | 0 | 0, data = main)
tripledid_base_fe_ctrl2 <- felm(log(total_amount_loan) ~  d_median_all + d_ffr_indicator + d_median_all * hhi + d_ffr_indicator * hhi + d_median_all * d_ffr_indicator * hhi + cnty_pop + mean_earning | fips + year | 0 | 0, data = main)
tripledid_base_fe_ctrl3 <- felm(log(total_amount_loan) ~  d_median_all + d_ffr_indicator + d_median_all * hhi + d_ffr_indicator * hhi + d_median_all * d_ffr_indicator * hhi + cnty_pop + mean_earning + ur| fips + year | 0 | 0, data = main)
tripledid_base_fe_ctrl4 <- felm(log(total_amount_loan) ~  d_median_all + d_ffr_indicator + d_median_all * hhi + d_ffr_indicator * hhi + d_median_all * d_ffr_indicator * hhi + cnty_pop + mean_earning + ur + mean_emp | fips + year | 0 | 0, data = main)
tripledid_base_fe_ctrl5 <- felm(log(total_amount_loan) ~  d_median_all + d_ffr_indicator + d_median_all * hhi + d_ffr_indicator * hhi + d_median_all * d_ffr_indicator * hhi + cnty_pop + mean_earning + ur + mean_emp + d_msa| fips + year | 0 | 0, data = main)

summary()
summary(res)
