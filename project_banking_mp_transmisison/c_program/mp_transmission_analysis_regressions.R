# TARGET: Includes the main regression results & ATE after Callaway & Sant'Anna (2021)
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

# 0. Documentation =============================================================

#' What regression specifications have been tested?
#' 
#' i. The following additional control variables have been tested in different specifications:
#' 
#'    - employment 
#'    - log employment
#'    - change in employment
#'    - earnings not logged
#'    - lagged employment
#'    - lagged log employment
#'    - lagged unemployment rate
#'    - lagged log earnings
#'    - lagged earnings
#'    - employment rate (employment over county population)
#'    - county density
#' 
#' ii. The following alternative dummy variables for treatment and control variables
#' have been tested:
#' 
#'    - d_mean: 1 when HHI of county is greater than median
#'    - d_170: 1 when HHI of county is greater than the 70th percentile
#'    - d_market_definition: 1 when HHI of county is greater than HHI of 2500
#'    
#' iii. Additionally:
#' 
#'    - Specification with demeaned variables by year in order to control for 
#'      unobserved time-variant characteristics
#'
#' Every Specification was tried:
#' 
#'    - with State FE + Cluster SE on State Level 
#'    - without State FE + without Clustered SE on State-level
#'    - without State FE + without Clustered SE on State-level
#'    
#' Final Specification:
#' 
#'  - State FE + Clustered SE on State-level
#'  - Baseline Model: with no control variable
#'  - Control Set 1: Unemployment Rate + log Earnings
#'  - Control Set 2: Unemployment Rate + log Earnings + Dummy MSA


# 01. Load Data =================================================================


# Load Data
df_base <- LOAD(dfinput = "mp_transmission_main")
setDT(df_base)


# 02. Calculate the ATT  [INCLUDED IN PRESENTATION] ============================

# Basic DiD regression
base_formel <- c("lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator")

# Different control set specifications
formula_base <- as.formula(paste0(base_formel, " | state | 0 | state"))
formula_ur <- as.formula(paste0(base_formel, " + ur + log_earnings| state | 0 | state"))
formula_ur_msa <- as.formula(paste0(base_formel, " + ur + log_earnings + d_msa| state | 0 | state"))
formula_ur_bank <- as.formula(paste0(base_formel, " + ur + log_earnings + d_top_bank | state | 0 | state"))
formula_ur_bank_msa <- as.formula(paste0(base_formel, " + ur + log_earnings + d_top_bank + d_msa| state | 0 | state"))

# list of formula names
formula_list <- list(formula_base, formula_ur, formula_ur_msa, formula_ur_bank, formula_ur_bank_msa)

# naming the list according to the formula
names(formula_list) <- c("formula_base", "formula_ur", "formula_ur_msa",
                         "formula_ur_bank", "formula_ur_bank_msa")

# Different additional covariate settings for pre-treatment effects
cov_base <- c("")
cov_ur <- c("+ ur + log_earnings")
cov_ur_msa <- c("+ ur + log_earnings + d_msa")
cov_ur_bank <- c("+ ur + log_earnings + d_top_bank")
cov_ur_msa_bank <- c("+ ur + log_earnings + d_top_bank + d_msa")


# Combine all covariate settings into a list
covariate_list <- list(cov_base, cov_ur, cov_ur_msa, cov_ur_bank, cov_ur_msa_bank)

# Assign names for readability in results
names(covariate_list) <- c("cov_base", "cov_ur", "cov_ur_msa", "cov_ur_bank", "cov_ur_msa_bank")

# Define the range of anticipation values
anticipation_values <- 0:1

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

# Creating Graphs with GGDID (Own Function)
gg_anticp0 <- GGDID(dfx = anticipation_0$cov_ur_msa)
gg_anticp1 <- GGDID(dfx = anticipation_1$cov_ur_msa)

gg_anticp0_bank <- GGDID(dfx = anticipation_0$cov_ur_msa_bank)
gg_anticp1_bank <- GGDID(dfx = anticipation_1$cov_ur_msa_bank)

if (PRODUCE_FIGS) {
  # Saving as pdf - No Bank Indicator
  pdf(paste0(FIGURE, "did_graph_full_specifciation.pdf"), width = 14, height = 7)
  grid.arrange(gg_anticp0, gg_anticp1, ncol = 2)
  dev.off()
  # Saving as pdf - Bank Indicator
  pdf(paste0(FIGURE, "did_graph_bank.pdf"), width = 14, height = 7)
  grid.arrange(gg_anticp0_bank, gg_anticp1_bank, ncol = 2)
  dev.off()
}


# 03. Final specification with a formatted stargazer table  [INCLUDED IN PRESENTATION] ===================

# Restrict Period according to anticipation period & three periods after treatment
df_antcp0 <- df_base[inrange(year, 2007, 2010)]
df_antcp1 <- df_base[inrange(year, 2006, 2010)]


# Regressions without anticipation
did1 <- felm(as.formula(paste0(base_formel, " | state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)
did2 <- felm(as.formula(paste0(base_formel, " + ur + log_earnings | state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)
did3 <- felm(as.formula(paste0(base_formel, " + ur + log_earnings + d_msa| state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)

# Regression with one year of anticipation
did4 <- felm(as.formula(paste0(base_formel, " | state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)
did5 <- felm(as.formula(paste0(base_formel, " + ur + log_earnings | state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)
did6 <- felm(as.formula(paste0(base_formel, " + ur + log_earnings + d_msa| state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)

# Condition to print out figures and tables
output_main_results <- if (PRODUCE_FIGS) paste0(LATEX, "regression_main_results.tex") else NULL

# Stargazer table
stargazer(did1, did2, did3, did4, did5, did6,
          type = "text",
          digits = 3,
          dep.var.caption  = "Dependent Variable: Log One-Year Ahead Mortgage Loan Amount",
          column.labels = c("Anticipation: 0 Years", "Anticipation: 1 Year"),
          column.separate = c(3, 3), 
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          covariate.labels = c("Dummy: Market Concentration", "Dummy: Great Recession",
                               "Unemployment Rate", "Log Earnings", "Dummy: MSA", "DiD Estimator"),
          add.lines = list(
            c("State FE:", "True", "True", "True", "True", "True", "True"),
            c("Clustered SE on State-Level:", "True", "True", "True", "True", "True", "True")
          ),
          omit.stat = c("LL", "ser", "f", "rsq"),
          no.space = FALSE, 
          notes.append = TRUE,
          out = output_main_results
          )


# 04. Placebo in Post-Treatment Period  [INCLUDED IN PRESENTATION] =========================================

# Restrict Period according to anticipation period & three periods after treatment
df_antcp0 <- df_base[inrange(year, 2013, 2016)]
df_antcp1 <- df_base[inrange(year, 2012, 2016)]

# Placebo Formula
placebo_formel <- c("lead_ln_loan_amount ~ d_median_all_pre + d_placebo_2014 + d_median_all_pre:d_placebo_2014")

# Regression without anticipation
did7 <- felm(as.formula(paste0(placebo_formel, " | state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)
did8 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings | state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)
did9 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings + d_msa| state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)

# Regression with one year of anticipation
did10 <- felm(as.formula(paste0(placebo_formel, " | state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)
did11 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings | state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)
did12 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings + d_msa| state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)

# Condition to print out figures and tables
output_placebo_post_results <- if (PRODUCE_FIGS) paste0(LATEX, "regression_placebo_post.tex") else NULL

# Stargazer table
stargazer(did7, did8, did9, did10, did11, did12,
          type = "text",
          digits = 3,
          dep.var.caption  = "Dependent Variable: Log One-Year Ahead Mortgage Loan Amount",
          column.labels = c("Anticipation: 0 Years", "Anticipation: 1 Year"),  
          column.separate = c(3, 3), 
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          covariate.labels = c("Dummy: Market Concentration", "Dummy: Placebo Treatment 2014",
                               "Unemployment Rate", "Log Earnings", "Dummy: MSA", "DiD Estimator"),
          add.lines = list(
            c("State FE:", "True", "True", "True", "True", "True", "True"),
            c("Clustered SE on State-Level:", "True", "True", "True", "True", "True", "True")
          ),
          omit.stat = c("LL", "ser", "f", "rsq"),
          no.space = FALSE,
          notes.append = TRUE,
          out = output_placebo_post_results
          )


# 05. Placebo in Pre-Treatment Period [INCLUDED IN PRESENTATION] ==========================================

# Restrict Period according to anticipation period & three periods after treatment
df_antcp0 <- df_base[inrange(year, 2003, 2006)]
df_antcp1 <- df_base[inrange(year, 2002, 2006)]

# Placebo Formula
placebo_formel <- c("lead_ln_loan_amount ~ d_median_all_pre + d_placebo_2004 + d_median_all_pre:d_placebo_2004")

# Regression without anticipation
did13 <- felm(as.formula(paste0(placebo_formel, " | state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)
did14 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings | state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)
did15 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings + d_msa| state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)

# Regression with one year of anticipation
did16 <- felm(as.formula(paste0(placebo_formel, " | state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)
did17 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings | state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)
did18 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings + d_msa| state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)

# Condition to print out figures and tables
output_placebo_pre_results <- if (PRODUCE_FIGS) paste0(LATEX, "regression_placebo_pre.tex") else NULL

# Stargazer table
stargazer(did13, did14, did15, did16, did17, did18,
          type = "text",
          digits = 3,
          dep.var.caption  = "Dependent Variable: Log One-Year Ahead Mortgage Loan Amount",
          column.labels = c("Anticipation: 0 Years", "Anticipation: 1 Year"),
          column.separate = c(3, 3), 
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          covariate.labels = c("Dummy: Market Concentration", "Dummy: Placebo Treatment 2004",
                               "Unemployment Rate", "Log Earnings", "Dummy: MSA", "DiD Estimator"),
          add.lines = list(
            c("State FE:", "True", "True", "True", "True", "True", "True"),
            c("Clustered SE on State-Level:", "True", "True", "True", "True", "True", "True")
          ),
          omit.stat = c("LL", "ser", "f", "rsq"),
          no.space = FALSE,
          notes.append = TRUE,
          out = output_placebo_pre_results
          )

# 06. Presence of Top 5 Banks in county [INCLUDED IN PRESENTATION] =============

# Controlling if one of the top 5 banks are available in a county 

# Restrict Period according to anticipation period & three periods after treatment
df_antcp0 <- df_base[inrange(year, 2007, 2010)]
df_antcp1 <- df_base[inrange(year, 2006, 2010)]

# Basic DiD regression
base_formel <- c("lead_ln_loan_amount ~ d_median_all_pre + d_ffr_indicator + d_median_all_pre:d_ffr_indicator")

# Regressions without anticipation
did19 <- felm(as.formula(paste0(base_formel, " | state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)
did20<- felm(as.formula(paste0(base_formel, " + ur + log_earnings + d_top_bank | state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)
did21 <- felm(as.formula(paste0(base_formel, " + ur + log_earnings + d_top_bank + d_msa| state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)

# Regressions without anticipation
did22 <- felm(as.formula(paste0(base_formel, " | state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)
did23 <- felm(as.formula(paste0(base_formel, " + ur + log_earnings + d_top_bank | state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)
did24 <- felm(as.formula(paste0(base_formel, " + ur + log_earnings + d_top_bank + d_msa| state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)

# Condition to print out figures and tables
output_robust_results <- if (PRODUCE_FIGS) paste0(LATEX, "regression_robust_results.tex") else NULL

# Stargazer table
stargazer(did19, did20, did21, did22, did23, did24,
          type = "text",
          digits = 3,
          dep.var.caption  = "Dependent Variable: Log One-Year Ahead Mortgage Loan Amount",
          column.labels = c("Anticipation: 0 Years", "Anticipation: 1 Year"),
          column.separate = c(3, 3),
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          covariate.labels = c("Dummy: Market Concentration", "Dummy: Great Recession",
                               "Unemployment Rate", "Log Earnings", "Dummy: Top 5 Bank" ,"Dummy: MSA", "DiD Estimator"),
          add.lines = list(
            c("State FE:", "True", "True", "True", "True", "True", "True"),
            c("Clustered SE on State-Level:", "True", "True", "True", "True", "True", "True")
          ),
          omit.stat = c("LL", "ser", "f", "rsq"),
          no.space = FALSE,
          notes.append = TRUE,
          out = output_robust_results
          )

# 06. Placebo in Post-Treatment Period  [INCLUDED IN PRESENTATION] =========================================
PRODUCE_FIGS <- TRUE
# Restrict Period according to anticipation period & three periods after treatment
df_antcp0 <- df_base[inrange(year, 2013, 2016)]
df_antcp1 <- df_base[inrange(year, 2012, 2016)]

# Placebo Formula
placebo_formel <- c("lead_ln_loan_amount ~ d_median_all_pre + d_placebo_2014 + d_median_all_pre:d_placebo_2014")

# Regression without anticipation
did7 <- felm(as.formula(paste0(placebo_formel, " | state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)
did8 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings + d_top_bank | state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)
did9 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings + d_top_bank + d_msa| state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)

# Regression with one year of anticipation
did10 <- felm(as.formula(paste0(placebo_formel, " | state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)
did11 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings + d_top_bank | state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)
did12 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings + d_top_bank + d_msa| state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)

# Condition to print out figures and tables
output_placebo_post_bank <- if (PRODUCE_FIGS) paste0(LATEX, "regression_placebo_post_bank.tex") else NULL

# Stargazer table
stargazer(did7, did8, did9, did10, did11, did12,
          type = "text",
          digits = 3,
          dep.var.caption  = "Dependent Variable: Log One-Year Ahead Mortgage Loan Amount",
          column.labels = c("Anticipation: 0 Years", "Anticipation: 1 Year"),  
          column.separate = c(3, 3), 
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          covariate.labels = c("Dummy: Market Concentration", "Dummy: Great Recession",
                               "Unemployment Rate", "Log Earnings", "Dummy: Top 5 Bank" ,"Dummy: MSA", "DiD Estimator"),
          add.lines = list(
            c("State FE:", "True", "True", "True", "True", "True", "True"),
            c("Clustered SE on State-Level:", "True", "True", "True", "True", "True", "True")
          ),
          omit.stat = c("LL", "ser", "f", "rsq"),
          no.space = FALSE,
          notes.append = TRUE,
          out = output_placebo_post_bank
)


# 07. Placebo in Pre-Treatment Period [INCLUDED IN PRESENTATION] ==========================================

# Restrict Period according to anticipation period & three periods after treatment
df_antcp0 <- df_base[inrange(year, 2003, 2006)]
df_antcp1 <- df_base[inrange(year, 2002, 2006)]

# Placebo Formula
placebo_formel <- c("lead_ln_loan_amount ~ d_median_all_pre + d_placebo_2004 + d_median_all_pre:d_placebo_2004")

# Regression without anticipation
did13 <- felm(as.formula(paste0(placebo_formel, " | state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)
did14 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings + d_top_bank | state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)
did15 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings + d_top_bank + d_msa| state | 0 | state")), data = df_antcp0, weights = 1/df_antcp0$cnty_pop)

# Regression with one year of anticipation
did16 <- felm(as.formula(paste0(placebo_formel, " | state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)
did17 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings + d_top_bank | state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)
did18 <- felm(as.formula(paste0(placebo_formel, " + ur + log_earnings + d_top_bank + d_msa| state | 0 | state")), data = df_antcp1, weights = 1/df_antcp1$cnty_pop)

# Condition to print out figures and tables
output_placebo_pre_bank <- if (PRODUCE_FIGS) paste0(LATEX, "regression_placebo_pre_bank.tex") else NULL

# Stargazer table
stargazer(did13, did14, did15, did16, did17, did18,
          type = "text",
          digits = 3,
          dep.var.caption  = "Dependent Variable: Log One-Year Ahead Mortgage Loan Amount",
          column.labels = c("Anticipation: 0 Years", "Anticipation: 1 Year"),
          column.separate = c(3, 3), 
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          covariate.labels = c("Dummy: Market Concentration", "Dummy: Great Recession",
                               "Unemployment Rate", "Log Earnings", "Dummy: Top 5 Bank" ,"Dummy: MSA", "DiD Estimator"),
          add.lines = list(
            c("State FE:", "True", "True", "True", "True", "True", "True"),
            c("Clustered SE on State-Level:", "True", "True", "True", "True", "True", "True")
          ),
          omit.stat = c("LL", "ser", "f", "rsq"),
          no.space = FALSE,
          notes.append = TRUE,
          out = output_placebo_pre_bank
)

############################## END ############################################+