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

# 0. Import the main datasets ==================================================

main_allfin_data <- LOAD(dfinput = "main_allfin_data")
main_banks_data <- LOAD(dfinput = "main_banks_data")
load(paste0(TEMP, "/", "fips_data.rda"))
setDT(main_banks_data)

# 1. Analyse the attrition of counties =========================================

# Check the number of counties of the united states
state_counties <- fips_data[, .N, by = .(state_code)]

setnames(main_banks_data, old = "state.x", new = "state")

# Number of counties per state in the main dataset with all commercial banks
banks_state_cnty <- main_banks_data[, c("fips", "state")]
banks_state_cnty <- unique(banks_state_cnty)
banks_state_cnty <- banks_state_cnty[, .N , by = state]

# List of state code with the corresponding state name
state_name <- fips_data[, c("state_name", "state_code")]
state_name <- unique(state_name)

# Visualized the attrition of the counties lost by only including states that 
# are observed over the whole period
attrition_county <- ggplot() +
  # First geom_bar for state_counties dataset
  geom_bar(data = state_counties, aes(x = state_code, y = N), stat = "identity", fill = "red", alpha = 0.7) +
  
  # Second geom_bar for banks_dstate dataset
  geom_bar(data = banks_state_cnty, aes(x = state, y = N), stat = "identity", fill = "blue", alpha = 0.5, position = "dodge") +
  theme_minimal() +
  labs(title = "Number of Counties by State", x = "State", y = "Number of Counties") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  scale_x_discrete() +  # Ensure all x-axis labels are displayed
  scale_y_continuous(breaks = seq(0, max(state_counties$N), by = 25))

filename <- paste0("mean_value_plot_", i, ".png")
ggsave(filename = paste0(FIGURE, "attrition_county",".png") , plot = attrition_county, width = 4, height = 4)


n_cnty <- sum(banks_state_cnty$N)
n <- sum(state_counties$N)

# Attrition:
sh_attrition <- (n - n_cnty) / n
abs_attrition <- n - n_cnty

# 2. Check the distribution of treatment and control group =====================

# Get the relevant variable and go to county-level (ignore years for this analysis)
check_dist <- main_banks_data[, c("fips", "d_mean_all", "d_median_all", "d_marketdef_all", "d_qu70_all")]
check_dist <- unique(check_dist, by = "fips")

# Mean:
# Treated counties: 1092 and Control counties: 1867
# -> the unbalance results from the easily influenced mean 
table(check_dist$d_mean_all)

# Median:
# Treated counties: 1462 and Control counties: 1497
# -> slightly more counties were lost on the side of treated counties
table(check_dist$d_median_all)

# Market Definition of High Market Concentration:
# Treated counties: 1682 and Control counties: 1277
table(check_dist$d_marketdef_all)

# Above the 70%-quantile:
# Treated counties: 2101  and Control Counties: 858 
table(check_dist$d_qu70_all)

# 3. Check the Mean of variables and see how they evolve for treatment and control group

# Median
check_pta <- main_banks_data[, .(mean_loan = mean(total_amount_loan)), by = .(d_median_all, year)]


ggplot(check_pta, aes(x = year, y = mean_loan, color = factor(d_median_all), group = d_median_all)) +
  geom_line(size = 1) +  # Line plot
  geom_point() +         # Add points to the line
  labs(title = "Mean Loan by Year and d_median_all",
       x = "Year", y = "Mean Loan",
       color = "d_median_all") +
  theme_minimal()

# Weight loan by population size
check_wtd <- main_banks_data[, state_pop := sum(cnty_pop), by = state]
check_wtd <- check_wtd[, wt_pop_cnty := cnty_pop / state_pop ]
check_wtd <- check_wtd[, wdt_tot_loan := total_amount_loan * wt_pop_cnty]

check_wtd <- main_banks_data[, .(mean_loan = mean(wdt_tot_loan)), by = .(d_median_all, year)]

ggplot(check_wtd, aes(x = year, y = mean_loan, color = factor(d_median_all), group = d_median_all)) +
  geom_line(size = 1) +  # Line plot
  geom_point() +         # Add points to the line
  labs(title = "Mean Loan by Year and d_median_all",
       x = "Year", y = "Mean Loan",
       color = "d_median_all") +
  theme_minimal()

#    

vars <- c("hhi", "total_amount_loan", "cnty_pop", "ur", "mean_earning", "mean_emp")  # List of variables to summarize

# Loop over each variable using lapply
descriptive_stats_list <- lapply(vars, function(var) {
  main_banks_data[, .(
    count = .N,
    mean_value = as.numeric(mean(get(var), na.rm = TRUE)),   # Ensure consistency as numeric
    median_value = as.numeric(median(get(var), na.rm = TRUE)),
    sd_value = as.numeric(sd(get(var), na.rm = TRUE))
  ), by = .(year, d_median_all)]
})

# Naming the list elements for better clarity
names(descriptive_stats_list) <- vars


mean_value_plots <- lapply(names(descriptive_stats_list), function(varname) {
  # varname <-  "ur"
  # Subset data for d_median_all == 1 and d_median_all == 0
  data_1 <- descriptive_stats_list[[varname]][d_median_all == 1]
  data_0 <- descriptive_stats_list[[varname]][d_median_all == 0]
  
  # Create a ggplot object to plot the mean_value over time
  plot <- ggplot() +
    geom_line(data = data_1, aes(x = year, y = mean_value), color = "blue") +
    geom_line(data = data_0, aes(x = year, y = mean_value), color = "red") +
    ggtitle(paste("Mean of", varname, "over time")) +
    labs(x = "Year", y = "Mean Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_continuous(breaks = unique(descriptive_stats_list[[varname]]$year)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_minimal()
  
  # Return the plot
  return(plot)
})

for (plot in mean_value_plots) {
  print(plot)
}

# Loop through each plot in the mean_value_plots list
for (i in seq_along(mean_value_plots)) {
  # Get the plot object
  plot <- mean_value_plots[[i]]
  
  # Create a filename for each plot (e.g., "mean_value_plot_1.pdf", "mean_value_plot_2.pdf", etc.)
  filename <- paste0("mean_value_plot_", i, ".png")
  
  # Save the plot using ggsave
  ggsave(filename = paste0(FIGURE, filename), plot = plot, device = "png", width = 10, height = 6)
}

