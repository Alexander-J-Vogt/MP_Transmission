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

# Load main dataset
main_banks_data <- LOAD(dfinput = "main_banks_data")
setDT(main_banks_data)

# Load dataset on all counties by fips
load(paste0(TEMP, "/", "fips_data.rda"))


# 1. Check the distribution of treatment and control group =====================

# Get the relevant variable and go to county-level (ignore years for this analysis)
check_dist <- main_banks_data[, c("fips", "d_mean_all", "d_median_all", "d_marketdef_all", "d_q70_all")]
check_dist <- unique(check_dist, by = "fips")

# Mean:
# Treated counties: 1006 and Control counties: 1764
# -> the unbalance results from the easily influenced mean 
table(check_dist$d_mean_all)

# Median:
# Treated counties: 1337 and Control counties: 1433
# -> slightly more counties were lost on the side of treated counties
table(check_dist$d_median_all)

# Market Definition of High Market Concentration1218
# Treated counties: 1553 and Control counties: 1277
table(check_dist$d_marketdef_all)

# Above the 70%-quantile:
# Treated counties: 792  and Control Counties: 1978 
table(check_dist$d_q70_all)

# 2. Check the Descriptive Statistics ==========================================

# List most important variables
vars <- c("hhi", "total_amount_loan", "cnty_pop", "ur", "mean_earning", "mean_emp")  # List of variables to summarize

# Loop over each variable using lapply and calculate mean statistics
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

# Plot quickly variable over time to get first view on their development
mean_value_plots <- lapply(names(descriptive_stats_list), function(varname) {
  
  # get data by treatment and control group
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

# Print graphs with loop
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


# 3. Descriptive Statistics of yearly data =====================================

# Restrict data to relevant variables
des_stats <- main_banks_data[, c("year", "total_amount_loan", "lead_ln_loan_amount", 
                                 "hhi", "cnty_pop", "pop_density", "mean_earning", "log_earnings",
                                 "mean_emp", "ur", "lag_ur","log_emp")]

# Vector with relevant variable names
update_key_var <- c("lead_ln_loan_amount", "hhi", "cnty_pop", "pop_density", "ur", 
                    "lag_ur", "mean_emp", "log_emp", "mean_earning", "log_earnings")

# Initate empty list
descriptive_stats <- list()

# Get descriptive statistics by variable and year
descriptive_stats <- lapply(update_key_var, function(var) {
  des_stats[, .(
    mean = mean(get(var), na.rm = TRUE),
    median = median(get(var), na.rm = TRUE),
    sd = sd(get(var), na.rm = TRUE),
    min = min(get(var), na.rm = TRUE),
    max = max(get(var), na.rm = TRUE),
    count = .N
  ), by = year]
})

# name element lists
names(descriptive_stats) <- update_key_var

# 4. Correlation of variables by year and pooled [SAVED PLOT] ==================

# Calculate correlation tables for all variables in key_var for each year and pooled correlation
correlation_tables <- lapply(unique(des_stats$year), function(yr) {
  main_year <- des_stats[year == yr, ..update_key_var]
  cor(main_year, use = "complete.obs")
})

# Name the list elements based on year
names(correlation_tables) <- paste0("year_", unique(des_stats$year))

# Pooled correlation table across all years
pooled_correlation <- cor(des_stats[, ..update_key_var],use = "complete.obs")

# Print the pooled correlation table
print(pooled_correlation)

# Optional: Visualize the pooled correlation matrix
png(paste0(FIGURE, "correlation_plot.png"), width = 800, height = 600)
corrplot(pooled_correlation, method = "number", type = "lower", tl.col = "black", tl.srt = 45)
dev.off()

# 5. Density of Earning ========================================================

# Density plot for earnings and log earnings
ggplot() +
  geom_density(data = main_banks_data, aes(x = log(mean_earning)), color = "red")

ggplot() + 
  geom_density(data = main_banks_data, aes(x = mean_earning), color = "blue")



############################### ENDE ##########################################+