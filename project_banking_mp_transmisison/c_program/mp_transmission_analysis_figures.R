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

# 1. Plot FFR Over Time [NOT SAVED] ============================================

# Load data on federal fund rate
ffr_monthly <- LOAD(dfinput = "ffr")
ffr_annual <- LOAD(dfinput = "ffr_annual")
setDT(ffr_annual)
setDT(ffr_monthly)

# Select relevant variables
ffr_annual <- ffr_annual[, c("year", "ffr_mean")]

# Merge annual and montly data
ffr_main <- merge(ffr_monthly, ffr_annual, by = c("year"))
ffr_main <- ffr_main[inrange(year, 2002, 2016)]

# Display evoluation of monthly and annual federal funds rate
graph_ffr <- ggplot() +
  # Plotting the mean federal funds rate with a red line
  geom_line(data = ffr_main, aes(x = date, y = ffr_mean), color = "red", size = 1.1, linetype = "dashed") +
  
  # Plotting the actual federal funds rate with a blue line
  geom_line(data = ffr_main, aes(x = date, y = ffr), color = "blue", size = 1.1) +
  
  # Adding a vertical line for December 2008
  geom_vline(xintercept = as.Date("2007-12-01"), linetype = "dotdash", color = "black", size = 1) +
  
  # Applying a minimal theme for a clean look
  theme_minimal() +
  
  # Adding labels for the axes
  labs(
    title = "Federal Funds Rate Over Time",
    subtitle = "Comparison between Actual Rate and Mean Rate",
    x = "Date",
    y = "Federal Funds Rate",
    color = "Legend"
  ) +
  
  # Adding a legend to distinguish between the lines
  scale_color_manual(
    values = c("Actual Rate" = "blue", "Mean Rate" = "red")
  ) +
  # Formatting the date axis for better readability
  scale_x_date(date_labels = "%b %Y", date_breaks = "12 months") +
  
  # Customizing the theme for better aesthetics
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

# Save as pdf
ggsave(filename = paste0(FIGURE, "graph_ffr.png"), plot = graph_ffr, device = "png", width = 10, height = 6)


# 2. GDP Time Line [NOT SAVED] =================================================

# Load data on GDP of U.S.
gdp_data <- LOAD(dfinput = "gdp_data")
setDT(gdp_data)

# Restrict and format variables
gdp_data <- gdp_data[, year := as.numeric(year)]
gdp_data <- gdp_data[year >= 1995 & year <= 2020]

# Simple plot that is not getting exported
ggplot() +
  
  # Plotting gdp
  geom_line(data = gdp_data, aes(year, gdp_growth), size = 1) +
  
  # use minimal design
  theme_minimal() +
  
  # Adjust x- and y axid
  scale_y_continuous(breaks = seq(-2, 7, by = 1)) +
  scale_x_continuous(breaks = seq(min(gdp_data$year), max(gdp_data$year), by = 1)) +
  
  # Adding labels for the axes
  labs(
    title = "GDP Growth Rate in the U.S.",
    x = "Year",
    y = "GDP Growth Rate",
    color = "Legend"
  )  +
  
  # Customizing the theme for better aesthetics
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )


# 3. Plot Market Concentration over time [NOT SAVED] ===========================

# Load data 
main <- LOAD(dfinput = "main_allfin_data")
setDT(main)

# Restrict dataset and format variables
main <- main[, c("fips", "year", "hhi")]
main <- main[year %in% c(2004, 2015)]
main <- main[, year := as.factor(year)]

# Plot density of HH in order to evaluate how market concentration is evolving
# over time
ggplot() +
  
  # PLot density of hhi
  geom_density(data = main, aes(x = hhi, group = year, color = year), alpha = 0.5, size = 1) +  # Density plot for dt
  
  # Vertical line at critical value of HHI = 2500
  geom_vline(xintercept = 2500, linetype = "dotdash", color = "black", size = 1) +  # Vertical line at 2500
  
  # Label graph
  labs(
    title = "Density Plot of HHI by Year",
    x = "Herfindahl-Hirschman Index (HHI)",
    y = "Density",
    color = "Year",
    fill = "Year"
  ) +
  
  # Adjust x- and y-axis
  scale_x_continuous(breaks = seq(0, max(main$hhi, na.rm = TRUE), by = 1000)) +  # Set x-axis breaks of 1000
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # Better scaling for y-axis
  
  # Add minimal theme
  theme_minimal() +
  
  # Format labels
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )


# 4. Plot Mortgage Loan Amount [NOT SAVED] =====================================

# Load data
main <- LOAD(dfinput = "main_banks_data")
setDT(main)

# Restrict data to relevant variables
mortgage <- main[, c("fips","year", "d_median_all_pre", "total_amount_loan", "ln_loan_amount")]

# Calculate mean loan amount by year 
mortgage <- mortgage[, .(tot_mean = mean(total_amount_loan),
                         ln_mean = mean(ln_loan_amount)),
                      by = .(year, d_median_all_pre)]

# Rescale variable to 1000s
mortgage <- mortgage[, tot_mean_000s := tot_mean / 1000]

# Determine the tot_mean in the year 2002 for treatment and control group
tot_mean_2002_0 <- mortgage[year == 2002 & d_median_all_pre == 0, tot_mean]
tot_mean_2002_1 <- mortgage[year == 2002 & d_median_all_pre == 1, tot_mean]

# Index tot_mean
mortgage <- mortgage[, mean_index:= ifelse(d_median_all_pre == 0, tot_mean / tot_mean_2002_0, tot_mean / tot_mean_2002_1)]

# Evaluate the indexed mortgage loan amount 
ggplot(data = mortgage ) +
  
  # Draw line for indexed tot_mean by group
  geom_line(aes(year, mean_index, color = factor(d_median_all_pre)), size = 0.7) +
  
  
  # Scatterplot for indexed tot_mean by group
  geom_point(aes(year, mean_index, color = factor(d_median_all_pre))) +
  
  # label graph
  labs(
    color = "Group",                        
    x = "Year",                             
    y = "Mean Index"                        
  ) + 
  
  # Adjust Legend
  scale_color_manual(
    values = c("0" = "blue", "1" = "red"), # Define specific colors for each group
    labels = c("Control Group", "Treatment Group")   # Customize legend labels
  ) +
  
  # Minimal Design
  theme_minimal() +
  
  # Adjust labels size
  theme(
    legend.position = "right",              # Customize legend position (e.g., right, top, bottom, left)
    legend.title = element_text(size = 12, face = "bold"), # Customize legend title font
    legend.text = element_text(size = 10)   # Customize legend text font
  )

########################### ENDE ##############################################+