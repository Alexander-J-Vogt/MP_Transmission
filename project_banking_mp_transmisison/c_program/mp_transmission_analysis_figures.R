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

# 1. Plot FFR Over Time [SAVED] ================================================

# Load data on federal fund rate
ffr_monthly <- LOAD(dfinput = "mp_transmission_databasics_ffr")
ffr_annual <- LOAD(dfinput = "ffr_annual")
setDT(ffr_annual)
setDT(ffr_monthly)

# Select relevant variables
ffr_annual <- ffr_annual[, c("year", "ffr_mean")]

# Merge annual and montly data
ffr_main <- merge(ffr_monthly, ffr_annual, by = c("year"))
ffr_main <- ffr_main[inrange(year, 2002, 2016)]

# Display evolution of monthly and annual federal funds rate
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
if (PRODUCE_FIGS) {
ggsave(filename = paste0(FIGURE, "graph_ffr.pdf"), plot = graph_ffr, device = "pdf", width = 10, height = 6)
}


# 2. GDP Time Line [NOT SAVED] =================================================

# Load data on GDP of U.S.
gdp_data <- LOAD(dfinput = "mp_transmission_databasics_gdp")
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
main <- LOAD(dfinput = "mp_transmission_main")
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
main <- LOAD(dfinput = "mp_transmission_main")
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



# 5. Analyse the attrition of counties [SAVED] =================================

# Load main dataset
main_banks_data <- LOAD(dfinput = "mp_transmission_main")
setDT(main_banks_data)

# Load dataset on all counties by fips
# load(paste0(TEMP, "/", "fips_data.rda"))
fips_data <- LOAD(dfinput = "mp_transmission_databasics_fips")
setDT(fips_data)

# Check the number of counties of the united states
state_counties <- fips_data[, .N, by = .(state_code)]

# Number of counties per state in the main dataset with all commercial banks
banks_state_cnty <- main_banks_data[, c("fips", "state")]
banks_state_cnty <- unique(banks_state_cnty)
banks_state_cnty <- banks_state_cnty[, .N , by = state]

# List of state code with the corresponding state name
state_name <- fips_data[, c("state_name", "state_code")]
state_name <- unique(state_name)

# Visualize the attrition of the counties lost by only including states that 
# are observed over the whole period
attrition_county <- ggplot() +
  # First geom_bar for state_counties dataset
  geom_bar(data = state_counties, aes(x = state_code, y = N), stat = "identity", fill = "red", alpha = 1) +
  
  # Second geom_bar for banks_dstate dataset
  geom_bar(data = banks_state_cnty, aes(x = state, y = N), stat = "identity", fill = "blue", alpha = 1, position = "dodge") +
  theme_minimal() +
  labs(title = "Number of Counties by State", x = "State", y = "Number of Counties") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  scale_x_discrete() +  # Ensure all x-axis labels are displayed
  scale_y_continuous(breaks = seq(0, max(state_counties$N), by = 25))

# save plot as pdf
if (PRODUCE_FIGS) {
ggsave(filename = paste0(FIGURE, "attrition_county",".png") , plot = attrition_county, width = 4, height = 4)
}

# Attrition in Numbers
n_cnty <- sum(banks_state_cnty$N)
n <- sum(state_counties$N)

# Attrition:
sh_attrition <- (n - n_cnty) / n
abs_attrition <- n - n_cnty


# 6. Plotting Marketshare of Top 5 Banks over time [SAVE] ======================

# load dataset
raw_sod <- LOAD(dfinput = "raw_sod")
setDT(raw_sod)

# Market power of commercial banks
raw_sod <- raw_sod[, .(depsumbank = sum(depsumcnty)), by = .(year, rssdid)]
raw_sod <- raw_sod[, tot_marketvalue_yearly  := as.numeric(sum(depsumbank)), by = year]
raw_sod <- raw_sod[, marketshare_yearly := depsumbank / tot_marketvalue_yearly]

# Filter data
raw_sod <- raw_sod[marketshare_yearly > 0.01]

# Plotting market share density for each year for all banks with more tha 1% marketshare
ggplot(raw_sod, aes(x = marketshare_yearly, color = factor(year))) +
  geom_density(linewidth = 0.9) +
  # scale_color_manual(values = rep("black", length(unique(raw_sod$year)))) +
  labs(x = "Variable", y = "Density", color = "Year") +
  theme_minimal()

# Determine top 5 banks
top_banks <- raw_sod %>%
  group_by(year) %>%
  arrange(desc(marketshare_yearly)) %>%
  slice_head(n = 5) %>%
  ungroup()

# Sum up marketshare of the top 5 banks
marketshare <- top_banks |>
  group_by(year) |> 
  mutate(tot_market_share = sum( marketshare_yearly)) |> 
  distinct(year, tot_market_share)

# Plotting the sum of market share of the top 5 banks
graph_marketshare <- ggplot(data = marketshare, aes(x = year, y = tot_market_share)) +
  geom_line(color = "blue", size = 1) +  # Line color and thickness
  geom_point(color = "blue", size = 2) +  # Point color and size
  labs(
    title = "Total Market Share Over Time",
    x = "Year",
    y = "Total Market Share (in %)"
  ) +
  theme_minimal() +  # Minimal theme for a clean look
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center title, increase size, bold
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),  # X-axis label formatting
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),  # Y-axis label formatting
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  ) +
  scale_x_continuous(breaks = seq(min(marketshare$year), max(marketshare$year), by = 1)) +  # Set x-axis breaks for each year
  scale_y_continuous(limits = c(0.2, 0.5),
                     breaks = seq(0.2, 0.5, by = 0.05),
                     labels = scales::percent_format(scale = 1)) 

# Save as pdf
if (PRODUCE_FIGS) {
  ggsave(filename = paste0(FIGURE, "graph_marketshare.pdf"), plot = graph_marketshare, device = "pdf", width = 10, height = 6)
}

########################### ENDE ##############################################+