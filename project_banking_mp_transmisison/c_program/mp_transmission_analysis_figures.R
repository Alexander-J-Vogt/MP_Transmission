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

# 1. Plot FFR over time ========================================================

ffr_monthly <- LOAD(dfinput = "ffr")
ffr_annual <- LOAD(dfinput = "ffr_annual")
setDT(ffr_annual)
setDT(ffr_monthly)

ffr_annual <- ffr_annual[, c("year", "ffr_mean")]

ffr_main <- merge(ffr_monthly, ffr_annual, by = c("year"))
ffr_main <- ffr_main[year >= 1994]
# Display average 

graph_ffr <- ggplot() +
  # Plotting the mean federal funds rate with a red line
  geom_line(data = ffr_main, aes(x = date, y = ffr_mean), color = "red", size = 1.1, linetype = "dashed") +
  
  # Plotting the actual federal funds rate with a blue line
  geom_line(data = ffr_main, aes(x = date, y = ffr), color = "blue", size = 1.1) +
  
  # Adding a vertical line for December 2008
  geom_vline(xintercept = as.Date("2008-12-01"), linetype = "dotdash", color = "black", size = 1) +
  
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
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  
  # Customizing the theme for better aesthetics
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

ggsave(filename = paste0(FIGURE, "graph_ffr.png"), plot = graph_ffr, device = "png", width = 10, height = 6)


# 2. GDP Time Line =============================================================
gdp_data <- LOAD(dfinput = "gdp_data")
setDT(gdp_data)
gdp_data <- gdp_data[, year := as.numeric(year)]
gdp_data <- gdp_data[year >= 1995 & year <= 2020]

ggplot() +
  geom_line(data = gdp_data, aes(year, gdp_growth), size = 1) +
  theme_minimal() +
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


# 3. Plot Market Conentration over time ========================================

main <- LOAD(dfinput = "main_allfin_data")
hhi_county <- LOAD(dfinput = "main_allfin_data")
setDT(main)
setDT(hhi_county)
main <- main[, c("fips", "year", "hhi")]
# main <- main[year %in% seq(2005, 2015, by = 2)]
main <- main[, year := as.factor(year)]
main <- main[year %in% c(2004, 2015)]
hhi_county <- hhi_county[, c("fips", "mean_hhi", "mean_hhi_pre")]
hhi_county <- unique(hhi_county, by = c("fips"))

ggplot() +
  geom_density(data = main, aes(x = hhi, group = year, color = year), alpha = 0.5, size = 1) +  # Density plot for dt
  geom_density(data = hhi_county, aes(x = mean_hhi), alpha = 0.3, size = 1) +  # Density plot for mean_hhi from hhi_county
  geom_density(data = hhi_county, aes(x = mean_hhi_pre), alpha = 0.3, size = 1, color = "midnightblue") +  # Density plot for mean_hhi from hhi_county
  geom_vline(xintercept = 2500, linetype = "dotdash", color = "black", size = 1) +  # Vertical line at 2500
  labs(
    title = "Density Plot of HHI by Year",
    x = "Herfindahl-Hirschman Index (HHI)",
    y = "Density",
    color = "Year",
    fill = "Year"
  ) +
  scale_x_continuous(breaks = seq(0, max(main$hhi, na.rm = TRUE), by = 1000)) +  # Set x-axis breaks of 1000
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # Better scaling for y-axis
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "right"
  )



