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
