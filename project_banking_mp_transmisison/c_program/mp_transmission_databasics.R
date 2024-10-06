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

### Importing Call Report Data from Wang et al. (2022) -------------------------
# Call Rreport: Information on bank-level data
df_callreport <- read_dta(paste0(A,"wang_22_data/", "callreport_ffiec_merged.dta"))

### Importing SUmmary of Deposits (by FDIC) ------------------------------------
## Importing Summary of Deposits (by FDIC) from Wang et al. (2022) -------------
df_sod <- read_dta(paste0(A,"wang_22_data/", "FDIC_SOD.dta"))

### Importing Summary of Deposits (by FDIC) for the years 2018 to 2024
files_sod <- list.files(paste0(A, "sod_direct/"))
files_sod <- files_sod[grepl("\\csv$",files_sod)]

# for (i in files_sod) {
  file <- read_csv(paste0(A, "sod_direct/", files_sod[1]))
  year <- str_sub(files_sod[1], start = 5, end = 8)
  save(file, paste0(TEMP,"/", "sod_", year, ".rda"))
# }



df_sod_2024 <- read.csv(paste0(A, "ALL_2024.csv"))
df_sod_2024 <- 

colnames(df_sod_2024) <- colnames(df_sod)

df_mortgage <- read.csv(paste0(A, "mortgage_data/", "nmdb-new-mortgage-statistics-state-annual.csv"))
df_mortgage_performance <- read.csv(paste0(A, "mortgage_data/", "nmdb-mortgage-performance-statistics-states-quarterly.csv"))
df_mortgage_statistics <- read.csv(paste0(A, "mortgage_data/", "nmdb-new-mortgage-statistics-national-census-areas-annual.csv"))



Viewdf_callreport <- read.delim(paste0())
anyDuplicated(df$rssdid, df$date)
df %>%
  select(rssdid, date) %>%
  distinct() %>%
  nrow() == nrow(df)

excel <- read.csv((paste0(A, "._ZIP_COUNTY_122017.csv")))

list.files(path = paste0(A, "mortgage_data/"), all.files = TRUE)
