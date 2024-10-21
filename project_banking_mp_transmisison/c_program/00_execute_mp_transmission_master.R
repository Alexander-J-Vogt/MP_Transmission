# PROJECT'S FULL NAME

# TARGET (of this R.script): ordered execution of R.script(s) within public file routine
#NO detailed descriptions of data, method, etc. HERE -> done in R.scripts called upon

# CONVENTIONS of file naming, abbreviations:

#R.scripts: 	in general 		-> named like the dataset they produce, i.e., _varcreation_x.R
#PROGRAMS: 	-> CAPITAL LETTERS, automatically called upon by R.script executed

#df dataframe
#l list
#var variable

# BASIC STRUCTURE:

#1) DATA PREP
# create base df
#input: 
#output: 

# create separate dfs for each var type
#input: base df
#output: 

# create analysis df and subsamples
#input: combine var type dfs
#output: xx.rda and subsamples

#2) ANALYSIS
#for each sample: sumstats, regs, figures


################################################################################################################+
# INTRO ####

#clear console
cat("\014")

#clear all globals in memory
rm(list = ls()) #needs to go before user-written functions (not libraries) are loaded
sink()

######################+
# non-automatable globals #####

#for master scriptname and extension #####
library(rstudioapi)
MAINNAME <- rstudioapi::getActiveDocumentContext()$path #returns path+name
MAINNAME <- sub(".*/|^[^/]*$", "", MAINNAME)
MAINNAME <- substr(MAINNAME,1,nchar(MAINNAME)-2) #cut off .R

# paths ####
HOME <- "C:/Users/al8in/R_Projects/MP_Transmission" #here: path to 'projects' dir 
DO <- paste0(HOME,"/project_banking_mp_transmisison/c_program/") #here: path to folder with R.code

######################+
# launch set-up scripts #####
input <- '00_execute_mp_transmission_intro_aux.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)
#DEBUG <- T

################################################################################################################+
# MAIN PART ####


#############################################+
# read-in + basic editing of raw data: create base df ####
input <- 'mp_transmission_databasics.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

#############################################+
# manipulating outcome, treatment and control variables ####
input <- 'mp_transmission_outcome.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

input <- 'mp_transmission_treatment.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

input <- 'mp_transmission_control.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

#############################################+
# merge outcome, treatment and control variables
input <- 'mp_transmission_merge.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

#############################################+
# main work on dataset
input <- 'mp_transmission_main.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)


#############################################+
# analysis of dataset
input <- 'mp_transmission_analysis_vardistribution.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

input <- 'mp_transmission_analysis_regression.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

input <- 'mp_transmission_analysis_figures.R'
source(paste0(DO,input,sep=""), echo=TRUE, max=1000)

