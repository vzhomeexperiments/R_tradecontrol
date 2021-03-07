## This is a dedicated script for the Lazy Trading 4th Course: Statistical Analysis of Trades
# Copyright (C) 2018,2021 Vladimir Zhbanko

# PURPOSE: Analyse trade results in Terminal 1. Indicate when to optimize non performing systems
# NOTE:    Results are written in the Trading System Version Control Repository. 
#          Important: Trading System version control must contain a list of trading systems in use
#                     inside the file Setup.csv

# load packages. 
library(dplyr)
library(magrittr)
library(readr)
library(lubridate)
library(lazytrade)

# ----------- Main Steps -----------------
# -- Read information on which systems are in 'production'
# -- Read trading results from Terminal 1
# -- Analyse results and filter systems where profit factor < 0.7
# -- Write command 'to optimize' to the file

# ----------- TESTS -----------------
# -- Select entire content of the script and execute
# -- Pass: object DFT1 is dataframe
# -- Pass: file 'Date-Re-Train.csv' is written into the trading robot folder
# -- Fail: object DFT1 has class 'try-error'


# =============================================
# *************Used Functions******************
# =============================================
# *** make sure to customize this path
# Update: below functions are added to the R package
# Clone repository https://github.com/vzhomeexperiments/lazytrade or
# use documentation / examples from 'lazytrade' package

# =============================================
# ************End of Used Functions************
# =============================================

# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
#path to user repo:
#!!!Setup Environmental Variables!!! 
path_user <- normalizePath(Sys.getenv('PATH_DSS_Repo'), winslash = '/')
path_user <- file.path(path_user, "R_tradecontrol")

# terminal 1 path
#!!!Setup Environmental Variables!!! 
path_T1 <- normalizePath(Sys.getenv('PATH_T1'), winslash = '/')

# trading system project folder
# NOTE: Robot repository must have a folder with file TEST/Setup.csv
# File Setup.csv should contain magic numbers of the working systems 
path_PRJCT <- normalizePath(Sys.getenv('PATH_DSS_Repo'), winslash = '/')
#path_PRJCT_1 <- file.path(path_PRJCT,"FALCON_A/")
path_PRJCT_2 <- file.path(path_PRJCT,"FALCON_F2/")
#path_PRJCT_3 <- file.path(path_PRJCT,"FALCON_B/")

# -------------------------
### DEMO/TEST MODE 
# use code below to test functionality without MT4 platform installed
# # # -------------------------
# DFT1 <- try(import_data(path_sbxm = file.path(path_user, '_TEST_DATA'),
#                         trade_log_file = "OrdersResultsT1.csv"),
#              silent = TRUE)
# # Uncomment code chunk below
# syst_PRJCT_1 <- read_csv(system.file("extdata", "Setup.csv", package = "lazytrade"))
# DFT1 %>% check_if_optimize(system_list = syst_PRJCT_1,
#                            path_data = path_user,
#                            num_trades_to_consider = 10,
#                            profit_factor_limit = 1.7,
#                            write_mode = FALSE)
# -------------------------
# -------------------------
# read data from trades in terminal 1
# -------------------------
DFT1 <- try(import_data(path_T1, "OrdersResultsT1.csv"), silent = TRUE)

# -------------------------
# data frame T1 analysis and manipulation
# -------------------------

#### SORTING AND DECIDE IF SYSTEM NEEDS TO BE RE-TRAINED/RE-OPTIMIZED #### -----------------------------
# Function that uses last 20 orders on DEMO && pr.fact < 0.7
#
# Results will be written to the file in the respective folder
### PROJECT 1
#
# syst_PRJCT_1 <- read_csv(file.path(path_PRJCT_1, "TEST/Setup.csv"))
# 
# DFT1 %>% check_if_optimize(system_list = syst_PRJCT_1,
#                            path_data = path_PRJCT_1,
#                            num_trades_to_consider = 4,
#                            profit_factor_limit = 0.8,
#                            write_mode = TRUE)
### PROJECT 2
#
syst_PRJCT_2 <- read_csv(file.path(path_PRJCT_2, "TEST/Setup.csv"))

DFT1 %>% check_if_optimize(system_list = syst_PRJCT_2,
                           path_data = path_PRJCT_2,
                           num_trades_to_consider = 10,
                           profit_factor_limit = 0.8,
                           write_mode = TRUE)

### PROJECT 3
# syst_PRJCT_3 <- read_csv(file.path(path_PRJCT_3, "TEST/Setup.csv"))
# 
# DFT1 %>% check_if_optimize(system_list = syst_PRJCT_3,
#                            path_data = path_PRJCT_3,
#                            num_trades_to_consider = 4,
#                            profit_factor_limit = 0.8,
#                            write_mode = TRUE)
