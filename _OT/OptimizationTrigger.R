## This is a dedicated script for the Lazy Trading 4th Course: Statistical Analysis of Trades
# Copyright (C) 2018 Vladimir Zhbanko

# PURPOSE: Analyse trade results in Terminal 1. Indicate when to optimize non performing systems
# NOTE:    Results are written in the Trading System Version Control Repository. 
#          Important: Trading System version control must contain a list of trading systems in use
#                     inside the file Setup.csv

# load packages. 
library(tidyverse)
library(lubridate)

# ----------- Main Steps -----------------
# -- Read trading results from Terminal 1
# -- Analyse results and filter systems where profit factor < 0.7
# -- Write command 'to optimize' to the file

# =============================================
# *************Used Functions******************
# =============================================
# *** make sure to customize this path
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/get_profit_factorDF.R")
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/import_data.R")
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/check_if_optimize.R")

# =============================================
# ************End of Used Functions************
# =============================================

# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
# terminal 1 path
path_T1 <- "C:/Program Files (x86)/FxPro - Terminal1/MQL4/Files/"

# trading system project folder
path_PRJCT_1 <- "C:/Users/fxtrams/Documents/000_TradingRepo/FALCON_A/"
path_PRJCT_2 <- "C:/Users/fxtrams/Documents/000_TradingRepo/FALCON_F2/"

# -------------------------
# read data from trades in terminal 1
# -------------------------
DFT1 <- try(import_data(path_T1, "OrdersResultsT1.csv"), silent = TRUE)

# -------------------------
# data frame T1 analysis and manipulation
# -------------------------

#### SORTING AND DECIDE IF SYSTEM NEEDS TO BE RE-TRAINED/RE-OPTIMIZED #### -----------------------------
# 4. Function that uses last 20 orders on DEMO && pr.fact < 0.7

#
### PROJECT 1
#
DFT1 %>% check_if_optimize(path_trading_robot = path_PRJCT_1,
                           num_trades_to_consider = 20,
                           profit_factor_limit = 0.7)
#
### PROJECT 2
#
DFT1 %>% check_if_optimize(path_trading_robot = path_PRJCT_2,
                           num_trades_to_consider = 20,
                           profit_factor_limit = 0.7)
##======================================== end of script
