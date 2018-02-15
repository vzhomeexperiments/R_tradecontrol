## This is a dedicated script for the Lazy Trading 4th Course: Statistical Analysis of Trades
# Copyright (C) 2018 Vladimir Zhbanko

# PURPOSE: Analyse trade results in Terminal 1. Indicate when to optimize non performing systems
# NOTE:    Results are written in the Trading System Version Control Repository. 
#          Important: Trading System version control must contain a list of trading systems in use

# load packages. To install run command e.g.: install.packages("readxl")
library(tidyverse)
library(lubridate)
library(readxl)

# ----------- Main Steps -----------------
# -- Read trading results from Terminal 1
# -- Analyse results and filter systems where profit factor < 0.7
# -- Write command 'to optimize' to the file

# =============================================
# *************Used Functions******************
# =============================================
# *** make sure to customize this path
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/profit_factorDF.R")
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/import_data.R")

# =============================================
# ************End of Used Functions************
# =============================================

# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
# terminal 1 path
path_T1 <- "C:/Program Files (x86)/FxPro - Terminal1/MQL4/Files/"

# trading system project folder
path_PRJCT <- "C:/Users/fxtrams/Documents/000_TradingRepo/FALCON_B/"

# -------------------------
# read data from trades in terminal 2
# -------------------------
DFT1 <- try(import_data(path_T1, "OrdersResultsT1.csv"), silent = TRUE)

# -------------------------
# data frame T1 analysis and manipulation
# -------------------------

#### SORTING AND DECIDE IF SYSTEM NEEDS TO BE RE-TRAINED/RE-OPTIMIZED #### -----------------------------
# 4. Last 20 orders on DEMO && pr.fact < 0.7 SUGGEST TO RE-TRAINE
if(!class(DFT1)[1]=='try-error'){
DFT1 %>%  # filtered to contain last 20 orders for each system
  group_by(MagicNumber) %>% 
  arrange(MagicNumber, desc(OrderCloseTime)) %>% 
  filter(row_number() <= 21) %>% 
  profit_factorDF(20) %>% 
  ungroup() %>% 
  filter(PrFact < 0.7) %>% 
  select(MagicNumber, PrFact) %>% 
  mutate(ToOptimize = 1) %>% 
  inner_join(y = read_excel(path = paste0(path_PRJCT, "Setup.xlsx")), by = c("MagicNumber" = "Magic")) %>% 
  write_csv(path = paste0(path_PRJCT, Sys.Date(), "-Re-Train", ".csv"))}


##======================================== end of script
