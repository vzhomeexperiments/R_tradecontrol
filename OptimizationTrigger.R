## This is a dedicated script for the Lazy Trading 4th Course: Statistical Analysis of Trades
# Copyright (C) 2018 Vladimir Zhbanko

# PURPOSE: Analyse trade results in Terminal 1. Write a summary, indicate when to optimize systems
# NOTE:    Results are written in the Trading System Version Control Repository. 
#          Important: Trading System version control must contain a list of trading systems in use

# load packages
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
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/profitFactor.R")

# =============================================
# ************End of Used Functions************
# =============================================

# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
# terminal 1 path
path_T1 <- "C:/Program Files (x86)/FxPro - Terminal1/MQL4/Files/"
# terminal 3 path
path_T3 <- "C:/Program Files (x86)/FxPro - Terminal3/MQL4/Files/"
# trading system project folder
path_PRJCT <- "C:/Users/fxtrams/Documents/000_TradingRepo/FALCON_B/"

# -------------------------
# read data from trades in terminal 2
# -------------------------
DFT1 <- try(read_csv(file = paste0(path_T1, "OrdersResultsT1.csv"), 
                     col_names = c("MagicNumber", "TicketNumber", "OrderStartTime", 
                                   "OrderCloseTime", "Profit", "Symbol", "OrderType"),
                     col_types = "iiccdci"), silent = TRUE)

# data frame preparation, function ymd_hms from 'lubridate' package
DFT1$OrderStartTime <- ymd_hms(DFT1$OrderStartTime)
DFT1$OrderCloseTime <- ymd_hms(DFT1$OrderCloseTime)
DFT1$OrderType      <- as.factor(DFT1$OrderType)


# -------------------------
# data frame T1 analysis and manipulation
# -------------------------
# ----------------
# Summarise orders acc.to the logic below
#-----------------
# 1.	ON TERMINAL 1
# 2.	Group by trading systems
# 3.	analyse last 20 trades --> data is in *DFT1*
# 4.	get their profit factor
# 5.	filter those systems where profit factor is lower than 0.7 
# 6.	run this analysis weekly, every Friday
# 7.	re-train only those systems with lower factor
# ----------------
# Implementation of logic
#-----------------
#### SORTING AND DECIDE IF SYSTEM NEEDS TO BE RE-TRAINED/RE-OPTIMIZED #### -----------------------------
# 4. Last 20 orders on DEMO && pr.fact < 0.7 SUGGEST TO RE-TRAINE
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
  write_csv(path = paste0(path_PRJCT, Sys.Date(), "-Re-Train", ".csv"))


##======================================== end of script
