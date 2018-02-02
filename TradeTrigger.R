# This is a dedicated script for the Lazy Trading 4th Course: Statistical Analysis and Control of Trades
# Copyright (C) 2018 Vladimir Zhbanko

# PURPOSE: Analyse trade results in Terminal 1 and Trigger or Stop Trades in Terminal 3
# NOTE:    Results are triggered by writing to the file of the MT4 Trading Terminal

# packages used *** make sure to install these packages
library(tidyverse) #install.packages("tidyverse")
library(lubridate) #install.packages("lubridate") 

# ----------- Applied Logic -----------------
# -- Read trading results from Terminal 1
# -- Split trading results from Terminal 1 into categories using profit factor
# -- Depend on conditions of LAST 10 trades in Terminal 1, allow trades in Terminal 3
#   -- on last 10 trades in T1
#   -- enable T3 when Profit factor of 10 trades > 2
#   -- disable T3 when Profit Factor of 10 trades < 1.6
# -- Start/Stop trades on Terminals at MacroEconomic news releases (will be covered in Course #5)

# ----------------
# Used Functions
#-----------------
# *** make sure to customize this path
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/writeCommandViaCSV.R")
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/profit_factor.R")
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/profit_factorDF.R")

# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
# terminal 1 path *** make sure to customize this path
path_T1 <- "C:/Program Files (x86)/FxPro - Terminal1/MQL4/Files/"
# terminal 3 path *** make sure to customize this path
path_T3 <- "C:/Program Files (x86)/FxPro - Terminal3/MQL4/Files/"

# -------------------------
# read data from trades in terminal 1
# -------------------------
DFT1 <- try(read_csv(file = file.path(path_T1, "OrdersResultsT1.csv"), 
                     col_names = c("MagicNumber", "TicketNumber", "OrderStartTime", 
                                   "OrderCloseTime", "Profit", "Symbol", "OrderType"),
                     col_types = "iiccdci"), 
            silent = TRUE)

# data frame preparation
DFT1$OrderStartTime <- ymd_hms(DFT1$OrderStartTime)
DFT1$OrderCloseTime <- ymd_hms(DFT1$OrderCloseTime)
DFT1$OrderType      <- as.factor(DFT1$OrderType)

# get last 10 trades for each Magic system and arrange orders to have descending order
DFT1_L <- DFT1 %>%  # filtered to contain last 10 orders for each system
  group_by(MagicNumber) %>% 
  arrange(MagicNumber, desc(OrderCloseTime)) %>% 
  filter(row_number() <= 11) # +1 for the function to work
  
# ----------------
# Implementation of logic
#-----------------
#### SORTING AND DECIDE TRADING ON THE DEMO ACCOUNT #### -----------------------------
# DEMO always allow trades in Terminal 1                               
DFT1_L %>%
  group_by(MagicNumber) %>%
  summarise(nOrders = n()) %>%
  select(MagicNumber) %>%
  mutate(IsEnabled = 1) %>% 
  # Write command "allow"
  writeCommandViaCSV(path_T1)

#### DECIDE IF TRADING ON THE T3 ACCOUNT #### -----------------------------
# Last 10 orders on DEMO && pr.fact >= 2 start trade T3
DFT1_L %>%
  profit_factorDF(10) %>% 
  ungroup() %>% 
  filter(PrFact >= 2) %>% 
  select(MagicNumber) %>% 
  mutate(MagicNumber = MagicNumber + 200, IsEnabled = 1) %>% 
  # Write command "allow"
  writeCommandViaCSV(path_T3)

#### DECIDE IF NOT TO TRADING ON THE T3 ACCOUNT #### -----------------------------
# 4. Last 10 orders on DEMO && pr.fact < 1.6 stop trade T3
DFT1_L %>%
  profit_factorDF(10) %>% 
  ungroup() %>% 
  filter(PrFact < 1.6) %>% 
  select(MagicNumber) %>% 
  mutate(MagicNumber = MagicNumber + 200, IsEnabled = 0) %>% 
  # Write command "allow"
  writeCommandViaCSV(path_T3)


##========================================
# -------------------------
# stopping all systems when macroeconomic event is present
# this will be covered in the Course #5 of the Lazy Trading Series!
# -------------------------

DF_NT <- read_csv(file= file.path(path_T1, "01_MacroeconomicEvent.csv"), col_types = "i")
if(DF_NT[1,1] == 1) {
  # disable trades
  DF_DisableT1 <- DFT1 %>% 
    group_by(MagicNumber) %>% select(MagicNumber) %>% mutate(IsEnabled = 0)
  DF_DisableT3 <- DFT3 %>% 
    group_by(MagicNumber) %>% select(MagicNumber) %>% mutate(IsEnabled = 0)
  # write commands to disable systems
  writeCommandViaCSV(DF_DisableT1, path_T1)
  writeCommandViaCSV(DF_DisableT3, path_T3)
}

