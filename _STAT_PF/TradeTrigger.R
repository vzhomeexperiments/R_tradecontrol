# This is a dedicated script for the Lazy Trading 4th Course: Statistical Analysis and Control of Trades
# Copyright (C) 2018,2021 Vladimir Zhbanko

# PURPOSE: Analyse trade results in Terminal 1 and Trigger or Stop Trades in Terminal 3
# NOTE:    Results are triggered by writing to the file of the MT4 Trading Terminal

# packages used *** make sure to install these packages
library(readr)
library(dplyr)
library(magrittr)
library(lubridate)
library(lazytrade)

# ----------- Applied Logic -----------------
# -- Read trading results from Terminal 1
# -- Split trading results from Terminal 1 into categories using profit factor
# -- Depend on conditions of LAST 10 trades in Terminal 1, allow trades in Terminal 3
#   -- on last 10 trades in T1
#   -- enable T3 when Profit factor of 10 trades > 2
#   -- disable T3 when Profit Factor of 10 trades < 1.6
# -- Start/Stop trades on Terminals at MacroEconomic news releases (will be covered in Course #5)

# ----------- TESTS -----------------
# -- Select entire content of the script and execute
# -- Pass: Data object DFT1 contains observations
# -- Pass: xxx
# -- Pass: Files SystemControlXXXXXXX.csv are generated in the Terminal 3 sandbox
# -- Pass: If file "01_MacroeconomicEvent.csv" exists trade policy is overwritten
# -- Fail: DFT1 class 'try-error'
# -- Fail: xxx
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
#!!!Change this path!!!
# terminal 1 path *** 
#!!!Setup Environmental Variables!!! 
## NOTE: your path must contain: 'Terminal1'
path_T1 <- normalizePath(Sys.getenv('PATH_T1'), winslash = '/')

# terminal 3 path *** 
#!!!Setup Environmental Variables!!! 
## NOTE: your path must contain: 'Terminal3'
path_T3 <- normalizePath(Sys.getenv('PATH_T3'), winslash = '/')


# # uncomment code below to test functionality without MT4 platform installed
# -------------------------
### DEMO/TEST MODE 
# use code below to test functionality without MT4 platform installed
# # # -------------------------
# DFT1 <- try(import_data(path_sbxm = file.path(path_user, '_TEST_DATA'),
#                         trade_log_file = "OrdersResultsT1.csv"),
#             silent = TRUE)


# -------------------------
# read data from trades in terminal 1
# -------------------------
DFT1 <- try(import_data(path_T1, "OrdersResultsT1.csv"),silent = TRUE)


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
  write_command_via_csv(path_T1)

#### DECIDE IF TRADING ON THE T3 ACCOUNT #### -----------------------------
# Last 10 orders on DEMO && pr.fact >= 2 start trade T3
DFT1_L %>%
  get_profit_factorDF(10) %>% 
  ungroup() %>% 
  filter(PrFact >= 1.6) %>% 
  select(MagicNumber) %>% 
  mutate(MagicNumber = MagicNumber + 200, IsEnabled = 1) %>% 
  # Write command "allow"
  write_command_via_csv(path_T3)

#### DECIDE IF NOT TO TRADING ON THE T3 ACCOUNT #### -----------------------------
# 4. Last 10 orders on DEMO && pr.fact < 1.6 stop trade T3
DFT1_L %>%
  get_profit_factorDF(10) %>% 
  ungroup() %>% 
  filter(PrFact < 1.3) %>% 
  select(MagicNumber) %>% 
  mutate(MagicNumber = MagicNumber + 200, IsEnabled = 0) %>% 
  # Write command "allow"
  write_command_via_csv(path_T3)

#write_rds(DFT1_L, "test_data_profit_factorDF.rds")
##========================================
# -------------------------
# read data from trades in terminal 3
# -------------------------
DFT3 <- try(import_data(path_T3, "OrdersResultsT3.csv"),silent = TRUE)

# -------------------------
# stopping all systems when macroeconomic event is present
# this will be covered in the Course #5 of the Lazy Trading Series!
# -------------------------
##========================================
# -------------------------
# stopping all systems when macroeconomic event is present
# this will be covered in the Course #5 of the Lazy Trading Series!
# -------------------------
if(file.exists(file.path(path_T1, "01_MacroeconomicEvent.csv"))){
  DF_NT <- read_csv(file= file.path(path_T1, "01_MacroeconomicEvent.csv"), col_types = "i")
  if(DF_NT[1,1] == 1) {
    # disable trades
    if(!class(DFT1)[1]=='try-error'){
      DFT1 %>%
        group_by(MagicNumber) %>% select(MagicNumber) %>% mutate(IsEnabled = 0) %>% 
        # write commands to disable systems
        writeCommandViaCSV(path_T1)}
    if(!class(DFT3)[1]=='try-error'){
      DFT3 %>%
        group_by(MagicNumber) %>% select(MagicNumber) %>% mutate(IsEnabled = 0) %>% 
        write_command_via_csv(path_T3)}
    
    
  }
  # enable systems of T1 in case they were disabled previously
  if(DF_NT[1,1] == 0) {
    # enable trades
    if(!class(DFT1)[1]=='try-error'){
      DFT1 %>%
        group_by(MagicNumber) %>% select(MagicNumber) %>% mutate(IsEnabled = 1) %>% 
        # write commands to disable systems
        write_command_via_csv(path_T1)}
   
  }
  
}