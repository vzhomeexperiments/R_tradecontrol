# This is a dedicated script for the Lazy Trading 4th Course: Statistical Analysis and Control of Trades
# Copyright (C) 2018 Vladimir Zhbanko
# Preferrably to be used only with the courses Lazy Trading see: https://vladdsm.github.io/myblog_attempt/index.html
# https://www.udemy.com/course/your-trading-control-reinforcement-learning/?referralCode=7AB82127FC5C2334AE8D
# PURPOSE: Adapt RL control parameters and write them to the file

# packages used *** make sure to install these packages
library(dplyr) 
library(magrittr)
library(readr)
library(lubridate) 
library(ReinforcementLearning)
library(magrittr)
library(lazytrade)

# ----------- Main Steps -----------------
# -- Read trading results from Terminal 1
# -- Use function find control parameters to write best RL control parameters for every trading robot


# ----------- TESTS -----------------
# -- Select entire content of the script and execute
# -- Pass: Data object DFT1 contains observations
# -- Pass: DFT1_sum contains trades summary
# -- Pass: Files XXXXXXX.rds are generated in the folder _RL/control
# -- Pass: 
# -- Fail: DFT1 class 'try-error'
# -- Fail: xxx

# =============================================
# *************Used Functions******************
# =============================================
# *** make sure to customize this path
# Update: below functions are added to the R package
# Clone repository https://github.com/vzhomeexperiments/lazytrade or
# use documentation / examples from 'lazytrade' package
# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
#path to user repo:
#!!!Change this path!!! 
path_user <- "C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol"
#!!!Change this path!!!

# terminal 1 path *** make sure to customize this path
path_T1 <- "C:/Program Files (x86)/FxPro - Terminal1/MQL4/Files/"

# path with folder containing control parameters
path_control_files = file.path(path_user, "_RL/control")

# -------------------------
### DEMO/TEST MODE 
# use code below to test functionality without MT4 platform installed
# # -------------------------
DFT1 <- try(import_data(path_sbxm = file.path(path_user, '_TEST_DATA'),
                        trade_log_file = "OrdersResultsT1.csv"),
            silent = TRUE)

# -------------------------
# read data from trades in terminal 1
# -------------------------
DFT1 <- try(import_data(path_T1, "OrdersResultsT1.csv"), silent = TRUE)

# Vector with unique Trading Systems
vector_systems <- DFT1 %$% MagicNumber %>% unique() %>% sort()

# For debugging: summarise number of trades to see desired number of trades was achieved
DFT1_sum <- DFT1 %>% 
  group_by(MagicNumber) %>% 
  summarise(Num_Trades = n(),
            Mean_profit = sum(Profit)) %>% 
  arrange(desc(Num_Trades))

StartAdaptation <- Sys.time()
### ============== FOR EVERY TRADING SYSTEM ###
for (i in 1:length(vector_systems)) {
  # tryCatch() function will not abort the entire for loop in case of the error in one iteration
  tryCatch({
    # execute this code below for debugging:
    # i <- 7 #policy off
    # i <- 2 #policy on
    
    # extract current magic number id
  trading_system <- vector_systems[i]
  # get trading summary data only for one system 
  trading_systemDF <- DFT1 %>% filter(MagicNumber == trading_system)
  
  ## -- Go to the next Loop iteration if there is too little trades! -- ##
  if(nrow(trading_systemDF) < 5) { next }
  
  #==============================================================================
  # Define state and action sets for Reinforcement Learning
  states <- c("tradewin", "tradeloss")
  actions <- c("ON", "OFF") # 'ON' and 'OFF' are referring to decision to trade with Slave system
  
  # Define reinforcement learning parameters (see explanation below or in vignette)
  # -----
  # alpha - learning rate      0.1 <- slow       | fast        -> 0.9
  # gamma - reward rate        0.1 <- short term | long term   -> 0.9
  # epsilon - sampling rate    0.1 <- high sample| low sample  -> 0.9
  # iter 
  # ----- 
  # to uncomment desired learning parameters:
  # NOTE: more research is required to find best parameters TDL TDL TDL
  #control <- list(alpha = 0.5, gamma = 0.5, epsilon = 0.5)
  #control <- list(alpha = 0.9, gamma = 0.9, epsilon = 0.9)
  #control <- list(alpha = 0.7, gamma = 0.5, epsilon = 0.9)
  #control <- list(alpha = 0.3, gamma = 0.6, epsilon = 0.1) 
  
  # or to use optimal control parameters found by auxiliary function
  st <- Sys.time()
  rl_write_control_parameters(trading_systemDF, 
                              path_control_files = path_control_files,
                              num_trades_to_consider = 300)
  en <- Sys.time()
  #cntrl <- read_rds(paste0(path_control_files, "/", trading_system, ".rds"))
  #cntrl <- read_rds(paste0(path_control_files, "/", 8139106, ".rds"))
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
}
### ============== END of FOR EVERY TRADING SYSTEM ###
EndAdaptation <- Sys.time()
