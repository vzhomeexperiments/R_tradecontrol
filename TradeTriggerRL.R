# This is a dedicated script for the Lazy Trading 4th Course: Statistical Analysis and Control of Trades
# Copyright (C) 2018 Vladimir Zhbanko
# Preferrably to be used only with the courses Lazy Trading see: https://vladdsm.github.io/myblog_attempt/index.html

# PURPOSE: Analyse trade results in Terminal 1 and Trigger or Stop Trades in Terminal 3
# NOTE:    Results are triggered by writing to the file of the MT4 Trading Terminal
# REINFORCEMENT LEARNING! EXPERIMENTING ONLY! USE AT YOUR OWN RISK!

# packages used *** make sure to install these packages
library(tidyverse) #install.packages("tidyverse")
library(lubridate) #install.packages("lubridate") 
library(ReinforcementLearning) #devtools::install_github("nproellochs/ReinforcementLearning")
library(magrittr)
library(openssl)

# ----------- Applied Logic -----------------
# -- Read trading results from Terminal 1
# -- Rearrange data for RL
# -- Perform Reinforcement Learning or Update Model with New Data
# -- Start/Stop trades in Terminal 3 based on New Policy
# -- Start/Stop trades on Terminals at MacroEconomic news releases (will be covered in Course #5)

# ----------------
# Used Functions (to make code more compact)
#-----------------
# *** make sure to customize this path
 source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/writeCommandViaCSV.R")
 source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/apply_policy.R")
 source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/data_4_RL.R")

# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
# terminal 1 path *** make sure to customize this path
path_T1 <- "C:/Program Files (x86)/FxPro - Terminal1/MQL4/Files/"
# path for Reinforcement Learning objects *** make sure to create folder 'RL' inside sandbox
path_RL <- paste0(path_T1, "RL/")
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

# Vector with unique Trading Systems
vector_systems <- DFT1 %$% MagicNumber %>% unique() %>% sort()

# -------------------------
# read data from trades in terminal 3
# -------------------------
DFT3 <- try(read_csv(file = file.path(path_T3, "OrdersResultsT3.csv"), 
                     col_names = c("MagicNumber", "TicketNumber", "OrderStartTime", 
                                   "OrderCloseTime", "Profit", "Symbol", "OrderType"),
                     col_types = "iiccdci"), 
            silent = TRUE)

# data frame preparation
DFT3$OrderStartTime <- ymd_hms(DFT3$OrderStartTime)
DFT3$OrderCloseTime <- ymd_hms(DFT3$OrderCloseTime)
DFT3$OrderType      <- as.factor(DFT3$OrderType)


### ============== FOR EVERY TRADING SYSTEM ###
for (i in 1:length(vector_systems)) {
  # i <- 2
  trading_system <- vector_systems[i]
  # get only data for one system 
  trading_systemDF <- DFT1 %>% filter(MagicNumber == trading_system)
  
  # get the latest trade of that system (will be used to match with policy of RL)
  latest_trade <- DFT1 %>% 
    filter(MagicNumber == trading_system) %>% 
    arrange(desc(OrderCloseTime)) %>% 
    mutate(NextState = ifelse(Profit>0, "tradewin",
                              ifelse(Profit<0, "tradeloss", NA)),
           Reward =  Profit,
           State = NextState) %>% head(1) %$% NextState
  
  # -------------------------
  # Perform Data Manipulation for RL
  # -------------------------
  ### ** ALL TRADES BY THIS SYSTEM **
  # add additional column with cumulative profit # group_by(id)%>%mutate(csum=cumsum(value))
  trading_systemDFRL <- trading_systemDF %>% 
  group_by(MagicNumber) %>% 
  mutate(csum=cumsum(Profit)) %>% 
    # arrange as ascending
    arrange(OrderCloseTime) %>% 
    # we will always consider more recent history
    #tail(20) %>% 
    # create column State
    mutate(NextState = ifelse(Profit>0, "tradewin",
                              ifelse(Profit<0, "tradeloss", NA)),
           # very simple logic: whenever cumulative sum is positive - we trade...
           Action = ifelse(csum > 0, "ON",
                           ifelse(csum < 0, "OFF", NA)),
           Reward =  Profit,
           State = lag(NextState)) %>% # State column will be shifted down
    # remove row with empty data
    na.omit() %>% 
    ungroup() %>% # to get rid of grouping column
    select(State, Action, Reward, NextState) %>% 
    as.data.frame.data.frame() # ReinforcementLearning function seems to only work with 'dataframe'
    
  ### ** RECENT TRADES BY THIS SYSTEM **
  # add additional column with cumulative profit # group_by(id)%>%mutate(csum=cumsum(value))
  trading_systemDFRL20 <- trading_systemDF %>% 
    group_by(MagicNumber) %>% 
    mutate(csum=cumsum(Profit)) %>% 
    # arrange as ascending
    arrange(OrderCloseTime) %>% 
    # we will always consider more recent history
    tail(20) %>% 
    # create column State
    mutate(NextState = ifelse(Profit>0, "tradewin",
                              ifelse(Profit<0, "tradeloss", NA)),
           # very simple logic: whenever cumulative sum is positive - we trade...
           Action = ifelse(csum > 0, "ON",
                           ifelse(csum < 0, "OFF", NA)),
           Reward =  Profit,
           State = lag(NextState)) %>% # State column will be shifted down
    # remove row with empty data
    na.omit() %>% 
    ungroup() %>% # to get rid of grouping column
    select(State, Action, Reward, NextState) %>% 
    as.data.frame.data.frame() # ReinforcementLearning function seems to only work with 'dataframe'
  
  # -------------------------
  # Perform Reinforcement Learning
  # -------------------------
  # get the unique id of the last trade. This is to know if to retrain the model
  recent_name <- trading_systemDF %>% head(1) %>% as.character() %>% as.vector() %>% paste(collapse = "") %>% sha1()
  recent_name_file <- paste0(path_RL, recent_name)
  
  # Define state and action sets
  states <- c("tradewin", "tradeloss")
  actions <- c("ON", "OFF")
  
  # Define reinforcement learning parameters (see explanation below or in vignette)
  # -----
  # alpha - learning rate      0.1 <- slow       | fast        -> 0.9
  # gamma - reward rate        0.1 <- short term | long term   -> 0.9
  # epsilon - sampling rate    0.1 <- high sample| low sample  -> 0.9
  # iter 
  # ----- 
  # to uncommend desired learning parameters:
  #control <- list(alpha = 0.5, gamma = 0.5, epsilon = 0.5)
  #control <- list(alpha = 0.9, gamma = 0.9, epsilon = 0.9)
  control <- list(alpha = 0.3, gamma = 0.6, epsilon = 0.1)
  # -----
  # -----------------------------------------------------------------------------
  #==============================================================================
  # running RL first time when model not exist yet
  if(!file.exists(recent_name_file)){
  
  # perform RL
  model <- ReinforcementLearning(trading_systemDFRL, s = "State", a = "Action", r = "Reward", s_new = "NextState",iter = 1, control = control)
  
  # apply policy based on model
  apply_policy(trading_system = trading_system, model = model, last_trade = latest_trade, path_sandbox = path_T3)
  
  # save model to file
  write_rds(model, recent_name_file)
  } else { 
    # perform model update
    
    # update model
    model_old <- read_rds(recent_name_file)
    # model on recent data
    model_new <- ReinforcementLearning(trading_systemDFRL20, s = "State", a = "Action", r = "Reward",
                                       s_new = "NextState", control = control, iter = 1, model = model_old)
    
    # write new model to file
    write_rds(model_new, recent_name_file)
    
    # -------------------------
    # Apply policy
    # -------------------------
    apply_policy(trading_system = trading_system, model = model_new, last_trade = latest_trade, path_sandbox = path_T3)
    
    
    }
  
  # # debugging policies
  # policy(model)
  # policy(model_new)
  # print(model_new)
  # summary(model_new)
  
  
  
  
  


}
### ============== END of FOR EVERY TRADING SYSTEM ###








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

