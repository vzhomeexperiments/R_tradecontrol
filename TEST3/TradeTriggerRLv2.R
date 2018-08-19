# This is a dedicated script for the Lazy Trading 4th Course: Statistical Analysis and Control of Trades
# Copyright (C) 2018 Vladimir Zhbanko
# Preferrably to be used only with the courses Lazy Trading see: https://vladdsm.github.io/myblog_attempt/index.html
# https://www.udemy.com/your-trading-control-reinforcement-learning/?couponCode=LAZYTRADE4-10
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
 source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/data_4_RL_Slave.R")
 source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/import_data.R")

# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
# terminal 1 path *** make sure to customize this path
path_T1 <- "C:/Program Files (x86)/FxPro - Terminal1/MQL4/Files/"

# path for Reinforcement Learning objects *** create folder 'RL' inside sandbox if it's not exist
path_RL <- paste0(path_T1, "RL/")
if(!dir.exists(path_RL)){dir.create(path_RL)}

# terminal 3 path *** make sure to customize this path
path_T4 <- "C:/Program Files (x86)/FxPro - Terminal3/MQL4/Files/"

# -------------------------
# read data from trades in terminal 1
# -------------------------
DFT1 <- try(import_data(path_T1, "OrdersResultsT1.csv"), silent = TRUE)
# -------------------------
# read data from trades in terminal 3
# -------------------------
DFT4 <- try(import_data(path_T4, "OrdersResultsT3.csv"), silent = TRUE)

# Vector with unique Trading Systems
vector_systems <- DFT1 %$% MagicNumber %>% unique() %>% sort()

# For debugging: summarise number of trades to see desired number of trades was achieved
DFT1_sum <- DFT1 %>% 
  group_by(MagicNumber) %>% 
  summarise(Num_Trades = n()) %>% 
  arrange(desc(Num_Trades))

### ============== FOR EVERY TRADING SYSTEM ###
for (i in 1:length(vector_systems)) {
  
  tryCatch({
    # i <- 19
    
    
  
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
  ### ** ALL TRADES BY THIS SYSTEM in T1 **
  # add additional column with cumulative profit # group_by(id)%>%mutate(csum=cumsum(value))
  trading_systemDFRL <- trading_systemDF %>% data_4_RL(all_trades = TRUE)
  
  ## -- Exit for Loop if there is too little trades! -- ##
  if(nrow(trading_systemDFRL) < 15) { next }
  
  ### ** RECENT TRADES BY THIS SYSTEM in T1 **
  # simple consideration of latest trades and actions as they are!
  # these would be the same trades for T3 as well
  trading_systemDFRL5 <- trading_systemDF %>% data_4_RL_slave(all_trades = FALSE, num_trades = 6) #use last trades
  
  # -------------------------
  # Perform Reinforcement Learning
  # -------------------------
  # get the unique id of the last trade. This is to know if to retrain the model. Note we are using hashing 
  # algorithm to simplify generation of unique string
  recent_name <- trading_systemDF %>% tail(1) %>% as.character() %>% as.vector() %>% paste(collapse = "") %>% sha1()
  recent_name_file <- paste0(path_RL, recent_name)
  
  # Define state and action sets for Reinforcement Learning
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
  #control <- list(alpha = 0.8, gamma = 0.3, epsilon = 0.5)
  #control <- list(alpha = 0.3, gamma = 0.6, epsilon = 0.1) #TEST2
  control <- list(alpha = 0.9, gamma = 0.6, epsilon = 0.1)  #TEST3
  # -----
  # -----------------------------------------------------------------------------
  #==============================================================================
  # running RL for all data of Terminal 1 when current model is not exist
  if(!file.exists(recent_name_file)){
  
    # perform RL on the all data!
    model <- ReinforcementLearning(trading_systemDFRL, s = "State", a = "Action", r = "Reward", 
                                   s_new = "NextState",iter = 1, control = control)
    # perform RL model update on recent 6 trades of Terminal 1
    model_new <- ReinforcementLearning(trading_systemDFRL5, s = "State", a = "Action", r = "Reward",
                                         s_new = "NextState", control = control, iter = 1, model = model)
    # apply the policy
    apply_policy(trading_system = trading_system, model = model_new, last_trade = latest_trade, path_sandbox = path_T4)
    # save model to file
    write_rds(model_new, recent_name_file)
  # running RL model to update the relevant model
  } else { 
    # no new trades was generated we can use the old model
    model_old <- read_rds(recent_name_file)
    # apply the policy
    apply_policy(trading_system = trading_system, model = model_old, last_trade = latest_trade, path_sandbox = path_T4)
    }
  
  # # debugging policies
  # policy(model)
  # policy(model_new)
  # print(model_new)
  # summary(model_new)
  # plot(model_new)

  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
}
### ============== END of FOR EVERY TRADING SYSTEM ###






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
  if(!class(DFT4)[1]=='try-error'){
  DFT4 %>%
    group_by(MagicNumber) %>% select(MagicNumber) %>% mutate(IsEnabled = 0) %>% 
    writeCommandViaCSV(path_T4)}
  
  
}
# enable systems of T1 in case they were disabled previously
if(DF_NT[1,1] == 0) {
  # enable trades
  if(!class(DFT1)[1]=='try-error'){
    DFT1 %>%
      group_by(MagicNumber) %>% select(MagicNumber) %>% mutate(IsEnabled = 1) %>% 
      # write commands to disable systems
      writeCommandViaCSV(path_T1)}
}

}
