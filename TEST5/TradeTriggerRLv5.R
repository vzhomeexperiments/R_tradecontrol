# This is a dedicated script for the Lazy Trading 4th Course: Statistical Analysis and Control of Trades
# This is a dedicated script for the Lazy Trading 6th Course: Detect Market Type with Artificial Intelligence
# Copyright (C) 2018 Vladimir Zhbanko
# Preferrably to be used only with the courses Lazy Trading see: https://vladdsm.github.io/myblog_attempt/index.html
# https://www.udemy.com/your-trading-control-reinforcement-learning/?couponCode=LAZYTRADE4-10
# PURPOSE: Analyse trade results in Terminal 1 and Trigger or Stop Trades in Terminal 3
# DETAILS: Trades are analysed and RL model is created for each single Expert Advisor
#        : Q states function is calculated, whenever Action 'ON' is > than 'OFF' trade trigger will be active   
#        : Results are written to the file of the MT4 Trading Terminal
# NOTE:    TEST4
#          : Next action is defined as a random sequence ON/OFF/ON/OFF for the first 10 trades
#          : specific model is trained
#          : for the subsequent trades next action is generated using obtained model
#          : model is retrained using data from the following trades...
#          : reinforcement learning model objects are not saved
# NOTE:    TEST5
#          : Reinforcement Learning Models are created for each specific Market Type


# packages used *** make sure to install these packages
library(tidyverse) #install.packages("tidyverse")
library(lubridate) #install.packages("lubridate") 
library(ReinforcementLearning) #devtools::install_github("nproellochs/ReinforcementLearning")
library(magrittr)

# ----------- Applied Logic -----------------
# -- Read trading results from Terminal 1
# -- Read market type logs from Terminal 1
# -- Rearrange data for RL
# -- Perform Reinforcement Learning or Update Model with New Data
# -- Start/Stop trades in Terminal 3 based on New Policy
# -- Start/Stop trades on Terminals at MacroEconomic news releases (covered in Course #5)

# ----------------
# Used Functions (to make code more compact). See detail of each function in the repository
#-----------------
# *** make sure to customize this path
 source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/writeCommandViaCSV.R")
 source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/TEST5/apply_policy.R")
 source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/TEST5/data_4_RL.R")
 source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/import_data.R")
 source("C:/Users/fxtrams/Documents/000_TradingRepo/R_tradecontrol/TEST5/import_data_mt.R")
# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
# terminal 1 path *** make sure to customize this path
path_T1 <- "C:/Program Files (x86)/FxPro - Terminal1/MQL4/Files/"

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
  # tryCatch() function will not abort the entire for loop in case of the error in one iteration
  tryCatch({
    # execute this code below for debugging:
    # i <- 25
    
  # extract current magic number id
  trading_system <- vector_systems[i]
  # get trading summary data only for one system 
  trading_systemDF <- DFT1 %>% filter(MagicNumber == trading_system)
  # write this data for further debugging or tests
  # write_rds(trading_systemDF,path = "test_data/data_4_RL.rds")
  # try to extract market type information for that system
  DFT1_MT <- try(import_data_mt(path_T1, trading_system), silent = TRUE)
  # join the data in case Market Type information is available
  if(class(DFT1_MT)[1] != "try-error") {
    # joining the data with market type info
    trading_systemDF <- inner_join(trading_systemDF, DFT1_MT, by = "TicketNumber")
    # get symbol of this system
    symbol <- trading_systemDF %>% head(1) %$% Symbol
    # get the latest market type state (we will have to read it from the sandbox!)
    latest_mt <- read_csv(file.path(path_T1, paste0("AI_MarketType_", symbol,"15.csv")),col_names = F) %$% X1[2]
    # filter to have only results from the current market type
    # Comment the code below to have all market type results
    # trading_systemDF <- trading_systemDF %>% 
    #   filter(MarketType == latest_mt)
    }
  
  # get the latest trade of that system (will be used to match with policy of RL)
  latest_trade <- trading_systemDF %>% 
    arrange(desc(OrderCloseTime)) %>% 
    mutate(NextState = ifelse(Profit>0, "tradewin",
                              ifelse(Profit<0, "tradeloss", NA)),
           Reward =  Profit,
           State = NextState) %>% head(1) %$% NextState
  
  
  ## -- Exit for Loop if there is too little trades! -- ##
  if(nrow(trading_systemDF) < 28) { next }
  # -------------------------
  # Perform Data Manipulation for RL
  # -------------------------
  ### ** FIRST TRADES BY THIS SYSTEM in T1 **
  # retrieve the first trades of the trading system (for initial model building)
  DFRL_start <- trading_systemDF %>% data_4_RL(all_trades = FALSE, num_trades = 20) #use first trades

  ### ** RECENT TRADES BY THIS SYSTEM in T1 **
  # retrieve the last trades of the trading system (for model update)
  DFRL_update <- trading_systemDF %>% data_4_RL(all_trades = TRUE, num_trades = 10) #use last trades
  
  # -------------------------
  # Perform Reinforcement Learning
  # -------------------------
  
  #==============================================================================
  # Define state and action sets for Reinforcement Learning
  states <- c("BUN", "BUV", "BEN", "BEV", "RAN", "RAV")
  actions <- c("ON", "OFF") # 'ON' and 'OFF' are referring to decision to trade with Slave system
  
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
  control <- list(alpha = 0.3, gamma = 0.6, epsilon = 0.1) #TEST4
  # -----
  #==============================================================================
  # remove model object if exist
  if(exists("model")) {rm(model)}
  # Perform initial RL of the first trades of this system
    model <- ReinforcementLearning(DFRL_start, s = "State", a = "Action", r = "Reward", 
                                   s_new = "NextState",iter = 1, control = control)
  # perform RL model update on recent trades of Terminal 1
    model <- ReinforcementLearning(DFRL_update, s = "State", a = "Action", r = "Reward",
                                         s_new = "NextState", control = control, iter = 1, model = model)
    #plot(model)
    #print(model)
    # apply the policy
    apply_policy(trading_system = trading_system, model = model, last_trade = latest_trade, path_sandbox = path_T4)
    # save model to file
    
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
