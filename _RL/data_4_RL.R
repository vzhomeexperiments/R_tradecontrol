# -------------------------
# Perform Data Manipulation for RL
# -------------------------
# FUNCTION data_for_RL
# PURPOSE: This function will take dataframe with trade result and it will prepare data four RL function
# Function uses cumulative sum principle to define the output action for Reinforcement Learning
# The NextState for every trade will be obtained using random decided action

# Function was created to make code more compact
# write_rds(trading_systemDF, "test_data/data_4_RL.rds")
# x <- read_rds("test_data/data_4_RL.rds")
# Notabene: No fail safe mechanism at the moment! Use on your own risk!!!

#' Function to convert trading data into data siutable for Reinforcement Learning
#'
#' @param x - Dataframe containing trading data see data
#' @param all_trades. Logical when TRUE uses all trades by default
#' @param num_trades Possible to specify how big history of the latest trades to take (sorted by OrderCloseTime)
#'
#' @return Function returns data frame in the Reinforcement Learning format
#' @export
#'
#' @examples
data_4_RL <- function(x, all_trades = TRUE, num_trades = 20){
  # uncomment to debug code inside the function
  # x <- read_rds("test_data/data_4_RL.rds")
  # all_trades = TRUE
  # num_trades = 8
  # get total number of trades done by this system
  total_trades = nrow(x)
  # -------------
  ### This part will extract only first trades used for initial building of the system
  # -------------
  if(all_trades == FALSE){
    
    ### ** OLDEST TRADES BY THIS SYSTEM **
    trading_systemDFRL <- x %>% 
      # arrange as ascending
      arrange(OrderCloseTime) %>% 
      # extract the first trades
      head(num_trades) %>% 
      # create column with cumulative profit
      #mutate(csum=cumsum(Profit)) %>% 
      ## create column with random variable 0 or 1
      mutate(profit_rand = rep(0:1, length.out = num_trades)) %>% #non random
      #mutate(profit_rand = sample(0:1, n(), replace = TRUE)) %>% # random
      # create column State
      mutate(NextState = ifelse(Profit>0, "tradewin",
                                ifelse(Profit<0, "tradeloss", NA)),
             # for initial Action randomly generated values
             Action = ifelse(profit_rand == 1, "ON",
                             ifelse(profit_rand == 0, "OFF", NA)),
             Reward =  Profit,
             State = lag(NextState)) %>% # State column will be shifted down
      # remove row with empty data
      na.omit() %>% 
      # select only relevant columns
      select(State, Action, Reward, NextState) %>% 
      # ReinforcementLearning function seems to only work with 'dataframe'
      as.data.frame.data.frame() 
    
    return(trading_systemDFRL)
  }  
  
  # -------------
  ### This part will extract all the trades except those used for initial building of the system
  # -------------
  if(all_trades == TRUE){
  # create RL model object
trading_systemDFRL <- x %>% 
  # arrange as ascending
  arrange(OrderCloseTime) %>% 
  # extract the first trades
  tail(total_trades - num_trades) %>% 
  # create column with cumulative profit
  #mutate(csum=cumsum(Profit)) %>% 
  ## create column with random variable 0 or 1
  #mutate(profit_rand = rep(0:1, length.out = num_trades)) %>% #non random
  #mutate(profit_rand = sample(0:1, n(), replace = TRUE)) %>% # random
  # create column State
  mutate(NextState = ifelse(Profit>0, "tradewin",
                            ifelse(Profit<0, "tradeloss", NA)),
         # very simple logic: whenever cumulative sum is positive - we trade...
         Action = ifelse(Profit > 0, "ON",
                         ifelse(Profit <= 0, "OFF", NA)),
         Reward =  Profit,
         State = lag(NextState)) %>% # State column will be shifted down
  # remove row with empty data
  na.omit() %>% 
  # select only relevant columns
  select(State, Action, Reward, NextState) %>% 
  # ReinforcementLearning function seems to only work with 'dataframe'
  as.data.frame.data.frame() 

return(trading_systemDFRL)
}
  
 

}