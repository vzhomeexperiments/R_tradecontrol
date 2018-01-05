# 20161109 
# This is a dedicated script for 2nd and 4th Terminals /Demo account/
# Copyright (C)Vladimir Zhbanko 20170108
# Version 06 (copy of Version05) just dedicated to 2 and 4 terminals 
#  - managing both 2nd and 4th terminals within the same script
#  - having a function that sends a commands (writing files) to the specified path
#  - more robust programming methods implementation, using dplyr packages
#  - real account is managed on the 10 last trades basis to protect against deterioration of the systems
# Version 07 
#  - including reading file to check for the news events and block trades on accounts
# Version 07.1
#  - stopping real accoun t if there are more than 2 consecutive losses
# Version 7.2 to return to this use code in console: system("git checkout cde7d6ea")
#  - version only account for last 10 trades in Demo terminal and demo system will always trade!
# Version 7.2.1 to return to this use code in console: system("git checkout 93d1ffc8")
#  - simplified logic and code

# packages used
library(lubridate) #install.packages("lubridate")
library(dplyr)
library(readr)   #used to import data read_csv

# ----------- Applied Logic -----------------
# -- Read trading results from Terminal 1
# -- Split trading results from Terminal 1 into categories using profit factor
# -- Depend on conditions of LAST 10 trades in Terminal 1, allow trades in Terminal 3
#   -- on last 10 trades in T1
#   -- enable T3 when Profit factor of 10 trades > 1.6
#   -- enable T3 when Profit factor of 2 trades > 1.2
# -- Start/Stop trades on Terminals at MacroEconomic news releases

# ----------------
# Used Functions
#-----------------
#===============================
# WRITE COMMAND VIA CSV FUNCTION
#===============================
# write a function that will write csv files to the folder with specified address
writeCommandViaCSV <- function(x, pathToTerminal, fileName = "SystemControl"){
  if(exists("x") == TRUE && nrow(x) != 0) {
  for(i in 1:nrow(x))
  {
    # write the file for MQL4 usage
    write.csv(x[i, ], file = paste(pathToTerminal, fileName, as.character(x[i, 1]), ".csv", sep = ""),
              row.names = FALSE)
  }
  } 
}

#===============================
# PROFIT FACTOR FUNCTION
#===============================
# function that returns the profit factors of the systems
#
# x - data frame with orders
#     df must contain MagicNumber and Profit columns!
# numOrders - desired number of orders to base profit factor calculation
# 
#
profitFactor <- function(x, numOrders){
  # generate DF with only MagicNumbers when > 10 trades and all trades are losers
  DF_L <- x %>%
    group_by(MagicNumber) %>%
    summarise(nOrders = n())%>%
    filter(nOrders > numOrders)%>%
    select(MagicNumber)%>%
    # subset only rows that contans magic numbers from x
    inner_join(x, by = "MagicNumber")%>%
    group_by(MagicNumber)%>%
    filter(Profit < 0) %>%
    summarise(Loss = abs(sum(Profit)))
  # generate DF with only MagicNumbers when > 10 trades and all trades are profits
  DF_P <- x %>%
    group_by(MagicNumber) %>%
    summarise(nOrders = n())%>%
    filter(nOrders > numOrders)%>%
    select(MagicNumber)%>%
    # subset only rows that contans magic numbers from x
    inner_join(x, by = "MagicNumber")%>%
    group_by(MagicNumber)%>%
    filter(Profit > 0) %>%
    summarise(Gain = abs(sum(Profit)))
  # generate DF containing profit factor of all systems
  DF <- DF_P %>%
    full_join(DF_L, by = "MagicNumber")
  # replace any NA with zeroes!
  DF[is.na(DF)] <- 1
  # calculate profit factors of the each system!
  DF_PF <- DF%>%
    group_by(MagicNumber)%>%
    mutate(PrFact = Gain/(0.001+Loss))%>%
    select(MagicNumber, PrFact)
  return(DF_PF)
}

# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
# terminal 1 path
path_T2 <- "C:/Program Files (x86)/FxPro - Terminal1/MQL4/Files/"
# terminal 3 path
path_T4 <- "C:/Program Files (x86)/FxPro - Terminal3/MQL4/Files/"

# -------------------------
# read data from trades in terminal 1
# -------------------------
DFT2 <- try(read_csv(file = paste(path_T2, "OrdersResultsT1.csv", sep = ""), 
                     col_names = c("MagicNumber", "TicketNumber", "OrderStartTime", 
                                   "OrderCloseTime", "Profit", "Symbol", "OrderType"),
                     col_types = "iiccdci"), silent = TRUE)

# data frame preparation
DFT2$OrderStartTime <- ymd_hms(DFT2$OrderStartTime)
DFT2$OrderCloseTime <- ymd_hms(DFT2$OrderCloseTime)
DFT2$OrderType      <- as.factor(DFT2$OrderType)

# get last 10 trades for each Magic system and arrange orders to have descending order
DFT2_L <- DFT2 %>%  # filtered to contain last 10 orders for each system
  group_by(MagicNumber) %>% 
  arrange(MagicNumber, desc(OrderCloseTime)) %>% 
  filter(row_number() <= 11)

# get last 2 trades for each Magic system and arrange orders to have descending order
DFT2_L2 <- DFT2 %>%  # filtered to contain last 10 orders for each system
  group_by(MagicNumber) %>% 
  arrange(MagicNumber, desc(OrderCloseTime)) %>% 
  filter(row_number() <= 3)
# -------------------------
# read data from trades in terminal 4
# -------------------------
#try(read.csv(file = paste(path_T4, "OrdersResultsT4.csv", sep = ""), header = FALSE), silent = TRUE)
# use try to avoid error!!!
DFT4 <- try(read_csv(file = paste(path_T4, "OrdersResultsT3.csv", sep = ""),
                     col_names = c("MagicNumber", "TicketNumber", "OrderStartTime", 
                                   "OrderCloseTime", "Profit", "Symbol", "OrderType"),
                     col_types = "iiccdci"), silent = TRUE)
                     
# data frame preparation
DFT4$OrderStartTime <- ymd_hms(DFT4$OrderStartTime)
DFT4$OrderCloseTime <- ymd_hms(DFT4$OrderCloseTime)
DFT4$OrderType      <- as.factor(DFT4$OrderType)

# get last 15 trades for each Magic system and arrange orders to have descending order
DFT4_L <- DFT4 %>%  # filtered to contain last 15 orders for each system
  group_by(MagicNumber) %>% 
  arrange(MagicNumber, desc(OrderCloseTime)) %>% 
  filter(row_number() < 15)
# get last 2 trades for each Magic system and arrange orders to have descending order
DFT4_L2 <- DFT4 %>%  # filtered to contain last 2 orders for each system
  group_by(MagicNumber) %>% 
  arrange(MagicNumber, desc(OrderCloseTime)) %>% 
  filter(row_number() <= 2)


# -------------------------
# data frame T1 analysis and manipulation
# -------------------------
# ----------------
# Summarise orders
#-----------------
# Logic for DEMO and REAL terminal:
# == Always allow trades on DEMO terminal
# == Not enable Real account until > than 10 trades on Demo account were completed
# == Enable Real account when > than 10 trades and profit factor in T1 is > 1.6
# == Enable Real account when > than 2 trades and profit factor in T1 is > 1.2
# == Not trade when there is a news macroscopic event

# ----------------
# Implementation of logic
#-----------------
#### SORTING AND DECIDE IF TRADING ON THE DEMO ACCOUNT #### -----------------------------
# DEMO always allow trades                               
DFT2_L %>%
  group_by(MagicNumber) %>%
  summarise(nOrders = n()) %>%
  #filter(nOrders <= 10)%>%
  select(MagicNumber) %>%
  mutate(IsEnabled = 1) %>% 
  # Write command "allow"
  writeCommandViaCSV(path_T2)
# ======================= OK

#### DECIDE IF TRADING ON THE REAL ACCOUNT #### -----------------------------
# Last 10 orders on DEMO && pr.fact > 1.6 start trade REAL         
DFT2_L %>%
  profitFactor(10) %>% 
  ungroup() %>% 
  filter(PrFact >= 1.6) %>% 
  select(MagicNumber) %>% 
  mutate(MagicNumber = MagicNumber + 200, IsEnabled = 1) %>% 
  # Write command "allow"
  writeCommandViaCSV(path_T4)

# 4. Last 2 orders on DEMO && pr.fact > 1.2 trade REAL          
DFT2_L2 %>%
  profitFactor(2) %>% 
  ungroup() %>% 
  filter(PrFact >= 1.2) %>% 
  select(MagicNumber) %>% 
  mutate(MagicNumber = MagicNumber + 200, IsEnabled = 1) %>% 
  # Write command "allow"
  writeCommandViaCSV(path_T4)

#### DECIDE IF NOT TO TRADING ON THE REAL ACCOUNT #### -----------------------------
# 4. Last 9 orders on DEMO && pr.fact < 1.6 start trade REAL
DFT2_L %>%
  profitFactor(10) %>% 
  ungroup() %>% 
  filter(PrFact < 1.6) %>% 
  select(MagicNumber) %>% 
  mutate(MagicNumber = MagicNumber + 200, IsEnabled = 0) %>% 
  # Write command "allow"
  writeCommandViaCSV(path_T4)

# 4. Last 9 orders on DEMO && pr.fact < 1.2 stop trade REAL
DFT2_L2 %>%
  profitFactor(2) %>% 
  ungroup() %>% 
  filter(PrFact < 1.2) %>% 
  select(MagicNumber) %>% 
  mutate(MagicNumber = MagicNumber + 200, IsEnabled = 0) %>% 
  # Write command "allow"
  writeCommandViaCSV(path_T4)


##========================================
# stopping all systems when macroeconomic event is present
DF_NT <- read_csv(file= file.path(path_T2, "01_MacroeconomicEvent.csv"), col_types = "i")
if(DF_NT[1,1] == 1) {
  # disable trades
  DF_DisableT2 <- DFT2 %>% 
    group_by(MagicNumber) %>% select(MagicNumber) %>% mutate(IsEnabled = 0)
  DF_DisableT4 <- DFT4 %>% 
    group_by(MagicNumber) %>% select(MagicNumber) %>% mutate(IsEnabled = 0)
  
  writeCommandViaCSV(DF_DisableT2, path_T2)
  writeCommandViaCSV(DF_DisableT4, path_T4)
}


##========================================
## write a summary T2 and T4
# Terminal 2
# Summary results
DFT2 %>%
  group_by(MagicNumber) %>%
  summarise(SumProfit = sum(Profit),
            nOrders = n()) %>% 
  # create a summary file for review
  write.csv(file = paste(path_T2, "SystemSummary", ".csv", sep = ""),
            row.names = FALSE)

# Terminal 4
# Summary results
DFT4%>%
  group_by(MagicNumber) %>%
  summarise(SumProfit = sum(Profit),
            nOrders = n()) %>% 
  # create a summary file for review
  write.csv(file = paste(path_T4, "SystemSummary", ".csv", sep = ""),
          row.names = FALSE)