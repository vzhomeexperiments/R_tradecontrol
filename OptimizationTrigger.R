# 20170822 
# This is a dedicated script for 1st Terminal /Demo account/
# Copyright (C)Vladimir Zhbanko 20170822

# Version 1.0 (started from 07_TradeAnalyser_T1_3_v7.2.R)
#           1 (adding profit factor to the output table)
#

#  - **Main objective:** to output summary table of the systems that needs to be re-trained or re-optimized
#  - LOGIC:
# 1.	ON TERMINAL 1
# 2.	Group by trading systems
# 3.	analyse last 20 trades
# 4.	get their profit factor
# 5.	filter those systems where profit factor is lower than 1.0 write them to the table
# 6.	run this analysis bi-weekly, every Saturday, etc
# 7.	re-train only the systems that will pop up in the list...

#  - Only apply to the systems currently under development!
#  - List of systems under development...
#  - Path to trading system project...

# use only tidyverse package that contains all others...
library(tidyverse)

# ----------- Main Steps -----------------
# -- Read trading results from Terminal 1
# -- Split trading results from Terminal 1 into categories
# -- Depend on conditions allow trades in Terminal 2 and Terminal 4
# -- Monitor trade results using profit factor on both Terminals when profitable trades continue during 10 trades
# -- Start/Stop trades on Terminals at desired profit factor levels of the systems

# =============================================
# *************Used Functions******************
# =============================================
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
# =============================================
# ************End of Used Functions************
# =============================================

# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
# terminal 1 path
path_T2 <- "C:/Program Files (x86)/FxPro - Terminal1/MQL4/Files/"
# terminal 3 path
path_T4 <- "C:/Program Files (x86)/FxPro - Terminal3/MQL4/Files/"
# trading system project folder
path_PRJCT <- "C:/Users/fxtrams/Documents/00_FXTRAMS_2.0/01_TradingStrategies/19_AI_TradeEuclidean/"

# -------------------------
# read data from trades in terminal 2
# -------------------------
DFT2 <- try(read_csv(file = paste(path_T2, "OrdersResultsT1.csv", sep = ""), 
                     col_names = c("MagicNumber", "TicketNumber", "OrderStartTime", 
                                   "OrderCloseTime", "Profit", "Symbol", "OrderType"),
                     col_types = "iiccdci"), silent = TRUE)

# data frame preparation, function ymd_hms from 'lubridate' package
DFT2$OrderStartTime <- lubridate::ymd_hms(DFT2$OrderStartTime)
DFT2$OrderCloseTime <- lubridate::ymd_hms(DFT2$OrderCloseTime)
DFT2$OrderType      <- as.factor(DFT2$OrderType)


# -------------------------
# data frame T2 analysis and manipulation
# -------------------------
# ----------------
# Summarise orders acc.to the logic below
#-----------------
# 1.	ON TERMINAL 1
# 2.	Group by trading systems
# 3.	analyse last 20 trades --> data is in *DFT2_L*
# 4.	get their profit factor
# 5.	filter those systems where profit factor is lower than 1.6 write them to the table
# 6.	run this analysis weekly, every Friday
# 7.	re-train only the systems 
# ----------------
# Implementation of logic
#-----------------
#### SORTING AND DECIDE IF SYSTEM NEEDS TO BE RE-TRAINED/RE-OPTIMIZED #### -----------------------------
# 4. Last 20 orders on DEMO && pr.fact < 1.0 SUGGEST TO RE-TRAINE           -> DFT2_1 
DFT2_1 <- DFT2 %>%  # filtered to contain last 20 orders for each system
  group_by(MagicNumber) %>% 
  arrange(MagicNumber, desc(OrderCloseTime)) %>% 
  filter(row_number() <= 21) %>% 
  profitFactor(20) %>% 
  ungroup() %>% 
  filter(PrFact < 0.7) %>% 
  select(MagicNumber, PrFact) %>% 
  mutate(ToOptimize = 1) %>% 
  inner_join(y = readxl::read_excel(path = paste0(path_PRJCT, "Setup_20170703.xlsx")), by = c("MagicNumber" = "Magic")) %>% 
  write_csv(path = paste0(path_PRJCT, Sys.Date(), "-Re-Train", ".csv"))


##======================================== end of script
