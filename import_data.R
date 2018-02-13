# import data function
# (C) 2018 Vladimir Zhbanko
# -------------------------
# Import Data to R
# -------------------------
# Function imports file and change data column type
# Function return the dataframe with trade data

import_data <- function(path_terminal, trade_log_file){
  # path_terminal <- "C:/Program Files (x86)/FxPro - Terminal4/MQL4/Files/"
  # trade_log_file <- "OrdersResultsT4.csv"
  require(tidyverse)
  require(lubridate)
  DFT1 <- try(read_csv(file = file.path(path_terminal, trade_log_file), 
               col_names = c("MagicNumber", "TicketNumber", "OrderStartTime", 
                             "OrderCloseTime", "Profit", "Symbol", "OrderType"),
               col_types = "iiccdci"), 
      silent = TRUE)
  if(class(DFT1)[1] == "try-error") {stop("Error reading file. File with trades may not exist yet!",
                                       call. = FALSE)}
  if(!nrow(DFT1)==0){
    # data frame preparation
    DFT1$OrderStartTime <- ymd_hms(DFT1$OrderStartTime)
    DFT1$OrderCloseTime <- ymd_hms(DFT1$OrderCloseTime)
    DFT1$OrderType      <- as.factor(DFT1$OrderType)
    
    return(DFT1)
  } else {
    stop("No trades executed so far. Trade data log is empty!",
         call. = FALSE)
    }

}