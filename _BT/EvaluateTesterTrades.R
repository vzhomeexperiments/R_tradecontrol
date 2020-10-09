# evolution of trades for 28 pairs
# DRAFT EXPERIMENTING ONLY
# using FALCON_D robot
library(lazytrade)
library(tidyverse)
library(lubridate)

# folder with data
DFOLDER <- "_TEST_DATA"
DFOLDER <- "C:/Program Files (x86)/FxPro - Terminal2/tester/files"

# import data 
DF_TEST <- import_data(path_terminal = DFOLDER, trade_log_file = "OrdersResultsT2.csv")

# sort, create cumulative sum and visualize
DFR <- DF_TEST %>% 
  #sort by close date
  arrange(OrderCloseTime) %>% 
  mutate(CUMSUM_PNL = cumsum(Profit))

# visualize those trades
DFR %>% ggplot(aes(x = OrderCloseTime, y = CUMSUM_PNL))+geom_line()

## -------------
# Handling multiple files
DFOLDER1 <- "C:/Users/fxtrams/Documents/000_TradingRepo/FALCON_D/SIM/USDCHF"
DFOLDER2 <- "C:/Users/fxtrams/Documents/000_TradingRepo/FALCON_D/SIM/GBPUSD"
DFOLDER3 <- "C:/Users/fxtrams/Documents/000_TradingRepo/FALCON_D/SIM/EURUSD"
DFOLDER4 <- "C:/Users/fxtrams/Documents/000_TradingRepo/FALCON_D/SIM/USDJPY"
DFOLDER5 <- "C:/Users/fxtrams/Documents/000_TradingRepo/FALCON_D/SIM/USDCAD"

DFOLDER <- c(DFOLDER1, DFOLDER2, DFOLDER3, DFOLDER4, DFOLDER5)

for (FOLDER in DFOLDER) {
  

filesToTranslate <-list.files(FOLDER, pattern="*.csv", full.names=F)

for (FILE in filesToTranslate) {
  #FILE <- filesToTranslate[1]
  # import data 
  DF_TEST <- import_data(path_terminal = FOLDER, trade_log_file = FILE)
  #agregate
  if (!exists("DF_TEMP")) {
    DF_TEMP <- DF_TEST
  } else {
    DF_TEMP <- DF_TEMP %>% bind_rows(DF_TEST)
  }
  
}

  #agregate
  if (!exists("DF_TEMP1")) {
    DF_TEMP1 <- DF_TEMP
  } else {
    DF_TEMP1 <- DF_TEMP1 %>% bind_rows(DF_TEMP)
  }

# remove results from the single strategy
rm(DF_TEMP)

}


# sort, create cumulative sum and visualize
DFR <- DF_TEMP1 %>% 
  #sort by close date
  arrange(OrderCloseTime) %>% 
  mutate(CUMSUM_PNL = cumsum(Profit))

# visualize those trades
DFR %>% ggplot(aes(x = OrderCloseTime, y = CUMSUM_PNL))+geom_line()

# data quality check
plot(DFR$TicketNumber)
