# use 28 files to backtest_28_pairs.R
library(lazytrade)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

Pairs = c("EURUSD", "GBPUSD", "AUDUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY",
          "EURGBP", "EURJPY", "EURCHF", "EURNZD", "EURCAD", "EURAUD", "GBPAUD",
          "GBPCAD", "GBPCHF", "GBPJPY", "GBPNZD", "AUDCAD", "AUDCHF", "AUDJPY",
          "AUDNZD", "CADJPY", "CHFJPY", "NZDJPY", "NZDCAD", "NZDCHF", "CADCHF")

for (PAIR in Pairs) {
  #PAIR <- Pairs[1]
  #path to the file
  path_2_ini_file <- paste0("C:\\Users\\fxtrams\\Documents\\000_TradingRepo\\FALCON_D\\AUTO_BACKTEST\\1\\",
                            PAIR, "-File",".ini")

  #generate command string
  command <- paste('"C:\\Program Files (x86)\\FxPro - Terminal4\\terminal.exe"',
                   '/portable ', path_2_ini_file)


  # crean content of the tester folder
  file.remove("C:/Program Files (x86)/FxPro - Terminal4/tester/files/OrdersResultsT2.csv")

  # try to run from R
  system(command)

  # retrieve the results of the robot backtest and save that to the separate frame
  DF_TEMP <- import_data("C:/Program Files (x86)/FxPro - Terminal4/tester/files",
                         "OrdersResultsT2.csv")
  #agregate
  if (!exists("DF_TEMP1")) {
    DF_TEMP1 <- DF_TEMP
  } else {
    DF_TEMP1 <- DF_TEMP1 %>% bind_rows(DF_TEMP)
  }
  
  Sys.sleep(15)

}

# visualized the results!
DFR <- DF_TEMP1 %>%
  #sort by close date
  arrange(OrderCloseTime) %>%
  #create cumulative sum column
  mutate(CUMSUM_PNL = cumsum(Profit))

opt_create_graphs(DFR,
                  outp_path = "C:/Program Files (x86)/FxPro - Terminal4/tester/files")

