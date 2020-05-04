# learn how to optimize robot

library(lazytrade)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

Pairs = c("EURUSD", "GBPUSD", "AUDUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY",
          "EURGBP", "EURJPY", "EURCHF", "EURNZD", "EURCAD", "EURAUD", "GBPAUD",
          "GBPCAD", "GBPCHF", "GBPJPY", "GBPNZD", "AUDCAD", "AUDCHF", "AUDJPY",
          "AUDNZD", "CADJPY", "CHFJPY", "NZDJPY", "NZDCAD", "NZDCHF", "CADCHF")


TO <- format(as.Date(Sys.Date()-60), "%Y.%m.%d")
FROM <- format(as.Date(Sys.Date()-180), "%Y.%m.%d")

## files for optimization
for (PAIR in Pairs) {
  #PAIR <- "EURUSD"
  
  #name of the Report
  TestReportName <- paste0(PAIR, "-Report")
  #name of the File
  TestFileName <- paste0(PAIR, "-File",".ini")
  
  # test file for MT4 use for backtesting
  write_ini_file(mt4_Profile = "TradesControl",
                 mt4_Login = "70510022",
                 mt4_Password = "Yf2Jcy3g",
                 mt4_Server = "FxPro.com-Demo04",
                 mt4_TestExpert="FALCON_D\\Falcon_D",
                 mt4_TestExpertParameters="Falcon_D.set",
                 mt4_TestSymbol=PAIR,
                 mt4_TestPeriod="H1",
                 mt4_TestModel="2",
                 mt4_TestSpread="20",
                 mt4_TestOptimization="false",
                 mt4_TestDateEnable="true",
                 mt4_TestFromDate=FROM,
                 mt4_TestToDate=TO,
                 mt4_TestReport=TestReportName,
                 mt4_TestReplaceReport="false",
                 mt4_TestShutdownTerminal="true",
                 mt4_TestVisualEnable="false",
                 dss_inifilepath = "C:/Users/fxtrams/Documents/000_TradingRepo/FALCON_D/AUTO_BACKTEST/2",
                 dss_inifilename = TestFileName,
                 dss_mode = "opt")
  
}

## TDL log parameters to the csv results file!


for (PAIR in Pairs) {
  #PAIR <- Pairs[1]
  #path to the file
  path_2_ini_file <- paste0("C:\\Users\\fxtrams\\Documents\\000_TradingRepo\\FALCON_D\\AUTO_BACKTEST\\2\\",
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
  
  Sys.sleep(3)

}

# visualized the results!
DFR <- DF_TEMP1 %>%
  #sort by close date
  arrange(OrderCloseTime) %>%
  #create cumulative sum column
  mutate(CUMSUM_PNL = cumsum(Profit))

opt_create_graphs(DFR,
                  outp_path = "C:/Program Files (x86)/FxPro - Terminal4/tester/files")

