# Generate files for backtest of 28 pairs on one robot for the last 60 days
library(lazytrade)

TO <- format(as.Date(Sys.Date()), "%Y.%m.%d")
FROM <- format(as.Date(Sys.Date()-60), "%Y.%m.%d")


Pairs = c("EURUSD", "GBPUSD", "AUDUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY",
          "EURGBP", "EURJPY", "EURCHF", "EURNZD", "EURCAD", "EURAUD", "GBPAUD",
          "GBPCAD", "GBPCHF", "GBPJPY", "GBPNZD", "AUDCAD", "AUDCHF", "AUDJPY",
          "AUDNZD", "CADJPY", "CHFJPY", "NZDJPY", "NZDCAD", "NZDCHF", "CADCHF")

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
                 dss_inifilepath = "C:/Users/fxtrams/Documents/000_TradingRepo/FALCON_D/AUTO_BACKTEST/1",
                 dss_inifilename = TestFileName,
                 dss_mode = "backtest")

}




