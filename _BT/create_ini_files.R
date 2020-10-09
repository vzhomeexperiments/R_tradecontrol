# Create initialization files for robot optimization

## plan

# - create ini file
# - launch terminal.exe from R



# Vector of currency pairs
Pairs = c("EURUSD", "GBPUSD", "AUDUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY",
          "EURGBP", "EURJPY", "EURCHF", "EURNZD", "EURCAD", "EURAUD", "GBPAUD",
          "GBPCAD", "GBPCHF", "GBPJPY", "GBPNZD", "AUDCAD", "AUDCHF", "AUDJPY",
          "AUDNZD", "CADJPY", "CHFJPY", "NZDJPY", "NZDCAD", "NZDCHF", "CADCHF")   

# path to the trading robot repo
path_robot <- "C:/Users/fxtrams/Documents/000_TradingRepo/FALCON_D/AUTO_BACKTEST"
name_file <- "test.ini"
out_file_path <- file.path(path_robot, name_file)
# path to the tester
path_tester <- "C:/Program Files (x86)/FxPro - Terminal4/tester"
# path to the file folder
path_files <- file.path(path_tester, 'files')

##

 
## ==== generate file ====
content <- c("; common settings",
"Profile=TradesControl",
";MarketWatch=set2.set",
"Login=12345678",
"Password=xxxYYYXJS",
"Server=FxPro.com-Demo04",
"AutoConfiguration=false",
"EnableNews=false",
"; start strategy tester",
"TestExpert=FALCON_D\\Falcon_D",
"TestExpertParameters=Falcon_D.set",
"TestSymbol=EURUSD",
"TestPeriod=H1",
"TestModel=2",
"TestSpread=20",
";TestOptimization=false",
"TestOptimization=true",
"TestDateEnable=true",
"TestFromDate=2019.08.21",
"TestToDate=2019.12.21",
"TestReport=tester\\FalconDReport_EURUSD",
"TestReplaceReport=false",
"TestShutdownTerminal=true")


## ==== end of generate file ====

writeLines(content, out_file_path)

command <- paste('"C:\\Program Files (x86)\\FxPro - Terminal4\\terminal.exe"',
                 '/portable C:\\Users\\fxtrams\\Documents\\000_TradingRepo\\FALCON_D\\AUTO_BACKTEST\\test.ini')
# try to run from R
system(command)
