#test ini file

library(lazytrade)

# test file for MT4 start with specific parameters
write_ini_file(mt4_Profile = "FUNDAMENTAL",
               mt4_Login = "70510018",
               mt4_Password = "tWa5lM2s", 
               mt4_Server = "FxPro.com-Demo04",
               dss_inifilepath = "C:/Users/fxtrams/Documents/000_TradingRepo/FALCON_D/AUTO_BACKTEST",
               dss_inifilename = "prod_profile.ini",
               dss_mode = "prod")


