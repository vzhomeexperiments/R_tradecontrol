#ini files for production terminals

library(lazytrade)

## TERMINAL 1
# test file for MT4 start with specific parameters
write_ini_file(mt4_Profile = "T1_FALCON_T",
               mt4_Login = "70510020",
               mt4_Password = "1D5w1nRJ", 
               mt4_Server = "FxPro.com-Demo04",
               dss_inifilepath = "C:/Users/fxtrams/Documents/000_TradingRepo/AutoLaunchMT4",
               dss_inifilename = "prod_T1.ini",
               dss_mode = "prod")

## TERMINAL 2
# test file for MT4 start with specific parameters
write_ini_file(mt4_Profile = "FUNDAMENTAL",
               mt4_Login = "70510018",
               mt4_Password = "tWa5lM2s", 
               mt4_Server = "FxPro.com-Demo04",
               dss_inifilepath = "C:/Users/fxtrams/Documents/000_TradingRepo/AutoLaunchMT4",
               dss_inifilename = "prod_T2.ini",
               dss_mode = "prod")

## TERMINAL 3
# test file for MT4 start with specific parameters
write_ini_file(mt4_Profile = "T3_FALCON_T",
               mt4_Login = "70510022",
               mt4_Password = "Yf2Jcy3g", 
               mt4_Server = "FxPro.com-Demo04",
               dss_inifilepath = "C:/Users/fxtrams/Documents/000_TradingRepo/AutoLaunchMT4",
               dss_inifilename = "prod_T3.ini",
               dss_mode = "prod")

## TERMINAL 4
# test file for MT4 start with specific parameters
write_ini_file(mt4_Profile = "TradesControl",
               mt4_Login = "70510021",
               mt4_Password = "aYIGbpF7", 
               mt4_Server = "FxPro.com-Demo04",
               dss_inifilepath = "C:/Users/fxtrams/Documents/000_TradingRepo/AutoLaunchMT4",
               dss_inifilename = "prod_T4.ini",
               dss_mode = "prod")
