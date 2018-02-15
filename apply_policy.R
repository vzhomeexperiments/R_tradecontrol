# apply_policy function
# (C) 2018 Vladimir Zhbanko
# -------------------------
# Apply policy
# -------------------------
# Function that uses trading system magic number, model and uses policy of the model to write decision to the file
# trading_system - magic number
# model - Reinforcement learning model, 
# last_trade - information about last trade
# path_sandbox - path to sandbox where to write the decision
# path_sandbox <- "C:/Program Files (x86)/FxPro - Terminal4/MQL4/Files/"
apply_policy <- function(trading_system, model, last_trade, path_sandbox){
# recover decision based on updated policy
  decision <- policy(model)[last_trade]
# derive which terminal should be enabled (using path to sandbox) and using variable 'addition'
  is_T3 <- str_detect(path_sandbox, "Terminal3") 
  if(is_T3 == TRUE) { addition <- 200 }
  is_T4 <- str_detect(path_sandbox, "Terminal4")
  if(is_T4 == TRUE) { addition <- 300 }
# build dataframe for sending to the file
if(decision == "ON"){
  
  decision_DF <- data.frame(MagicNumber = trading_system + addition,
                            IsEnabled = 1)
  # -------------------------
  # Write Decision/Update Policy
  # -------------------------
  # write the file for MQL4 usage
  write.csv(decision_DF, file = paste0(path_sandbox, "SystemControl", as.character(decision_DF[1, 1]), ".csv"),
            row.names = FALSE)
  
} else {
  decision_DF <- data.frame(MagicNumber = trading_system + addition,
                            IsEnabled = 0)
  # -------------------------
  # Write Decision/Update Policy
  # -------------------------
  # write the file for MQL4 usage
  write.csv(decision_DF, file = paste0(path_sandbox, "SystemControl", as.character(decision_DF[1, 1]), ".csv"),
            row.names = FALSE)
}

}