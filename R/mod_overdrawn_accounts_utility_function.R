# get hold of revious deposits file
# prev_depfile <- data.table::fread("//MADDC00001/Shares/maishahomes/DWANGARI/Daily_Reports/2020/Jun/28.06.2020/Deposit_28.06.2020.csv",showProgress = F) %>%
#   select(ClientID,ClearBalance,AccountName,OpenedDate) %>%
#   filter(ClearBalance < 0)
# active_depfile <- data.table::fread("//MADDC00001/Shares/maishahomes/DWANGARI/Daily_Reports/2020/Jun/29.06.2020/Deposit_29.06.2020.csv",showProgress = F) %>%
#   select(AccountID,AccountName,ProductName,ClientID,ClearBalance,OpenedDate) %>%
#   filter(ClearBalance < 0)

overdrawn_function <- function(prev_depfile,active_depfile) {
  accounts = prev_depfile %>% full_join(active_depfile,by=c("ClientID")) %>%
    rename(Prev_ClearBalance = ClearBalance.x,Active_ClearBalance = ClearBalance.y)

  #  accounts checklist

  accounts_check <- accounts %>%
    mutate(AccountsCheck = ifelse(is.na(Prev_ClearBalance),"New Negative Account",
                                  ifelse(abs(Active_ClearBalance) - abs(Prev_ClearBalance)< 0 | is.na(Active_ClearBalance),"New Deposit",
                                         ifelse(abs(Active_ClearBalance) - abs(Prev_ClearBalance)> 0,"Overdrawn With Changes","No Change"))),
           AccountName = ifelse(is.na(AccountName.y),AccountName.x,AccountName.y),
           OpenedDate = ifelse(is.na(OpenedDate.y),OpenedDate.x,OpenedDate.y)
    ) %>%
    select( ClientID,AccountID,ProductName,OpenedDate,Prev_ClearBalance,Active_ClearBalance,AccountName,OpenedDate,AccountsCheck) %>%
    split(.$AccountsCheck)

  return(accounts_check)
}

# overdrawn_function()
