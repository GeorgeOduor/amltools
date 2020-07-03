disbFunction = function (Disbursement_file, filter = c("all", "mfanisi", "branch")){
  # Disbursement_file = Disbursement
  # filter = "all"
  filter = switch(str_to_lower(filter), all = c("Mfanisi_2",
                                                "Mfanisi_1", "Branch"), mfanisi = c("Mfanisi_2",
                                                                                    "Mfanisi_1"), branch = "Branch")
  disbCleaned = Disbursement_file %>% separate(CreditOfficerName,
                                          into = c(NA, "CreditOfficerName"), sep = "-") %>%
    mutate(source = ifelse(ProductID == "MLS904", "Mfanisi_2",
                           ifelse(grepl("MFC", ProductID, T), "Mfanisi_1",
                                  "Branch")), Product = ifelse(source ==
                                                                 "Branch", CreditOfficerName, source)) %>% filter(source %in%
                                                                                                                    filter)
  disbreport1 = disbCleaned %>% mutate(LoanAmount = as.numeric(LoanAmount)) %>%
    reshape2::dcast(Product ~ DisbursedOn, value.var = "LoanAmount",fun.aggregate = sum) %>% adorn_totals("row") %>%
    adorn_totals("col") %>% format(big.mark = ",")

  disbreport2 = disbCleaned %>% mutate(LoanAmount = as.numeric(LoanAmount)) %>%
    reshape2::dcast(Product ~ DisbursedOn, value.var = "LoanAmount",fun.aggregate = length) %>% adorn_totals("row") %>%
    adorn_totals("col") %>% format(big.mark = ",")
  disbreport=list(disbreport1 = disbreport1,disbreport2=disbreport2)
  return(disbreport)
}



DuplicatesCheck <- function(data) {

  disb = data %>% mutate(PhoneNumber2 = paste(PhoneNumber,Loanseries,sep = "_"),
                         AccountI2 = paste(AccountID,Loanseries,sep="_"),
                         IDNumber2 = paste(IDNumber,Loanseries,sep = "_"))


  disb = disb %>% split(.$ProductID)

  prods = c("MFC802","MFC805","MFC806","MLS904")

  # duplicates by phone
  # mfc802
  mfc802 = disb$MFC802
  mfc805 = disb$MFC805
  mfc806 = disb$MFC806



  dupsbyphone = rbind(
    mfc802[duplicated(mfc802$PhoneNumber2) | duplicated(mfc802$PhoneNumber2,fromLast = T),] %>%
      select(TitleOfAccount,IDNumber,PhoneNumber,DisbursedOn,LoanAmount),
    mfc805[duplicated(mfc805$PhoneNumber2) | duplicated(mfc805$PhoneNumber2,fromLast = T),] %>%
      select(TitleOfAccount,IDNumber,PhoneNumber,DisbursedOn,LoanAmount),
    mfc802[duplicated(mfc806$PhoneNumber2) | duplicated(mfc806$PhoneNumber2,fromLast = T),] %>%
      select(TitleOfAccount,IDNumber,PhoneNumber,DisbursedOn,LoanAmount)
  ) %>% mutate(DuplicatedBy = "Phone Number")

  dupsbyAcc = rbind(
    mfc802[duplicated(mfc802$AccountI2) | duplicated(mfc802$AccountI2,fromLast = T),] %>%
      select(TitleOfAccount,IDNumber,PhoneNumber,DisbursedOn,LoanAmount),
    mfc805[duplicated(mfc805$AccountI2) | duplicated(mfc805$AccountI2,fromLast = T),] %>%
      select(TitleOfAccount,IDNumber,PhoneNumber,DisbursedOn,LoanAmount),
    mfc806[duplicated(mfc806$AccountI2) | duplicated(mfc806$AccountI2,fromLast = T),] %>%
      select(TitleOfAccount,IDNumber,PhoneNumber,DisbursedOn,LoanAmount)
  ) %>% mutate(DuplicatedBy = "Account Id")

  dupsbyID = rbind(
    mfc802[duplicated(mfc802$IDNumber2) | duplicated(mfc802$IDNumber2,fromLast = T),] %>%
      select(TitleOfAccount,IDNumber,PhoneNumber,DisbursedOn,LoanAmount),
    mfc805[duplicated(mfc802$IDNumber2) | duplicated(mfc802$IDNumber2,fromLast = T),] %>%
      select(TitleOfAccount,IDNumber,PhoneNumber,DisbursedOn,LoanAmount),
    mfc806[duplicated(mfc806$IDNumber2) | duplicated(mfc806$IDNumber2,fromLast = T),] %>%
      select(TitleOfAccount,IDNumber,PhoneNumber,DisbursedOn,LoanAmount)
  ) %>% mutate(DuplicatedBy = "National Id")

  allDups = rbind(dupsbyAcc,dupsbyID,dupsbyphone) %>% group_by(DuplicatedBy,DisbursedOn) %>% summarise(Counts = n()) %>% filter(!is.na(DisbursedOn))

  return(list("duplicaesbyacc" = dupsbyAcc,
              "duplicatesbyID" = dupsbyID,
              "duplicatesbyphone" = dupsbyphone,
              "allDups" = allDups))
}
# DuplicatesCheck(data = disb)


# TRX FUNCTIONS

maisha_dailytrx_monitor <- function(trxData,valuevar,trxamount_flag,trx_countflag,day) {
  PastDays= as.numeric(Sys.Date()-as.Date(day))
  start = Sys.time()
  message("Generating daily aggregates")
  transactions = trxData %>%select(-1) %>%
    filter(`TrxDate` <= Sys.Date()-PastDays) %>%
    group_by(`AccountID`, `TrxTypeID`) %>%
    mutate(AmountTrans = as.numeric(Debit)+as.numeric(Credit)) %>%
    summarise(NumerOfTransactions = n(),Amount = sum(AmountTrans)) %>%
    arrange(desc(NumerOfTransactions)) %>%
    reshape2::dcast(.,`AccountID`~`TrxTypeID`,value.var = valuevar) %>%
    mutate(Name = trxData$`AccountName`[match(`AccountID`,trxData$`AccountID`)]) %>%
    select(Name,everything()) %>%
    mutate_all(.funs = function(x)ifelse(is.na(x),0,x)) %>%
    janitor::adorn_totals("col")
  # accounts check
  message("Checking transactions anomalies")
  if (valuevar == "Amount") {
    transactions_check <- transactions %>% filter(TD >= as.numeric(trxamount_flag)|TC >= as.numeric(trxamount_flag) | CC>=as.numeric(trxamount_flag) | CD >= as.numeric(trxamount_flag) )

  }else{
    transactions_check = transactions %>% filter(Total >= 10)
  }

  output = list("transactions" = transactions,"transactions_check"=transactions_check)


  return(output)


}


maisha_weeklytrx_monitor <- function(trxData,valuevar){
  trasndata = trxData  %>%
    group_by(`AccountID`, `TrxTypeID`) %>%
    mutate(AmountTrans = as.numeric(Debit)+as.numeric(Credit)) %>%
    summarise(NumerOfTransactions = n(),Amount = sum(AmountTrans)) %>%
    reshape2::dcast(.,`AccountID`~`TrxTypeID`,value.var = valuevar) %>%
    mutate(Name = trxData$`AccountName`[match(`AccountID`,trxData$`AccountID`)]) %>%
    select(Name,everything()) %>%
    mutate_all(.funs = function(x)ifelse(is.na(x),0,x)) %>%
    janitor::adorn_totals("col") %>% filter(Total >= 20)%>%
    arrange(desc(Total))



  if (valuevar == "Amount") {
    trasndata = trasndata %>% filter(Total >= 1000000)
  }



  return(trasndata)
}

# maisha_weeklytrx_monitor(trxData = largeTransactions,"Amount")
# maisha_weeklytrx_monitor(trxData = largeTransactions,"NumerOfTransactions")


valueboxes = function(trxData){
  summary_ = trxData %>% summarise(
    TotalCredit = sum(as.numeric(Credit),na.rm = T),
    TotalDebit = sum(as.numeric(Debit),na.rm = T),
    Totatrans = n()
  )

  return(summary_)
}



