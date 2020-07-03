source("R/disbursementfunction.R")
# disbfile =
limits_analysis <- function(date,disbfile,livelim) {
  date = Sys.Date()-1
  day = format(anytime::anydate(date),"%d.%m.%Y")
  month = lubridate::month(date,T)
  year = lubridate::year(date)
  pathtodisbfile = paste0("//MADDC00001/Shares//maishahomes/DWANGARI/Daily_Reports/",year,"/",month,"/",day,"/Disbursement_",day,".xlsx")
  pathtolimits = "files/finallimits.xlsx"
 # get disbursements
  if (file.exists(pathtodisbfile)){
    disbursment <- read.xlsx(detectDates = T,pathtodisbfile)
  }else if (!is.null(disbfile)) {
    # req(disbfile)
    disbursment <- rio::import(disbfile$datapath)
  }else{
    print(disbfile$datapath)
  }


  # get limits
  if (file.exists(pathtolimits)) {
    livelims <- rio::import(pathtolimits)
  }else if (!is.null(livelim)) {
    livelims <- rio::import(livelim$datapath)
  }
  req(disbursment)
  req(livelims)
# disb = rio::import("d:/workprojects/Disbursements.xlsx")

  # limits check
  disb = disbursment %>%
    mutate(Net_Loan = ifelse(grepl("MFC802|MLS904",ProductID,T),LoanAmount/1.10,
                                     ifelse(ProductID == "MFC805",LoanAmount/1.048,LoanAmount/1.072)),
           IDNumber = trimws(gsub("_","",IDNumber)))

   limitscheck =  disb %>% left_join(livelims ,by = c("PhoneNumber" = "MSISDN")) %>%
    select(LoanAmount,Net_Loan,Final_Limit,Loanseries,DisbursedOn,PhoneNumber,ProductID) %>%
    mutate(Variance = as.numeric(Final_Limit)  - as.numeric(Net_Loan),
           LimitsCheck = ifelse(Variance < 0 ,"Above Limit","Within Limit"))# %>%
     # left_join(disbursment %>% filter(Loanseries == max(as.numeric(Loanseries))-1) %>% select(PhoneNumber,LoanAmount),by="PhoneNumber")




  # disbursements report
  # 1. Daily Disbursements
  maisha_daily_report <- function(disb,type) {

    type = switch (type,
      "day" = as.character(max(as.Date(disb$DisbursedOn))),
      "cummulative" = "Total"
    )
    # Disbursement = disb
    report = disbFunction(disb,"all") %>%  map(~select(.,"Product",type) %>%
                          mutate(Chanell = ifelse(grepl("mfanisi",Product,T),"Mfanisi","Branch"))  %>%
                          mutate_at(.vars = type,.funs = function(x)as.numeric(gsub(",","",x))) %>%
                          filter(Product !=  "Total") %>%
                          split(.$Chanell) %>% map_df(~adorn_totals(.,"row") %>% select(Chanell,everything()))) %>%
      bind_cols() %>% as_tibble() %>% clean_names(case = "upper_camel") %>%
      select(1,2,3,6) %>% mutate(Product = ifelse(Product == "Total","Sub-Total",Product)) %>%
      bind_rows(filter(.,Product == "Sub-Total") %>% adorn_totals("row") %>%
                  filter(Product == "-")%>% mutate(Product = Chanell,Chanell = "-")) %>%
      mutate(Product = trimws(Product)) %>% dplyr::rename("Loan Amount" = 3) %>% dplyr::rename("Counts" = 4) %>%
      mutate_all(.funs = function(x)format(x,big.mark = ","))

    return(report)
  }

  disbreports = list("day","cummulative") %>% map(~ maisha_daily_report(disb = disb,type = .)) %>% setNames(list("day","cummulative"))

  alldisb =  disbFunction(disb,"all")

 alldisb = disbFunction(disb,"all")$disbreport1 %>% select(-Total) %>% reshape2::melt(id.vars = "Product") %>%
   filter(Product != "Total") %>% mutate(value = as.numeric(gsub(",","",value))) %>% split(.$Product) %>%
   map_df(~mutate(.,Cumulative = cumsum(value))) %>%
   mutate(Product = ifelse(!grepl("Mfanisi",Product,T),"Branch",Product)) %>% group_by(Product,variable) %>%
   summarise(value = sum(value),Cumulative = sum(Cumulative))

duplicates = DuplicatesCheck(data = disbursment)

# disb %>%  %>% select(DisbursedOn)

  out = list(
    "disbursment" = disbursment,"limitscheck" = limitscheck,"disbreports" = disbreports,
    "alldisb" =alldisb ,dups = duplicates
  )

  return(out)
}