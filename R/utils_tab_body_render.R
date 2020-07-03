aml_body <- function() {
  tabItems(
    tabItem(tabName = "hometab","Welcome home"),
    tabItem(tabName = "loanLimits",mod_loanlimits_ui("loanlims")),
    tabItem(tabName = "trxMonitoring",
            mod_trx_monitoring_ui("trx_monitoring_ui_1")),
    tabItem(tabName = "overdrawnAccounts",mod_overdrawn_accounts_ui("overdrawn_accounts_ui_1"))
    # tabItem(tabName = "loanLimits"),
  )
}