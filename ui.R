ui <- dashboardPagePlus(title = "AML TOOLS",enable_preloader = T,loading_duration = 1,md = F,
    header = dashboardHeaderPlus(),
    sidebar = dashboardSidebar(mod_sideBar_ui("sideBar_ui_1")),
    body = dashboardBody(shinyDashboardThemes(theme = "onenote"),mod_body_ui("body")),
    footer = dashboardFooter(left_text = HTML(paste0("&copy;"," Copyright Maishabank 2020")))
)