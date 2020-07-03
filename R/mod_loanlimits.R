#' loanlimits UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_loanlimits_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("loanlims"))
    # uiOutput(ns("disb_ui_report"))
  )
}

#' loanlimits Server Function
#'
#' @noRd
mod_loanlimits_server <- function(input, output, session){
  ns <- session$ns



output$loanlims <- renderUI({
  tagList(
    panel(
      column(3,popify(el = fileInput(ns("newlims"),label = "Upload New Limits",multiple = T,accept = c('.csv','.xlsx'),buttonLabel = "Upload",placeholder = "Upload File"),
                      title = "Limits Upload",content = "Use this button strictly if there is a new Limits file.")),
      column(3,popify(el = fileInput(ns("disbFile"),label = "Upload Disbursement File",accept = c('.csv','.xlsx'),buttonLabel = "Upload",placeholder = "Upload File"),
                      title = "Disbursement File Upload",content = "Use this for manual disbursement Upload if the file doesnot exist in your shared folder.File can be frontend or Backend generated.")),
      column(3,airDatepickerInput(ns("disbdate"),"Select Disbursement Date",Sys.Date()-1,placeholder = "Select Date",maxDate = Sys.Date(),autoClose = T,disabledDates = 6)),
      column(3,br(),actionBttn(ns("submit_disb"),"Submit",icon = icon("send"),style = "gradient",size = "sm",color = "royal"))
    ),
  uiOutput(ns("disb_ui_report"))
  )
})

# show reports

observeEvent(input$submit_disb,{

limits_analysis <- limits_analysis(date = input$disbdate,disbfile = input$disbFile,livelim = input$newlims)

 output$disbursement_listing_day <- renderDataTable({
    datatable(limits_analysis$disbursment %>% filter(as.Date(DisbursedOn) == max(as.Date(DisbursedOn))),
              options = list(scrollX = "1200px",scrollY = '270px'))
  })
 output$disbursement_listing_week <- renderDataTable({
    datatable(limits_analysis$disbursment %>%
                filter(as.Date(DisbursedOn) %in% (max(as.Date(DisbursedOn)) - 0:6) ),options = list(scrollX = T,scrollY = '300px'))
  })

 output$limitsreport <- renderText({
   abovelimits = limits_analysis$limitscheck %>%
     filter(LimitsCheck !=  "Within Limit",as.Date(DisbursedOn) == max(as.Date(DisbursedOn)))

   if (nrow(abovelimits) == 0 ) {
     report = paste0("No Loans Above limit")
   }else{
     report = paste(nrow(abovelimits) ," Loans Above limit as shown below.")
   }

 })

 output$abovelimtable <- function(){
   limits_analysis$limitscheck %>% filter(LimitsCheck == "Above Limit",as.Date(DisbursedOn) == max(as.Date(DisbursedOn))) %>%
     select(-LimitsCheck) %>% kableExtra::kable() %>% kableExtra::kable_styling()%>%
     scroll_box(height = "290px",width="100%")
 }

 output$daydisb <- function(){

     # table_type <- ifelse(input$mode=='Previous Day',"day","cummulative")
     # table_type <- switch (input$mode,'Previous Day' = "day",'Cummulative' = "cummulative")

     markers = grep("Sub-Total|Total",pluck(limits_analysis$disbreports,"day")$Product)

     disb_particulars <-pluck(limits_analysis$disbreports,"day") %>% kable() %>% kable_styling(font_size = 11) %>% collapse_rows(1) %>%
       column_spec(column = 1:3,border_right = T,color = "black") %>%
       row_spec(c(0,markers),bold = T,underline = T,background = "lightgray",italic = T) %>%
       scroll_box(height = "100%",width="100%")

 }

 output$dailytrend <- renderHighchart({

   limits_analysis$alldisb %>%
     hchart('spline',hcaes(x = 'variable', y = "value", group = "Product")) %>%
     hc_add_theme(hc_theme_google()) %>%  hc_tooltip(crosshairs = TRUE) %>%
     hc_scrollbar()

 })
  output$cummulative <- renderHighchart({

   limits_analysis$alldisb %>%
     hchart('spline',hcaes(x = 'variable', y = "Cumulative", group = "Product")) %>%
     hc_add_theme(hc_theme_google()) %>%  hc_tooltip(crosshairs = TRUE)

 })

 output$cummulativedisb <- function(){

   # table_type <- ifelse(input$mode=='Previous Day',"day","cummulative")
   # table_type <- switch (input$mode,'Previous Day' = "day",'Cummulative' = "cummulative")

   markers = grep("Sub-Total|Total",pluck(limits_analysis$disbreports,"cummulative")$Product)

   disb_particulars <-pluck(limits_analysis$disbreports,"cummulative") %>% kable() %>% kable_styling(font_size = 11) %>% collapse_rows(1) %>%
     column_spec(column = 1:3,border_right = T,color = "black") %>%
     row_spec(c(0,markers),bold = T,underline = T,background = "lightgray",italic = T) %>%
     scroll_box(height = "100%",width="100%")

 }

 # show loan duplicates here



 output$phone_duplicates <- function(){
   phonedups = pluck(limits_analysis$dups,"duplicatesbyphone")

   if (nrow(phonedups) > 0) {
   phonedups %>% kable() %>% kable_styling() %>%
    row_spec(1:nrow(phonedups),color = "black") %>%
    column_spec(0,background = "gray") %>%
    scroll_box(width = "100%",height = "100%")
}else{data_frame('Duplicates_Report' = "No Duplicates by Phone")%>% kable() %>% kable_styling()}


 }
 output$dupsbyID <- function(){
  dupsbyID = pluck(limits_analysis$dups,"duplicatesbyID")

   if (nrow(dupsbyID) > 0) {
     dupsbyID %>% kable() %>% kable_styling() %>%
       row_spec(1:nrow(dupsbyID),color = "black") %>%
       column_spec(0,background = "gray")%>%
       scroll_box(width = "100%",height = "100%")
   }else{data_frame('Duplicates_Report' = "No Duplicates by National ID")%>% kable() %>% kable_styling()}

 }
 output$dupsbyAcc <- function(){
  dupsbyAcc = pluck(limits_analysis$dups,"duplicaesbyacc")

   if (nrow(dupsbyAcc) > 0) {
     dupsbyAcc %>% kable() %>% kable_styling() %>%
       row_spec(1:nrow(dupsbyID),color = "black") %>%
       column_spec(0,background = "gray")%>%
       scroll_box(width = "100%",height = "100%")
   }else{data_frame('Duplicates_Report' = "No Duplicates by Account No")%>% kable() %>% kable_styling()}

 }

 output$disb_ui_report <- renderUI({
   tabBox(id = "disb_view_report",title = "Disbursements Full Report",width = 12,
          tabPanel(title = "Disbursement Listing",
                   dataTableOutput(ns("disbursement_listing_day"))),
          tabPanel(title = "Limits & Multile Loans",
                   tagList(
                     column(6,"Loan Limits Check",
                                    textOutput(ns("limitsreport")),
                                    tableOutput(ns("abovelimtable"))),
                     column(6,tabBox(title = "Duplicates by :",side = "right",width = 12,
                       tabPanel(title = "IDs" ,tableOutput(ns("dupsbyID"))),
                       tabPanel(title = "Phone Number" ,tableOutput(ns("phone_duplicates"))),
                       tabPanel(title = "Account Number" ,tableOutput(ns("dupsbyAcc")))
                     ))

                     # column(4,panel(heading = "Loan Limits Check",collapsed = F))
                   ) ),
          tabPanel(title = "Day Disbursements",
                   column(6,tableOutput(ns("daydisb"))),
                   column(6,highchartOutput(ns("dailytrend")))),
          tabPanel(title = "Cumulative(Month) Disbursements",
                   column(6,tableOutput(ns("cummulativedisb"))),
                   column(6,highchartOutput(ns("cummulative"))))
          # tabPanel(title = "Dashboard"),
   )})
 })




 # download above lims
# output$abovelimtable <-


}


## To be copied in the UI
# mod_loanlimits_ui("loanlimits_ui_1")

## To be copied in the server
# callModule(mod_loanlimits_server, "loanlimits_ui_1")

