#Current and Past Monitored Sites
#Show all monitored CWL sites, split into current and past

#shiny
library(shiny)
#shiny themes for color 
library(shinythemes)
#tidyverse for data manipulation 
library(tidyverse)
#pool for database connections
library(pool)
#odbc for database connection
library(odbc)
#easy javascript commands
library(shinyjs)
#datatables
library(DT)

options(stringsAsFactors=FALSE)

#set up

#set default page length for datatables
options(DT.options = list(pageLength = 15))

#set db connection
#using a pool connection so separate connections are unified
#gets environmental variables saved in local or pwdrstudio environment

#poolConn <- dbPool(odbc(), dsn = "mars14_data", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))

poolConn <- dbPool(RPostgres::Postgres(),
                   host = "PWDMARSDBS1.pwd.phila.local",
                   port = 5434,
                   dbname = "mars_prod",
                   user = Sys.getenv("shiny_uid"),
                   password = Sys.getenv("shiny_pwd")
)

#disconnect from db on stop 
onStop(function(){
    poolClose(poolConn)
})


#js warning about leaving page
jscode <- 'window.onbeforeunload = function() { return "Please use the button on the webpage"; };'

# Define UI for application that draws a histogram
ui <-  navbarPage("MARS CWL Monitoring History", theme = shinytheme("cerulean"),
               tabPanel("Current Monitoring Sites", value = "current_tab", 
                        titlePanel("Current Continuous Water Level Monitoring Sites"), 
                        DTOutput("active_table"), 
               ), 
               tabPanel("Past Monitoring Sites", value = "past_tab", 
                        titlePanel("Past Continuous Water Level Monitoring Sites"), 
                        DTOutput("past_table")
               )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    #initialize reactiveValues
    rv <- reactiveValues()
    
    #query active table
    active_cwl_sites_query <- "select * from fieldwork.viw_active_cwl_sites order by smp_id, site_name"
    
    rv$active_cwl_sites_db <- reactive(dbGetQuery(poolConn, active_cwl_sites_query))
    
    #adjust active table for user experience
    rv$active_cwl_sites <- reactive(rv$active_cwl_sites_db() %>% 
                                        mutate_at("first_deployment_date", as.character) %>% 
                                        mutate_at(vars(one_of("public")),
                                                  funs(case_when(. == 1 ~ "Yes", 
                                                                 . == 0 ~ "No"))) %>% 
                                        dplyr::select("smp_id", "project_name", "component_id", "location_type", 
                                                      "type", "public", "first_deployment_date") %>% 
                                        dplyr::rename("SMP ID" = "smp_id", "Project Name" = "project_name", 
                                                      "Component ID" = "component_id", "Location Type" = "location_type", 
                                                      "Sensor Type" = "type",
                                                      "Public" = "public", "First Deployment Date" = "first_deployment_date"))
    
    #render active table
    output$active_table <- renderDT(
        rv$active_cwl_sites(),
        selection = 'single', 
        style = 'bootstrap',
        class = 'table-responsive, table-hover', 
        rownames = FALSE
    )
    
    #query past table
    past_cwl_sites_query <- "select * from fieldwork.viw_previous_cwl_sites order by smp_id, site_name"
    
    rv$past_cwl_sites_db <- reactive(dbGetQuery(poolConn, past_cwl_sites_query))
    
    rv$past_cwl_sites <- reactive(rv$past_cwl_sites_db() %>% 
                                      mutate_at(c("first_deployment_date", "last_collection_date"), as.character) %>% 
                                      mutate_at(vars(one_of("public")),
                                                funs(case_when(. == 1 ~ "Yes", 
                                                               . == 0 ~ "No"))) %>% 
                                      dplyr::select("smp_id", "project_name", "component_id", "location_type", 
                                                    "type", "public", "first_deployment_date", "last_collection_date") %>% 
                                      dplyr::rename("SMP ID" = "smp_id", "Project Name" = "project_name", 
                                                    "Component ID" = "component_id", "Location Type" = "location_type", 
                                                    "Sensor Type" = "type",
                                                    "Public" = "public", "First Deployment Date" = "first_deployment_date",
                                                    "Last Collection Date" = "last_collection_date"))
    
    #render past table
    output$past_table <- renderDT(
        rv$past_cwl_sites(),
        selection = 'single', 
        style = 'bootstrap',
        class = 'table-responsive, table-hover', 
        rownames = FALSE
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
