
### Loading the required packages 

library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(DT)
library(dplyr)
library(readxl)
library(plotly)
library(ggtext)
library(ggcorrplot)
library(shinycssloaders)
library(forecast)
library(fmsb)



 
# data sets used

labour_data<- read.csv("labour_data.csv")
Empoly_rate_by_economicsector<- read_excel("Empoly_rate_by_economicsector.xlsx")
laborforce_participation_by_sex<- read_excel("laborforce_participation_by_sex.xlsx")
unemployment_rate_by_education_level<- read_excel("unemployment_rate_by_education_level.xlsx")
unempolyment_rate_by_province<- read_excel("unempolyment_rate_by_province.xlsx")
Summary_labour_force_indicators_by_District<- read_excel("Summary_labour_force_indicators _by_District.xlsx")
unemployment_rate_by_martialstatus<- read_excel("unemployment_rate_by_martialstatus.xlsx")
unemployment_rate_trends<- read_excel("unemploymentratetrends.xlsx")

# RLFS 2022 summury
workingpopulation<-"7,963,586"
outsidelaborforce<- "3,500,290"
insidelabourforce<-"4,463,296"
employed<-"3,546,352"
unemployed<- "916,944"
peopleinsubusistencefoodproduction<- "1,310,734"
annualunemploymentrate<-"20.5 %"
unemploymentratefemales<- "23.7 %"
unemploymentratemales<- "17.9 %"
unemploymentraterural<- "20.6 %"
unemploymentrateurban<- "20.4 %"
unemploymentrateyouth<- "25.6 %"
unemploymentrateadult<- "17.1 %"
informalemployment<- "3,239,356"
timerelatedunderemployed<- "1,125,425"
potentiallaborforce<- "1,246,103"
laborunderutlization<- "57.5 %"
laborswithdisability<- "17.7 %"
laborwithoutdisability<- "57 %"




# Few  New updates on RLFS 2023 Q3

working_population <- "8,100,000"
employed <- "3,972,193"
unemployed <- "874,876"
outside_labor_force <- "3,253,361"
Employment_to_population_Ratio<- "49.0"
unemploment_rate<- "18.0"
unemployment_rate_female<- "21.9"
unemployment_rate_male<- "14.8"



## Shiny UI component for the Dashboard

dashboardPage( title = "demo app", skin = "purple",
               
##Dashboard Header
               
  dashboardHeader( title = "Rwanda Labor Force Survey 2022" , titleWidth =350,
                   tags$li(
                     class = "dropdown",
                     tags$a(
                       href = "#",
                       class = "dropdown-toggle",
                       "data-toggle" = "dropdown",
                       tags$a(
                         href = "https://www.statistics.gov.rw/home",  
                         tags$img(
                           src = "nisrlogo.jpg",
                           height = "30px"
                         )
                       )
                     )
                   ),
                   tags$li(
                     class = "dropdown",
                     tags$a(
                       href = "#",
                       class = "dropdown-toggle",
                       "data-toggle" = "dropdown",
                       tags$a(
                         href = "https://www.gov.rw",  
                         tags$img(
                           src = "rw.jpg",
                           height = "30px"
                         )
                       )
                     )
                   ),
                   
                   
                   
                   
                   
                   dropdownMenu(type = "notification", icon = icon("bell"),
                                messageItem(from  = " Global Unemployment Rate", message = " GUR Is expected to reach 5.8 by 2023", icon = icon("line-chart")),
                                messageItem(from  = "RLFS Q4 2023", message = "The RLFS 2023 Q4 is out soon stay tuned ", icon = icon("users"))
                                
                     
                   ),
                  
                   tags$li(class="dropdown", tags$a(href="https://github.com/ngamijex/RLFS-2022-R-dashboard", icon("github"), "Github", target="_blank")),
                   tags$li(class="dropdown", tags$a(href="https://microdata.statistics.gov.rw/index.php/catalog/104", icon("info-circle"), "Source of data", target="_blank"))
                   
),

# Dashboard Sidebar: Menu Items   
  
  dashboardSidebar( 
    sidebarMenu( id= "sidebar",
                 menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                 menuItem("Home", tabName = "data", icon = icon("home")),
                 menuItem(text = " RLFS 2022 Overview", tabName = "view", icon = icon("book")),
                 menuItem(text = "Data Tables", tabName = "tab", icon = icon("table")),
                 menuItem(text = "visualization",tabName = "viz",icon = icon("line-chart")),
                 conditionalPanel("input.sidebar == 'viz' && input.t3 == 'distro'", selectInput(inputId="var1", label = "select the Labor force Indicator",choices = c1)),
                 conditionalPanel("input.sidebar == 'viz' && input.t3  == 'trends' ", selectInput(inputId = "var2" , label ="Select the labour force indicator" , choices = c1, selected = "Employed" )),
                 conditionalPanel("input.sidebar == 'viz' && input.t3 == 'relation'", selectInput(inputId="var3", label = "select the Dependent variable",choices = c1, selected = "Outside labour force ")),
                 conditionalPanel("input.sidebar == 'viz' && input.t3 == 'relation'", selectInput(inputId= "var4", label = "select the Independent variable", choices = c1, selected = "Employment_to_population Ratio")),
                 menuItem("Time Series Trends and forecasts", tabName = "time_series_trends", icon = icon("bar-chart")),
                 menuItem("Predictive Model", tabName = "predictive_model", icon = icon("area-chart")),
                 menuItem(text = "RLFS 2023 Q3 New updates ", tabName = "new",  icon = icon("star"), badgeLabel = "New", badgeColor = "green"),
                 menuItem("User Guide", tabName = "user_guide", icon = icon("question"))
                
                 
                 
                 
                 )
    
                 
                

                
    
    
  ),
# Dashboard Body:Tab items 


  dashboardBody( 
    
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                # Welcoming fluid row
                tags$div(
                  style = "text-align: center; padding-top: 20px;",
                  tags$img(src = "welcome.jpg", width = 800, height = 400),
                  h2("Welcome to the Rwanda Labor Force Survey Dashboard", style = "color: #5E5E5E;"),
                  p("Explore and analyze labor force indicators across different dimensions."),
                  p("Select various tabs to access different datasets and visualizations.")
                )
              )),
                     
      tabItem(tabName = "data",
             tabBox(id="t1", width = 12,
                    tabPanel("About", icon = icon("address-card"), fluidRow(
                      column(width = 8, tags$img(src="Labor.png", width =900, height = 400),
                              tags$br()) ,
                      
                      column(width=10, tags$br(),
                             tags$p("Rwanda Labor Force survey 2022 .   
                                    The Labor Force Survey (LFS) programme collect data on employment 
                                    and labor underutilization characteristics of the population on a continuous basis, providing quarterly 
                                    estimates of the main labour force aggregates with sufficient precision at the National level since 2016. 
                                    This dashboard provides a clear picture of Summary labor force indicators such as employment and unemployment rates,
                                    labor underutilization, participation rate and so on , all in different aspects such as sex, 
                                    region and education level. ")))),
                    tabPanel(title="DataSet", icon =icon("database"),dataTableOutput("dataT")),
                    tabPanel(title="Data structure", icon = icon("address-card"),verbatimTextOutput("structure")),
                    tabPanel(title="summary statistics", icon = icon("address-card"),verbatimTextOutput("summary"))
                    )),
             
      tabItem(tabName = "tab",
              tabBox(id="t2",width = 12,
                     tabPanel(title="	Unemployment rate by education level", icon = icon("graduation-cap"),dataTableOutput("dataL")),
                     tabPanel(title="Employed population by sector ", icon = icon("industry"),dataTableOutput("dataF")),
                     tabPanel(title="unemployment rate by Province", icon = icon("city"),dataTableOutput("dataG")),
                     tabPanel(title="Labor Force Particpation By sex ", icon = icon("venus-mars"),dataTableOutput("dataH"))
                     
                     )),
      tabItem(tabName = "viz",
              tabBox(id="t3",width = 12,
                     tabPanel(title="Labor force Indicators Trends By District", icon = icon("users"), value ="trends",
                              fluidRow(tags$div(align="center", box(tableOutput("top5"), title = textOutput("head1") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE)),
                                       tags$div(align="center", box(tableOutput("low5"), title = textOutput("head2") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE))
                                       
                              ),
                              plotlyOutput("bar") ),
                     tabPanel(title="Distribution ", icon = icon("sliders"), value = "distro", plotlyOutput("histplot") ),
                     tabPanel(title= " Correlation Matrix",  icon =icon("link"),plotlyOutput("cor")),
                     tabPanel(title="Relationships between labour force indicators ",  icon = icon("line-chart"), value = "relation", 
                              radioButtons(inputId="fit", label = "select smooth method", choices =c("loess", "lm"), selected = "lm", inline = TRUE ),
                              plotlyOutput("scatter")),
                     tabPanel(title= "unemployment rate by province", icon = icon("city"), plotlyOutput("bar1")),
                     tabPanel(title= "unemployment rate by educational level", icon = icon("graduation-cap"), plotlyOutput("pie")),
                     tabPanel(title="Employment rate by economic sectors ", icon = icon("industry"),plotlyOutput("bar2")),
                     tabPanel(title= "Unemployment rate by martial status", icon = icon("ring"), plotlyOutput("pie2")))
                     ),
      
      
      
      
      tabItem(tabName = "new",
              fluidRow(
                infoBox("Working Population", working_population, icon = icon("users"), color = "red"),
                infoBox("Employed", employed, icon = icon("check"), color = "yellow"),
                infoBox("Unemployed", unemployed, icon = icon("times"), color = "green"),
                infoBox("Outside Labor Force", outside_labor_force, icon = icon("briefcase"), color = "blue"),
                infoBox("Employment/population Ratio",Employment_to_population_Ratio, icon = icon("users"), color = "maroon"),
                infoBox("Unemployment Rate",unemploment_rate, icon = icon("star")),
                infoBox("Unemployment Rate Female",unemployment_rate_female, icon = icon("female"), color = "purple"),
                infoBox("Unemployment Rate Male",unemployment_rate_male, icon = icon("male"), color = "black")
              )
              
              
              
    
                
              
      
      
      
              
                
              
              
              ),
      tabItem(tabName = "view",
              fluidRow(
                infoBox("Working Population", workingpopulation,icon = icon("users"), color = "red"),
                infoBox("Outside Labor Force", outsidelaborforce,icon = icon("paper-plane"),color = "yellow"),
                infoBox("Inside Labor Force ",insidelabourforce, icon = icon("inbox"), color = "green"),
                infoBox("Employed", employed, icon = icon("check"), color = "blue"),
                infoBox("Unemplpyed",unemployed, icon = icon("times"), color = "maroon"),
                infoBox("Unemployment Rate",annualunemploymentrate, icon = icon("star"), color = "black"),
                infoBox("unemp.. rate female",unemploymentratefemales, icon = icon("female"), color = "lime"),
                infoBox("Unemp.. rate male",unemploymentratemales, icon = icon("male"), color = "blue"),
                infoBox("unemp.. rate youth",unemploymentrateyouth, icon = icon("child"), color = "navy"),
                infoBox("unemp.. rate adult", unemploymentrateadult, icon = icon("user-friends"), color = "aqua"),
                infoBox("unemp.. rate rural",unemploymentraterural, icon = icon("tree"), color = "yellow"),
                infoBox("unemp.. rate urban", unemploymentrateurban, icon = icon("city"),color = "fuchsia"),
                infoBox(" Informal Employment",informalemployment, icon = icon("times"),color = "lime"),
                infoBox("Time related underemployed",timerelatedunderemployed,icon = icon("clock"), color = "light-blue"),
                infoBox("Potential Labor Force", potentiallaborforce, icon = icon("briefcase"),color = "maroon"),
                infoBox("Labor UnderUtilisation",laborunderutlization, icon = icon("hard-hat"), color = "black"),
                infoBox("Labor With Disability", laborswithdisability, icon = icon("wheelchair"), color = "olive"),
                infoBox("Labor Without Disability", laborwithoutdisability, icon = icon("user"), color = "teal")
                
                
                
                
                
                
                
                
                
                
                
                
              )
              
              
              
              
              
              
              
              
              
              ),
      
      tabItem(tabName = "time_series_trends",
              fluidRow(
                box(
                  title = "Unemployment Rate Trends",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  selectInput("indicator_select", "Select Indicator", choices = colnames(unemployment_rate_trends)[-1]),
                  plotlyOutput("time_series_plot")
                ),
                box(
                  title = "Unemployment Rate Forecast",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("forecast_plot")
                )
              )
      ),
      
            
      tabItem(
        tabName = "user_guide",
        fluidRow(
          column(
            width = 12,
            tags$h2("Welcome to the User Guide"),
            tags$p(
              "This section provides instructions and guidance on how to use different features of the dashboard."
            ),
            tags$h3("How to Use the Dashboard:"),
            tags$ol(
              tags$li("Explore the 'Dashboard' tab for an overview."),
              tags$li("Navigate to 'Data' and 'Data Tables' for detailed information."),
              tags$li("Select 'Visualization' for interactive charts."),
              tags$li("Visit 'Time Series Trends' for historical trends and forcasting."),
              tags$li("Vist prediction model for making models by prediction "),
              tags$li("Check out 'New Updates' for the latest information.")
            ),
            tags$h3("Tooltips:"),
            tags$p(
              "Hover over data points or elements to view tooltips with additional information."
            ),
            tags$h3("Feedback:"),
            tags$p(
              "Your feedback is important. Feel free to provide comments or suggestions via the provided GitHub link."
            )
            
          )
        )),
      tabItem(tabName = "predictive_model",
              fluidRow(
                box(
                  title = "Predictive Model",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  selectInput("predictor_var", "Select Predictor Variable", choices = colnames(Summary_labour_force_indicators_by_District)),
                  actionButton("run_model", "Run Model"),
                  plotlyOutput("model_plot")
                )
              )
      )    
              
      
      
      
      
    
      
    )))
    

  
  
  
  
  
  

