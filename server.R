

## Shiny Server component for dashboard

function(input, output, session){
 
  output$structure<- renderPrint(
    labour_data %>%
      str() 
    
  )
  output$summary<-renderPrint(
    labour_data %>%
      summary()
  )
  
  output$dataT<- renderDataTable(labour_data)
   
   output$dataL<- renderDataTable(unemployment_rate_by_education_level)
    
   output$dataF<- renderDataTable(Empoly_rate_by_economicsector)

  output$dataG<- renderDataTable(unempolyment_rate_by_province)
  output$dataH<- renderDataTable(laborforce_participation_by_sex)
  
  # stacked histogram and box plot  
   
  output$histplot <- renderPlotly({
    p1 <- Summary_labour_force_indicators_by_District %>%
      plot_ly() %>%
      add_histogram(~ get(input$var1), text = ~paste("District: ", District, "<br>Unemployment Rate: ", get(input$var1))) %>%
      layout(xaxis = list(title = "Unemployment Rate"))
    
    p2 <- Summary_labour_force_indicators_by_District %>%
      plot_ly() %>%
      add_boxplot(~get(input$var1), text = ~paste("District: ", District, "<br>Unemployment Rate: ", get(input$var1))) %>%
      layout(yaxis = list(showticklabels = F))
    
    subplot(p2, p1, nrows = 2) %>% 
      layout(title = "Distribution chart - Histogram and Box plot",
             yaxis = list(title = "Frequency"))
  })
  #scatter 
  #scatter
  
  output$scatter<- renderPlotly({
    
    # creating scatter plot for relationship using ggplot
    p = Summary_labour_force_indicators_by_District %>%
      ggplot(aes(x= get(input$var3), y= get(input$var4)))+
      geom_point()+
      geom_smooth(method = get(input$fit))+
      labs(title = paste("Relationship between", input$var3 ,"and" , input$var4),
           x = input$var3,
           y=  input$var4)+
      theme(plot.title = element_textbox_simple(size=10,
                                                halign=0.5))
    ggplotly(p)
    
    
  })
  ## Correlation plot
  
  output$cor<- renderPlotly({
    my_df <- Summary_labour_force_indicators_by_District%>%
      select(-"District",-"Combined_rate_of_unemployment _and_time_related_under_employment",
             -"Combined_rate_of_unemployment_and_potential_labor_force",
             - "Composite_rate_of_unemployment_and_potential_labour_force",
             -"...1")
    # Compute a correlation matrix
    corr <- round(cor(my_df), 1)
    
    # Compute a matrix of correlation p-values
    p.mat <- cor_pmat(my_df)
    
    corr.plot <- ggcorrplot(
      corr,
      hc.order = TRUE,
      lab= TRUE,
      outline.col = "white",
      p.mat = p.mat
    )
    
    ggplotly(corr.plot)
    
    
    
    
    
  })
  
  ## bar chart District  wise trend 
  output$bar<- renderPlotly({
    Summary_labour_force_indicators_by_District %>%
      plot_ly()%>%
      add_bars(x=~District, y=~get(input$var2) ) %>%
      layout(title=paste("labour force indicator :", input$var2 ),
             xaxis = list(title= "District "),
             yaxis = list(title= paste(input$var2)))
    
    
  
})
  
  # rendering the box header 
  output$head1<- renderText(paste("5  District  with high number of ", input$var2, "in population"))
  
  output$head2<- renderText(paste("5  District  with low number of ", input$var2, " in population"))
  
  # Rendering table with 5 Province with high labor force indicator rates 
  
  output$top5<- renderTable({
    Summary_labour_force_indicators_by_District %>%
      select(District,input$var2) %>%
      arrange(desc(get(input$var2))) %>%
      head(5)
    
    
  })
  # rendering table with low labor force indicators rates  
  
  output$low5<- renderTable({
    Summary_labour_force_indicators_by_District %>%
      select(District,input$var2) %>%
      arrange(get(input$var2)) %>%
      head(5)
    
  })
 # rendering bar for unemployment rate by province 
 Color<- c("blue","yellow","green","red","navy")
  
  output$bar1<- renderPlotly({
  
    plot_ly( unempolyment_rate_by_province, x= ~Province, y = ~unemploymentrate,
             type = "bar", name = "unemploment rate", marker= list(color= ~Color)) %>%
      layout(title= " Unemployment rate by province" )
    
    
  })
  
  
      
    
 output$pie<- renderPlotly({
   plot_ly(unemployment_rate_by_education_level, labels = ~Educationlevel, values = ~Unemploymentrate, type = "pie", pull= c(0.1,0,0,0,0)) %>%
     layout(title = "Unemployment Rate by Education Level",
            titlefont= list(size= 18, color= "darkblue"),
            font= list(family= "Arial", size= 14, color= "black"))
   
 })
 
 
 
 
 
 output$bar2<- renderPlotly({
   
   plot_ly(
     Empoly_rate_by_economicsector,
     x = ~Economicsector,
     y = ~Employmentrate,
     size = ~Employedpopulation,
     type = "scatter",
     mode = "markers",
     
     marker = list(sizemode = "diameter")
   ) %>%
     layout(
       title = "Employment Rate By economic sectors",
       xaxis = list(title = "Economic sectors"),
       yaxis = list(title = "Employment rate "),
       margin= list(l= 50, r = 50, b = 50, t=50),
       height= 700, width=800
     )
   
   
   
   
 })
 
 output$pie2<- renderPlotly({
   
   plot_ly(unemployment_rate_by_martialstatus, labels=~MartialStatus, values=~Unemploymentrate, type = "pie", pull=c(0,0,0,0.1,0))%>%
     layout(title="Unemployment Rate By Martial Status")
   
   
 })
 
 # Define the server logic to render the interactive trend chart and forecast
 output$time_series_plot <- renderPlotly({
   selected_indicator <- input$indicator_select
   
   # Create an interactive trend chart using plotly
   trend_chart <- plot_ly(
     data = unemployment_rate_trends,
     x = ~year,
     y = ~get(selected_indicator),
     type = "scatter",
     mode = "lines+markers",
     line = list(color = "blue"),
     marker = list(color = "red")
   ) %>%
     layout(
       title = paste("Trends Over Time -", selected_indicator),
       xaxis = list(title = "Year"),
       yaxis = list(title = selected_indicator)
     )
   
   trend_chart
 })
 
 output$forecast_plot <- renderPlotly({
   selected_indicator <- input$indicator_select
   
   # Fit a time series model
   unemployment_model <- auto.arima(ts(unemployment_rate_trends[[selected_indicator]], start = c(2016), frequency = 1))
   
   # Forecast the next five years
   unemployment_forecast <- forecast(unemployment_model, h = 5)
   
   # Plot the forecast using plotly
   forecast_plot <- plot_ly() %>%
     add_trace(
       x = ~c(2024:2028),
       y = ~unemployment_forecast$mean,
       type = "scatter",
       mode = "lines+markers",
       line = list(color = "green"),
       marker = list(color = "black")
     ) %>%
     add_trace(
       x = ~c(2024:2028),
       y = ~unemployment_forecast$lower,
       type = "scatter",
       mode = "lines",
       fill = "tonexty",
       line = list(color = "transparent"),
       showlegend = FALSE
     ) %>%
     add_trace(
       x = ~c(2024:2028),
       y = ~unemployment_forecast$upper,
       type = "scatter",
       mode = "lines",
       fill = "tonexty",
       line = list(color = "transparent"),
       showlegend = FALSE
     ) %>%
     layout(
       title = paste("Forecast for the Next Five Years -", selected_indicator),
       xaxis = list(title = "Year"),
       yaxis = list(title = selected_indicator)
     )
   
   forecast_plot
 })
 
 
 model_data <- reactive({
   selected_predictor <- input$predictor_var
   selected_response <- "Employed"  # Change this to your response variable
   
   data <- Summary_labour_force_indicators_by_District %>%
     select(selected_predictor, selected_response) %>%
     na.omit()
   
   return(data)
 })
 
 observeEvent(input$run_model, {
   selected_predictor <- input$predictor_var
   selected_response <- "Employed"  # Change this to your response variable
   
   # Train a simple linear regression model
   lm_model <- lm(paste(selected_response, "~", selected_predictor), data = model_data())
   
   # Plot the model
   model_plot <- plot_ly(model_data(), x = as.formula(paste0("~", selected_predictor)), y = as.formula(paste0("~", selected_response)), mode = "markers", type = "scatter") %>%
     add_lines(x = as.formula(paste0("~", selected_predictor)), y = predict(lm_model), line = list(color = 'red'))
   
   output$model_plot <- renderPlotly(model_plot)
 })
 

 
 
} 







