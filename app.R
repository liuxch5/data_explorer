# load packages
library(maps)
library(mapdata)
library(tidyverse)
library(eurostat)
library(plotly)
library(lubridate)
library(shinyjs)
library(shinyflags)
library(countrycode)
library(DT)
library(shiny)
library(shinyWidgets)
library(shinythemes)

# set locale for correct display of dates in English
Sys.setlocale("LC_ALL", "C")

# get data
my_map <- map_data("world")
my_map$region[which(my_map$region == "UK")] <- "United Kingdom"

get_eurostat("ei_lmhr_m") %>%
    filter(geo %in% eu_countries$code,
           indic == "LM-UN-T-TOT",
           s_adj == "SA") %>%
    select(time, geo, values) %>%
    label_eurostat(countrycode = "country.name",
                   custom_dic = c(CZ = "Czech Republic")) -> unemp

get_eurostat("gov_10dd_edpt1") %>%
    filter(geo %in% eu_countries$code) -> gov_data

gov_data %>% filter(na_item == "B1GQ", sector == "S1", unit == "MIO_EUR") %>%
    select(time, geo, values) %>%
    label_eurostat(countrycode = "country.name",
                   custom_dic = c(CZ = "Czech Republic")) -> tot_gdp

gov_data %>% filter(na_item == "GD", sector == "S13", unit == "MIO_EUR") %>%
    select(time, geo, values) %>%
    label_eurostat(countrycode = "country.name",
                   custom_dic = c(CZ = "Czech Republic")) -> tot_debt

gov_data %>% filter(na_item == "GD", sector == "S13", unit == "PC_GDP") %>%
    select(time, geo, values) %>%
    label_eurostat(countrycode = "country.name",
                   custom_dic = c(CZ = "Czech Republic")) -> rel_debt

# inputs
datasets <- list(rel_debt, tot_debt, tot_gdp, unemp)
plottitle <-
    c(
        "Government Debt in Percent of GDP",
        "Government Debt in EUR Millions",
        "GDP in EUR Millions",
        "Total Unemployment Rate"
    )
unit_label <-
    c("% of GDP", "EUR Mio", "EUR Mio", "% of Labour Force")

# Define UI------------------------------------------------------------------------------------------
ui <- navbarPage(
    theme = shinytheme("flatly"),
    "Explore EU Data",
    tabPanel("Documentation",
             useShinyjs(),
             mainPanel(
                 h1("Data"),
                 p(
                     "The datasets are taken from the ", 
                     tags$a(href="https://ec.europa.eu/eurostat/data/database", "Eurostat Database"),
                     " using the ", strong("eurostat package.")
                 ),
                 p(
                     "Data on the following indicators is currently explorable with this app:",
                     br(),
                     p(
                         "- Government debt in % of GDP (annual frequency)",
                         br(),
                         "- Government debt in EUR millions (annual frequency)",
                         br(),
                         "- GDP in EUR millions (annual frequency)",
                         br(),
                         "- Unemployment rate in % (monthly frequency)"
                     )
                 ),
                 
                 h1("Functionality"),
                 p(
                     "The data can be explored by clicking on the ",
                     strong("Charts and Data"),
                     " tab in the navigation bar above. ",
                     "There you can select a dataset from the dropdown list on the left and the way in
                     which you want the data to be plotted, i.e., as line chart or as a map. ",
                     "You can use the slider below to select the reference period. This input is dependent on the selected
                     chart type: For maps you can select a single month/year, whereas in a line chart you can select a time 
                     range and look at developments over time. The last input widget lists all the countries for which there 
                     is data available, based on the other selected inputs. You can exclude one or multiple countries from 
                     the analysis by clicking on them. When done with the selection, click on the ",
                     strong("Apply"), " button to produce the output.",
                     br(),br(),
                     strong("Note:"), "It can take several seconds to render the plots, depending on the size of the dataset 
                     and the chart type."
                 ),
                 
                 h1("Output"),
                 p(
                     "Output is produced on three separate tabs: ", strong("Plot"), ",", strong("Summary"), " and ", 
                     strong("Input Data"),
                     ". The first tab contains an interactive chart and a brief description of the output.",
                     "The ", strong("Summary"), " tab contains a table with summary statistics for each point in time in the 
                     selected time period. A tidy datatable of the input data is provided in the ", strong("Input Data"), "tab."
                 )
                 
                 )),
    
    tabPanel("Charts and Data",
             sidebarLayout(
                 sidebarPanel(
                     # Input: Selector for choosing dataset
                     selectInput(
                         inputId = "dataset",
                         label = "Select a dataset:",
                         choices = c(
                             "Relative Government Debt" = 1,
                             "Absolute Government Debt" = 2,
                             "GDP" = 3,
                             "Unemployment" = 4
                         ),
                         selected = 1
                     ),
                     
                     # Input: Selector for choosing chart type
                     radioGroupButtons(
                         inputId = "charttype",
                         label = "Select a chart type:",
                         choices = c("Line" = 1,
                                     "Map" = 2),
                         selected = 1,
                         justified = TRUE,
                         status = "primary",
                         checkIcon = list(
                             yes = icon("ok",
                                        lib = "glyphicon"),
                             no = icon("remove",
                                       lib = "glyphicon")
                         )
                     ),
                     
                     # Input: Slider for time period, range slider for line chart
                     chooseSliderSkin("Shiny", color = "#18BC9C"),
                     uiOutput("selectTime"),
                     
                     # Input: Filter countries
                     uiOutput("chooseCountries"),
                     
                     # Action button
                     actionBttn(
                         inputId = "runPlot",
                         label = "Apply",
                         style = "simple",
                         color = "success",
                         block = TRUE
                     )
                     
                 ),
                 mainPanel(
                     tabsetPanel(
                         id = "mainPanelTab",
                         tabPanel(
                             "Plot",
                             h3(textOutput("tabHeader1")),
                             plotlyOutput("plot", width = "100%", height = "100%"),
                             br(),
                             p(textOutput("description1")),
                             p(textOutput("description2"))
                         ),
                         tabPanel("Summary",
                                  h3(textOutput("tabHeader2")),
                                  br(),
                                  dataTableOutput("summary")),
                         
                         tabPanel("Input Data",
                                  h3(textOutput("tabHeader3")),
                                  br(),
                                  dataTableOutput("data"))
                     )
                 )
             ))
    )





# Server function ------------------------------------------------------------------------------
server <- function(input, output) {
    # Assign dataset to inputData()
    inputData <- reactive({
        datasets[[as.numeric(input$dataset)]]
    })
    
    # Create slider based on chart type selection-----------------------------------------------
    # Store time format in variable, based on selected dataset
    inputTimeFormat <- reactive({
        ifelse(as.numeric(input$dataset) == 4, "%b %Y", "%Y")
    })
    
    # Store strings based on selected dataset to convert dates back later
    inputTimePaste <- reactive({
        ifelse(as.numeric(input$dataset) == 4, " 01", "-01-01")
    })
    inputTimePasteF <- reactive({
        ifelse(as.numeric(input$dataset) == 4, "%b %Y %d", "%Y-%m-%d")
    })
    
    # Store time unit in variable, based on selected dataset
    inputTimeUnit <- reactive({
        ifelse(as.numeric(input$dataset) == 4, "month", "year")
    })
    
    # Store possible choices in variable, based on selected dataset
    inputChoices <- reactive({
        inputData()$time %>% sort() %>% format(format = inputTimeFormat()) %>% unique() %>% as.character()
    })
    
    # Store possible country choices in variable, based on selected data
    inputCountries <- reactive({
        #inputData()$geo %>% unique() %>% as.character() %>% sort()
        switch(
            input$charttype,
            # Line chart
            "1" = inputData() %>% filter(time >= filterTime()[1] &
                                             time <= filterTime()[2]) %>%
                drop_na(values) %>%
                mutate(geo = as.character(geo)) %>%
                pull(geo) %>% unique() %>% sort()
            
            ,
            # Map
            "2" = inputData() %>% filter(time == filterTime()) %>%
                drop_na(values) %>%
                mutate(geo = as.character(geo)) %>%
                pull(geo) %>% unique() %>% sort()
        )
    })
    
    # Time period, either as 2x1 vector or scalar, depending on chart type, for filtering
    filterTime <- reactive({
        input$timePeriod %>% paste0(., inputTimePaste()) %>% as_date(format = inputTimePasteF(), tz = "CET")
    })
    
    # Create time period slider
    output$selectTime <- renderUI({
        switch(
            input$charttype,
            # Line chart
            "1" = sliderTextInput(
                inputId = "timePeriod",
                label = "Select time range:",
                choices = inputChoices(),
                selected = c(inputChoices()[1],
                             inputChoices()[length(inputChoices())])
            ),
            # Map
            "2" = sliderTextInput(
                inputId = "timePeriod",
                label = "Select reference period:",
                choices = inputChoices(),
                selected = inputChoices()[length(inputChoices())]
            )
        )
    })
    
    
    # Include/exclude countries
    output$chooseCountries <- renderUI({
        multiInput(
            inputId = "countries",
            label = "Include/exclude countries:",
            choices = inputCountries(),
            options = list(
                enable_search = FALSE,
                non_selected_header = "Included",
                selected_header = "Excluded"
            )
        )
    })
    
    # Create data for plotting and calculations based on inputs------------------------------------
    
    # only run when button is hit!
    activeData <- eventReactive(input$runPlot, {
        switch(
            input$charttype,
            # Line chart
            "1" = inputData() %>% filter(time >= filterTime()[1] &
                                             time <= filterTime()[2]) %>%
                filter(!(geo %in% input$countries))
            ,
            # Map
            "2" = inputData() %>% filter(time == filterTime()) %>%
                filter(!(geo %in% input$countries))
        )
    })
    
    
    # Render DataTable containing underlying data
    output$data <-
        renderDataTable({
            datatable(
                activeData(),
                colnames = c(
                    "Reference period" = "time",
                    "Country" = "geo",
                    "Value" = "values"
                )
                , extensions = "Responsive"
            )
        })
    
    # Create separate dataset with data for the plots, dependent on chart type input
    plot_data <- eventReactive(input$runPlot, {
        switch(
            input$charttype,
            # Line chart
            "1" = activeData() %>%
                group_by(time) %>%
                summarise(values = mean(values)) %>%
                mutate(geo = factor("Mean of selected countries")) %>%
                merge(
                    activeData(),
                    .,
                    by.x = "geo",
                    by.y = "geo",
                    all = TRUE
                ) %>%
                mutate(
                    time = coalesce(time.x, time.y),
                    values = coalesce(values.x, values.y)
                ) %>%
                mutate(values = round(values, digits = 1)) %>%
                rename(
                    Time = time,
                    Country = geo,
                    Value = values
                ) %>%
                select(Time, Country, Value),
            
            # Map
            "2" = merge(
                my_map,
                activeData(),
                by.x = "region",
                by.y = "geo",
                all.x = TRUE
            ) %>%
                rename(
                    Time = time,
                    Country = region,
                    Value = values
                ) %>% arrange(order)
        )
    })
    
    
    # Create ggplot objects dependent on selected chart type to send to ggplotly
    plotObject <- reactive({
        switch(
            isolate(input$charttype),
            # Line chart
            "1" = ggplot(data = plot_data(), aes(
                x = Time,
                y = Value,
                color = Country
            )) +
                theme_bw() +
                geom_line() +
                geom_line(stat = "summary", fun.y = "mean") +
                labs(x = "Time", y = isolate(unit_label[as.numeric(isolate(input$dataset))]))
            ,
            
            # Map
            "2" = ggplot(data = plot_data(), aes(label = Country)) +
                theme_bw() +
                geom_polygon(aes(
                    x = long,
                    y = lat,
                    group = group,
                    fill = Value
                ),
                color = "white") +
                scale_fill_viridis_c(
                    name = isolate(unit_label[as.numeric(isolate(input$dataset))]),
                    option = "magma",
                    direction = -1
                ) +
                coord_equal(
                    ratio = 1.3,
                    ylim = c(30, 70),
                    xlim = c(-20, 40)
                ) +
                labs(x = "Longitude", y = "Latitude")
        )
    })
    
    # Create plotly plot
    output$plot <- renderPlotly({
        ggplotly(plotObject())
    })
    
    # Create summary statistics for second tab
    output$summary <- renderDataTable({
        activeData() %>% group_by(time) %>%
            summarise(
                count = n(),
                min = min(values),
                p25 = quantile(values, .10),
                p50 = quantile(values, .10),
                p75 = quantile(values, .10),
                max = max(values)
            ) %>%
            datatable(
                colnames = c(
                    "Reference period" = "time",
                    "Countries" = "count",
                    "Minimum" = "min",
                    "25th percentile" = "p25",
                    "Median" = "p50",
                    "75th percentile" = "p75",
                    "Maximum" = "max"
                ),
                options = list(searching = FALSE)
                , extensions = "Responsive"
            )
    })
    
    # Headers for the tabs dependent on selections
    tabHeaders <-  eventReactive(input$runPlot, {
        switch(
            isolate(input$charttype),
            # Line chart
            "1" = paste0(
                isolate(plottitle[as.numeric(input$dataset)]),
                " from ",
                format(isolate(filterTime()[1]),
                       format = isolate(inputTimeFormat())),
                " to ",
                format(isolate(filterTime()[2]),
                       format = isolate(inputTimeFormat()))
            )
            ,
            # Map
            "2" = paste0(
                isolate(plottitle[as.numeric(input$dataset)]),
                " in ",
                format(isolate(filterTime()),
                       format = isolate(inputTimeFormat()))
            )
        )
    })
    
    output$tabHeader1 <- renderText({
        tabHeaders()
    })
    output$tabHeader2 <- renderText({
        tabHeaders()
    })
    output$tabHeader3 <- renderText({
        tabHeaders()
    })
    
    # Some text describing the data to display below the chart
    maxValue <- reactive({
        activeData() %>% filter(time == max(time)) %>% filter(values == max(values)) %>% pull(values) %>% as.character()
    })
    maxCountry <- reactive({
        activeData() %>% filter(time == max(time)) %>% filter(values == max(values)) %>% pull(geo) %>% as.character()
    })
    minValue <- reactive({
        activeData() %>% filter(time == max(time)) %>% filter(values == min(values)) %>% pull(values) %>% as.character()
    })
    minCountry <- reactive({
        activeData() %>% filter(time == max(time)) %>% filter(values == min(values)) %>% pull(geo) %>% as.character()
    })
    firstMean <- reactive({
        plot_data() %>% filter(Time == min(Time)) %>%
            filter(Country == "Mean of selected countries") %>%
            pull(Value) %>%
            as.character()
    })
    lastMean <- reactive({
        plot_data() %>% filter(Time == max(Time)) %>%
            filter(Country == "Mean of selected countries") %>%
            pull(Value) %>%
            as.character()
    })
    changeMean <- reactive({
        format((as.numeric(lastMean()) - as.numeric(firstMean())) / as.numeric(firstMean()) *
                   100,
               digits = 2,
               nsmall = 1
        )
    })
    numCountries <- reactive({
        activeData()$geo %>% unique() %>% length() %>% as.character()
    })
    meanMap <- reactive({
        activeData()$values %>% mean()
    })
    
    # Paragraph 1
    description1 <-  eventReactive(input$runPlot, {
        switch(isolate(input$charttype),
               # Line chart
               "1" = switch(
                   isolate(input$dataset),
                   # rel_debt
                   "1" =
                       paste0(
                           "In ",
                           format(isolate(filterTime()[2]), format = isolate(inputTimeFormat())),
                           ", the country with the highest government debt as a percentage of Gross Domestic Product (GDP) was ",
                           maxCountry(),
                           " (",
                           maxValue(),
                           " %), while the country with the lowest government debt was ",
                           minCountry(),
                           " with ",
                           minValue(),
                           " %."
                       )
                   ,
                   # tot_debt
                   "2" =
                       paste0(
                           "In ",
                           format(isolate(filterTime()[2]), format = isolate(inputTimeFormat())),
                           ", the country with the highest absolute government debt was ",
                           maxCountry(),
                           " (",
                           format(
                               as.numeric(maxValue()) / 1000,
                               digits = 2,
                               nsmall = 1,
                               big.mark = ","
                           ),
                           " EUR Bn), while the country with the lowest government debt was ",
                           minCountry(),
                           " with ",
                           format(
                               as.numeric(minValue()) / 1000,
                               digits = 2,
                               nsmall = 1,
                               big.mark = ","
                           ),
                           " EUR Bn."
                       ),
                   # tot_gdp
                   "3" =
                       paste0(
                           "In ",
                           format(isolate(filterTime()[2]), format = isolate(inputTimeFormat())),
                           ", the country with the highest Gross Domestic Product (GDP) was ",
                           maxCountry(),
                           " (",
                           format(
                               as.numeric(maxValue()) / 1000,
                               digits = 2,
                               nsmall = 1,
                               big.mark = ","
                           ),
                           " EUR Bn), while the country with the lowest GDP was ",
                           minCountry(),
                           " with ",
                           format(
                               as.numeric(minValue()) / 1000,
                               digits = 2,
                               nsmall = 1,
                               big.mark = ","
                           ),
                           " EUR Bn."
                       ),
                   # unemp
                   "4" =
                       paste0(
                           "In ",
                           format(isolate(filterTime()[2]), format = "%B %Y"),
                           ", the country with the highest rate of unemployment was ",
                           maxCountry(),
                           " (",
                           maxValue(),
                           " %), while the country with the lowest unemployment rate was ",
                           minCountry(),
                           " with ",
                           minValue(),
                           " %."
                       )
               )
               
               ,
               # Map
               "2" = switch(
                   isolate(input$dataset),
                   # rel_debt
                   "1" =
                       paste0(
                           "In ",
                           format(isolate(filterTime()), format = isolate(inputTimeFormat())),
                           ", the country with the highest government debt as a percentage of Gross Domestic Product (GDP) was ",
                           maxCountry(),
                           " (",
                           maxValue(),
                           " %), while the country with the lowest government debt was ",
                           minCountry(),
                           " with ",
                           minValue(),
                           " %."
                       ),
                   # tot_debt
                   "2" =
                       paste0(
                           "In ",
                           format(isolate(filterTime()), format = isolate(inputTimeFormat())),
                           ", the country with the highest absolute government debt was ",
                           maxCountry(),
                           " (",
                           format(
                               as.numeric(maxValue()) / 1000,
                               digits = 2,
                               nsmall = 1,
                               big.mark = ","
                           ),
                           " EUR Bn), while the country with the lowest government debt was ",
                           minCountry(),
                           " with ",
                           format(
                               as.numeric(minValue()) / 1000,
                               digits = 2,
                               nsmall = 1,
                               big.mark = ","
                           ),
                           " EUR Bn."
                       ),
                   # tot_gdp
                   "3" =
                       paste0(
                           "In ",
                           format(isolate(filterTime()), format = isolate(inputTimeFormat())),
                           ", the country with the highest Gross Domestic Product (GDP) was ",
                           maxCountry(),
                           " (",
                           format(
                               as.numeric(maxValue()) / 1000,
                               digits = 2,
                               nsmall = 1,
                               big.mark = ","
                           ),
                           " EUR Bn), while the country with the lowest GDP was ",
                           minCountry(),
                           " with ",
                           format(
                               as.numeric(minValue()) / 1000,
                               digits = 2,
                               nsmall = 1,
                               big.mark = ","
                           ),
                           " EUR Bn."
                       ),
                   # unemp
                   "4" =
                       paste0(
                           "In ",
                           format(isolate(filterTime()), format = "%B %Y"),
                           ", the country with the highest rate of unemployment was ",
                           maxCountry(),
                           " (",
                           maxValue(),
                           " %), while the country with the lowest unemployment rate was ",
                           minCountry(),
                           " with ",
                           minValue(),
                           " %."
                       )
                   
               ))
        
    })
    
    # Paragraph 2
    description2 <-  eventReactive(input$runPlot, {
        switch(isolate(input$charttype),
               # Line chart
               "1" = switch(
                   isolate(input$dataset),
                   # rel_debt
                   "1" =
                       paste0(
                           "Over the selected time period, mean government debt in the selected countries ",
                           "(n = ",
                           numCountries(),
                           ") went from ",
                           firstMean(),
                           " % of GDP in ",
                           format(isolate(filterTime()[1]), format = isolate(inputTimeFormat())),
                           " to ",
                           lastMean(),
                           " % in ",
                           format(isolate(filterTime()[2]), format = isolate(inputTimeFormat())),
                           ", a change of ",
                           changeMean(),
                           " %."
                           
                       )
                   ,
                   # tot_debt
                   "2" =
                       paste0(
                           "Over the selected time period, mean government debt in the selected countries ",
                           "(n = ",
                           numCountries(),
                           ") went from ",
                           format(
                               as.numeric(firstMean()) / 1000,
                               digits = 2,
                               nsmall = 1,
                               big.mark = ","
                           ),
                           " EUR Bn in ",
                           format(isolate(filterTime()[1]), format = isolate(inputTimeFormat())),
                           " to ",
                           format(
                               as.numeric(lastMean()) / 1000,
                               digits = 2,
                               nsmall = 1,
                               big.mark = ","
                           ),
                           " EUR Bn in ",
                           format(isolate(filterTime()[2]), format = isolate(inputTimeFormat())),
                           ", a change of ",
                           changeMean(),
                           " %."
                       ),
                   # tot_gdp
                   "3" =
                       paste0(
                           "Over the selected time period, mean GDP in the selected countries ",
                           "(n = ",
                           numCountries(),
                           ") went from ",
                           format(
                               as.numeric(firstMean()) / 1000,
                               digits = 2,
                               nsmall = 1,
                               big.mark = ","
                           ),
                           " EUR Bn in ",
                           format(isolate(filterTime()[1]), format = isolate(inputTimeFormat())),
                           " to ",
                           format(
                               as.numeric(lastMean()) / 1000,
                               digits = 2,
                               nsmall = 1,
                               big.mark = ","
                           ),
                           " EUR Bn in ",
                           format(isolate(filterTime()[2]), format = isolate(inputTimeFormat())),
                           ", a change of ",
                           changeMean(),
                           " %."
                       ),
                   # unemp
                   "4" =
                       paste0(
                           "Over the selected time period, the average unemployment rate in the selected countries ",
                           "(n = ",
                           numCountries(),
                           ") went from ",
                           firstMean(),
                           " % in ",
                           format(isolate(filterTime()[1]), format = "%B %Y"),
                           " to ",
                           lastMean(),
                           " % in ",
                           format(isolate(filterTime()[2]), format = "%B %Y"),
                           ", a change of ",
                           changeMean(),
                           " %."
                       )
               )
               
               ,
               # Map
               "2" = switch(
                   isolate(input$dataset),
                   # rel_debt
                   "1" =
                       paste0(
                           "Mean government debt in the selected countries ",
                           "(n = ",
                           numCountries(),
                           ") was ",
                           format(meanMap(), digits = 2, nsmall = 1),
                           " % of GDP in ",
                           format(isolate(filterTime()), format = isolate(inputTimeFormat())),
                           "."
                       ),
                   # tot_debt
                   "2" =
                       paste0(
                           "Mean government debt in the selected countries ",
                           "(n = ",
                           numCountries(),
                           ") was ",
                           format(
                               meanMap() / 1000,
                               digits = 2,
                               nsmall = 1,
                               big.mark = ","
                           ),
                           " EUR Bn in ",
                           format(isolate(filterTime()), format = isolate(inputTimeFormat())),
                           "."
                       ),
                   # tot_gdp
                   "3" =
                       paste0(
                           "Mean GDP in the selected countries ",
                           "(n = ",
                           numCountries(),
                           ") was ",
                           format(
                               meanMap() / 1000,
                               digits = 2,
                               nsmall = 1,
                               big.mark = ","
                           ),
                           " EUR Bn in ",
                           format(isolate(filterTime()), format = isolate(inputTimeFormat())),
                           "."
                       ),
                   # unemp
                   "4" =
                       paste0(
                           "The average unemployment rate in the selected countries ",
                           "(n = ",
                           numCountries(),
                           ") was ",
                           format(meanMap(), digits = 2, nsmall = 1),
                           " % in ",
                           format(isolate(filterTime()), format = "%B %Y"),
                           "."
                       )
                   
               ))
        
    })
    
    output$description1 <- renderText({
        description1()
    })
    output$description2 <- renderText({
        description2()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
