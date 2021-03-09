# Loading the required libraries
library(shiny)
library(leaflet)
library(tidyverse)
library(dplyr)
library(shinydashboard)
library(plotly)
library(shinyWidgets)

# reading the datasets that were created after wrangling
country_lockdown_condition_data <- read.csv("country_lockdown_condition_data.csv")
confirmed_covid_region_copy <- read.csv("confirmed_covid_region_copy.csv")
confirmed_covid_region_copy <- confirmed_covid_region_copy[(confirmed_covid_region_copy$cases!=0),]
comb_confirmed_covid_country <- read.csv("comb_confirmed_covid_country.csv")

mobility <- read.csv("clean_mobility_data.csv")

# creating country lists for each region and also for my mobility data,, as I want to pass them as a list to selectInput()
country_choice_mobility <- data.frame(
  "country_mobility" = unique(mobility$country_region),
  "num_mobility" = 1:length(unique(mobility$country_region))
)
country_list_mobility <- as.list(country_choice_mobility$num_mobility)
names(country_list_mobility) <- country_choice_mobility$country_mobility

country_choice_european <- data.frame(
  "country_european" = unique(comb_confirmed_covid_country$Country.Region[comb_confirmed_covid_country$WHO.region == "European Region"]),
  "num_european" = 1:length(unique(comb_confirmed_covid_country$Country.Region[comb_confirmed_covid_country$WHO.region == "European Region"]))
)
country_list_european <- as.list(country_choice_european$num_european)
names(country_list_european) <- country_choice_european$country_european

country_choice_african <- data.frame(
  "country_african" = unique(comb_confirmed_covid_country$Country.Region[comb_confirmed_covid_country$WHO.region == "African Region"]),
  "num_african" = 1:length(unique(comb_confirmed_covid_country$Country.Region[comb_confirmed_covid_country$WHO.region == "African Region"]))
)
country_list_african <- as.list(country_choice_african$num_african)
names(country_list_african) <- country_choice_african$country_african

country_choice_asia <- data.frame(
  "country_asia" = unique(comb_confirmed_covid_country$Country.Region[comb_confirmed_covid_country$WHO.region == "South-East Asia Region"]),
  "num_asia" = 1:length(unique(comb_confirmed_covid_country$Country.Region[comb_confirmed_covid_country$WHO.region == "South-East Asia Region"]))
)
country_list_asia <- as.list(country_choice_asia$num_asia)
names(country_list_asia) <- country_choice_asia$country_asia

country_choice_pacific <- data.frame(
  "country_pacific" = unique(comb_confirmed_covid_country$Country.Region[comb_confirmed_covid_country$WHO.region == "Western Pacific Region"]),
  "num_pacific" = 1:length(unique(comb_confirmed_covid_country$Country.Region[comb_confirmed_covid_country$WHO.region == "Western Pacific Region"]))
)
country_list_pacific <- as.list(country_choice_pacific$num_pacific)
names(country_list_pacific) <- country_choice_pacific$country_pacific

country_choice_eastern <- data.frame(
  "country_eastern" = unique(comb_confirmed_covid_country$Country.Region[comb_confirmed_covid_country$WHO.region == "Eastern Mediterranean Region"]),
  "num_eastern" = 1:length(unique(comb_confirmed_covid_country$Country.Region[comb_confirmed_covid_country$WHO.region == "Eastern Mediterranean Region"]))
)
country_list_eastern <- as.list(country_choice_eastern$num_eastern)
names(country_list_eastern) <- country_choice_eastern$country_eastern

country_choice_americas <- data.frame(
  "country_americas" = unique(comb_confirmed_covid_country$Country.Region[comb_confirmed_covid_country$WHO.region == "Region of the Americas"]),
  "num_americas" = 1:length(unique(comb_confirmed_covid_country$Country.Region[comb_confirmed_covid_country$WHO.region == "Region of the Americas"]))
)
country_list_americas <- as.list(country_choice_americas$num_americas)
names(country_list_americas) <- country_choice_americas$country_americas


# shiny UI
ui <- shinyUI(fluidPage(
  # setting a background image
  setBackgroundImage(
    src = "https://image.freepik.com/free-vector/blur-coronavirus-2019-ncov-white-background-medical-healthcare-microbiology-concept-design-covid-19-outbreaking_34926-393.jpg"
  ),
  fluidRow(column(1), 
           column(8, h1("COVID-19: Visualising the Global Impact", style="text-align: center; font-weight: bold;"))),
  br(), br(), br(),
  mainPanel(
    # introduction
    fluidRow(
      column(3),
      column(9,
      p("2020 started the beginning of a new decade, but definitely not in a good way. The Australian Bushfires already destroyed almost 46 million acres of land, along with countless number of species. But, I think that was just the beginning of how bad it was going to be in the coming months. The entire world stopped in its tracks when people everywhere started getting infected by Corona Virus. Originating in Wuhan, China this virus has already killed 432,000
        people, with 7.82 million current confirmed cases. Schools, Universities, Workplaces, Bars, Restaurants, and so many more places have had to be shutdown. People have to follow social distancing norms everyday, students are attending classes online, working from home is the new norm. In simple terms, it is a pandemic that has affected everyone in the world. Every country has gone into lockdown and trying their level best to control the situation.", style="text-align: center; font-weight: bold;"), br(),
      p("My main intention was too see how people all around the world adapted tp the current situatin, how countries were affected by Corona Virus. I also wanted to visualize how daily lives of people were affected, and through this I would like people to get a fair idea of what is going on. I know that people watch the daily news and keep themselves updated, but this will allow the user to see how the situation is progressing through the visulalisation.", style="text-align: center; font-weight: bold;"), br(),
      h3("Keep scrolling down to know more", style="text-align: center; font-weight: bold;"), br(), 
      h4("World Map to display countries that have OR have not implemented Lockdown by 6th April, 2020", style="text-align: center; font-weight: bold; font-size:18px;"), br(),
      p("Use the radio buttons alongside the World Map below to know which Countries implemented Lockdown and the Countries that have not. feel free to click on the marker points on the map, to know the name of the Country and it's Confirmed Case Count", style="text-align: center; font-weight: bold;"), br()),
    
    # world map for countries implementing and not implementing lockdown
    fluidRow(
      column(1),
      column(2, div(style = "font-size:18px;", radioButtons("world_country", label = div(style = "font-size:20px", "Choose an Option:"), 
                              choices=c("Countries Implementing Lockdown" = '1', 
                                        "Countries Not Implementing Lockdown" = '0'),
                              selected = '1'))),
      column(6, leafletOutput("mymap", height = 600, width = 1050))),
    br(),br(),
    
    fluidRow(
      column(3),
      column(9, h4("That was just an Overview. Keep scrolling if you want to know how individual regions were affected and what were the Top 10 Countries affected in that Region ", style="text-align: center; font-weight: bold; font-size:18px;"), br(),
                p("Use the dropdown available under the Region tab to view the Confirmed Cases and Deaths trend. You can view the Top 10 Countries affected in each Region by simply clickiing on the next tab. If you want to know the Case Count for a particular Country that you fancy, click on the final tab to get a dropdown list of Countries for the Region selected.", style="text-align: center; font-weight: bold;"), br())),
    
    # tabBox with multiple tabs one for Region, one too display Top 10 Countries in that region, and last one for individual Countries
    fluidRow(
      column(2),
      column(10, tabBox(
        # first tab  
        tabPanel("Region",
                 selectInput("world_region", "", 
                             c("African Region" = "African Region", 
                               "European Region" = "European Region",
                               "South-East Asia Region" = "South-East Asia Region",
                               "Western Pacific Region" = "Western Pacific Region",
                               "Eastern Mediterranean Region" = "Eastern Mediterranean Region",
                               "Region of the Americas" = "Region of the Americas"),
                             selected = "European Region"
                 ),
                 plotlyOutput("covidRegion", height = 600, width = 1200)
          ),
        # second tab  
        tabPanel("Top 10 Countries",
                 plotlyOutput("covid_top_country", height = 600, width = 1200)
          ),
        # third tab: for each selectInput() i am passing the list based on the Region selected
        tabPanel("Country",
                 conditionalPanel(
                   condition = "input.world_region == 'European Region'",
                   selectInput("country_europe",
                               label = h3("Select Country"),
                               choices = country_list_european)),
                 
                 conditionalPanel(
                   condition = "input.world_region == 'African Region'",
                   selectInput("country_africa",
                               label = h3("Select Country"),
                               choices = country_list_african)),
                 
                 conditionalPanel(
                   condition = "input.world_region == 'South-East Asia Region'",
                   selectInput("country_asia",
                               label = h3("Select Country"),
                               choices = country_list_asia)),
                 
                 conditionalPanel(
                   condition = "input.world_region == 'Western Pacific Region'",
                   selectInput("country_pacific",
                               label = h3("Select Country"),
                               choices = country_list_pacific)),
                 
                 conditionalPanel(
                   condition = "input.world_region == 'Eastern Mediterranean Region'",
                   selectInput("country_eatsern",
                               label = h3("Select Country"),
                               choices = country_list_eastern)),
                 
                 conditionalPanel(
                   condition = "input.world_region == 'Region of the Americas'",
                   selectInput("country_americas",
                               label = h3("Select Country"),
                               choices = country_list_americas)),
                 
                 plotlyOutput("covidCountry", height = 600, width = 1200)
          )))),
    
    br(),
    # intro before Google Mobility Report
    fluidRow(column(3),
             column(9, h3("Visualizing Change In People's Daily Lives Using Google's COVID-19 Community Mobility Reports", style="text-align: center; font-size:18px; font-weight: bold;"), br(),
      p("As the pandemic started to grow globally, Google started accumulating data which tries to show trends by region, across various categories of places. These reports were created using aggregated, anonymized sets of data from various Users for whom the Location History setting is turned on. These reports were mainly intended to help Public Health Officials with their daily work.", style="text-align: center; font-weight: bold;"),
      p("All the Mobility Indexes, show a pecentage change from ", span("baseline value", style = "color:blue"), " which simply represents a normal value for that day of the week. The baseline day is nothing but the median value from the 5- week period between 3rd January - 6th February, 2020.", style="text-align: center; font-weight: bold;"), br(), p("Using the Mobility Reports I have tried to visualize change in different aspects like how much time are people spending in workplaces, their own residence, groceries & pharmacies, transit stations and retail & recreation places.", style="text-align: center; font-weight: bold;"), br(),
      p("To give you a brief overview, the below plot will show how the different Mobility Indexes change over time for Australia.", style="text-align: center; font-weight: bold;"))), br(),
    
    # displaying plot for Australia and also mentioning findings after that
    fluidRow(column(2),
               column(10, box(plotlyOutput("mobilityAU", height = 600, width = 1200)))),
    br(),
    
    fluidRow(column(3),
             column(9, 
                    p("Looking at the Google Mobility Report for Australia, before 18th March, it was a bit erratic as I believe people were still going about their daily lives, and the Residential index was also almost close to 0 at that time. But from 18th all the Mobility Indexes apart from the Residential index started going down, so we can assume that people started to take self precautionary measures.", style="text-align: center; font-weight: bold;"), br(),
                    h4("You can now scroll down if you want to know how the Google Mobility Indexes vary for other Countries. You can make your own inferences and play around with the data.", style="text-align: center; font-weight: bold;"), br(),
                    p("Use the dropdown available alongside the plot below to Select Country and view the Mobility Indexes for that Country.", style="text-align: center; font-weight: bold;"))),
    br(),
    
    # giving users an option to select a Country for which they want to view the Mobility Index  
    fluidRow(column(1),
               column(3, selectInput("country_mobility",
                                     label = h3("Select Country"),
                                     choices = country_list_mobility)),
               column(8, plotlyOutput("mobilityWorld", height = 600, width = 1100))),
    br(),
    fluidRow(column(3),
             column(9, 
                    p("You scrolled till the END. AWESOME!!!", style="text-align: center; font-weight: bold;"), br(),
                    h4("Hope the Visualisation was informative. Until Next Time.", style="text-align: center; font-weight: bold;"), br(),
                    p("THE END!!!", style="text-align: center; font-weight: bold; font-size:25px;"))), br(),
  ))
))

# shiny server
server <- shinyServer(function(input, output, session) {
  
  # creating leaflet map for displaying the world map with countries having or not havng lockdown protocols.
  output$mymap <- renderLeaflet({ 
    leaflet(data = country_lockdown_condition_data %>% filter(lockdown == input$world_country)) %>% 
      addTiles() %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      # use the random generated points as markers on the map
      addMarkers(~CENTER_LON, ~CENTER_LAT, popup = ~as.character(paste("Country:", Country.Region, "<br>",
                                                                       "Confirmed Cases:", Cases)))
  })
  
  # creating multiple line chart for displaying the Confirmed case Count and Deaths for each World Region
  output$covidRegion <- renderPlotly({
    plot_ly(data = confirmed_covid_region_copy %>% filter(WHO.region == input$world_region), 
            x = ~Date, y=~cases, color =~Case.Status, type='scatter', mode = 'line') %>%
      layout(title = "COVID-19 Cases By Region Till 6th April",
             yaxis = list(title = "No. of Cases"))
  })
  
  # creating interactive bar chart for displaying the Top 10 Countries with the most Confirmed case Count for each World Region
  output$covid_top_country <- renderPlotly({
    region_str <- paste("Top 10 COVID-19 Cases For", toString(input$world_region), sep=" ")
    top_country <- comb_confirmed_covid_country %>% filter(Cases > 0) %>% filter(WHO.region == input$world_region)
    top_country <- as.data.frame(aggregate(Cases ~ Country.Region, data = top_country, max)) %>% arrange(desc(Cases))
    top_country <- head(top_country, 10)
    top_country$Country.Region <- factor(top_country$Country.Region, levels=unique(as.character(top_country$Country.Region)))
    plot_ly(data = top_country, x = ~Country.Region, y = ~Cases, type = 'bar', text = ~Cases,
            marker = list(color = 'rgb(58,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(title = region_str,
             xaxis = list(title = "Country"),
             yaxis = list(title = "No. of Cases"))
  })
  
  # creating Area chart for displaying the Confirmed Case Count for each Country in that particular World Region
  output$covidCountry <- renderPlotly({
    comb_confirmed_covid_country
    if (input$world_region == "European Region"){
      index <- as.numeric(input$country_europe)
      plot_ly(data = comb_confirmed_covid_country %>% filter(Cases > 0) %>% filter(Country.Region == country_choice_european[index,1]), 
              x = ~Dates, y = ~Cases, type = 'scatter', mode = 'line', fill = 'tozeroy') %>%
        layout(title = "COVID-19 Cases By Country",
               yaxis = list(title = "No. of Cases"))
    }
    else if (input$world_region == "African Region"){
      index <- as.numeric(input$country_africa)
      plot_ly(data = comb_confirmed_covid_country %>% filter(Cases > 0) %>% filter(Country.Region == country_choice_african[index,1]), 
              x = ~Dates, y = ~Cases, type = 'scatter', mode = 'line', fill = 'tozeroy') %>%
        layout(title = "COVID-19 Cases By Country",
               yaxis = list(title = "No. of Cases"))
    }
    else if (input$world_region == "South-East Asia Region"){
      index <- as.numeric(input$country_asia)
      plot_ly(data = comb_confirmed_covid_country %>% filter(Cases > 0) %>% filter(Country.Region == country_choice_asia[index,1]), 
              x = ~Dates, y = ~Cases, type = 'scatter', mode = 'line', fill = 'tozeroy') %>%
        layout(title = "COVID-19 Cases By Country",
               yaxis = list(title = "No. of Cases"))
    }
    else if (input$world_region == "Western Pacific Region"){
      index <- as.numeric(input$country_pacific)
      plot_ly(data = comb_confirmed_covid_country %>% filter(Cases > 0) %>% filter(Country.Region == country_choice_pacific[index,1]), 
              x = ~Dates, y = ~Cases, type = 'scatter', mode = 'line', fill = 'tozeroy') %>%
        layout(title = "COVID-19 Cases By Country",
               yaxis = list(title = "No. of Cases"))
    }
    else if (input$world_region == "Eastern Mediterranean Region"){
      index <- as.numeric(input$country_eatsern)
      plot_ly(data = comb_confirmed_covid_country %>% filter(Cases > 0) %>% filter(Country.Region == country_choice_eastern[index,1]), 
              x = ~Dates, y = ~Cases, type = 'scatter', mode = 'line', fill = 'tozeroy') %>%
        layout(title = "COVID-19 Cases By Country",
               yaxis = list(title = "No. of Cases"))
    }
    else if (input$world_region == "Region of the Americas"){
      index <- as.numeric(input$country_americas)
      plot_ly(data = comb_confirmed_covid_country %>% filter(Cases > 0) %>% filter(Country.Region == country_choice_americas[index,1]), 
              x = ~Dates, y = ~Cases, type = 'scatter', mode = 'line', fill = 'tozeroy') %>%
        layout(title = "COVID-19 Cases By Country",
               yaxis = list(title = "No. of Cases"))
    }
  })
  
  # creating multiple Line chart for displaying the Google Mobility Indexes for Australia
  output$mobilityAU <- renderPlotly({
    mobility_AU <- mobility[mobility$country_region=="Australia",]
    mobility_AU <- mobility_AU[,c(-1, -2)]
    
    # creating an empty object
    p <- plot_ly()%>%
      layout(title = "Community Mobility Trend for Australia Over Time",
             xaxis = list(title = "Date"),
             yaxis = list (title = "Percentage change from baseline") )
    
    # taking out the required column names
    ToAdd <- setdiff(colnames(mobility_AU),"date")
    
    # taking out a row based on maximum grocery_pharmacy value
    m <- mobility_AU[which.max(mobility_AU$grocery_pharmacy),]
    
    # for annotation
    a <- list(
      x = m$date, y = m$grocery_pharmacy, text = "People Spent a lot of time Stocking UP!!!", xref = "x", yref = "y", showarrow = TRUE, arrowhead = 7, ax = -120, ay = -40
    )
    
    # for annotation
    b <- list(
      x = "2020-03-18", y = -50, text = "ALL MOBILITY INDEX VALUES EXCEPT FOR RESIDENTIAL KEPT ON GOING DOWN", xref = "x", yref = "y", showarrow = TRUE, arrowhead = 5, ax = -120, ay = 40
    )
    
    ## Add the traces one at a time to the empty object
    for(i in ToAdd){
      p <- p %>% add_trace(x = ~mobility_AU[["date"]], y = mobility_AU[[i]], name = i,
                           type = 'scatter',
                           mode = 'line+markers',
                           line = list(color = i, width = 2))
    }
    p <- p %>% layout(annotations = a)
    
    p <- p %>% add_segments(x="2020-03-18", xend = "2020-03-18", y = -60, yend = 40, showlegend=FALSE)
    p <- p %>% layout(annotations = b)
    p
  })
  
  # creating multiple Line chart for displaying the Google Mobility Indexes for all countries in the world
  output$mobilityWorld <- renderPlotly({
    index <- as.numeric(input$country_mobility)
    mobility_country <- mobility[mobility$country_region==country_choice_mobility[index,1],]
    mobility_country <- mobility_country[,c(-1, -2)]
    
    # creating an empty object
    p <- plot_ly()%>%
      layout(title = "Trend Over Time",
             xaxis = list(title = "Date"),
             yaxis = list (title = "Percentage change from baseline") )
    
    ToAdd <- setdiff(colnames(mobility_country),"date")
    
    ## Add the traces one at a time
    for(i in ToAdd){
      p <- p %>% add_trace(x = ~mobility_country[["date"]], y = mobility_country[[i]], name = i,
                           type = 'scatter',
                           mode = 'line+markers',
                           line = list(color = i, width = 2))
    }
    p
  })
  
})

shinyApp(ui = ui, server = server)