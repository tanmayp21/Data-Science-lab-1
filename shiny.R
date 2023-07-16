load("DATA.Rdata")
library(shiny)
library(ggplot2)
packages = c('sf', 'tmap', 'tidyverse', 'plotly','viridis','hrbrthemes', 'ggthemes')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}


# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("World happiness Index"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a Year:",
                  choices = c("2015", "2016", "2017", "2018", "2019")),
      
      # Input: Selector for choosing attributes ----
      selectInput(inputId = "selectedCol",
                  label = "Select column for X axis",
                  choices = c("GDP.per.capita", "Life.expectancy", "Freedom.of.life.choices", "Absence.of.Govt.corruption", "Generosity", "Poverty","literacy rate")),

      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h2("Attributes and their characteristics"),
      
      # Output: Tabset w/ boxplot, plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("box",plotOutput("box")),
                  tabPanel("Plot", plotOutput("Plot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("view")),),
      
      # output: scatter plots and map
      plotOutput("distPlot"),
      
      h2("The World Distribution of Happiness Index"),
      tmapOutput("map")
    )
  )
)

# Defining server logic to summarize and view selected dataset ----
server <- function(input, output,session) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "2015" = fif,
           "2016" =  six,
           "2017" =  seven,
           "2018" = eight,
           "2019" = nine)
  })
  
  # Return the requested x-axis ----
  selectedColInput <- reactive({
    switch(input$selectedCol,
           "GDP.per.capita" = "GDP",
           "Life.expectancy" = "life",
           "Freedom.of.life.choices" = "freedom",
           "Absence.of.Govt.corruption"= "corrupt",
           "Generosity" = "gen",
           "Poverty" = "pov",
           "literacy rate" = "rate")
  })
  
  # Return the boxplot ---
  output$box <- renderPlot({
    dataset <- datasetInput()
   bp <- ggplot(dataset, aes(Continent, Happiness.Score))
   bp+geom_boxplot(fill = "#FFDB6D", color = "#C4961A")
   })
  
  # Return the data summary ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  # Return the scatterplot x-axis vs happiness index ----
  output$Plot <- renderPlot({
    dataset <- datasetInput()
    selectedCol <- selectedColInput()
    if( selectedCol== "GDP"){
      
      p <- ggplot(dataset,
                  aes(x = dataset$GDP.per.capita, y = dataset$Happiness.Score, col = dataset$Continent)) +
        geom_point()+
        labs(y = "Happiness Scores", x = "GDP per capita")+
        geom_smooth(method="lm", se=F)
      p
    }
    else if( selectedCol=="life"){
      
      
      p <- ggplot(dataset,
                  aes(x = dataset$Life.expentancy, y = dataset$Happiness.Score,col = dataset$Continent)) +
        geom_point()+
        labs(y = "Happiness Scores", x = "Life expectancy")+
        geom_smooth(method="lm", se=F)
      p
    }
    else  if( selectedCol=="freedom"){
      p <- ggplot(dataset,
                  aes(x = dataset$Freedom.of.life.choices, y = dataset$Happiness.Score, col = dataset$Continent)) +
        geom_point()+
        labs(y = "Happiness Scores", x = "Freedom of life choices")+
        geom_smooth(method="lm", se=F)
      p
    }
    else  if( selectedCol=="corrupt"){
      p <- ggplot(dataset,
                  aes(x = dataset$Govt.corruption, y = dataset$Happiness.Score, col = dataset$Continent)) +
        geom_point()+
        labs(y = "Happiness Scores", x = "Absence of Govt Corruption")+
        geom_smooth(method="lm", se=F)
      p
    }
    else  if( selectedCol=="gen"){
      p <- ggplot(dataset,
                  aes(x = dataset$Generosity, y = dataset$Happiness.Score, col = dataset$Continent)) +
        geom_point()+
        labs(y = "Happiness Scores", x = "Generosity")+
        geom_smooth(method="lm", se=F)
      p
    }
    else  if( selectedCol=="pov"){
      p <- ggplot(dataset,
                  aes(x = dataset$percPoverty, y = dataset$Happiness.Score, col = dataset$Continent)) +
        geom_point()+
        geom_smooth(method="lm", se=F)+
        labs(y = "Happiness Scores", x = "Poverty index")
     p
    }
    
    else  if( selectedCol=="rate"){
      q <- cor(dataset$Happiness.Score,dataset$latestRate)
      p <- ggplot(dataset,
                  aes(x = latestRate, y = Happiness.Score,col = Continent)) +
        geom_point()+
        geom_smooth(method="lm", se=F)+
        labs(y = "Happiness Scores", x = "literacy rate", subtitle =  )
      p
    }
    
})
  
  # Return the map ----
   output$map <- renderTmap({
    dataset <- datasetInput()
    join <- left_join(countries,dataset, by = c("COUNTRY"="Country" ))
    dataset2 <- join %>% filter(!is.na("Happiness.Score"))
    
    map <- tm_shape(dataset2) +
      tm_polygons("Happiness.Score",
                  style = "quantile", 
                  palette = "RdYlGn") +
      tm_layout(legend.height = 0.45, 
                legend.width = 0.3,
                legend.outside = FALSE,
                legend.position = c("right", "bottom"),
                frame = FALSE) +
      tm_borders(alpha = 0.5) 
    
    tmap_mode("view")
    map
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

