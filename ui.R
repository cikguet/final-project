library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("IMDB Most Popular Films Released 2017"),
  
  # Sidebar with a slider input for number of bins
  fluidRow(
    column(2,
    # sidebarPanel(
     fixedPanel(
    # mainPanel(
      # Genre VS Runtime
      sliderInput("runtime",
                  "Plot1: Genre VS Runtime",
                  width="75%",
                  min = 0,
                  max = 250,
                  value = c(0,250),
                  dragRange = TRUE),
      # Vote VS Runtime
      sliderInput("vote",
                  "Plot 2: Vote VS Runtime",
                  width="75%",
                  min = 0,
                  max = 250,
                  value = c(0,250),
                  dragRange = TRUE),
      # Revenue VS Runtime
      sliderInput("revenue",
                  "Plot 3: Revenue VS Runtime",
                  width="75%",
                  min = 0,
                  max = 250,
                  value = c(0,250),
                  dragRange = TRUE),
      # Revenue VS Rating
      sliderInput("rating",
                  "Plot 4: Revenue VS Rating",
                  width="75%",
                  min = 0,
                  max = 10,
                  value = c(0,10),
                  dragRange = TRUE,
                  step = 0.1),
      # Revenue VS Rating
      sliderInput("director",
                  "Plot 5: Revenue VS Rating VS Director",
                  width="75%",
                  min = 0,
                  max = 10,
                  value = c(0,10),
                  dragRange = TRUE,
                  step = 0.1)
    # )
     )
    ),
    
    # Show a plot of the generated distribution
    column(10,
      #h3('Films Genre VS Runtime'),
      #plotOutput("runtimePlot")
      tabsetPanel(
        tabPanel("Read Me",
                 fluidRow(
                   h3(' '),
                   column(12,
                   wellPanel(
                   h3('Project Objective'),
                   p('To explain "Runtime", "Vote", "Gross Revenue" corelation to "Rating", "Genre" and "Runtime" pattern, TOP 10 features list.')
                   ),
                   
                   wellPanel(
                     h3('Project Data'),
                     p('IMDb - Movies, TV and Celebrities'),
                     p("IMDb, the world's most popular and authoritative source for movie, TV and celebrity content."),
                     p('Data source link:'),
                     pre('http://www.imdb.com/search/title?count=2500&release_date=2017,2017&title_type=feature&page=1&ref_=adv_nxt')
                   ),
                   
                   wellPanel(
                     h3('Project Preprocessing'),
                     
                     pre(
                       tags$code("
## load library
library(rvest) 
library(shiny)  
library(ggplot2) 
library(lubridate) 
library(devtools) 
library(dplyr)

## read and scrape website, target_row = 10,000
# 1 - 2500
url <- 'http://www.imdb.com/search/title?count=2500&release_date=2017,2017&title_type=feature&page=1&ref_=adv_nxt'
                                 imdb <- read_html(url)
                                 str(imdb)
                                 # 2501 - 5000
                                 url1 <- 'http://www.imdb.com/search/title?count=2500&release_date=2017,2017&title_type=feature&page=2&ref_=adv_nxt'
                                 imdb1 <- read_html(url1)
                                 str(imdb1)
                                 # 5001 - 7500
                                 url2 <- 'http://www.imdb.com/search/title?count=2500&release_date=2017,2017&title_type=feature&page=3&ref_=adv_nxt'
                                 imdb2 <- read_html(url2)
                                 str(imdb2)
                                 # 7501 - 10000
                                 url3 <- 'http://www.imdb.com/search/title?count=2500&release_date=2017,2017&title_type=feature&page=4&ref_=adv_nxt'
                                 imdb3 <- read_html(url3)
                                 str(imdb3)

## check data structure
list_data_html <- html_nodes(imdb,'.lister-item')
typeof(list_data_html)
length(list_data_html)
str(list_data_html)

list_data_html1 <- html_nodes(imdb1,'.lister-item')
typeof(list_data_html1)
length(list_data_html1)
str(list_data_html1)

list_data_html2 <- html_nodes(imdb2,'.lister-item')
typeof(list_data_html2)
length(list_data_html2)
str(list_data_html2)

list_data_html3 <- html_nodes(imdb3,'.lister-item')
typeof(list_data_html3)
length(list_data_html3)
str(list_data_html3)

...... and more ")
 )
                   ),

                    wellPanel(
                      h3('Github'),
                      pre('https://github.com/cikguet/final-project')
                    ),
                   
                   wellPanel(
                     h3('Shiny Dashboard'),
                     pre('https://eds2.shinyapps.io/final_project2/')
                   ),
 
                   wellPanel(
                     h3('R Library'),
                     #tag$li('rvest shiny\n ggplot2\n lubridate\n devtools\n dplyr\n DT\n tm\n SnowballC\n wordcloud\n RColorBrewer\n scales\n')
                     tags$ul(
                       tags$li("rvest"), 
                       tags$li("shiny"), 
                       tags$li("ggplot2"),
                       tags$li("lubridate"),
                       tags$li("devtools"),
                       tags$li("dplyr"),
                       tags$li("DT"),
                       tags$li("tm"),
                       tags$li("SnowballC"),
                       tags$li("wordcloud"),
                       tags$li("RColorBrewer"),
                       tags$li("scales")
                     )
                   ),
                   
                   wellPanel(
                     h3('Project Model'),
                     tags$b('Logistic Regression')
                   )
                   
                   
                  
                   
                   )
                   
                 )
          
        ),
        tabPanel("Plot", 
                 
                 fluidRow(
                   column(6,
                      # Films Title Cloud
                      h3('Films Title Analysis'),
                      plotOutput("titleWordCloud")
                   ),
                   column(6,
                      # Films Genre Cloud
                      h3('Films Genre Analysis'),
                      plotOutput("genreFreqPlot")
                   )
                   
                 ),
                 
                 # Genre VS Runtime
                 h3('Plot 1: Genre VS Runtime'),
                 p('Chart showed films genre relation to runtime'),
                 plotOutput("runtimePlot"),
                 p(em(strong('Summary: Most films production runtime in 2017 tend to fall in range 90 mins to 100 mins'))),
                 
                 # Vote VS Runtime
                 h3('Plot 2:Vote VS Runtime'),
                 p('Chart showed films voting relation to runtime'),
                 plotOutput("votePlot"),
                 p(em(strong('Summary: Films with higher voting tend to have higher rating.'))),
                 
                 # Revenue VS Runtime
                 h3('Plot 3: Revenue VS Runtime'),
                 p('Chart showed films gross revenue relation to runtime'),
                 plotOutput("revenuePlot"),
                 p(em(strong('Summary: Films with higher runtime tend to have higher rating.'))),
                 

                 # Revenue VS Rating
                 h3('Plot 4: Revenue VS Rating'),
                 p('Chart showed films gross revenue relation to rating'),
                 plotOutput("revenue_rating_plot"),
                 p(em(strong('Summary: Films with higher rating tend to have higher gross revenue.'))),
                 
                 # Revenue VS Rating
                 h3('Plot 5: Revenue VS Rating by Director'),
                 #p('Chart showed films gross revenue relation to rating'),
                 plotOutput("revenue_rating_director_plot")
                 #p(em(strong('Summary: Films with higher rating tend to have higher gross revenue.')))

                 # # Revenue VS Vote
                 # h3('Films Revenue VS Runtime'),
                 # plotOutput("revenuePlot")
                 
                 
                 ), 
        
        tabPanel("Model",
                h3('Model Plot'),
                fluidRow(
                  plotOutput("lm_plot"),
                  plotOutput("pair_plot")
                ),
                
                
                fluidRow(
                   
                   column(6,
                          h3('Dataset Summary'),
                     pre(
                       textOutput('lm_summary')
                     )
                   ),
                   column(6,
                          h3('Dataset Anova'),
                          pre(
                            textOutput('lm_anova')
                          )
                   )
                ),
                 
                h3('Prediction'),
                fluidRow(
                   
                   column(6,
                          numericInput("predict","Predict Film Rating", 1,min = 1, max = 100)
                          #actionButton("goPredict", "Predict Now!")
                   ),
                   column(6,
                          h4('Predicted Rating'),
                          textOutput('predict_output')
                   ),
                   h3(' ')
                )
                
                
                
        ),
        
        tabPanel("Summary", 
                 
                 fluidRow(
                   h3(' '),
                   column(4,
                          wellPanel(
                          h3('Top 10 Genre'),
                          tableOutput('top10GenreTable')
                          )
                   ),
                   column(4,
                          wellPanel(
                          h3('Top 10 Runtime'),
                          tableOutput('top10RuntimeTable')
                          )
                   ),
                   column(4,
                          wellPanel(
                            h3('Top 10 Rating'),
                            tableOutput('top10RatingTable')
                          )
                   )
                 ),
                 
                 fluidRow(
                   column(6,
                          wellPanel(
                            h3('Top 10 Highest Vote Films'),
                            tableOutput('top10VoteTable')
                          )
                   ),
                   column(6,
                          wellPanel(
                            h3('Top 10 Highest Rating Films'),
                            tableOutput('top10RatingTable1')
                          )
                   )
                 ),
                 
                 fluidRow(
                   column(6,
                          wellPanel(
                          h3('Top 10 Highest Metascore Films'),
                          tableOutput('top10MetascoreTable')
                          )
                   ),
                   column(6,
                          wellPanel(
                          h3('Top 10 Highest Gross Revenue Films'),
                          tableOutput('top10GrossTable')
                          )
                   )
                 ),
                 
                 # fluidRow(
                 #   column(6,
                 #          verbatimTextOutput("Top 10 Revenue of Films")
                 #   ),
                 #   column(6,
                 #          verbatimTextOutput("Top 10 Rating of Films")
                 #   )
                 # ),
                 
                 fluidRow(
                   column(6,
                          wellPanel(
                            h3('Top 10 Most Films Made Director'),
                            tableOutput('top10DirectorTable')
                          )
                   ),
                   column(6,
                          wellPanel(
                            h3('Top 10 Most Films Acted Actor'),
                            tableOutput('top10ActorTable')
                          )
                   )
                 ),
                 
                 fluidRow(
                   column(6,
                          wellPanel(
                            h3('Top 10 Most Revenue Earned Director'),
                            tableOutput('top10DirectorEarnedTable')
                          )
                   ),
                   column(6,
                          wellPanel(
                            h3('Top 10 Most Revenue Earned Actor'),
                            tableOutput('top10ActorEarnedTable')
                          )
                   )
                 )
                 
                 ), 
        
        tabPanel("Table",
                 # IMDB 2017 data
                 h3("IMDB 2017 Data"),
                 DT::dataTableOutput("imdb_table")
                 )
      )
    )
  )
))