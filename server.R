library(shiny)
library(ggplot2)
library(lubridate)
library(devtools)
library(DT)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(scales)
library(dplyr)

# load dataset
imdb_raw <- read.csv("imdb_2017_raw_10000.csv")
imdb_raw_lm <- read.csv("imdb_2017_clean_10000.csv")

# # replace 0 gross revenue to min revenue of 0.01
# imdb_raw_lm[imdb_raw_lm$revenue==0,] <- mean(imdb_raw_lm$revenue, na.rm = TRUE)
# 
# # replace NA rating to mean value
# imdb_raw_lm[c("rating")][is.na(imdb_raw_lm[c("rating")])] <- mean(imdb_raw_lm$rating, trim = 0.1, na.rm = TRUE)
# 
# # replace NA vote to mean value
# imdb_raw_lm[c("vote")][is.na(imdb_raw_lm[c("vote")])] <- mean(imdb_raw_lm$vote, trim = 0.1, na.rm = TRUE)
# 
# # replace NA metascore to mean value
# imdb_raw_lm[c("metascore")][is.na(imdb_raw_lm[c("metascore")])] <- mean(imdb_raw_lm$metascore, trim = 0.1, na.rm = TRUE)
# 
# # replace NA runtime to mean value
# imdb_raw_lm[c("runtime")][is.na(imdb_raw_lm[c("runtime")])] <- mean(imdb_raw_lm$runtime, trim = 0.1, na.rm = TRUE)



shinyServer(function(input, output) {
  
  # Films Title Cloud
  output$titleWordCloud <-renderPlot({
    # Word Cloud
    docs <- Corpus(VectorSource(imdb_raw$title))
    inspect(docs)
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    
    # Convert the text to lower case
    docs <- tm_map(docs, content_transformer(tolower))
    # Remove numbers
    docs <- tm_map(docs, removeNumbers)
    # Remove english common stopwords
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # Remove your own stop word
    # specify your stopwords as a character vector
    docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
    # Remove punctuations
    docs <- tm_map(docs, removePunctuation)
    # Eliminate extra white spaces
    docs <- tm_map(docs, stripWhitespace)
    # Text stemming
    #docs <- tm_map(docs, stemDocument)
    
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    head(d, 10)
    
    set.seed(18)
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  
  # Films Genre Cloud
  output$genreWordCloud <-renderPlot({
    # Word Cloud
    docs <- Corpus(VectorSource(imdb_raw$genre))
    inspect(docs)
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    
    # Convert the text to lower case
    docs <- tm_map(docs, content_transformer(tolower))
    # Remove numbers
    docs <- tm_map(docs, removeNumbers)
    # Remove english common stopwords
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # Remove your own stop word
    # specify your stopwords as a character vector
    docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
    # Remove punctuations
    docs <- tm_map(docs, removePunctuation)
    # Eliminate extra white spaces
    docs <- tm_map(docs, stripWhitespace)
    # Text stemming
    #docs <- tm_map(docs, stemDocument)
    
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    head(d, 10)
    
    set.seed(18)
    wordcloud(words = d$word, freq = d$freq, min.freq = 0,
              max.words=300, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  
  # Films Genre Analysis
  output$genreFreqPlot <- renderPlot({
    select_data <- aggregate(imdb_raw$genre, by=list(genre_freq=imdb_raw$genre), FUN=length)
    
    ggplot(select_data, aes(x=reorder(genre_freq,x), y=x, fill=genre_freq))+
      geom_bar(stat='identity')+
      coord_flip()+
      labs(x='Films Freq',y='Genre',colour='Genre')+
      geom_text(aes(label=sprintf('%s : %s',genre_freq,x)), size=3)+
      theme(
        # axis.text.x=element_blank()
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()
      )
  })
  
  # Genre VS Runtime 
  output$runtimePlot <- renderPlot({
    select_data <- imdb_raw[imdb_raw$runtime >= input$runtime[1] & imdb_raw$runtime <= input$runtime[2],]
    qplot(data = select_data,select_data$runtime,fill = select_data$genre,bins = 30) + labs(x='Runtime (min)',y='Genre Freq',fill='Genre')
  })
  
  # Vote VS Runtime
  output$votePlot <- renderPlot({
    select_data <- imdb_raw[imdb_raw$runtime >= input$vote[1] & imdb_raw$runtime <= input$vote[2],]
    ggplot(data=select_data, aes(x=select_data$runtime,y=select_data$vote))+
      geom_point(aes(size=select_data$vote,col=select_data$genre))+
      labs(x='Runtime (min)',y='Vote Freq',size='Vote',colour='Genre')
  })
  
  # Revenue VS Runtime
  output$revenuePlot <- renderPlot({
    select_data <- imdb_raw[imdb_raw$runtime >= input$revenue[1] & imdb_raw$runtime <= input$revenue[2],]
    ggplot(data=select_data, aes(x=select_data$runtime,y=select_data$revenue))+
      geom_point(aes(size=select_data$rating,col=select_data$genre))+
      labs(x='Runtime (min)',y='Gross Revenue (mil)',size='Rating',colour='Genre')
  })
  
  # Revenue VS Rating
  output$revenue_rating_plot <- renderPlot({
    select_data <- imdb_raw[imdb_raw$rating >= input$rating[1] & imdb_raw$rating <= input$rating[2],]
    ggplot(data=select_data, aes(x=select_data$rating,y=select_data$revenue))+
      geom_point(aes(size=select_data$vote,col=select_data$genre))+
      labs(x='Rating',y='Gross Revenue (mil)',size='Vote',colour='Genre')
  })
  
  # Revenue VS Rating
  output$revenue_rating_director_plot <- renderPlot({
    select_data <- imdb_raw[imdb_raw$rating >= input$director[1] & imdb_raw$rating <= input$director[2],]
    ggplot(data=select_data, aes(x=select_data$rating,y=select_data$revenue,label=select_data$director))+
      geom_text(check_overlap = TRUE)+
      labs(x='Rating',y='Gross Revenue (mil)')
  })
  
  
  
  output$lm_plot <- renderPlot({
    ggplot(data = imdb_raw_lm,aes(y=imdb_raw_lm$rating, x=imdb_raw_lm$runtime))+
      geom_point(aes(color=imdb_raw_lm$rating))+
      geom_smooth()+
      labs(x='Runtime (min)',y='Rating',colour='Rating')
  })
  
  output$pair_plot <- renderPlot({
    pairs(~rating+vote+runtime+revenue,data=imdb_raw_lm,col='red2')
  })
  
  output$lm_summary <- renderPrint({
    fit <- glm(rating ~ revenue + vote + runtime, data = imdb_raw_lm)
    summary(fit)
    
  })
  
  output$lm_anova <- renderPrint({
    fit <- glm(rating ~ revenue + vote + runtime, data = imdb_raw_lm)
    anova(fit)
  })
  
  output$predict_output <- renderText({
    
    #input$predict
    fit <- glm(rating ~ runtime, data = imdb_raw_lm)
    
    predict(fit,data.frame(runtime=input$predict))
  })
  
  
  # Top 10 Genre of Films
  #output$top10Genre <- renderText({ 'Top 10 Genre of Films' })
  output$top10GenreTable <- renderTable(width = "100%",{
    genre_df <- aggregate(x=imdb_raw$genre, by=list(genre=imdb_raw$genre), FUN=length)
    select_data <- genre_df[with(genre_df, order(-x)),]
    colnames(select_data) <- c("Genre", "Films")
    head(select_data,10) 
  })
  
  # Top 10 Runtime of Films
  output$top10RuntimeTable <- renderTable(width = "100%",{
    runtime_df <- aggregate(x=imdb_raw$runtime, by=list(runtime=imdb_raw$runtime), FUN=length)
    select_data <- runtime_df[with(runtime_df, order(-x)),]
    colnames(select_data) <- c("Runtime (Min)", "Films")
    head(select_data,10) 
  })
  
  # Top 10 Rating of Films
  output$top10RatingTable <- renderTable(width = "100%",{
    rating_df <- aggregate(x=imdb_raw$rating, by=list(runtime=imdb_raw$rating), FUN=length)
    select_data <- rating_df[with(rating_df, order(-x)),]
    colnames(select_data) <- c("Rating", "Films")
    head(select_data,10) 
  })
  
  # Top 10 Vote of Films
  output$top10VoteTable <- renderTable(width = "100%",{
    select_data <- imdb_raw[with(imdb_raw, order(-imdb_raw$vote)),]
    select_data <- select_data[,c('title','genre','vote')]
    colnames(select_data) <- c("Films Title", 'Genre',"Vote")
    head(select_data,10)
  })
  
  # Top 10 Highest Rating Films
  output$top10RatingTable1 <- renderTable(width = "100%",{
    select_data <- imdb_raw[with(imdb_raw, order(-imdb_raw$rating)),]
    select_data <- select_data[,c('title','genre','rating')]
    colnames(select_data) <- c("Films Title", 'Genre',"Rating")
    head(select_data,10)
  })
  
  # Top 10 Metascore of Films
  output$top10MetascoreTable <- renderTable(width = "100%",{
    select_data <- imdb_raw[with(imdb_raw, order(-imdb_raw$metascore)),]
    select_data <- select_data[,c('title','genre','metascore')]
    colnames(select_data) <- c("Films Title", 'Genre',"Metascroe")
    head(select_data,10)
  })
  
  # Top 10 Gross of Films
  output$top10GrossTable <- renderTable(width = "100%",{
    select_data <- imdb_raw[with(imdb_raw, order(-imdb_raw$revenue)),]
    select_data <- select_data[,c('title','genre','revenue')]
    colnames(select_data) <- c("Films Title", 'Genre',"Gross Revenue (mil)")
    head(select_data,10)
  })
  
  # Top 10 Most Films Made Director
  output$top10DirectorTable <- renderTable(width = "100%",{
    director_df <- aggregate(x=imdb_raw$director, by=list(director=imdb_raw$director), FUN=length)
    select_data <- director_df[with(director_df, order(-x)),]
    select_data <- select_data[-c(1,2), ] 
    colnames(select_data) <- c("Director", "Films")
    head(select_data,10) 
  })
  
  # Top 10 Most Films Acted Actor
  output$top10ActorTable <- renderTable(width = "100%",{
    actor_df <- aggregate(x=imdb_raw$actor, by=list(actor=imdb_raw$actor), FUN=length)
    select_data <- actor_df[with(actor_df, order(-x)),]
    colnames(select_data) <- c("Actor", "Films")
    head(select_data,10) 
  })
  
  # Top 10 Most Revenue Earned Director
  output$top10DirectorEarnedTable <- renderTable(width = "100%",{
    director_gross_df <- aggregate(x=imdb_raw$revenue, by=list(director=imdb_raw$director), FUN=sum)
    select_data <- director_gross_df[with(director_gross_df, order(-x)),]
    colnames(select_data) <- c("Director", "Gross Revenue (mil)")
    head(select_data,10)
  })
  
  # Top 10 Most Revenue Earned Actor
  output$top10ActorEarnedTable <- renderTable(width = "100%",{
    actor_gross_df <- aggregate(x=imdb_raw$revenue, by=list(actor=imdb_raw$actor), FUN=sum)
    select_data <- actor_gross_df[with(actor_gross_df, order(-x)),]
    colnames(select_data) <- c("Actor", "Gross Revenue (mil)")
    head(select_data,10)
  })
  
  
  
  # IMDB 2017 data
  output$imdb_table = DT::renderDataTable({
    imdb_raw
  })
  
})