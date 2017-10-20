## load library
library(rvest) # web scraping
library(shiny) # shiny app 
library(ggplot2) #graph plotting
library(lubridate) # datetime processing
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


## scraping process
ranking <- c()
title <- c()
desc <- c()
runtime <- c()
genre <- c()
rating <- c()
vote <- c()
director <- c()
actor <- c()
metascore <- c()
revenue <- c()
for(i in 1:length(list_data_html)){
  print(i)
  temp_revenue <- ''
  
  ranking <- append(ranking,as.numeric(html_text(html_node(list_data_html[i],'.text-primary'))))
  title <- append(title,html_text(html_node(list_data_html[i],'.lister-item-header a')))
  desc <- append(desc,gsub("\n","",html_text(html_node(list_data_html[i],'.ratings-bar+ .text-muted'))))
  runtime <- append(runtime,as.numeric(gsub(" min","",html_text(html_node(list_data_html[i],'.text-muted .runtime')))))
  genre <- append(genre,gsub(",.*","",gsub(" ","",gsub("\n","",html_text(html_node(list_data_html[i],'.genre'))))))
  rating <- append(rating,as.numeric(html_text(html_node(list_data_html[i],'.ratings-imdb-rating strong'))))
  vote <- append(vote,as.numeric(gsub(",","",html_text(html_node(list_data_html[i],'.sort-num_votes-visible span:nth-child(2)')))))
  
  if(i==2){
    director <- append(director,'Rian Johnson')
  }else{
    director <- append(director,html_text(html_node(list_data_html[i],'.text-muted + p a:nth-child(1)')))
  }
  
  actor <- append(actor,html_text(html_node(list_data_html[i],'.lister-item-content .ghost ~ a')))
  metascore <- append(metascore,as.numeric(gsub(" ","",html_text(html_node(list_data_html[i],'.metascore')))))
  
  temp_revenue <- gsub("M","",html_text(html_nodes(list_data_html[i],'.sort-num_votes-visible .ghost ~ .text-muted + span')))
  if(length(temp_revenue) == 0){
    revenue <- append(revenue,0)
  }else{
    revenue <- append(revenue,as.numeric(substring(temp_revenue,2,nchar(temp_revenue))))
  }
}

imdb_df <- data.frame(ranking = ranking, 
                      title = title,
                      desc = desc,
                      runtime = runtime,
                      genre = genre,
                      rating = rating,
                      vote = vote,
                      director = director,
                      actor = actor,
                      metascore = metascore,
                      revenue = revenue)
# check dataframe structure
View(imdb_df)
str(imdb_df)

ranking1 <- c()
title1 <- c()
desc1 <- c()
runtime1 <- c()
genre1 <- c()
rating1 <- c()
vote1 <- c()
director1 <- c()
actor1 <- c()
metascore1 <- c()
revenue1 <- c()
for(i in 1:length(list_data_html1)){
  print(i)
  temp_revenue1 <- ''
  
  ranking1 <- append(ranking1,as.numeric(html_text(html_node(list_data_html1[i],'.text-primary'))))
  title1 <- append(title1,html_text(html_node(list_data_html1[i],'.lister-item-header a')))
  desc1 <- append(desc1,gsub("\n","",html_text(html_node(list_data_html1[i],'.ratings-bar+ .text-muted'))))
  runtime1 <- append(runtime1,as.numeric(gsub(" min","",html_text(html_node(list_data_html1[i],'.text-muted .runtime')))))
  genre1 <- append(genre1,gsub(",.*","",gsub(" ","",gsub("\n","",html_text(html_node(list_data_html1[i],'.genre'))))))
  rating1 <- append(rating1,as.numeric(html_text(html_node(list_data_html1[i],'.ratings-imdb-rating strong'))))
  vote1 <- append(vote1,as.numeric(gsub(",","",html_text(html_node(list_data_html1[i],'.sort-num_votes-visible span:nth-child(2)')))))
  
  if(i==2){
    director1 <- append(director1,'Rian Johnson')
  }else{
    director1 <- append(director1,html_text(html_node(list_data_html1[i],'.text-muted + p a:nth-child(1)')))
  }
  
  actor1 <- append(actor1,html_text(html_node(list_data_html1[i],'.lister-item-content .ghost ~ a')))
  metascore1 <- append(metascore1,as.numeric(gsub(" ","",html_text(html_node(list_data_html1[i],'.metascore')))))
  
  temp_revenue1 <- gsub("M","",html_text(html_nodes(list_data_html1[i],'.sort-num_votes-visible .ghost ~ .text-muted + span')))
  if(length(temp_revenue1) == 0){
    revenue1 <- append(revenue1,0)
  }else{
    revenue1 <- append(revenue1,as.numeric(substring(temp_revenue1,2,nchar(temp_revenue1))))
  }
}

imdb_df1 <- data.frame(ranking = ranking1, 
                      title = title1,
                      desc = desc1,
                      runtime = runtime1,
                      genre = genre1,
                      rating = rating1,
                      vote = vote1,
                      director = director1,
                      actor = actor1,
                      metascore = metascore1,
                      revenue = revenue1)
# check dataframe structure
View(imdb_df1)
str(imdb_df1)

ranking2 <- c()
title2 <- c()
desc2 <- c()
runtime2 <- c()
genre2 <- c()
rating2 <- c()
vote2 <- c()
director2 <- c()
actor2 <- c()
metascore2 <- c()
revenue2 <- c()
for(i in 1:length(list_data_html2)){
  print(i)
  temp_revenue2 <- ''
  
  ranking2 <- append(ranking2,as.numeric(html_text(html_node(list_data_html2[i],'.text-primary'))))
  title2 <- append(title2,html_text(html_node(list_data_html2[i],'.lister-item-header a')))
  desc2 <- append(desc2,gsub("\n","",html_text(html_node(list_data_html2[i],'.ratings-bar+ .text-muted'))))
  runtime2 <- append(runtime2,as.numeric(gsub(" min","",html_text(html_node(list_data_html2[i],'.text-muted .runtime')))))
  genre2 <- append(genre2,gsub(",.*","",gsub(" ","",gsub("\n","",html_text(html_node(list_data_html2[i],'.genre'))))))
  rating2 <- append(rating2,as.numeric(html_text(html_node(list_data_html2[i],'.ratings-imdb-rating strong'))))
  vote2 <- append(vote2,as.numeric(gsub(",","",html_text(html_node(list_data_html2[i],'.sort-num_votes-visible span:nth-child(2)')))))
  
  if(i==2){
    director2 <- append(director2,'Rian Johnson')
  }else{
    director2 <- append(director2,html_text(html_node(list_data_html2[i],'.text-muted + p a:nth-child(1)')))
  }
  
  actor2 <- append(actor2,html_text(html_node(list_data_html2[i],'.lister-item-content .ghost ~ a')))
  metascore2 <- append(metascore2,as.numeric(gsub(" ","",html_text(html_node(list_data_html2[i],'.metascore')))))
  
  temp_revenue2 <- gsub("M","",html_text(html_nodes(list_data_html2[i],'.sort-num_votes-visible .ghost ~ .text-muted + span')))
  if(length(temp_revenue2) == 0){
    revenue2 <- append(revenue2,0)
  }else{
    revenue2 <- append(revenue2,as.numeric(substring(temp_revenue2,2,nchar(temp_revenue2))))
  }
}

imdb_df2 <- data.frame(ranking = ranking2, 
                      title = title2,
                      desc = desc2,
                      runtime = runtime2,
                      genre = genre2,
                      rating = rating2,
                      vote = vote2,
                      director = director2,
                      actor = actor2,
                      metascore = metascore2,
                      revenue = revenue2)
# check dataframe structure
View(imdb_df2)
str(imdb_df2)

ranking3 <- c()
title3 <- c()
desc3 <- c()
runtime3 <- c()
genre3 <- c()
rating3 <- c()
vote3 <- c()
director3 <- c()
actor3 <- c()
metascore3 <- c()
revenue3 <- c()
for(i in 1:length(list_data_html3)){
  print(i)
  temp_revenue3 <- ''
  
  ranking3 <- append(ranking3,as.numeric(html_text(html_node(list_data_html3[i],'.text-primary'))))
  title3 <- append(title3,html_text(html_node(list_data_html3[i],'.lister-item-header a')))
  desc3 <- append(desc3,gsub("\n","",html_text(html_node(list_data_html3[i],'.ratings-bar+ .text-muted'))))
  runtime3 <- append(runtime3,as.numeric(gsub(" min","",html_text(html_node(list_data_html3[i],'.text-muted .runtime')))))
  genre3 <- append(genre3,gsub(",.*","",gsub(" ","",gsub("\n","",html_text(html_node(list_data_html3[i],'.genre'))))))
  rating3 <- append(rating3,as.numeric(html_text(html_node(list_data_html3[i],'.ratings-imdb-rating strong'))))
  vote3 <- append(vote3,as.numeric(gsub(",","",html_text(html_node(list_data_html3[i],'.sort-num_votes-visible span:nth-child(2)')))))
  
  if(i==2){
    director3 <- append(director3,'Rian Johnson')
  }else{
    director3 <- append(director3,html_text(html_node(list_data_html3[i],'.text-muted + p a:nth-child(1)')))
  }
  
  actor3 <- append(actor3,html_text(html_node(list_data_html3[i],'.lister-item-content .ghost ~ a')))
  metascore3 <- append(metascore3,as.numeric(gsub(" ","",html_text(html_node(list_data_html3[i],'.metascore')))))
  
  temp_revenue3 <- gsub("M","",html_text(html_nodes(list_data_html3[i],'.sort-num_votes-visible .ghost ~ .text-muted + span')))
  if(length(temp_revenue3) == 0){
    revenue3 <- append(revenue3,0)
  }else{
    revenue3 <- append(revenue3,as.numeric(substring(temp_revenue3,2,nchar(temp_revenue3))))
  }
}

imdb_df3 <- data.frame(ranking = ranking3, 
                      title = title3,
                      desc = desc3,
                      runtime = runtime3,
                      genre = genre3,
                      rating = rating3,
                      vote = vote3,
                      director = director3,
                      actor = actor3,
                      metascore = metascore3,
                      revenue = revenue3)
# check dataframe structure
View(imdb_df3)
str(imdb_df3)


## merge dataframe
final_imdb <- ''
final_imdb <- rbind(imdb_df, imdb_df1)
str(final_imdb)
final_imdb <- rbind(final_imdb, imdb_df2)
str(final_imdb)
final_imdb <- rbind(final_imdb, imdb_df3)
str(final_imdb)
View(final_imdb)


## output file
# save as raw
write.csv(final_imdb, file = "imdb_2017_raw_10000.csv")

