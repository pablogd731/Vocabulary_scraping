#API Words in english 
#here, there are many public API's 'https://github.com/public-apis/public-apis'

# I use this API 'https://dictionaryapi.dev/' in this project 

#Packages used in this project 
library(stringr)
library(jsonlite)

web<- "https://api.dictionaryapi.dev/api/v2/entries/en/"
word_exemple <- "drawback"

#easy example
direction <- str_c(web, word_exemple)
direction

#Function of scraping
scraping <- function(api){
store_df <- list()
for(i in api){
  data <- fromJSON(i)
  store_df[[i]] <- data
}
return(store_df)
}

#Calling to scrape function
db <- scraping(direction)




#extraer contenido
data <- fromJSON(direction)
phonetics <- data$phonetics

#
hello <- phonetics[[1]]["text"]
