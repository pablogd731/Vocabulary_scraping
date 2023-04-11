#API Words in english 
#here, there are many public API's 'https://github.com/public-apis/public-apis'

# I use this API 'https://dictionaryapi.dev/' in this project 

#Packages used in this project 
library(stringr)
library(jsonlite)
library(rio)

#main directory
setwd("/home/pablogd731/Documents/Vocabulary_scraping")


#Structure of API
web <- "https://api.dictionaryapi.dev/api/v2/entries/en/"
word_exemple <- "drawback"

#easy example
direction <- str_c(web, word_exemple)
direction

#use a personal vocabulary learned
vocabulary <- as.vector(import("words", "txt", header = F))

#concat each word with direction api
together <- function(file, web_api){
bill <- list()
for(j in 1:nrow(file)){
  link <- str_c(web_api,tolower(file[j,]))
  bill[[j]] <- link
}
return(bill)
}

#alternative "together" function
# together <- function(file, web_api) {
#   links <- lapply(tolower(file), function(word) {
#     str_c(web_api, word)
#   })
#   names(links) <- file
#   return(links)
# }


#Call function together 
links <- together(vocabulary, web)
names(links) <- t(vocabulary)

#Function of scraping
scraping <- function(api){
store_df <- list()
for(i in api){
  data <- fromJSON(i)
  store_df[[i]] <- data
  Sys.sleep(3)
}
return(store_df)
}

#Calling to scrape function
db <- scraping(links)
names(db) <- t(vocabulary)




# #extraer contenido
# data <- fromJSON(direction)
# phonetics <- data$phonetics
# 
# #
# hello <- phonetics[[1]]["text"]




