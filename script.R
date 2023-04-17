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
#vocabulary <- as.data.frame(sort(t(vocabulary)))

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
  Sys.sleep(0)
  print(str_c("waiting for ", i))
}
return(store_df)
}

#Calling to scrape function
db <- scraping(links)
names(db) <- t(vocabulary)

#phonetic function
phonetic_fn <- function(database){
store_ph <- list()
for(m in 1:length(database)){
 ph <- database[[m]]$phonetic
 if(typeof(ph)=="character"){
   store_ph[[m]]<-ph
 }
 else if(typeof(ph)=="list"){
   #ph <- database[[m]]$phonetics[[1]]$text
   ph <- "Not found"
   store_ph[[m]]<-ph
 }
 
}
return(store_ph)
}

#Call phonetic function
phonetics <- phonetic_fn(db)
names(phonetics) <- t(vocabulary)

#meaning function
meaning  <- function(database){
store_mg <- list()
for(n in 1:length(database)){
  mg <- database[[n]]$meanings[[1]]$definitions[[1]]$definition
  store_mg[[n]] <- str_c(mg, "*** ")
}
  return(store_mg)
}

#call meaning function
definitions <- meaning(db)

#Audio links functions 
audio_links <- function(database){
  al <- list() 
  for (o in 1:length(database)){
    link <- database[[o]]$phonetics[[1]]$audio
    al[[o]]<- link
  }
  return(al) 
}

#Call audio links functions
links_audios <- audio_links(db)
names(links_audios) <- t(vocabulary)

#Descargar audio
# for(p in links_audios){
#   if(typeof(p)=="character" && nchar(p)>0 ){
#     print(length(p))    
#     patron <- ".*/(.*)\\.mp3$" 
#     name <- str_c(folder,sub(patron, "\\1", p),".mp3")
#     download.file(p, name)
#     
#     #print(str_c("waiting for ", sub(patron,"\\1",p))
#   }
#   Sys.sleep(4)
# }

for(p in links_audios){
  if(typeof(p)=="character" ){
    print(length(p))
    folder <- "/home/pablogd731/Music/Audios/"
    patron <- ".*/(.*)\\.mp3$" 
    name <- str_c(folder,sub(patron, "\\1", p),".mp3")
    tryCatch(download.file(p, name), 
             error = function(e) {
               message(paste("Error al descargar", p))
               return()
             })
  }
  Sys.sleep(1)
}



#Table
table_doc <- matrix(c(t(vocabulary), phonetics, definitions), ncol=3)


# #extraer contenido
# data <- fromJSON(direction)
# phonetics <- data$phonetics
# 
# #
# hello <- phonetics[[1]]["text"]




