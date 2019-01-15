########## DEPENDENCIES ################
list.of.packages <- c("pdftools",
                      "rvest",
                      "httr",
                      "tidyverse", 
                      "stringr"
                      )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
########## PACKAGES ####################
require(rvest)
require(tidyverse)
library(pdftools)

########## Functions ##################

get_pdf <- function(url){
  download.file(as.character(url),
                sub('.*/', '', url)) #Download pdf
  text <- pdf_text(sub('.*/', '', url)) #Convert to text
  unlink(sub('.*/', '', url)) #Delete pdf
  text = paste(text, collapse = '') #make one string
  text = gsub("[^0-9A-Za-z///' ]"," " , text ,ignore.case = TRUE) #remove all special chars
  return(text)
}

########## WEB SCRAPING ################

all_movies_url = "https://www.simplyscripts.com/movie-screenplays.html"
all_movies_html = read_html(all_movies_url)
all_attr <- all_movies_html %>% html_nodes("#movie_wide a:nth-child(1)") %>% 
  html_attrs() 
all_text <- all_movies_html %>% html_nodes("#movie_wide a:nth-child(1)") %>% 
  html_text()  #This pulls all text but every other one is the source


urldf = data.frame(matrix(unlist(all_attr), nrow= length(all_text), byrow=T))
urldf$title = all_text
colnames(urldf)[1:2] = c("url", "target")

pdf_scripts <-  urldf %>%  
    filter(grepl(".pdf", url))
pdf_scripts$script = NA
for(u in pdf_scripts$url){
  example = try( get_pdf(url = u) )
  if(class(example) == "try-error"){
    next
  }else{
    pdf_scripts [which(pdf_scripts$url == u),"script"]  = example
  }
}
sum(is.na(pdf_scripts$script))
write.csv(pdf_scripts, "movie_scripts.csv")
