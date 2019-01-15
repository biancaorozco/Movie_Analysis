# 1.0 Dependencies ----
list.of.packages <- c("rvest",
                      "httr",
                      "tidyverse", 
                      "stringr"
)

new.packages <- list.of.packages[!(list.of.packages %in%  # Check if rvest, httr, etc. are  in  installed packages
                                     installed.packages()[, "Package"])]  # list of all installed packages ("Package" column only)

if(length(new.packages)) {  # if length of new.packages > 0
  install.packages(new.packages)  # install missing packages
}


# 1.1 Libraries ----
# General-purpose data wrangling
library(tidyverse)

# Parsing of HTML/XML files
library(rvest)

# String manipulation
library(stringr)


# 1.2 Reminders ----
# Pipes %>%
# iris %>% head() %>% summary() is equivalent to summary(head(iris))


# 2.0 HTML Sources ----
# URL link to all movie scripts (one page)
all_movies_url = "https://www.simplyscripts.com/movie-screenplays.html"

# read html code to url link (<head>, <body>, etc.)
all_movies_html = read_html(all_movies_url)

# extracts exact pieces and attributes out of html gathered ^
all_attr <-
  all_movies_html %>%
  html_nodes("#movie_wide a:nth-child(1)") %>%   # "..." selects 1st link from each line (see website)
  html_attrs()

# this pulls all text from every other source
all_text <-
  all_movies_html %>%
  html_nodes("#movie_wide a:nth-child(1)") %>%
  html_text()


# 3.0 Data Frames ----
### Creating 2 data frames ###

# 3.1 All URLs Data Frame ----
# Create data frame from all attributes (2 cols and 2529 rows)
urldf = data.frame(matrix(
  unlist(all_attr),  
  nrow = length(all_text),  # Desired number of rows
  byrow = TRUE  # Matrix filled by rows
))

# Add 3rd col of movie titles to data frame
urldf$title = all_text

# Rename columns 
colnames(urldf)[1:2] = c("url", "target")

# 3.2 HTML Data Frame ----
# Create 3 col data frame of urls with ".html" and eliminates redundencies
html_scripts <- urldf[grepl(".html", urldf$url), ] #%>% unique # grepl checks for pattern match, '.html' in column 'url' within df 'urldf'

# Creates new column 'script' and fills with NA's
html_scripts$script = NA

# Saves data frame dimensions (547x4)
len_html = html_scripts %>% dim()

#all_urls = html_scripts$url %>% unique()


# 4.0 Function ----
# Created function that extracts script from each url (only those with 'pre' in html)
get_html <- function(html) {
  text = html %>%
    read_html() %>%
    html_nodes('pre') %>% 
    html_text() %>%
    stringr::str_trim() %>%  # Trim additional white space
    unlist()  # Convert list into vector
  text = paste0(text, collapse = " ")  # Concatenate vectors with single space between 
  return(text)
}


# 4.1 Function Testing ----
example = "http://www.horrorlair.com/movies/1408.html"
get_html(example) 


# 5.0 HTML Web Scraping ----
for(u in html_scripts) {  # For each html in data frame
  Sys.sleep(2)   # System pause so as not to crash their server
  text = try(get_html(html = u))   # Get script from html and save as 'text'
  if (class(text) == "try-error" | length(text) == 0) {  # If 'text' returns an error or empty, skip and continue
    next
  } else{  # assign the script to corresponding row in 'script' column
    indx = which(html_scripts$url == u)  # Defines row
    html_scripts[indx, "script"]  = text  # Save script to data frame
    print(paste("URL: ", u, "Index: ", indx[1], "Length: ", len_html[1]))  # To keep track while running
  }
}

sum(is.na(html_scripts$script)) / length(html_scripts$script)
write.csv(html_scripts, "movie_scripts_html.csv")
