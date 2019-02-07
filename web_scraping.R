#collect data on jobs demand and supply in Teheran metropolitan area
#from online job advertisment plateform
#Authors: Antoine Peris, Ali Sobhani. 

#parts are adapted from the webscraping tutorial at the summer school labex dynamite
#https://dynamitestaff.github.io/R-workshops/Web_data_collection/scrap_leboncoin/projet_leboncoin.html

rm(list = ls())

library(httr)
library(rvest)
library(tidyverse)
library(knitr)
library(dplyr)
library(readr)



#function to convert persian characters into integers
persian <- "\u0660\u0661\u0662\u0663\u0664\u0665\u0666\u0667\u0668\u0669\u06F0\u06F1\u06F2\u06F3\u06F4\u06F5\u06F6\u06F7\u06F8\u06F9"
english <- "01234567890123456789"
persian.tonumber <- function(s) as.numeric(chartr(persian,english,s))



#function that get one page sending info to act like a normal browser 
go_GET <- function(url){
  result=GET(url,
             add_headers(
               "User-Agent" = ###,
               "Accept"=###,
               "Accept-Language"=###))
  return(result)
}


#get the total number of pages to scrape
url_query <- "https://www.sheypoor.com/%D8%A7%D8%B3%D8%AA%D8%A7%D9%86-%D8%AA%D9%87%D8%B1%D8%A7%D9%86/%D8%A7%D8%B3%D8%AA%D8%AE%D8%AF%D8%A7%D9%85"
url_query_raw <- go_GET(url_query)
html_query <- read_html(url_query_raw)

nb_pages <- html_query  %>% 
  html_nodes(".center > strong:nth-child(2)") %>%
  html_text()%>%
  str_replace(",","") %>% 
  persian.tonumber()

url_base <- "https://www.sheypoor.com/%D8%A7%D8%B3%D8%AA%D8%A7%D9%86-%D8%AA%D9%87%D8%B1%D8%A7%D9%86/%D8%A7%D8%B3%D8%AA%D8%AE%D8%AF%D8%A7%D9%85?p="
url_suffix <- "&f=1539934533.2981"

pages=c(url_query, str_c(url_base,2:nb_pages, url_suffix))
pages <- pages[1:415]

rm(page)

#function to get the link to the different ads of a page
ads_by_page <- function(page){
  Sys.sleep(runif(1,1,5))
  my_html <- read_html(go_GET(page))
  links <- my_html %>%
    html_nodes("h2")%>%
    html_node("a")%>%
    html_attr("href")
  links <- data.frame(urls=links[!is.na(links)])
  tib <- links
  return(tib)
}  


#get the ads for the first 10 pages
tib_ads_urls <- map(pages[1:10],safely(ads_by_page)) %>%
  map("result") %>% bind_rows()



ad_info <- function(ad){
  Sys.sleep(runif(1,1,5))
  html_ad <- read_html(go_GET(ad))
  
  title <- html_ad %>% 
    html_node("#item-details > h1:nth-child(1)") %>% 
    html_text()
  
  time <- html_ad %>% 
    html_node("time") %>% 
    html_attr("datetime")
  
  location <- trimws(gsub("[\r\n]", "", html_ad %>% 
                            html_node("span.small-text") %>% 
                            html_text()), "both")
  
  job_info <- html_ad %>% 
    html_node("table.key-val:nth-child(3)")%>%
    html_nodes("td")%>%
    html_text()
  
  education <- job_info[1]
  agreement <- job_info[2]  
  
  #supply or demand
  sup_dem <- html_ad %>% 
    html_node("table.key-val:nth-child(4)")%>%
    html_nodes("td")%>%
    html_text()
  
  
  description <- html_ad %>%
    html_node(".description")%>%
    html_text()
  
  
  tib_ad=bind_cols(url=if(length(ad)==0){print("No record")}else{print(ad)},
                   title=if(length(title)==0){print("No record")}else{print(title)},
                   time=if(length(time)==0){print("No record")}else{print(time)},
                   sup_dem=if(length(sup_dem)==0){print("No record")}else{print(sup_dem)},
                   education=if(length(education)==0){print("No record")}else{print(education)},
                   agreement=if(length(agreement)==0){print("No record")}else{print(agreement)},
                   description=if(length(description)==0){print("No record")}else{print(description)},
                   location=if(length(location)==0){print("No record")}else{print(location)})
  
  
  
  return(tib_ad)
}


tmp=Sys.time()
tib_ads <- map(tib_ads_urls$urls[1:240],
               safely(ad_info)) %>% 
  map("result")%>% bind_rows()
time_for_100_ads <- Sys.time()-tmp
time_for_100_ads



write_excel_csv(tib_ads, "ads_240_test.csv")


