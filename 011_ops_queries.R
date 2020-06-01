### First the necessary packages are loaded ###
require(dplyr)
require(readr) 
require(remotes)
require("base64")
require(xml2)
require(stringr)
require(XML)
require(data.table)
require(tidyverse)
require(httr)
require(caTools)
require(rlist)

### This function generates the accesstoken that is needed for the main query ###

auth<- function (key, secret) {
  auth_enc <- base64encode(charToRaw(paste0(key, ':', secret)))
  heads <- c(auth_enc, "application/x-www-form-urlencoded")
  names(heads) <- c("Authorization", "content-type")
  auth <- httr::POST(url = "https://ops.epo.org/3.2/auth/accesstoken", 
                     httr::add_headers(heads), body = "grant_type=client_credentials")
  print(auth$status)
  content_auth<- content(auth, encoding = "UTF-8")
  access_token <- content_auth$access_token
  return(access_token)
}

### This is the query function itsself which requires a string as an input
### It uses the token generated above to send a POST request to the server
### This xml data is then parsed and the important information is extracted
### 

ops_query <- function(string_for_ops){
  #The API codes are stored locally
  #This reads the API code and then gets an OAuth2 token
  auth_codes <- readRDS("data/auth.rds")
  access_token<- auth(auth_codes[1], auth_codes[2])
  
  #This creates the header for the POST request 
  head_post <- c(paste("Bearer", access_token ),"application/exchange+xml", "text/plain")
  names(head_post) <- c("Authorization", "Accept", "Content-Type" )
  r_post<- POST("http://ops.epo.org/rest-services/published-data/publication/epodoc/biblio", 
                add_headers(head_post), body = string_for_ops)
  
  print(paste0("Per hour used: ",as.numeric(r_post$headers$`x-individualquotaperhour-used`)*1e-6))
  print(paste0("Per week used: ",as.numeric(r_post$headers$`x-registeredquotaperweek-used`)*1e-6))
  
  pat=xmlTreeParse(r_post) %>% xmlRoot() %>% .[[1]]
  
  return(pat)
}

### This is the function that structures the queries in groups of 100
### Within each query 80 patents are queried
### This function requires that a group number is given which is then queried


agg_ops_crawler <- function(group){
  print(group)
  ### This is the table in which the data will be written
  
  ops_data <- setNames(data.table(matrix(nrow = 0, ncol = 4)),
                       c("country", "int_pat_number", "kind_of_patent", "date"))
  
  ### This reads the file which earlier generated all the strings for query 
  
  ops_string_for_query <- readRDS("data/ops_string_for_query.rds")
  
  ## List of query results
  
  list_of_query_results <- vector(mode = "list")
  
  ## Creating start and ending points for the for-loop
  
  
  ops_start <- (group*100-99)
  
  ops_end <- (group*100)
  if(ops_end > length(ops_string_for_query)){
    print("last group")
    ops_end <- length(ops_string_for_query) -1
  }
  
  print(paste0("Querying from ", ops_start, " to ", ops_end))
  for (i in ops_start:ops_end){
    print(i)
    n <- 1+(i-ops_start)
    tryCatch(
      print(system.time(query_results<- ops_query(ops_string_for_query[i]))),
      error = function(c) {
        print("Error message")
        Sys.sleep(10)
        system.time(query_results<- ops_query(ops_string_for_query[i]))
      },
      warning = function(c) "warning")
    

    list_of_query_results[[n]] <- xmlToList(query_results)
    print(n)
    df <- xmlSApply(query_results, function(x) {
      test <- x[["bibliographic-data"]][["publication-reference"]][["document-id"]]
      xmlSApply(test, xmlValue)})
    df <- t(df)

    n.obs <- sapply(df, length)
    seq.max <- seq_len(max(n.obs))
    new_data <- t(sapply(df, "[", i = seq.max))
    colnames(new_data) <-  c("country", "int_pat_number", "kind_of_patent", "date")
    ops_data<- rbind(ops_data,new_data, fill=TRUE)
    
  }
  list.save(list_of_query_results, file =paste0("data/ops_files/list_of_results_group_",group,".rds") )
  #saveRDS(list_of_query_results, file = paste0("data/ops_files/list_of_results_group_",group,".rds"))
  saveRDS(ops_data,file=paste0("data/ops_files/ops_data_group_",group,".rds"))
  return(ops_data)
}

for(gr in 228:228){
  
  access_token<- auth("yKWzUMtLlyFi7ROAuuUARlDDAwmHKsXR", "AiwSox5Fvbg5Xr0Y")
  
  head_post <- c(paste("Bearer", access_token ),"application/exchange+xml", "text/plain")
  names(head_post) <- c("Authorization", "Accept", "Content-Type" )
  r_post<- POST("http://ops.epo.org/rest-services/published-data/publication/epodoc/biblio", 
                add_headers(head_post))
  
  
  if((as.numeric(r_post$headers$`x-individualquotaperhour-used`)*1e-6<300)&
     (as.numeric(r_post$headers$`x-registeredquotaperweek-used`)*1e-6<3900)){
    test <- agg_ops_crawler(gr)
  } else {
    Sys.sleep(60*50)
    access_token<- auth("yKWzUMtLlyFi7ROAuuUARlDDAwmHKsXR", "AiwSox5Fvbg5Xr0Y")
    
    head_post <- c(paste("Bearer", access_token ),"application/exchange+xml", "text/plain")
    names(head_post) <- c("Authorization", "Accept", "Content-Type" )
    r_post<- POST("http://ops.epo.org/rest-services/published-data/publication/epodoc/biblio", 
                  add_headers(head_post))
    if((as.numeric(r_post$headers$`x-individualquotaperhour-used`)*1e-6<300)&
       (as.numeric(r_post$headers$`x-registeredquotaperweek-used`)*1e-6<3900)){
      test <- agg_ops_crawler(gr)
    }else 
    {print("too much data used")
      print(paste0("Per week used: ",as.numeric(r_post$headers$`x-individualquotaperhour-used`)*1e-6))
      print(paste0("Per week used: ",as.numeric(r_post$headers$`x-registeredquotaperweek-used`)*1e-6))
      }
    }
  }

  
  



