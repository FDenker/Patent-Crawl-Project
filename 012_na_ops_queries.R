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
source("99_data_joining.R")
require(rlist)
full_ops_query_data <- read_ops_query_files()

## This function creates a string of for the patents that were not found in the first OPS search

create_ops_strings_for_na_query <- function(){
  vector_of_ops_na <- full_ops_query_data[is.na(full_ops_query_data$ops_int_pat_number),1]
  vector_of_ops_na$valid_pat_id <- gsub("U[0-9]?","",vector_of_ops_na$ops_country)
  #vector_of_ops_na$valid_pat_id <- gsub("[A-Z][0-9]?","",vector_of_ops_na$valid_pat_id)
  vector_of_ops_na$valid_pat_id <- gsub("\\[0-9]*","",vector_of_ops_na$valid_pat_id)
  
  
  ## check which of the NAs is already in the dataset
  
  vector_of_ops_na$ops_int_pat_number <- gsub("[A-Z]","", vector_of_ops_na$valid_pat_id)
  
  ## Only keep those which have not been already queried
  
  vector_of_ops_na <- anti_join(vector_of_ops_na, full_ops_query_data, by = c("ops_int_pat_number"="ops_int_pat_number"))
  ##writing all the patent ids into strings for depatisnet crawler###
  string_for_lookup_pat_id <- c("")
  for(i in 1:ceiling(nrow(vector_of_ops_na)/80)){
    if(i!=ceiling(nrow(vector_of_ops_na)/80)){
      string_for_lookup_pat_id[i] <- toString(vector_of_ops_na$valid_pat_id[((i-1)*80):(i*80)] ) 
    }else{
      string_for_lookup_pat_id[i] <- toString(vector_of_ops_na$valid_pat_id[((i-1)*80):nrow(vector_of_ops_na)])
    }
    
  }
  return(string_for_lookup_pat_id)
}


## This function creates a string of all the remaining patents to query

create_ops_strings_for_na_query <- function(){
  vector_of_ops_na <- anti_join(uspat_join,full_ops_query_data, by = c("uspto_fp_number_f_joining" = "ops_int_pat_number"))
  vector_of_ops_na$uspat_valid_pat_id_format_2 <- grepl( "DE[:digit:]{5,8}[:upper:]?[:digit:]?", vector_of_ops_na$uspto_fp_number_for_query)
  
  grepl("DE[0-9]{5,8}", "DE123466")
  
  vector_of_ops_na[vector_of_ops_na$uspto_fp_number_for_query]
  
  vector_of_ops_na[vector_of_ops_na$uspat_valid_pat_id_format=TRUE,]
  
  vector_of_ops_na$valid_pat_id <- gsub("U[0-9]?","",vector_of_ops_na$ops_country)
  #vector_of_ops_na$valid_pat_id <- gsub("[A-Z][0-9]?","",vector_of_ops_na$valid_pat_id)
  vector_of_ops_na$valid_pat_id <- gsub("\\[0-9]*","",vector_of_ops_na$valid_pat_id)
  
  
  ## check which of the NAs is already in the dataset
  
  vector_of_ops_na$ops_int_pat_number <- gsub("[A-Z]","", vector_of_ops_na$valid_pat_id)
  
  ## Only keep those which have not been already queried
  
  vector_of_ops_na <- anti_join(vector_of_ops_na, full_ops_query_data, by = c("ops_int_pat_number"="ops_int_pat_number"))
  ##writing all the patent ids into strings for depatisnet crawler###
  string_for_lookup_pat_id <- c("")
  for(i in 1:ceiling(nrow(vector_of_ops_na)/80)){
    if(i!=ceiling(nrow(vector_of_ops_na)/80)){
      string_for_lookup_pat_id[i] <- toString(vector_of_ops_na$valid_pat_id[((i-1)*80):(i*80)] ) 
    }else{
      string_for_lookup_pat_id[i] <- toString(vector_of_ops_na$valid_pat_id[((i-1)*80):nrow(vector_of_ops_na)])
    }
    
  }
  return(string_for_lookup_pat_id)
}


#ops_string_for_na_query<- create_ops_strings_for_na_query()
#View(ops_string_for_na_query)
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

ops_query_for_na <- function(string_for_ops){
  auth_codes <- readRDS("data/auth.rds")
  access_token<- auth(auth_codes[1], auth_codes[2])
  
  head_post <- c(paste("Bearer", access_token ),"application/exchange+xml", "text/plain")
  names(head_post) <- c("Authorization", "Accept", "Content-Type" )
  r_post<- POST("http://ops.epo.org/rest-services/published-data/publication/DOCDB/biblio", 
                add_headers(head_post), body = string_for_ops)
  print("string_for_ops")
  print(paste0("Per hour used: ",as.numeric(r_post$headers$`x-individualquotaperhour-used`)*1e-6))
  print(paste0("Per week used: ",as.numeric(r_post$headers$`x-registeredquotaperweek-used`)*1e-6))
  
  pat=xmlTreeParse(r_post) %>% xmlRoot() %>% .[[1]]
  
  return(pat)
}


#agg_ops_na_crawler <- function(){
  
  ### This is the table in which the data will be written
  
  ops_na_data <- setNames(data.table(matrix(nrow = 0, ncol = 4)),
                       c("country", "int_pat_number", "kind_of_patent", "date"))
  
  ### This reads the file which earlier generated all the strings for query 
  
  ops_string_for_query <- create_ops_strings_for_na_query()
  list_of_query_results <- vector(mode = "list")
  
  ## Creating start and ending points for the for-loop
  
  ops_start <- 1
  ops_end <- 400
  #ops_end <- length(ops_string_for_query) 
  
  print(paste0("Querying from ", ops_start, " to ", ops_end))
  for (i in ops_start:ops_end){
    print(i)
    n <- 1+(i-ops_start)
    tryCatch(
      print(system.time(query_results<- ops_query_for_na(ops_string_for_query[i]))),
      error = function(c) {
        print(paste0("Error message in group",i))
      },
      warning = function(c) "warning")
    
    #list_of_query_results[[n]] <- xmlToList(query_results)
    print(n)
    
    tryCatch(
      {df <- xmlSApply(query_results, function(x) {
        test <- x[["bibliographic-data"]][["publication-reference"]][["document-id"]]
        xmlSApply(test, xmlValue)})
      df <- t(df)}
      ,
      error = function(c) {
        print(paste0("Error message in group",i))
      },
      warning = function(c) "warning")
    
    #df <- xmlSApply(query_results, function(x) {
      #test <- x[["bibliographic-data"]][["publication-reference"]][["document-id"]]
     # xmlSApply(test, xmlValue)})
    n.obs <- sapply(df, length)
    seq.max <- seq_len(max(n.obs))
    new_data <- t(sapply(df, "[", i = seq.max))
    colnames(new_data) <-  c("country", "int_pat_number", "kind_of_patent", "date")
    ops_na_data<- rbind(ops_na_data,new_data, fill=TRUE)
    
  }
  #list.save(list_of_query_results, file =paste0("data/ops_files/na_list_of_results_group_",ops_start,"__",ops_end,".rds") )
  saveRDS(ops_na_data,file=paste0("data/ops_files/ops_na_data.rds"))
  return(ops_na_data)
}
#

agg_ops_na_crawler_2 <- function(group){
  print(group)
  ### This is the table in which the data will be written
  
  ops_data <- setNames(data.table(matrix(nrow = 0, ncol = 4)),
                       c("country", "int_pat_number", "kind_of_patent", "date"))
  
  ### This reads the file which earlier generated all the strings for query 
  
  ops_string_for_query <- ops_string_for_na_query
  
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
    print(paste0(i, "th element in the string"))
    n <- 1+(i-ops_start)
    tryCatch(
      print(system.time(query_results<- ops_query_for_na(ops_string_for_query[i]))),
      error = function(c) {
        print("Error message")
        Sys.sleep(10)
        system.time(query_results<- ops_query_for_na(ops_string_for_query[i]))
      },
      warning = function(c) "warning")
    tryCatch({
      list_of_query_results[[n]] <- xmlToList(query_results)
      print(n)
      df <- xmlSApply(query_results, function(x) {
        test <- x[["bibliographic-data"]][["publication-reference"]][["document-id"]]
        xmlSApply(test, xmlValue)})
      df <- t(df)
      
      n.obs <- sapply(df, length)
      seq.max <- seq_len(max(n.obs))
      new_data <- t(sapply(df, "[", i = seq.max))
    }
      ,
      error = function(c) {
        print("Error message")
        
        new_data <- setNames(data.table(matrix(nrow = 0, ncol = 4)),
                             c("country", "int_pat_number", "kind_of_patent", "date"))
        new_data[1,] <-c("ERROR",i,i,i)
      },
      warning = function(c) "warning")
    
    
    
    colnames(new_data) <-  c("country", "int_pat_number", "kind_of_patent", "date")
    ops_data<- rbind(ops_data,new_data, fill=TRUE)
    print(head(new_data))
    rm(new_data)
  }
  list.save(list_of_query_results, file =paste0("data/ops_files/list_of_results_group_na_",group,".rds") )
  #saveRDS(list_of_query_results, file = paste0("data/ops_files/list_of_results_group_",group,".rds"))
  saveRDS(ops_data,file=paste0("data/ops_files/ops_data_group_na_",group,".rds"))
  return(ops_data)
}



test <- agg_ops_na_crawler_2(18)

# 1 | 91 | is ambiguous
# 7 / 2 probably too

#test<- anti_join(vector_of_ops_na, full_ops_query_data, by = c("ops_int_pat_number"="ops_int_pat_number"))

#head(full_ops_query_data, 500)
