##Reading the full ops files and extracting additional information
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


View(list_of_results_group_99)

list_of_query_results[[1]] <- xmlToList(list_of_results_group_99)

df <- xmlSApply(list_of_results_group_99[[1]], function(x) {
  test <- x[["bibliographic-data"]][["publication-reference"]][["document-id"]]
  xmlSApply(test, xmlValue)})
df <- t(df)

View(list_of_results_group_99[[1]][[15]])

n.obs <- sapply(df, length)
seq.max <- seq_len(max(n.obs))
new_data <- t(sapply(df, "[", i = seq.max))
colnames(new_data) <-  c("country", "int_pat_number", "kind_of_patent", "date")
ops_data<- rbind(ops_data,new_data, fill=TRUE)


list_of_results_group_34[[3334]] %>% View()
