#install.packages("gtools")

####reading data ####


### initial reading uspto foreign citiations ###

#uspat_foreigncitiations <- as.data.table(fread("D:/foreigncitation.tsv"))
#names(uspat_foreigncitiations) <- c("uspto_uuid","uspto_us_patent_id", "uspto_date", 
#                                    "uspto_fp_id","uspto_fp_country","uspto_fp_category",
#                                    "uspto_sequence", "uspto_fp_number_for_query",
#                                    "uspat_valid_pat_id_format")
#uspat_foreigncitiations <- uspat_foreigncitiations[uspto_fp_country=="DE"]
#saveRDS(uspat_foreigncitiations, "data/uspat_foreigncitations_ger.rds")
#
#length(unique(uspat_foreigncitiations$uspto_us_patent_id))
#
#unique_rds_patent_ids<- unique(uspat_foreigncitiations$patent_id)
#saveRDS(unique_rds_patent_ids, "data/unique_rds_patent_ids.rds")
#
#

# The following function reads all the different query files from the European patent Office.
# Firstly it reads all of the normal groups and then it reads the ones that were queried because the original query had NAs

read_ops_query_files<- function(){
  i=1
  ops_query_files <- setNames(data.table(matrix(nrow = 0, ncol = 4)),
                                    c("ops_country", "ops_int_pat_number", "ops_kind_of_fp",
                                      "ops_date"))
while ( file.exists(paste0("data/ops_files/ops_data_group_",i,".rds"))){
    database <- readRDS(paste0("data/ops_files/ops_data_group_",i,".rds"))
    names(database) <-  c("ops_country", "ops_int_pat_number", "ops_kind_of_fp",
                          "ops_date")
    ops_query_files <- rbind(ops_query_files, database, fill=TRUE)
    print(i)
    i <- i+1
    
}
  i=1
 while ( file.exists(paste0("data/ops_files/ops_data_group_na_",i,".rds"))){
    database <- readRDS(paste0("data/ops_files/ops_data_group_na_",i,".rds"))
    names(database) <-  c("ops_country", "ops_int_pat_number", "ops_kind_of_fp",
                          "ops_date")
    ops_query_files <- rbind(ops_query_files, database, fill=TRUE)
    print(i)
    i <- i+1
    
  }
  ops_query_files <- unique(ops_query_files)
  
  ops_query_files$ops_kind_of_fp_coded <- "Patent"
  ops_query_files$ops_kind_of_fp_coded[grepl("U[0-9]?",ops_query_files$ops_kind_of_fp)]<- "Utility Model"
  
  #ops_query_files$date <- as.Date(ops_query_files$date, format = "%Y%m%d")
  return(ops_query_files)
}

# Import and join

#
#if (!exists("uspat_applications")){
#  uspat_applications<- readRDS("data/uspat_applications.rds")
#}
#
#
#if (!exists("uspat_foreigncitiations")){
#  uspat_foreigncitiations<- readRDS("data/uspat_foreigncitations_ger.rds")
#}
#uspat_applications %<>%  filter(date >= as.Date("1800-01-05") & date <= as.Date("2023-01-10"))
#names(uspat_applications) <- c("uspto_application_id", "uspto_us_patent_id", "uspto_series_code", "uspto_number", "uspto_country", "uspto_app_date")
#uspat_join <- inner_join(uspat_foreigncitiations, uspat_applications, by= "uspto_us_patent_id")
#

#remove(uspat_applications)

#uspat_join$uspto_fp_number_f_joining <- gsub("U(1)?","", uspat_join$uspto_fp_number_f_joining)

#saveRDS(uspat_join, "data/uspat_join.RDS")
#uspat_join %>% select("uspto_us_patent_id","uspto_fp_id", "uspto_fp_category", 
#                      "uspto_sequence","uspto_fp_number_for_query","uspat_valid_pat_id_format",
#                      "uspto_fp_number_f_joining","uspto_application_id","uspto_app_date") %>% saveRDS(file="data/uspto_join_slim.rds")

read_and_join_data <- function(){
  require(dplyr)
  require(tidyverse)
  require(data.table)
  require(gtools)
  require(lubridate)
  require(magrittr)
uspat_join <- readRDS("data/uspto_join_slim.rds")
## here filter the ones you do not need
full_ops_query_data<-read_ops_query_files()
full_ops_query_data$ops_date <- as.Date(full_ops_query_data$ops_date, format = "%Y%m%d")
full_ops_query_data %<>% 
  group_by(.$ops_int_pat_number,.$ops_kind_of_fp_coded ) %>% 
  summarise(mean_date_german_patent = mean.Date(ops_date, na.rm=TRUE))
colnames(full_ops_query_data) <- c("ops_int_pat_number","ops_kind_of_fp_coded","mean_date_german_patent")

complete_join_no_na <- inner_join(full_ops_query_data,uspat_join, by = c("ops_int_pat_number"="uspto_fp_number_f_joining"))
complete_join_no_na$year_difference <- as.numeric(difftime(as.Date(complete_join_no_na$uspto_app_date), as.Date(complete_join_no_na$mean_date_german_patent), units = "days"))/365
complete_join_no_na <- complete_join_no_na[!is.na(complete_join_no_na$year_difference),]
complete_join_no_na <- complete_join_no_na[!complete_join_no_na$ops_kind_of_fp_coded=="",]
complete_join_no_na$year_us_patent <- year(as.Date(complete_join_no_na$uspto_app_date))
complete_join_no_na$year_german_patent <- year(as.Date(complete_join_no_na$mean_date_german_patent))

complete_join_no_na[complete_join_no_na$uspto_fp_category=="NULL","uspto_fp_category"] <- NA
complete_join_no_na$uspto_fp_category <- as.factor(complete_join_no_na$uspto_fp_category)
complete_join_no_na$uspto_cited_by_examiner <- 0
complete_join_no_na[which(complete_join_no_na$uspto_fp_category=="cited by examiner"),]$uspto_cited_by_examiner <- 1
complete_join_no_na$adapted_year_uspto <- complete_join_no_na$year_us_patent - mean(complete_join_no_na$year_us_patent)
return(complete_join_no_na)
}


