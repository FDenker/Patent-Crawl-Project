library(readr)
library(tidyverse)
library(dplyr)
library(data.table)

geoc_app <- read_delim(file = "D:/Downloads/geoc_app.txt", delim = ",", col_names = TRUE)
geoc_inv <- read_delim(file = "D:/Downloads/geoc_inv.txt", delim = ",", col_names = TRUE)
epo_ipc <- read_delim(file = "D:/Downloads/202001_EPO_IPC.txt", delim = "|", col_names = TRUE)

geoc_inv <- geoc_inv[,1:3]

head(geoc_inv)
#head(geoc_app)
#ger_geoc_app <- geoc_app[geoc_app$ctry_code=="DE",]
#saveRDS(ger_geoc_app, "data/ger_geoc_app.rds")
#file2 <- read_delim(file = "E:/Frederics Dateien/first_and_subsequent_filings_2.txt", delim = ",", col_names = TRUE, col_types = "cdcccc")
#ger_filing_2 <- file2[file2$publn_auth=="DE",]
#c_ger_filing <-rbind(ger_filing_1,ger_filing_2)
#saveRDS(c_ger_filing, "data/c_ger_filing.rds")

c_ger_filing <- readRDS("data/c_ger_filing.rds")
file$appln_id <- as.character(file$appln_id)
names(c_ger_filing)
preliminary_filing_pat <- inner_join(uspat_foreigncitations_ger, c_ger_filing , by=c("uspto_fp_number_f_joining"="publn_nr"))
names(preliminary_filing_pat)
preliminary_filing_pat<- preliminary_filing_pat[,c(2,10,11)]

geoc_inv$appln_id <- as.character(geoc_inv$appln_id)

anti_join_geoc_inv <- anti_join(preliminary_filing_pat, geoc_inv)
length(unique(anti_join_geoc_inv$uspto_fp_number_f_joining))

geoc_app$appln_id <- as.character(geoc_app$appln_id)
epo_ipc$appln_id <- as.character(epo_ipc$appln_id)
anti_join_2 <- anti_join(anti_join_geoc_inv,epo_ipc) 
length(unique(anti_join_2$uspto_fp_number_f_joining))
head(anti_join_geoc_inv)

semi_geoc_app <- semi_join(preliminary_filing_pat,geoc_app)
length(unique(semi_geoc_app$uspto_fp_number_f_joining))

ops_data <- read_ops_query_files()
names(ops_data)
names(semi_geoc_app)

ops_semi_join <- anti_join(anti_join_2, ops_data, by= c("uspto_fp_number_f_joining"="ops_int_pat_number"))


length(unique(ops_semi_join$uspto_fp_number_f_joining))
length(unique(ops_data$ops_int_pat_number))
ops_data[is.na(ops_data$ops_int_pat_number)]

writeClipboard(toString(anti_join_geoc_inv$appln_id[1:600]))

 readClipboard() %>% nchar()

View(ops_data)
#
#ger_geoc_app<-readRDS("data/ger_geoc_app.rds")
#c_ger_filing$appln_id <- as.character(c_ger_filing$appln_id) 
#ger_geoc_app$appln_id <- as.character(ger_geoc_app$appln_id)
#join_app_filing <- left_join(c_ger_filing,ger_geoc_app)
#
#saveRDS(join_app_filing, "data/join_app_filing.rds")
#

epo_app_reg <- read_delim(file = "D:/Downloads/202001_EPO_App_reg/202001_EPO_App_reg.txt", delim = "|", col_names = TRUE)
names(epo_app_reg)

join_left_epo_app <- left_join(uspat_foreigncitations_ger, epo_app_reg , by=c("uspto_fp_number_f_joining"="pub_nbr"))
unique(join_left_epo_app[is.na(join_left_epo_app$app_name),"uspto_fp_number_f_joining"])
names(geoc_app)

geoc_app$appln_id <- as.character(geoc_app$appln_id)
join_app_filing <- left_join(c_ger_filing, geoc_app)


uspat_foreigncitations_ger <- readRDS("C:/Users/frede/OneDrive - Zeppelin-University gGmbH/Dokumente/Semester 7/us_ger_patent_scraping/data/uspat_foreigncitations_ger.rds")

head(join_app_filing, 100000)
nrow(join_app_filing[is.na(join_app_filing$filing_date),])

-nrow(join_app_filing)
join_app_filing <- left_join(c_ger_filing,ger_geoc_app)

join_left_full <- left_join(uspat_foreigncitations_ger, join_app_filing , by=c("uspto_fp_number_f_joining"="publn_nr"))
join_left <- left_join(uspat_foreigncitations_ger, join_app_filing , by=c("uspto_fp_number_f_joining"="publn_nr"))

full_test_join_inner <- inner_join(uspat_foreigncitations_ger, join_app_filing , by=c("uspto_fp_number_f_joining"="publn_nr"))
#HERE I AM LOOSING ABOUT 74k german patents (see through left join)
unique(join_left[is.na(join_left$filing_date),8])
head(join_left,1000) %>% view
names(join_left)
unique(join_left[!is.na(join_left$filing_date),8])
unique(join_left_full[is.na(join_left_full$appln_id),8]) # "73292 are not found" 
unique(join_left_full[!is.na(join_left_full$filing_date),8])
unique(uspat_foreigncitiations$uspto_fp_number_for_query)



names(join_left)
ger_geoc_app$appln_id <- as.character(ger_geoc_app$appln_id)
full_test_join_inner_2 <- inner_join(full_test_join_inner, ger_geoc_app , by=c("appln_id"="appln_id"))
full_test_join_left_2 <- 

ops_u  
  
norw

names(ger_geoc_app)

length(unique(full_test_join_inner$uspto_us_patent_id))

full_join_data <- read_and_join_data()
length(unique(full_join_data$uspto_fp_number_for_query))
length(unique(uspat_foreigncitiations$uspto_fp_number_for_query))

View(uspat_applications[as.Date(uspat_applications$date)>Sys.Date()])
uspat_applications[uspat_applications$id=="05/337167"]$date <- 19

length(unique(uspat_join$uspto_fp_number_for_query))
length(unique(complete_join_no_na$uspto_fp_id))

nber <- as.data.table(fread("D:/Downloads/nber.tsv/nber.tsv"))
names(nber) <- c("uuid", "uspto_us_patent_id","category_id","subcategory_id")

names(complete_join_no_na)
nber_join<-inner_join(complete_join_no_na, nber)

head(nber_join$category_id)
table(nber_join$category_id)

nber_join$category_id.f <- factor(nber_join$category_id)
is.factor(nber_join$category_id.f )

reg_nber_join <- lm(year_difference ~ category_id.f + uspto_cited_by_examiner+ uspto_cited_by_examiner*ops_kind_of_fp_coded + ops_kind_of_fp_coded+ adapted_year_uspto,nber_join)

summary(reg_nber_join)

table(nber_join$category_id, nber_join$ops_kind_of_fp_coded)
