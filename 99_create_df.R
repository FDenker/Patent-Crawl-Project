# This selects the relevant columns
df <- complete_join_no_na %>% select(uspto_us_patent_id, uspto_fp_number_for_query,uspto_fp_category, ops_kind_of_fp_coded,year_difference, uspto_cited_by_examiner)
# This section calculates how many citations are after the 10 and 20 year mark
# For this we first create a matrix with all the relevant german patents or utility models which are cited after 10 years
vector_cited_after_10<- unique(df[df$year_difference>10,2]) 
vector_cited_after_10<-  as.data.frame(vector_cited_after_10)
vector_cited_after_10$value <- TRUE
colnames(vector_cited_after_10) <- c("uspto_fp_number_for_query","cited_after_10")
df<- left_join(df, vector_cited_after_10 )
df[is.na(df$cited_after_10),]$cited_after_10 <- FALSE

vector_cited_after_20<- unique(df[df$year_difference>20,2]) 
vector_cited_after_20<-  as.data.frame(vector_cited_after_20)
vector_cited_after_20$value <- TRUE
colnames(vector_cited_after_20) <- c("uspto_fp_number_for_query","cited_after_20")
df<- left_join(df, vector_cited_after_20 )
df[is.na(df$cited_after_20),]$cited_after_20 <- FALSE

#we then  calculate the relative frequency of a citation after the validity deadline for each of the types of IP
rel_freq_table <- data.frame(row.names = c("Patent", "Utility Model"))
rel_freq_table[1,1] <- nrow(unique(df[(df$cited_after_20& df$ops_kind_of_fp_coded=="Patent") ,2]))/nrow(unique(df[df$ops_kind_of_fp_coded=="Patent",2]))
rel_freq_table[2,1] <- nrow(unique(df[(df$cited_after_10& df$ops_kind_of_fp_coded=="Utility Model") ,2]))/nrow(unique(df[df$ops_kind_of_fp_coded=="Utility Model",2]))
rel_freq_table[1,2] <- nrow(df[(df$year_difference>20 & df$ops_kind_of_fp_coded=="Patent") ,2])/nrow(df[df$ops_kind_of_fp_coded=="Patent",2])
rel_freq_table[2,2] <- nrow(df[(df$year_difference>10 & df$ops_kind_of_fp_coded=="Utility Model") ,2])/nrow(df[df$ops_kind_of_fp_coded=="Utility Model",2])
colnames(rel_freq_table) <- c("percent_of_ip_have_a_citation_after_IP_runout","percent_citation_after_IP_runout")

rel_freq_table_examiner <- data.frame(row.names = c("Utiliy model cited by applicants side", "Utility Model cited by examiner"))
rel_freq_table_examiner[1,1] <- nrow(df[(df$year_difference>10 & df$ops_kind_of_fp_coded=="Utility Model" & df$uspto_cited_by_examiner == 0 ) ,2])/nrow(df[df$ops_kind_of_fp_coded=="Utility Model" & df$uspto_cited_by_examiner == 0,2])
rel_freq_table_examiner[2,1] <- nrow(df[(df$year_difference>10 & df$ops_kind_of_fp_coded=="Utility Model" & df$uspto_cited_by_examiner == 1 ) ,2])/nrow(df[df$ops_kind_of_fp_coded=="Utility Model" & df$uspto_cited_by_examiner == 1,2])

rm(df)
## This creates the yearly count (This is unique patents per year!)

df_group_by_year_us <- complete_join_no_na[,c("uspto_us_patent_id","year_us_patent")]  %>% unique()  %>%  
  group_by(year_us_patent) %>% 
  summarise(count_us=n()) 

colnames(df_group_by_year_us) <- c("year_us_patent","count_us")

df_group_by_year_fp <- complete_join_no_na[complete_join_no_na$ops_kind_of_fp_coded=="Patent",c("ops_int_pat_number","year_german_patent")] %>% unique() %>% 
  group_by(year_german_patent) %>% 
  summarise(count_fp=n())
colnames(df_group_by_year_fp) <- c("year_german_patent","count_fp")

df_group_by_year_fum <- complete_join_no_na[complete_join_no_na$ops_kind_of_fp_coded=="Utility Model",c("ops_int_pat_number","year_german_patent")] %>% unique() %>% 
  group_by(year_german_patent) %>% 
  summarise(count_fum=n())

colnames(df_group_by_year_fum) <- c("year_german_patent","count_fum")

df_group_by_year_german <- full_join(df_group_by_year_fum,df_group_by_year_fp, by=c("year_german_patent"="year_german_patent"))

df_group_by_year_all <- full_join(df_group_by_year_us,df_group_by_year_german, by=c("year_us_patent"="year_german_patent"))

colnames(df_group_by_year_all)  <- c("year", "US patent application", "German utility model registration", "German patent application")

df_group_by_year_all %<>% pivot_longer(cols = c("US patent application", "German patent application", "German utility model registration")) 

df_group_by_year_all[is.na(df_group_by_year_all$value),3]<- 0
