
df <- complete_join_no_na %>% select(uspto_us_patent_id, uspto_fp_number_for_query,uspto_fp_category, ops_kind_of_fp_coded,year_difference)

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

rel_freq_table <- data.frame(row.names = c("Patent", "Utility Model"))
rel_freq_table[1,1] <- nrow(unique(df[(df$cited_after_20& df$ops_kind_of_fp_coded=="Patent") ,2]))/nrow(unique(df[df$ops_kind_of_fp_coded=="Patent",2]))
rel_freq_table[2,1] <- nrow(unique(df[(df$cited_after_10& df$ops_kind_of_fp_coded=="Utility Model") ,2]))/nrow(unique(df[df$ops_kind_of_fp_coded=="Utility Model",2]))
rel_freq_table[1,2] <- nrow(df[(df$year_difference>20 & df$ops_kind_of_fp_coded=="Patent") ,2])/nrow(df[df$ops_kind_of_fp_coded=="Patent",2])
rel_freq_table[2,2] <- nrow(df[(df$year_difference>10 & df$ops_kind_of_fp_coded=="Utility Model") ,2])/nrow(df[df$ops_kind_of_fp_coded=="Utility Model",2])
colnames(rel_freq_table) <- c("percent_of_ip_have_a_citation_after_IP_runout","percent_citation_after_IP_runout")

rm(df)
## This creates the yearly count (This is unique patents per year!)

df_group_by_year_us <- complete_join_no_na[,c("uspto_us_patent_id","year_us_patent")]  %>% unique()  %>%  
  group_by(year_us_patent) %>% 
  summarise(count_us=n()) 

colnames(df_group_by_year_us) <- c("year_us_patent","count_us")

df_group_by_year_fp <- complete_join_no_na[,c("ops_int_pat_number","year_german_patent")] %>% unique() %>% 
  group_by(year_german_patent) %>% 
  summarise(count_fp=n())

colnames(df_group_by_year_fp) <- c("year_german_patent","count_fp")

df_group_by_year_all <- full_join(df_group_by_year_us,df_group_by_year_fp, by=c("year_us_patent"="year_german_patent"))

colnames(df_group_by_year_all)  <- c("year", "count_us", "count_fp")

df_group_by_year_all %<>% pivot_longer(cols = c("count_us","count_fp")) 

df_group_by_year_all[is.na(df_group_by_year_all$value),3]<- 0


