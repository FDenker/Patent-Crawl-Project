### Analysing data ###
library(dplyr)
library(ggplot2)
library(magrittr)
library("lmtest")
library("sandwich")
names(complete_join_no_na)

complete_join_no_na$year_difference_squared <- (complete_join_no_na$year_difference)^2 

reg_schutzrecht <- lm(ops_kind_of_fp_coded ~ year_difference + year_difference_squared  ,complete_join_no_na)
summary(reg_schutzrecht)  
reg_uspto_year <- lm(year_difference ~ ,uspto_dpma_register_join)
summary(reg_uspto_year)

reg_year <- lm(as.numeric(year_difference) ~ year_us_patent, complete_join_no_na)
summary(reg_year)

df <- complete_join_no_na %>% select(uspto_us_patent_id, uspto_fp_number_for_query,uspto_fp_category, ops_kind_of_fp_coded,year_difference)
df$patent_dummy <- 0
df[df$ops_kind_of_fp=="Patent","patent_dummy"] <- 1

df[df$uspto_fp_category=="NULL","uspto_fp_category"] <- NA

df$uspto_fp_category <- as.factor(df$uspto_fp_category)
df<- df[df$year_difference<1000,] 
View(df)


##rdd 

head(df)

reg_schutzrecht <- lm(year_difference ~ patent_dummy,df)
summary(reg_schutzrecht)

reg_schutzrecht <- lm(patent_dummy ~ year_difference + I(year_difference^2)  ,df)
summary(reg_schutzrecht) 

ggplot(data=df,  aes(year_difference, group=ops_kind_of_fp_coded, colour=ops_kind_of_fp_coded)) + 
  geom_histogram(bins = 60) +xlab("Age of german patent/utility model cited (in years)")+
  guides(fill=guide_legend(title=NULL)) + xlim(0, 25)

reg_category <- lm(year_difference ~ uspto_fp_category + times_cited,df)
summary(reg_category)

reg_category_schutzrecht <- lm(year_difference ~ uspto_fp_category, uspto_fp_category*patent_dummy + patent_dummy,df)
summary(reg_category_schutzrecht)

names(df)

df2 <- df %>%
  select(year_difference, uspto_fp_category) %>% 
  filter(uspto_fp_category == "cited by examiner" | uspto_fp_category == "cited by other")
  

lm(year_difference ~ uspto_fp_category, data = df2)
View(df2)

df %>% 
  group_by(.$ops_kind_of_fp_coded,.$uspto_fp_category) %>% 
  summarise(count=n()) %>% View()

times_cited <- df %>% 
  group_by(.$uspto_fp_number_for_query) %>% 
  summarise(count=n())
colnames(times_cited) <- c("uspto_fp_number_for_query","times_cited")
df <- left_join(df,times_cited)

reg_times_cited <- lm(year_difference ~ times_cited + ops_kind_of_fp_coded ,df)
mean(df$year_difference, na.rm = F)
summary(reg_times_cited)
head(df)
#bin_model <- glm(uspto_fp_category ~ )
ggplot(data = head(df,10000), aes(x = times_cited, y = year_difference))+
  geom_point()+
  geom_abline(intercept = 1.271e+01, slope = 0.3323, col = "blue")+
  geom_abline(intercept = 1.271e+01+-3.173e+00, slope = 0.3323, col = "red") #utility model


vector_cited_before_10<- unique(df[df$year_difference<10,2]) 
vector_cited_before_10<-  as.data.frame(vector_cited_before_10)
vector_cited_before_10$value <- TRUE
colnames(vector_cited_before_10) <- c("uspto_fp_number_for_query","cited_before_10")
df<- left_join(df, vector_cited_before_10 )
df[is.na(df$cited_before_10),]$cited_before_10 <- FALSE
rm(vector_cited_before_10)
table(df$cited_before_10, df$ops_kind_of_fp_coded)


vector_cited_before_20<- unique(df[df$year_difference<20,2]) 
vector_cited_before_20<-  as.data.frame(vector_cited_before_20)
vector_cited_before_20$value <- TRUE
colnames(vector_cited_before_20) <- c("uspto_fp_number_for_query","cited_before_20")
df<- left_join(df, vector_cited_before_20 )
df[is.na(df$cited_before_20),]$cited_before_20 <- FALSE
rm(vector_cited_before_20)

View(df)

df[(df$cited_before_10=FALSE & df$cited_before_20 = FALSE)]


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

df$cited

table(df$cited_after_10, df$ops_kind_of_fp_coded)

rel_freq_table <- data.frame(row.names = c("Patent", "Utility Model"))
colnames(rel_freq_table) <- c("percent_of_ip_have_a_citation_after_IP_runout","percent_citation_after_IP_runout")
rel_freq_table[1,1] <- length(unique(df[(df$cited_after_20& df$ops_kind_of_fp_coded=="Patent") ,2]))/length(unique(df[df$ops_kind_of_fp_coded=="Patent",2]))
rel_freq_table[2,1] <- length(unique(df[(df$cited_after_10& df$ops_kind_of_fp_coded=="Utility Model") ,2]))/length(unique(df[df$ops_kind_of_fp_coded=="Utility Model",2]))
rel_freq_table[1,2] <- length(df[(df$year_difference>20 & df$ops_kind_of_fp_coded=="Patent") ,2])/length(df[df$ops_kind_of_fp_coded=="Patent",2])
rel_freq_table[2,2] <- length(df[(df$year_difference>10 & df$ops_kind_of_fp_coded=="Utility Model") ,2])/length(df[df$ops_kind_of_fp_coded=="Utility Model",2])

View(rel_freq_table)

ggplot(rel_freq_table)


#

class(df$ops_kind_of_fp_coded)

df %>% 
  group_by(.$uspto_fp_number_for_query, ) %>% 
  summarise(count=n())

names(df)
prop.table(df[,c(4,10)])

View(df)
df
plot(x=df$year_difference, y= df$patent_dummy, cex=0.5, pch=15)


ggplot(data=complete_join_no_na[complete_join_no_na$uspto_fp_category %in% c("cited by applicant", "cited by examiner","cited by other"),],  aes(year_difference, group=uspto_fp_category, colour=uspto_fp_category)) + 
  geom_density() +xlab("Age of german patent/utility model cited (in years)")+ guides(colour=guide_legend(title="Type of IP")) +
  geom_abline(intercept = -10, colour = "10 Years", show.legend = TRUE) +geom_abline(intercept = -20 , show.legend = T) + xlim(-10, 50)

df_group_by_year_us <- complete_join_no_na %>% select("uspto_us_patent_id","year_us_patent") %>% unique() %>% 
  group_by(.$year_us_patent, ) %>% 
  summarise(count_us=n()) 

df_group_by_year_fp <- complete_join_no_na %>% select("ops_int_pat_number","year_german_patent") %>% unique() %>% 
  group_by(.$year_german_patent, ) %>% 
  summarise(count_fp=n())

df_group_by_year_all <- full_join(df_group_by_year_us,df_group_by_year_fp, by=c(".$year_us_patent"=".$year_german_patent"))

colnames(df_group_by_year_all)  <- c("year", "count_us", "count_fp")

df_group_by_year_all %<>% pivot_longer(cols = c("count_us","count_fp")) 

df_group_by_year_all[is.na(df_group_by_year_all$value),3]<- 0

ggplot(data=df_group_by_year_all, )+ geom_line(aes(x=year, y=value, group=name, color = name)) + xlim(1920,2020)
plot(df_group_by_year_all)

df_group_by_year_fp <- complete_join_no_na %>% select("ops_int_pat_number","year_german_patent","ops_kind_of_fp_coded") %>% unique() %>% 
  group_by(.$ops_int_pat_number,.$ops_kind_of_fp_coded ) %>% 
  summarise(Mean = mean(year_german_patent, na.rm=TRUE)) %>% View()



names(complete_join_no_na)
head(complete_join_no_na)


reg_type_of_ip <- lm(year_difference ~ ops_kind_of_fp_coded,complete_join_no_na[!is.na(complete_join_no_na$uspto_fp_category),])
### 
plot(reg_type_of_ip)

bptest(reg_type_of_ip, ~ ops_kind_of_fp_coded, data = complete_join_no_na )
##We have heteroscedasticity
coeftest(reg_type_of_ip, vcov = vcovHC(reg_type_of_ip, type = "HC0"))

reg_category_type_of_ip <- lm(year_difference ~ uspto_fp_category+ uspto_fp_category*ops_kind_of_fp_coded + ops_kind_of_fp_coded,complete_join_no_na)
summary(reg_category_type_of_ip)
reg_category_type_of_ip_year <- lm(year_difference ~ uspto_fp_category+ uspto_fp_category*ops_kind_of_fp_coded + ops_kind_of_fp_coded+ german_patent_year_adapted,complete_join_no_na)
summary(reg_category_type_of_ip_year)

complete_join_no_na$german_patent_year_adapted <- complete_join_no_na$year_german_patent - 1990 

mean(complete_join_no_na$year_german_patent)

anova(reg_type_of_ip, reg_category_type_of_ip)

reg_type_of_ip_ln <- lm(log(year_difference) ~ ops_kind_of_fp_coded,complete_join_no_na[complete_join_no_na$year_difference>0,])


summary(reg_type_of_ip_ln)
complete_join_no_na[is.na(complete_join_no_na$year_difference),]
bptest(reg_type_of_ip_ln, ~ ops_kind_of_fp_coded, data = complete_join_no_na[complete_join_no_na$year_difference>0,] )

waldtest(reg_type_of_ip, reg_category_type_of_ip, vcov = vcovHC(reg_category_type_of_ip, type = "HC0"))
# heteroscedasticity even after level

lm_year <- lm(year_difference ~ year_german_patent, complete_join_no_na)
summary(lm_year)



times_cited <- complete_join_no_na %>% 
  group_by(uspto_fp_number_for_query) %>% 
  summarise(count=n())
colnames(times_cited) <- c("uspto_fp_number_for_query","times_cited")
complete_join_no_na <- left_join(complete_join_no_na,times_cited)

full_ops_query_data_no_na<- full_ops_query_data[full_ops_query_data$valid_date==TRUE,]
head(full_ops_query_data$ops_date)

full_ops_query_data$date_wo_year <- ""
View(full_ops_query_data)
full_ops_query_data_no_na$date_wo_year <-substr(full_ops_query_data_no_na$ops_date, 6, 11)
View(full_ops_query_data_no_na)
full_ops_query_data_no_na$date_wo_year <- as.Date(full_ops_query_data_no_na$date_wo_year, format = "%m%d")

ggplot(data=full_ops_query_data_no_na,  aes(full_ops_query_data_no_na$date_wo_year, group=ops_kind_of_fp_coded, colour=ops_kind_of_fp_coded, stat="count")) + 
  geom_bar()
head(full_ops_query_data$date_wo_year)
library(stringr)

full_ops_query_data[full_ops_query_data$date=="<NA>",]

library(naniar)

full_ops_query_data$ops_date %>% replace_with_na(replace = list(x = NA))
full_ops_query_data %>% replace_with_na_all(condition = ~.x == "NA")

full_ops_query_data$valid_date <- grepl( "[:digits:]?",full_ops_query_data$ops_date)


