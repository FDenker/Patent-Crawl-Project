require(ggplot2)
library(plotly)
library(MASS)
library(dplyr)
library(tidyverse)
library(data.table)
library(RColorBrewer)
library(ggridges)
source("99_data_joining.R")

length(unique(complete_join_no_na$uspto_us_patent_id))
complete_join_no_na <- read_and_join_data()

length(unique(full_ops_query_data$ops_int_pat_number))
length(unique(complete_join_no_na$ops_int_pat_number))

length(unique(uspat_foreigncitiations$uspto_fp_id))
length(unique(complete_join_no_na$uspto_us_patent_id))

length(unique(full_ops_query_data$ops_int_pat_number))
length(complete_join_no_na$uspto_us_patent_id)
length(unique(uspto_query_data$usptoq_patent_number))
uspto_query_data <- read_uspto_query_files()
full_ops_query_data<-read_ops_query_files()
length(unique(full_ops_query_data$ops_int_pat_number))

full_ops_query_data<-read_ops_query_files()
#test_join <- left_join(full_ops_query_data,uspat_join, by = c("ops_int_pat_number"="uspto_fp_number_f_joining"))
test_join <- left_join(full_ops_query_data,uspat_join, by = c("ops_int_pat_number"="uspto_fp_number_f_joining"))

test_join_no_na <- test_join[!is.na(test_join$ops_int_pat_number),] 
test_join_na <- test_join[is.na(test_join$ops_int_pat_number),] 
test_join_no_na[is.na(test_join_no_na$uspto_fp_country),] -> test_join_no_na
uspat_join$uspto_fp_number_f_joining <- gsub("U(1)?","", uspat_join$uspto_fp_number_f_joining)
uspat_join$uspto_fp_number_f_joining <- gsub("u(1)?","", uspat_join$uspto_fp_number_f_joining)
test_join_no_na2 <- full_join(test_join_no_na, uspat_join, by= c("ops_int_pat_number"="uspto_fp_number_f_joining"))

View(test_join_no_na2)

test <- right_join(uspto_query_data, uspat_foreigncitiations, by = c("usptoq_patent_number"="uspto_us_patent_id"), keep=TRUE, copy=TRUE)
length(unique(test$usptoq_patent_number))
length(test$usptoq_patent_year)
unique_test <- unique(test)
names(test)
length(unique())
view(head(unique_test, 100000))
view(unique_test)

anti_join_table<- anti_join(uspat_foreigncitiations, uspto_query_data, by = c("uspto_us_patent_id"="usptoq_patent_number"))
unique(anti_join_table$uspto_us_patent_id)
names(anti_join_table)

test_dt<- as.data.table(unique_rds_patent_ids)
names(test_dt)
anti_join(anti_join_table, test_dt, by= c("uspto_us_patent_id"="unique_rds_patent_ids"))


reg_schutzrecht <- lm(as.numeric(day_difference) ~ Schutzrecht,uspto_dpma_register_join)
summary(reg_schutzrecht)  
reg_uspto_year <- lm(year_difference ~ Uspat_app_year,uspto_dpma_register_join)
summary(reg_uspto_year)

reg_year <- lm(as.numeric(year_difference) ~ patent_year, uspto_depatisnet_join)
summary(reg_year)

mean(uspto_dpma_register_join$day_difference, na.rm=T)


uspto_depatisnet_join %>%  group_by(.$patent_year) %>% 
  summarise(count=n()) %>% as.data.frame() %>% 
  ggplot(aes(.[,1],count, group=1)) + geom_path()

uspto_depatisnet_join %>%  group_by(.$application_year_fp) %>% 
  summarise(count=n()) %>% as.data.frame() %>% 
  ggplot(aes(.[,1],count, group=1)) + geom_path()

yearly_application_numbers_us<- uspto_depatisnet_join %>%  group_by(.$patent_year) %>% 
  summarise(count=n()) %>% as.data.frame()

yearly_application_numbers_de <- uspto_depatisnet_join %>% 
  group_by(.$application_year_fp) %>% 
  summarise(count=n()) %>% as.data.frame()

yearly_application_numbers_joined<- full_join(yearly_application_numbers_us, yearly_application_numbers_de, by=c(".$patent_year"=".$application_year_fp"))
  
ggplot() + geom_path(data=yearly_application_numbers_joined, aes(yearly_application_numbers_joined[,1],yearly_application_numbers_joined[,2], group=1, colour="USPTO")) +
  geom_path(data=yearly_application_numbers_joined, aes(yearly_application_numbers_joined[,1],yearly_application_numbers_joined[,3], group=1))

ggplot(data=complete_join_no_na,  aes(year_difference, group=ops_kind_of_fp_coded, colour=ops_kind_of_fp_coded)) + 
  geom_density() +xlab("Age of german patent/utility model cited (in years)")+
  guides(fill=guide_legend(title=NULL)) + geom_abline(intercept = -10, show.legend = T) +geom_abline(intercept = -20 , show.legend = T) + xlim(-5, 55)

ggplot(data=complete_join_no_na,  aes(year_difference, y=ops_kind_of_fp_coded, colour=ops_kind_of_fp_coded)) + 
  geom_density() +xlab("Age of german patent/utility model cited (in years)")+
  guides(fill=guide_legend(title=NULL)) + geom_abline(intercept = -10, show.legend = T) +geom_abline(intercept = -20 , show.legend = T) + xlim(5, 25)

ggplot(data=complete_join_no_na[complete_join_no_na$year_us_patent>2000,],  aes(x=year_difference,
                                                                                y=as.factor(year_us_patent)),
                                                                                fill=stat(x)) + 
  geom_density_ridges_gradient(quantile_lines = TRUE, quantiles = 2, scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C")+
  scale_x_continuous(expand = c(0,0))+
  labs(x = "Time difference", y =  "Years") +
  theme_minimal() + 
  theme(legend.position = "bottom",
        panel.grid = element_blank())

names(complete_join_no_na)

ggplot(data=complete_join_no_na,  aes(x=year_us_patent,y=as.factor(year_german_patent))) + 
  geom_density2d()

#### Plot 3D ####

den3d <- kde2d(complete_join_no_na$year_us_patent, complete_join_no_na$year_german_patent)
names(den3d) <- c("US Patent Year", "German Patent Year", "Relative Density")

fig <- plot_ly(x=den3d$`US Patent Year`, y=den3d$`German Patent Year`, z=den3d$`Relative Density`) %>% add_surface()
fig <- fig %>% layout(
  title = "Patent citations over time",
  scene = list(
    xaxis = list(title = "US Patent Year"),
    yaxis = list(title = "German Patent Year"),
    zaxis = list(title = "Density", type = "log")
  ))
fig



names(complete_join_)

ggplot(data=complete_join_no_na,  aes(year_difference, group=ops_kind_of_fp_coded, colour=ops_kind_of_fp_coded)) + 
  geom_histogram(binwidth = 5) +xlab("Age of german patent/utility model cited (in years)")+
  guides(fill=guide_legend(title=NULL))  + geom_vline(xintercept = 10, colour = "Blue",) +geom_vline(xintercept = 20, colour = "Red")





ggplot(data=uspto_dpma_register_join,  aes(patent_year, y=day_difference)) + geom_line() 
ggplot(data=uspto_dpma_register_join,  aes(patent_year, stat="count")) + geom_histogram(binwidth = 1)

complete_join_no_na$decade <- "0"
complete_join_no_na$decade <- round(complete_join_no_na$year_us_patent, -1)
complete_join_no_na$cluster <- "0"
complete_join_no_na$cluster <- as.numeric(cut_number(complete_join_no_na$times_cited, 3))
View(head(complete_join_no_na,5000))
complete_join_no_na[complete_join_no_na$ops_kind_of_fp_coded=="Patent","cluster_2"] <- as.numeric(cut_number(complete_join_no_na[complete_join_no_na$ops_kind_of_fp_coded=="Patent",]$times_cited, 3))
complete_join_no_na[complete_join_no_na$ops_kind_of_fp_coded=="Utility Model","cluster_2"] <- as.numeric(cut_number(complete_join_no_na[complete_join_no_na$ops_kind_of_fp_coded=="Utility Model",]$times_cited, 3))
# vor google patent nach google patent schauen wir an ob die anders patentieren

complete_join_no_na$combined_cluster <- "0"
complete_join_no_na$combined_cluster <- paste(complete_join_no_na$ops_kind_of_fp_coded ,complete_join_no_na$cluster_2)

View(head(complete_join_no_na,5000))


ggplot(data=complete_join_no_na,  aes(year_difference, group=decade, colour=decade))+ geom_density()+xlab("Age of german patent/utility model cited (in years)")+ guides(colour=guide_legend(title="Type of IP")) + xlim(-10, 50) + ylim(0,0.1)

ggplot(data=complete_join_no_na,  aes(year_difference, group=cluster, colour=cluster))+ geom_density()+xlab("Age of german patent/utility model cited (in years)")+ guides(colour=guide_legend(title="Type of IP")) + xlim(-10, 50) + ylim(0,0.1)

ggplot(data=complete_join_no_na,  aes(year_difference, group=combined_cluster, colour=combined_cluster))+ geom_density()+xlab("Age of german patent/utility model cited (in years)")+ guides(colour=guide_legend(title="Type of IP")) + xlim(-10, 50) + ylim(0,0.1)

ggplot(data=complete_join_no_na,  aes(y=year_difference,x=year_german_patent))+ geom_point()
ggplot(data=complete_join_no_na,  aes(y=year_difference,x=year_german_patent))+ geom_density_2d()
