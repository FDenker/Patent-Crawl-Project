### Patent Crawl Project

This project is the basis for two undergraduate paper on the usage of German utility models in contrast to German patents.
There is also a [legacy repo for this project](https://github.com/FDenker/Legacy-patent-crawler) which uses rvest and RSelenium. Although the data collection precess was sucessfull I have decided to abondon the project due to potential problems using the data. 

This project builds on USPTOs ["patentsview.org"](https://www.patentsview.org/download/) and the [Open Patent Services API](https://www.epo.org/searching-for-patents/data/web-services/ops.html#tab-1) of the European Patent Office. 

The data collection process is explained in more detail in the [ASR_Appendix.Rmd](https://github.com/FDenker/Patent-Crawl-Project/blob/master/ASR_Appendix.Rmd). The resulting paper (which is still very much work in progress) can be found [here](https://github.com/FDenker/Patent-Crawl-Project/blob/master/ASR_Hausarbeit.pdf). 

It is the goal for the next few lines to give a short overview over the scripts:
[010_string_creation_for_ops.R](https://github.com/FDenker/Patent-Crawl-Project/blob/master/010_string_creation_for_ops.R) - this creates the strings which are then used to query the necessary information using the Open Patent Services API (OPS).

[011_ops_queries.R](https://github.com/FDenker/Patent-Crawl-Project/blob/master/011_ops_queries.R) - this queries the given strings and then saves them in the data folder. Due to API limitations only about 400 queries (with 80 patents) each can be queried each hour. Therefore the query process is split in a total of 172 groups which are saved as RDS files.

[012_na_ops_queries.R](https://github.com/FDenker/Patent-Crawl-Project/blob/master/012_na_ops_queries.R) - after the normal queries there are still NAs left. These are queried in this file.

[99_create_df.R](https://github.com/FDenker/Patent-Crawl-Project/blob/master/99_create_df.R) - this creates some frequency tables for patents and utility models. In addition it creates the dataframe that is used to plot the number of patent applications per year.

[99_data_joining.R](https://github.com/FDenker/Patent-Crawl-Project/blob/master/99_data_joining.R) - this script takes joins all the relevant data into one up to date dataframe which is used for the regressions and graphs

[ASR_Hausarbeit.Rmd](https://github.com/FDenker/Patent-Crawl-Project/blob/master/ASR_Hausarbeit.Rmd) - this script is the working version of the paper.

## Test-scripts (these are not used for the paper itsself but for testing)

[99_data_analysis.R](https://github.com/FDenker/Patent-Crawl-Project/blob/master/99_data_analysis.R)

[99_plotting_data.R](https://github.com/FDenker/Patent-Crawl-Project/blob/master/99_plotting_data.R)

[99_time_series.R)](https://github.com/FDenker/Patent-Crawl-Project/blob/master/99_time_series.R)


