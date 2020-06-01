library(igraph)

df <- complete_join_no_na %>% select(uspto_us_patent_id, uspto_fp_number_for_query, ops_kind_of_fp_coded,year_difference)
df$patent_dummy <- 0
df[df$ops_kind_of_fp=="Patent","patent_dummy"] <- 1

net <- graph_from_data_frame(d=head(df,10000), directed=F) 
plot(net, edge.arrow.size=.4, edge.curved=.1)

highly_cited_patents<-delete.vertices(net,which(degree(net)<10))
plot(highly_cited_patents)

write.csv(head(df,10000), file="for_gephi.csv")

edges_df <- df %>% select(uspto_us_patent_id, uspto_fp_number_for_query)


write.csv(edges_df, file="edges.csv",row.names = F)
colnames(edges_df) <- c("source", "target")


# make nodes
nodes<-c(df$uspto_us_patent_id, df$uspto_fp_number_for_query)
nodes<-as.data.frame(nodes)
nodes<-unique(nodes)
# gephi also requires Ids and labels that are the same as the node names
nodes$Id<-nodes$nodes
nodes$Label<-nodes$nodes
write.csv(nodes, file="nodes.csv")
