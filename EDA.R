library(ggpubr)
library(plyr)

combined_factor<-readRDS("combined_factor.rds")

#exploring how dates affect the number of downloads

#first convert the date created and date last modified into the number of days relative to 2021-01-31.
date_of_downloads<-as.Date.character("2021-01-31","%Y-%m-%d")
combined_factor$created_days<-as.numeric(difftime(date_of_downloads,combined_factor$date_created, units="days"))
combined_factor$modified_days<-as.numeric(difftime(date_of_downloads,combined_factor$date_last_mod, units="days"))


p_non_adj<-ggscatter(combined_factor, x = "created_days", y = "downloads",
          color = "red", cor.coef = TRUE, 
          cor.method = "spearman",
          xlab = "Age of Dataset", ylab = "Annual Downloads")
ggpar(p_non_adj, xlim =c (0,365))

#create a date adjusted number of downloads proportional to the percent of the year it existed for


j<-1
for(i in 1:length(combined_factor$downloads)){
  if (combined_factor$created_days[i]< 365){
  combined_factor$adj_downloads[j]<-round(365*(combined_factor$downloads[j]/combined_factor$created_days[j]),digits=0)
  } else combined_factor$adj_downloads[j]<-combined_factor$downloads[j]
  j<-j+1
}

p_adj<-ggscatter(combined_factor, x = "created_days", y = "adj_downloads",
          color = "red", cor.coef = TRUE, 
          cor.method = "spearman",
          xlab = "Age of Dataset (days)", ylab = "Adjusted Annual Downloads")
ggpar(p_adj, xlim =c (0,365))



#exploring how the collection type effects the downloads
collection_mean_non_adj<-ddply(combined_factor, .(combined_factor$collection), summarize, mean_downloads=mean(downloads))
collection_mean_adj<-ddply(combined_factor, .(combined_factor$collection), summarize, mean_downloads=mean(adj_downloads))
collection_mean<-cbind(collection_mean_adj,collection_mean_non_adj$mean_downloads)
names(collection_mean)[names(collection_mean)=="combined_factor$collection"]<-"collection"
names(collection_mean)[names(collection_mean)=="collection_mean_non_adj$mean_downloads"]<-"downloads non adjusted"
names(collection_mean)[names(collection_mean)=="mean_downloads"]<-"downloads adjusted"
collection_mean.long <- gather(collection_mean, variable,value, -collection)
names(collection_mean.long)[names(collection_mean.long)=="value"]<-"number of downloads"

collection_dl<-ggplot(data=collection_mean.long, aes(x=collection, y=`number of downloads`, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge())
collection_dl+theme(axis.text.x = element_text(angle=45, hjust=1))


#exploring how the frequency of update effects the downloads
freq_mean_non_adj<-ddply(combined_factor, .(combined_factor$freq), summarize, mean_downloads=mean(downloads))
freq_mean_adj<-ddply(combined_factor, .(combined_factor$freq), summarize, mean_downloads=mean(adj_downloads))
freq_mean<-cbind(freq_mean_adj,freq_mean_non_adj$mean_downloads)
names(freq_mean)[names(freq_mean)=="combined_factor$freq"]<-"update frequency"
names(freq_mean)[names(freq_mean)=="freq_mean_non_adj$mean_downloads"]<-"downloads non adjusted"
names(freq_mean)[names(freq_mean)=="mean_downloads"]<-"downloads adjusted"
freq_mean.long <- gather(freq_mean, variable,value, -`update frequency`)
names(freq_mean.long)[names(freq_mean.long)=="value"]<-"number of downloads"

collection_dl<-ggplot(data=freq_mean.long, aes(x=`update frequency`, y=`number of downloads`, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge())
collection_dl+theme(axis.text.x = element_text(angle=45, hjust=1))

#looking at the distribution of the downloads
summary(combined_factor$adj_downloads)
non_zero<-sum(combined_factor$adj_downloads>0)
non_zero
quant_list<-as.list(quantile(combined_factor$adj_downloads, probs = seq(0, 1, by= 0.01)))
quant_list[96]

#we are taking the 95% percentile as the definition of our 'popular' adjusted downloads. The we are adding a column to the dataframe which is binary encoded 
# with 1 for popular 0 for not popular

j<-1
for(i in 1:length(combined_factor$adj_downloads)){
  if (combined_factor$adj_downloads[j]< quant_list[96]){
    combined_factor$bin_downloads[j]<-0
  } else {
    combined_factor$bin_downloads[j]<-1
  }
  j<-j+1
}

combined_factor$bin_downloads<-as.factor(combined_factor$bin_downloads)

sum(combined_factor$bin_downloads)

saveRDS(combined_factor,"combined_factor_adj.rds")
