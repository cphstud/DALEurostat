library(restatapi)
library(eurostat)
library(dplyr)
library(ggplot2)
library(sf)


## RETRIEVAL - external
allTabs <- get_eurostat_toc()


# search for variables in title
colnames(allTabs)
sel=allTabs %>% select(c(title,code)) %>% filter(str_detect(title,"birth"))  %>% 
  filter(str_detect(title,"mother")) 

# find metadata
ymMeta <- get_eurostat_dsd("demo_r_fagec3")
unique(ymMeta$concept)
ymMeta %>% filter(concept=="age") %>% select(code) %>% unique()


# get data from eurostat
# first create list
myfilter=list(
             age=c("Y10-14","Y15-19"),
             geo=c("^DE.*","DK.*","ES.*","RO.*","FI.*","HU.*") )
             

clean_restatapi_cache()
ymDf = get_eurostat_data("demo_r_fagec3",
                         filters = myfilter,
                         date_filter="2000":"2024"
                         ) 
  
# prepare data for Germany
ymDfDE = ymDf[grepl("DE.*", ymDf$geo),]
ymDfDE$nut0 = grepl("DE$", ymDfDE$geo)
ymDfDE$nut1 = grepl("DE[0-9A-Z]{1}$", ymDfDE$geo)
ymDfDE$nut2 = grepl("DE[0-9A-Z]{2}$", ymDfDE$geo)
ymDfDE$nut3 = grepl("DE[0-9A-Z]{3}$", ymDfDE$geo)
#ymDfDE %>% filter(nut3==T) %>% count()

ymDfDE0 = ymDfDE[ymDfDE$nut0==T,]
ymDfDE1 = ymDfDE[ymDfDE$nut1==T,]
ymDfDE2 = ymDfDE[ymDfDE$nut2==T,]
ymDfDE3 = ymDfDE[ymDfDE$nut3==T,]

# aggregere de to age-cats
ymDfDE0A = aggregate(ymDfDE0,values ~ time+geo, FUN=sum)
ymDfDE1A = aggregate(ymDfDE1,values ~ time+geo, FUN=sum)
ymDfDE2A = aggregate(ymDfDE2,values ~ time+geo, FUN=sum)
ymDfDE3A = aggregate(ymDfDE3,values ~ time+geo, FUN=sum)

# remov factor
ymDfDE0A$time=as.integer(as.character(ymDfDE0A$time))
ymDfDE1A$time=as.integer(as.character(ymDfDE1A$time))
ymDfDE2A$time=as.integer(as.character(ymDfDE2A$time))
str(ymDfDE0A)
str(ymDfDE1A)

# get nuts-names
nl1=get_eurostat_geospatial(nuts_level = 1)
nl2=get_eurostat_geospatial(nuts_level = 2)
nl3=get_eurostat_geospatial(nuts_level = 3)
nl1DE=nl1[grepl("DE",nl1$CNTR_CODE),]
nl2DE=nl2[grepl("DE",nl2$CNTR_CODE),]
nl3DE=nl3[grepl("DE",nl3$CNTR_CODE),]


#merge into data
ymDfDE1AM=merge(nl1DE,ymDfDE1A, by.y = "geo",by.x = "NUTS_ID",all.y = T)
ymDfDE2AM=merge(nl2DE,ymDfDE2A, by.y = "geo", by.x = "NUTS_ID",all.y = T)
ymDfDE3AM=merge(nl3DE,ymDfDE3A, by.y = "geo", by.x = "NUTS_ID",all.y = T)

# line plot 
ggplot(ymDfDE0A,aes(x=time,y=values))+geom_line()
ggplot(ymDfDE1AM,aes(x=time,y=values))+geom_line(aes(colour = NUTS_NAME))
ggplot(ymDfDE2AM,aes(x=time,y=values))+geom_line(aes(colour = NUTS_NAME))
ggplot(ymDfDE2A,aes(x=time,y=values))+geom_line(aes(colour = geo))

# map plot 
ggplot(ymDfDE3AM) +
  geom_sf(aes(fill=values))+
  ggtitle("NUTS 2 Regions in Europe") +
  theme_minimal()+
  scale_fill_distiller(palette = "Spectral", direction = 1, na.value = "white")+
  facet_wrap(~time)
  
