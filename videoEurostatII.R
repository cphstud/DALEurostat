library(restatapi)
library(ggplot2)
library(sf)
library(eurostat)
library(lubridate)
library(dplyr)
library(stringr)


dfn3=get_eurostat_data("demo_r_fagec3")
dfnC2=get_eurostat_data("demo_r_fagec")
saveRDS(dfn3,"drfagec3.rds")
saveRDS(dfnC2,"drfagec2.rds")
dfn3=readRDS("drfagec3.rds")
unique(dfn3$age)

unique(dfn3de$time)
dfnC2e=dfnC2 %>% filter(str_detect(geo,"DE")) %>% 
  filter(str_detect(age,"Y1[0-9]{1}$")) %>% 
  mutate(nut0=str_detect(geo, "DE$"), 
         nut1=str_detect(geo, "DE[0-9A-Z]{1}$"),
         nut2=str_detect(geo, "DE[0-9A-Z]{1}$"),
         nut3=str_detect(geo, "DE[0-9A-Z]{3}$"))

dfn3de=dfn3 %>% filter(str_detect(geo,"DE")) %>% 
  filter(str_detect(age,"-")) %>% select(-unit) %>% 
  filter(str_detect(age,"10|11|12|13|14|15|16|17|18|19")) %>% 
  mutate(nut0=str_detect(geo, "DE$"), 
         nut1=str_detect(geo, "DE[0-9A-Z]{1}$"),
         nut2=str_detect(geo, "DE[0-9A-Z]{1}$"),
         nut3=str_detect(geo, "DE[0-9A-Z]{3}$"))

dfn0=dfn3de %>% filter(nut0)
dfn1=dfn3de %>% filter(nut1)
dfn2=dfn3de %>% filter(nut2)
dfn3=dfn3de %>% filter(nut3)


dfn3sub <- dfn3 %>% select(-c(nut0,nut1,nut2,nut3)) %>% filter(age %in% c("Y10-14","Y15-19"))
dfn2sub <- dfn2 %>% select(-c(nut0,nut1,nut2,nut3)) %>% filter(age %in% c("Y10-14","Y15-19"))
dfnC2sub <- dfn2 %>% select(-c(nut0,nut1,nut2,nut3)) %>% filter(str_detect(age,"Y[0-9]{2}$"))
dfn3sub$age = as.character(dfn3sub$age)
dfn3sub$geo = as.character(dfn3sub$geo)
dfn3sub$time = as.character(dfn3sub$time)
dfn3sub$time2 = as.Date(dfn3sub$time, "%Y")
dfn3sub$time2 = year(dfn3sub$time2)
unique(dfn3sub$age)
unique(dfn3sub$time)

dfnAgg = dfn3sub %>%  group_by(age,time2,geo) %>% summarize(less20=sum(values)) 
#dfnAgg = dfn2sub %>%  group_by(age,geo,time) %>% summarize(vals=sum(values)) 
dfnAgg = aggregate(data=dfn3sub,values ~ geo+time2,FUN=sum)
dfnAgg2 = aggregate(data=dfn2sub,values ~ geo+time,FUN=sum)
dfnAgg2$geo = as.character(dfnAgg2$geo)
dfnAgg2$time = as.character(dfnAgg2$time)

md=get_eurostat_geospatial(nuts_level = 3)
mdde=md %>% filter(str_detect(geo,"DE"))
nmap_data <- mdde %>% left_join(dfnAgg, by=c("geo"="geo"))

md2=get_eurostat_geospatial(nuts_level = 2)
md2=get_eurostat_geospatial(nuts_level = 1)
mdde2=md2 %>% filter(str_detect(geo,"DE"))
nmap_data2 <- merge(mdde2,dfnAgg2,by="geo", all.x=T)

ggplot(dfnAgg2,aes(fill = less20))+
  scale_fill_brewer(palette = "RdYlBu")+
  geom_sf(color=alpha("white",1/3),alpha= .6)

ggplot(data=nmap_data2)+
  geom_sf(aes(fill=values)) +
  ggtitle("NUTS 3 Regions in Europe") +
  theme_minimal()+
  scale_fill_distiller(palette = "Spectral", direction = 1, na.value = "white")+
  facet_wrap(~time)

