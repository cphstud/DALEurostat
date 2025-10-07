library(restatapi)
library(ggplot2)
library(sf)
library(eurostat)
library(lubridate)
library(dplyr)
library(stringr)

getCtrDF <- function(ctrcode, dfTot) {
    p0=sprintf("%s$",ctrcode)
    p1=sprintf("%s[0-9A-Z]{1}$",ctrcode)
    p2=sprintf("%s[0-9A-Z]{2}$",ctrcode)
    p3=sprintf("%s[0-9A-Z]{3}$",ctrcode)
  dfn3ctr=dfTot %>% filter(str_detect(geo,ctrcode)) %>% 
    filter(str_detect(age,"-")) %>% select(-unit) %>% 
    mutate(nut0=str_detect(geo, p0), 
           nut1=str_detect(geo, p1),
           nut2=str_detect(geo, p2),
           nut3=str_detect(geo, p3)
    )
    return(dfn3ctr)
}

## RETRIEVE DATA
# all tables at eurostat
alltabs <- get_eurostat_toc()


myFilter <- function(search) {
  res=alltabs %>% filter(str_detect(title,search))
  res=res %>% select(title,code, type)
  return(res)
}

myFilter("birth") %>% filter(str_detect(code,"demo_r"))
metaTest <- get_eurostat_dsd("teibs040")



allb <- alltabs %>% 
  filter(str_detect(title,"birth")) %>% 
  filter(str_detect(title, "mothe"))

# allbirth <- search_eurostat_toc("birth")
# hente metadata
metabirth <- get_eurostat_dsd("demo_r_fagec3")
metabirth0 <- get_eurostat_dsd("demo_r_fagec")
unique(metabirth0[metabirth0$concept=="geo",])


# Get labels and sizes of NUTS3
#ncodelist=get_eurostat_codelist(lang = "de")
nn=get_eurostat_data("reg_area3", label = T)
nnLandLabels=nn %>% filter(time=="2025")
nnNoLabs=get_eurostat_data("reg_area3", label = F)
summary(nnNoLabs$time)
nnNoLabs2=nnNoLabs %>% filter(time=="2025")
nnnuts=unique(nn$geo)

# find variabler og value-ranges mhp filtrering
variables=unique(metabirth$concept)
valgeo=unique(metabirth[metabirth$concept=="geo", ])
valage=unique(metabirth[metabirth$concept=="age", ])
valfreq=unique(metabirth[metabirth$concept=="freq", ])

# hvilke lande?
ctr=valgeo %>% filter(str_detect(code, "^[A-Z]{2}$"))

# filtrering fra hos eurostat

dtN<-get_eurostat_data("demo_r_fagec3",
                       filters=c("^DE.*","^HU.*","^ES.*","^IT.*")
                      )                         
saveRDS(dt,"youngmoms.rds")
dd=as.data.frame(table(dt$geo))
unique(dt$geo)
unique(dt$time)
clean_restatapi_cache()

# just to be safe
#saveRDS(dt,"drfagec3.rds")
#dfn3=readRDS("drfagec3.rds")

dfn3=dt
dfn3$geo
unique(dtN$geo)
unique(dfn3$age)

#base R
dfn3DE=dfn3[grepl("DE*",dfn3$geo),]
dfn3DE=dfn3DE[grepl("^Y1[0-9]{1}-",dfn3DE$age),]
dfn3DE$nut0=grepl("^DE$",dfn3DE$geo)
dfn3DE$nut1=grepl("DE[0-9A-Z]{1}$",dfn3DE$geo) 
dfn3DE$nut2=grepl("DE[0-9A-Z]{2}$",dfn3DE$geo) 
dfn3DE$nut3=grepl("DE[0-9A-Z]{3}$",dfn3DE$geo) 

#### SPAIN ####
# base filter
dfn3ESN=dtN[grepl("ES*",dtN$geo),]
dfn3ES=dfn3[grepl("ES*",dfn3$geo),]
dfn3ES=dfn3ES[grepl("^Y1[0-9]{1}-",dfn3ES$age),]
dfn3ES$nut0=grepl("^ES$",dfn3ES$geo)
dfn3ES$nut1=grepl("ES[0-9A-Z]{1}$",dfn3ES$geo) 
dfn3ES$nut2=grepl("ES[0-9A-Z]{2}$",dfn3ES$geo) 
dfn3ES$nut3=grepl("ES[0-9A-Z]{3}$",dfn3ES$geo) 
# now plot the regions at different levels - 0 is total germany
dfn0=dfn3ES %>% filter(nut0)
dfn1=dfn3ES %>% filter(nut1)
dfn2=dfn3ES %>% filter(nut2)
dfn3=dfn3ES %>% filter(nut3)
#narrower
dfn0sub <- dfn0 %>% select(-c(nut0,nut1,nut2,nut3,unit)) 
dfn1sub <- dfn1 %>% select(-c(nut0,nut1,nut2,nut3,unit))
dfn2sub <- dfn2 %>% select(-c(nut0,nut1,nut2,nut3,unit))
dfn3sub <- dfn3 %>% select(-c(nut0,nut1,nut2,nut3,unit))
# now aggregate level 0
dfnAgg0 = dfn0sub %>%  group_by(geo,time) %>% summarize(values=sum(values)) %>% ungroup()
dfnAgg0 = aggregate(data=dfn0sub,values ~ geo+time,FUN=sum) 
dfnAgg1 = aggregate(data=dfn1sub,values ~ geo+time,FUN=sum) 
dfnAgg2 = aggregate(data=dfn2sub,values ~ geo+time,FUN=sum) 
dfnAgg3 = aggregate(data=dfn3sub,values ~ geo+time,FUN=sum) 
# get rid of factors
dfnAgg0$time=as.integer(as.character(dfnAgg0$time))
dfnAgg1$time=as.integer(as.character(dfnAgg1$time))
dfnAgg2$time=as.integer(as.character(dfnAgg2$time))
dfnAgg3$time=as.integer(as.character(dfnAgg3$time))
# get geo for names and map-plot
md=get_eurostat_geospatial(nuts_level = 1) 
mdES1=get_eurostat_geospatial(nuts_level = 1) %>% filter(CNTR_CODE=="ES")
mdES2=get_eurostat_geospatial(nuts_level = 2) %>% filter(CNTR_CODE=="ES")
mdES3=get_eurostat_geospatial(nuts_level = 3) %>% filter(CNTR_CODE=="ES")
# merge geo-inf onto dataframe
nmap_data1 <- merge(mdES1,dfnAgg1,by="geo", all.x=T)
nmap_data2 <- merge(mdES2,dfnAgg2,by="geo", all.x=T)
nmap_data3 <- merge(mdES3,dfnAgg3,by="geo", all.x=T)
# now plot as line
ggplot(dfnAgg0, aes(x=time, y=values))+geom_line(color="blue")
ggplot(nmap_data1, aes(x=time, y=values))+geom_line(data=nmap_data1,aes(color=NUTS_NAME))+geom_point()
ggplot(nmap_data2, aes(x=time, y=values))+geom_line(data=nmap_data2,aes(color=NUTS_NAME))+geom_point()
ggplot(nmap_data3, aes(x=time, y=values))+geom_line(data=nmap_data3,aes(color=geo))+geom_point()
# now plot as map
ggplot(data=nmap_data3)+
  geom_sf(aes(fill=values)) +
  ggtitle("NUTS 3 Regions in Europe") +
  theme_minimal()+
  scale_fill_distiller(palette = "Spectral", direction = 1, na.value = "white")+
  facet_wrap(~time)
ggplot(data=nmap_data2)+
  geom_sf(aes(fill=values)) +
  ggtitle("NUTS 2 Regions in Europe") +
  theme_minimal()+
  scale_fill_distiller(palette = "Spectral", direction = 1, na.value = "white")+
  facet_wrap(~time)
ggplot(data=nmap_data1)+
  geom_sf(aes(fill=values)) +
  ggtitle("NUTS 1 Regions in Europe") +
  theme_minimal()+
  scale_fill_distiller(palette = "Spectral", direction = 1, na.value = "white")+
  facet_wrap(~time)
#### SPAIN DONE ####


#dplyr version
dfn3DE=dfn3 %>% filter(str_detect(geo,"DE")) %>% 
  filter(str_detect(age,"Y1[0-9]{1}$")) %>% 
  mutate(nut0=str_detect(geo, "DE$"), 
         nut1=str_detect(geo, "DE[0-9A-Z]{1}$"),
         nut2=str_detect(geo, "DE[0-9A-Z]{2}$"),
           nut3=str_detect(geo, "DE[0-9A-Z]{3}$")
         )


str(dfn3DE)
unique(dfn3DE$age)

# now plot the regions at different levels - 0 is total germany
dfn0=dfn3DE %>% filter(nut0)
dfn1=dfn3DE %>% filter(nut1)
dfn2=dfn3DE %>% filter(nut2)
dfn3=dfn3DE %>% filter(nut3)

#narrower
dfn0sub <- dfn0 %>% select(-c(nut0,nut1,nut2,nut3,unit)) 
dfn1sub <- dfn1 %>% select(-c(nut0,nut1,nut2,nut3,unit))
dfn2sub <- dfn2 %>% select(-c(nut0,nut1,nut2,nut3,unit))
dfn3sub <- dfn3 %>% select(-c(nut0,nut1,nut2,nut3,unit))

unique(dfn0sub$geo)
unique(dfn1sub$geo)
unique(dfn2sub$geo)
unique(dfn3sub$geo)

# now aggregate level 0
dfnAgg0 = dfn0sub %>%  group_by(geo,time) %>% summarize(values=sum(values)) %>% ungroup()
dfnAgg0 = aggregate(data=dfn0sub,values ~ geo+time,FUN=sum) 
dfnAgg1 = aggregate(data=dfn1sub,values ~ geo+time,FUN=sum) 
dfnAgg2 = aggregate(data=dfn2sub,values ~ geo+time,FUN=sum) 
dfnAgg3 = aggregate(data=dfn3sub,values ~ geo+time,FUN=sum) 
str(dfnAgg0)
str(dfnAgg3)

# get rid of factors
dfnAgg0$time=as.integer(as.character(dfnAgg0$time))
dfnAgg1$time=as.integer(as.character(dfnAgg1$time))
dfnAgg2$time=as.integer(as.character(dfnAgg2$time))
dfnAgg3$time=as.integer(as.character(dfnAgg3$time))

# get geo for names and map-plot
md=get_eurostat_geospatial(nuts_level = 1) 
mdDE1=get_eurostat_geospatial(nuts_level = 1) %>% filter(CNTR_CODE=="DE")
mdDE2=get_eurostat_geospatial(nuts_level = 2) %>% filter(CNTR_CODE=="DE")
mdDE3=get_eurostat_geospatial(nuts_level = 3) %>% filter(CNTR_CODE=="DE")

# merge geo-inf onto dataframe
nmap_data1 <- merge(mdDE1,dfnAgg1,by="geo", all.x=T)
nmap_data2 <- merge(mdDE2,dfnAgg2,by="geo", all.x=T)
nmap_data3 <- merge(mdDE3,dfnAgg3,by="geo", all.x=T)

# now plot as line
ggplot(dfnAgg0, aes(x=time, y=values))+geom_line(color="blue")
ggplot(nmap_data1, aes(x=time, y=values))+geom_line(data=nmap_data1,aes(color=NUTS_NAME))+geom_point()
ggplot(nmap_data2, aes(x=time, y=values))+geom_line(data=nmap_data2,aes(color=NUTS_NAME))+geom_point()
ggplot(nmap_data3, aes(x=time, y=values))+geom_line(data=nmap_data3,aes(color=geo))+geom_point()

# now plot as map
ggplot(data=nmap_data3)+
  geom_sf(aes(fill=values)) +
  ggtitle("NUTS 3 Regions in Europe") +
  theme_minimal()+
  scale_fill_distiller(palette = "Spectral", direction = 1, na.value = "white")+
  facet_wrap(~time)

ggplot(data=nmap_data2)+
  geom_sf(aes(fill=values)) +
  ggtitle("NUTS 2 Regions in Europe") +
  theme_minimal()+
  scale_fill_distiller(palette = "Spectral", direction = 1, na.value = "white")+
  facet_wrap(~time)

ggplot(data=nmap_data1)+
  geom_sf(aes(fill=values)) +
  ggtitle("NUTS 1 Regions in Europe") +
  theme_minimal()+
  scale_fill_distiller(palette = "Spectral", direction = 1, na.value = "white")+
  facet_wrap(~time)


