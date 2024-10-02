library(eurostat)
library(restatapi)

# total content
alltabs <- get_eurostat_toc()

# search
birth <- search_eurostat("birth")

# get table raw
allbirths <- get_eurostat_data("demo_fordagec")

# get metadata

# filtrering via eurostat
dd2 <- get_eurostat("demo_fordagec",
                    filters = list(
                      age = c("TOTAL","Y10-14","Y15-19","Y20-24","Y25-29","Y30-34","Y35-39","Y40-44","Y45-49","Y_GE50"),
                      ord_brth = c("TOTAL","1")
                    )
)
dffs2<- get_eurostat("demo_fordagec",
                     filters = list(
                       age = c("Y10-14","Y15-19","Y20-24","Y25-29","Y30-34","Y35-39","Y40-44","Y45-49","Y_GE50"),
                       ord_brth = c("1")
                     )
)

eurostat::clean_eurostat_cache()

#total numbers of births

#df_birth<-get_eurostat_data("demo_fmonth", stringsAsFactors = F)
#df_b <- df_birth[nchar(df_birth$geo)==2,]
#df_byear <- aggregate(df_b$values, by=list(year=df_b$time,geo=df_b$geo), FUN = sum)
#library(dplyr)
#library(tidyr)
#subctr
#df_p=df_byear %>% filter(geo =="DK")
#attributes(df_p)
#df_p=ungroup(df_p)
#ggplot(df_p, aes(x=year, y=x))+geom_point()
#

dd3 <- get_eurostat("demo_fordagec",
                    filters = list(
                      ord_brth = c("TOTAL")
                    )
)


#subdd3 <- dd3[dd3$time > '2007-01-01',]
#subdd3 <- dffs2[dffs2$time > '2007-01-01',]
#subdd3 <- subdd3 %>% filter(nchar(geo)==2)
#subt <- subdd3[is.na(subdd3$values),]
#dd <- get_eurostat("nama_10_gdp",
#   filters = list(
#     geo = "FI",
#     na_item = "B1GQ",
#     unit = "CLV_I10"
#   )
#)

# slanket min df
library(ggplot2)
library(dplyr)
ddsub2 <- dffs2[-c(1,2,4)]
ddsub2 <- ddsub2 %>% filter(nchar(geo)==2)
ddsub <- ddsub2
#ddsubdk <- ddsub[ddsub$geo=="DK",]
#ddsubdk2 <- ddsub2[ddsub2$geo=="DK",]
#ddsubde <- ddsub[ddsub$geo=="DE",]
#ddsubdedk <- ddsub[ddsub$geo %in% c("DE","DK"),]

#ddctr <- aggregate(ddsub$values, by = list(geo=ddsub$geo, time=ddsub$time, age=ddsub$age), FUN = sum)
#library(dplyr)
#ddctr <- aggregate(ddsub$values, by = list(geo=ddsub$geo, time=ddsub$time), FUN = sum)
#ddctrL <- ddctr %>% filter(ddctr$time == '1980-01-01')
#ddctrL <- ddsubdedk %>% filter(ddsubdedk$time == '1980-01-01')
#ddctrL <- ddsubdedk %>% filter(ddsubdedk$time == '1980-01-01')
#ddc <- ddc[order(ddc$`ddctr$time`),]
#ddc <- ddctr[order(ddctr$geo),]
#a=max(ddctr$time)
# Problemer med Tyskland

sss=ddctr %>% filter(geo=="DE") %>% mutate(r=min(time))
sss[1,'time']
a = as.numeric(format(a,"%Y"))
b = a - 13
totctr = unique(ddsub$geo)

# Liste af udvalge lande
subctr = c("SE","DK","FI","ES","RO","EL")
#subage = c("Y10-14","Y15-19","Y40-44")
subage = c("Y15-19")

#relativ til de samlede antal fødsler
#df_byear_ctr <- df_byear %>% filter(geo %in% subctr) 
#df_byear_ctr$year2 <- sapply(df_byear_ctr$year, function(x) (paste0(x,"-01-01")))
#df_byear_ctr$year3 <- as.Date(df_byear_ctr$year2)
#df_byear_ctr_sub <- df_byear_ctr %>% filter(year3 > '2007-01-01' & year3 < '2022-01-01')
#df_byear_ctr_sub <- df_byear_ctr_sub[,-c(1,4)]
#colnames(df_byear_ctr_sub) = c("geo","total","time")
#ggplot(df_byear_ctr_sub, aes(x=time,y=total,color=geo))+geom_point()+geom_line()+
#  ggtitle("Samlet antal fødsler")+
#  scale_y_continuous(breaks = seq(0,2000000,by=100000))
#
#relativ til det første år
ddmysel <- ddsub %>% filter( geo %in% subctr & age %in% subage & time > '2007-01-01')
#ddfv <- ddmysel %>% filter( time == min(time) ) %>% arrange(age)

#
#ddxx <- dd2 %>% filter(geo == "DK")
#ddxx <- ddxx[!is.na(ddxx$values),]
#ddxx2 <- ddxx %>% filter(age %in% subage)

#bvf = ddsub[ddsub$time=='1997-01-01' & ddsub$geo %in% subctr & ddsub$age %in% subage,]
#bvf97 = bvf[,-3]
ddplotsub <- ddsub %>% filter( geo %in% subctr & age %in% subage & time > '1996-01-01')  
bvmerge = ddplotsub %>%  left_join(bvf97, by=c("age","geo"))
colnames(bvmerge)=c("age","geo","time","values","startval")
bvmerge$prcchg = (bvmerge$values-bvmerge$startval)/bvmerge$startval




ggplot(ddplotsub, aes(x=time, y=values, color=geo))+geom_line()+geom_point()+
  facet_wrap(~age, scales="free")+theme(axis.text.x = element_text(angle = 90))+ggtitle("Spain lags")

library(ggplot2)
bvmerge %>% filter(age=='Y15-19') %>% 
  ggplot(aes(x=time, y=prcchg, color=geo))+geom_line()+geom_point()+
  facet_wrap(~age)

colnames()