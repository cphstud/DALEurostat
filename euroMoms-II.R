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
  
# get nuts-names
nl1=get_eurostat_geospatial(nuts_level = 1)
nl2=get_eurostat_geospatial(nuts_level = 2)
nl3=get_eurostat_geospatial(nuts_level = 3)

###### functions #######
getNutCtr <- function(indf,ctrcode) {
  p0=paste0("^",ctrcode,"$")
  p1=paste0(ctrcode,"[0-9A-Z]{1}$")
  p2=paste0(ctrcode,"[0-9A-Z]{2}$")
  p3=paste0(ctrcode,"[0-9A-Z]{3}$")
  ymDfCtr <- ymDf %>% filter(str_detect(geo,ctrcode)) %>% 
    mutate(
      nut0 = str_detect(geo,p0),
      nut1 = str_detect(geo,p1),
      nut2 = str_detect(geo,p2),
      nut3 = str_detect(geo,p3)
      )
  return(ymDfCtr)
}

getNutLevelCtrA <- function(df,nlevel) {
  ymDfCtrL = df[df[[nlevel]]==T,]
# aggregere de to age-cats
  ymDfCtrLA = aggregate(ymDfCtrL,values ~ time+geo, FUN=sum)
  return(ymDfCtrLA)
}

getNutLabel <- function(ctr,df,nlevel) {
  if (nlevel==0) {
    nlCtr=df
  } else if (nlevel==1) {
    nlCtr=nl1[grepl(ctr,nl1$CNTR_CODE),]
  } else if (nlevel == 2) {
    nlCtr=nl2[grepl(ctr,nl2$CNTR_CODE),]
  } else {
    nlCtr=nl3[grepl(ctr,nl2$CNTR_CODE),]
  }
  ymDfCtrAM=merge(nlCtr,df, by.y = "geo",by.x = "NUTS_ID",all.y = T)
  it=as.numeric(as.character(ymDfCtrAM$time))
  ymDfCtrAM$time=it
  return(ymDfCtrAM)
}

getLPlot <- function(df) {
  p <- ggplot(df,aes(x=time,y=values))+geom_line(aes(colour = NUTS_NAME))
  return(p)
}

getMPlot <- function(df) {
  # map plot 
  p <- ggplot(df) +
    geom_sf(aes(fill=values))+
    ggtitle("NUTS Regions in Europe") +
    theme_minimal()+
    scale_fill_distiller(palette = "Spectral", direction = 1, na.value = "white")+
    facet_wrap(~time)
  return(p)
}

###### functions end #######

# prepare data for Country using functions
ymDfFI = getNutCtr(ymDf,"FI") 
ymDfDK = getNutCtr(ymDf,"DK") 
ymDfDK3 = getNutLevelCtrA(ymDfDK,"nut3")
ymDfDK3L= getNutLabel("DK",ymDfDK3,3)

ymDfFIN0 = getNutLevelCtrA(ymDfES,"nut0")
ymDfFIN2 = getNutLevelCtrA(ymDfFI,"nut2")
ymDfFIN1A = getNutLabel("FI",ymDfFIN2,2)
p=getLPlot(ymDfFIN1A)
p=getMPlot(ymDfDK3L)
p

ymDfFIN0$time=as.integer(as.character(ymDfESN0$time))
ggplot(ymDfFIN0,aes(x=time,y=values))+geom_line()


ymDfESN1 = getNutLevelCtrA(ymDfES,"nut1")
ymDfESN2 = getNutLevelCtrA(ymDfES,"nut2")
ymDfESAM1 = getNutLabel("ES",ymDfESN1,1)
ymDfESAM2 = getNutLabel("ES",ymDfESN2,2)

p = getLPlot(ymDfESAM1)
p = getMPlot(ymDfESAM2)
p
