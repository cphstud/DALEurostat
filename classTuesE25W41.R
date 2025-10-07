# den lille tabel
colldf = as.data.frame(matrix(data=NA,nrow = 10,ncol = 10))
# 
# lav en sekvens som du looper med.
for (i in 1:10) {
  for( j in 1:10) {
    colldf[i,j]=i*abs(11-j)
  }
}


names=c("Kurt","Ib","Verner","Otto")
namesv=rep(names,times=3)

resultater=as.integer(runif(12,20,35))
now=Sys.Date()
tidspunkt=c(now-7,now-14,now-21)
tidspunktv=rep(tidspunkt,each=4)
tidspunktv
str(tidspunkt)

rundf=data.frame(deltagere=namesv,løb=tidspunktv,resultater=resultater)


# stamdata
names=c("Lone","Kaja","Bjarne","Ib","Verner","Otto")
gender=rep(c("M","K"),times=6)
age=c(34,32,45,21)
vej=c("Tornevej","Sidevej","Hovedvej","Pivot Drive")
zip=c("2300","2100","2100","8000")

stamdf=data.frame(navne=names,alder=age,vej=vej,postnr=zip)
stamdf=data.frame(gender=gender,navne=names,alder=age,vej=vej,postnr=zip)

runWithStam = left_join(rundf,stamdf, by=c("deltagere"="navne"))
runWithOnlyStam = inner_join(rundf,stamdf, by=c("deltagere"="navne"))
goneRunners=anti_join(rundf,stamdf, by=(c("deltagere"="navne")))

# average løbetid på løberne
avrundf <- runWithStam %>% group_by(deltagere,gender) %>% 
  summarise(avtid=mean(resultater))

# lav en liste over mulige hold med 1 til 4 deltagere

testliste=list()
testliste['names']=list(c("Kurt","Anton"))
testliste['parts']=list(c("Anton"))
testliste['girls']=list(c("Mona","Ib","Anton"))
testliste['scores']=list(1:200)

for(element in testliste) {
  print(element)
}

# loop igennem og gør noget ved hvert element
lapply(testliste, function(x) length(x))

names
# kombinationer af hold i en liste
df2=combn(names,2,simplify = F)

teamlist=list()
for(i in 1:length(names)) {
 #lav en kombination af i holdstørrelse  
  df2=combn(names,i,simplify = F)
  # put ind i listen
  teamlist[i]=list(df2)
}



