library(ineapir)


metad=ineapir::get_metadata_table_varval("1466")

series <- get_metadata_series_table(idTable = 1466)
dfYM <- get_data_table(idTable = 1466)
saveRDS(dfYM,"spanish_1466.rds")

dfYMtestMadrid <- dfYM %>% filter(str_detect(Nombre, "Mad"))

dfYmSub <- dfYM %>% filter(COD=="IDB11055")

# find 15 til 19 af Primero i Nombre for Madrid
dfYmSub2 <- dfYMtestMadrid %>% 
  filter(str_detect(Nombre,"Primero")) %>% 
  filter(str_detect(Nombre,"1[5-9] "))

testDataDataFrame=dfYmSub2[1,'Data']
testDataDataFrame2=testDataDataFrame[[1]]


