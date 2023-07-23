#!/usr/bin/env Rscript
library(httr)
library(dplyr)
library(stringr)
library(rvest)
library(googledrive)
library(pushoverr)

pushover(message='starting workflow', 
         user="uccrmx7ajshvdsgbx2e2qy17eorpsx", 
         app="akhzmh5yoco7koy31oos1micwsbxh7")

print('Authenticating drive')
temp<-tempfile(fileext = '.json')
download.file('https://www.dropbox.com/s/twobcoxscpv77h6/rcloud-298921-bdc6e1c25160.json?dl=1',temp)
drive_auth(path=temp)

#scrape data from JSON
print('Scraping from JSON')
url<-sprintf("https://aemo.com.au/aemo/api/v1/GasBBReporting/DailyProductionAndFlow?FacilityIds=540093,580010,540101,544261,540047,530030,540083,540096,580020,540071,540077,520075,540059,520054,520090,540094,540098,540080,540090,540086,540050,540097,540055,520047,540089,540070,540092,530071,530042,540088,540075,544253,540061,530038,530039,530040,580100,580040,540064,530043,550050,550045,550046,550054,520053,530061,520060,580050,540084,530041,530044,580060,580070,540065,550052,530060,540058,540085,540102,540073,540057,540095,544260,540110,540040,540082,540072,540062,540103,550061,550060,540060,540066,540067,540076,540068,580210,570050,540051,532005,530110,540045,540046,540091,580030,540069,540087,580180,540074&FromGasDate=%s&ToGasDate=%s",
             (Sys.Date()-365) %>% format(.,"%d/%m/%Y"),Sys.Date() %>% format(.,"%d/%m/%Y"))
rf <- GET(url)
rfc <- content(rf)

#parse JSON to dataframe
print('Parsing data')
df <- do.call(rbind, lapply(rfc$data$DailyProductionAndFlowList, function(x) {
  x[sapply(x, is.null)] <- "NULL"
  as.data.frame(x, stringsAsFactors = FALSE)
}))
df2<-df %>% mutate(Date=as.Date(GasDate, format = "%Y-%m-%d")) %>%
  mutate(HeldInStorage=HeldInStorage%>%as.numeric())

#extract storage data
roma<-df2 %>% filter(FacilityName=="Roma Underground Storage (RUGS)") %>%
  select(Date,HeldInStorage) 
roma[which(roma$Date=="2020-09-04"),2]<-43378
roma[which(roma$Date=="2020-09-05"),2]<-43378
roma <- roma %>% mutate(Levels=round((HeldInStorage*100/50000),1)) %>%
  mutate(facility="roma")

iona<-df2 %>% filter(FacilityName=="Iona Underground Gas Storage") %>%
  select(Date,HeldInStorage) %>% mutate(Levels=round((HeldInStorage*100/26000),1)) %>%
  mutate(facility="iona")
storage<-rbind(roma,iona)

#extract production data
longford <-df2 %>% filter(FacilityName=="Longford Gas Plant") %>%
  select(Date,Supply) %>% mutate(utilisation=round((Supply*100/1115),1))
#extract pipeline data 
msp<-df2 %>% filter(FacilityName=="Moomba to Sydney Pipeline System" &
                      LocationName=="Moomba Hub") %>%
  select(Date,TransferIn) %>% mutate(pipe='msp')
names(msp)[2]<-"flow"

vni<-df2 %>% filter(FacilityName=="Moomba to Sydney Pipeline System" &
                      LocationName=="Culcairn") %>%
  select(Date,TransferOut) %>% mutate(pipe="vni")
names(vni)[2]<-"flow"

swqp<-df2 %>% filter(FacilityName=="South West Queensland Pipeline" &
                       LocationName=="Wallumbilla Hub") %>% 
  mutate(flow=TransferIn-TransferOut,pipe="swqp") %>%
  select(Date,flow,pipe)

aplng<-df2 %>% filter(FacilityName=="APLNG Pipeline" &
                        LocationName=="Curtis Island") %>%
  mutate(flow=Demand,pipe="aplng") %>%
  select(Date,flow,pipe)

qclng<- df2 %>% filter(FacilityName=="Wallumbilla to Gladstone Pipeline" &
                         LocationName=="Curtis Island") %>%
  mutate(flow=Demand,pipe="qclng") %>%
  select(Date,flow,pipe)

glng<- df2 %>% filter(FacilityName=="GLNG Gas Transmission Pipeline" &
                        LocationName=="Curtis Island") %>%
  mutate(flow=Demand,pipe="glng") %>%
  select(Date,flow,pipe)

pipeline <- rbind(swqp,msp,vni) %>% mutate(flow=round(flow,1))
lng<-rbind(aplng,qclng,glng) %>% mutate(flow=round(flow,1))

#extract data for DWGM
print('Extracting DWGM data')
dat<-read.csv("http://www.nemweb.com.au/REPORTS/CURRENT/VicGas/INT310_V4_PRICE_AND_WITHDRAWALS_1.CSV") %>%
  mutate(gas_date=as.Date(gas_date, format = "%d %b %Y")) %>%
  filter(schedule_interval==1) %>% .[1:14,]

dat2<-read.csv("http://www.nemweb.com.au/REPORTS/CURRENT/VicGas/INT287_V4_GAS_CONSUMPTION_1.CSV") %>%
  mutate(gas_date=as.Date(gas_date, format = "%d %b %Y")) %>%
  arrange(desc(gas_date)) %>% .[1:14,] %>%
  subset(select=c(total_gas_used))
dat2<-round(dat2/1000,0)
dat2[1,1]<-NA

dwgm<-cbind(dat,dat2)

#extract GSH data 
#GSH
print('Extracting GSH data')
url <- "https://nemweb.com.au/Reports/Current/GSH/GSH_Daily_Trans_Summary/"
pg <- read_html(url)
x<-paste("https://nemweb.com.au",html_attr(html_nodes(pg, "a"), "href"),sep = "") %>% .[-1]
y<-x[(sum(x==x)-6):sum(x==x)]
z<-list()
for (i in y){
  temp <- tempfile()
  download.file(i,temp)
  dat <- read.table(unz(temp,str_extract(i,"PUBLIC_DAILYTRANSACTIONSUMMARY_\\d+_\\d+.zip") %>%
                          str_replace_all(.,"zip","CSV")),skip=1,header = T,fill=T,sep = ",")
  z[which(y==i)]<-list(dat[1:nrow(dat)-1,5:ncol(dat)-1])
}
gsh<-bind_rows(z, .id = "column_label") %>% .[,c(-1,-2,-8,-9,-10,-11)] %>% mutate(CURRENTDATE =as.Date(CURRENTDATE)) %>%
  mutate(FROM_GAS_DATE =as.Date(FROM_GAS_DATE)) %>%
  mutate(TO_GAS_DATE =as.Date(TO_GAS_DATE)) %>% 
  dplyr::arrange(desc(CURRENTDATE))

names(gsh)[names(gsh) == "CURRENTDATE"] <- "DATE"
names(gsh)[names(gsh) == "VOLUME_WEIGHTED_AVERAGE_PRICE"] <- "VOLUME WEIGHTED AVERAGE PRICE"
names(gsh)[names(gsh) == "TOTAL_NUMBER_OF_TRADES"] <- "TOTAL_TRADES"
names(gsh)[names(gsh) == "TOTAL_QUANTITY"] <- "QUANTITY"

print('Writing CSV')
write.csv(storage,'output/storage.csv')
write.csv(pipeline,'output/pipeline.csv')
write.csv(lng,'output/lng.csv')
write.csv(longford,'output/longford.csv')
write.csv(dwgm,'output/dwgm.csv')
write.csv(gsh,'output/gsh.csv')

list_df <- list(storage, pipeline, lng, longford, dwgm, gsh)
temp <- tempfile(fileext = '.rds')

saveRDS(list_df, temp)

drive_upload(temp, name = "list_df.rds", overwrite = TRUE)

print('Updating drive')
drive_update('dwgm','output/dwgm.csv')
drive_update('gsh','output/gsh.csv')
drive_update('lng','output/lng.csv')
drive_update('longford','output/longford.csv')
drive_update('pipeline','output/pipeline.csv')
drive_update('storage','output/storage.csv')

pushover(message='drive updated', 
         user="uccrmx7ajshvdsgbx2e2qy17eorpsx", 
         app="akhzmh5yoco7koy31oos1micwsbxh7")

