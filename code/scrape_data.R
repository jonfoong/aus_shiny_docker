#!/usr/bin/env Rscript
library(httr)
library(rvest)
library(googledrive)

print("docker build 23072023")

# authenticate gdrive

print('Authenticating drive')

#temp <- tempfile(fileext = '.json')

#download.file('https://www.dropbox.com/s/twobcoxscpv77h6/rcloud-298921-bdc6e1c25160.json?dl=1',temp)

drive_auth(path = "token/token.json")

# scrape data from JSON

print('Scraping from JSON')

url <- sprintf("https://aemo.com.au/aemo/api/v1/GasBBReporting/DailyProductionAndFlow?FacilityIds=540093,580010,540101,544261,540047,530030,540083,540096,580020,540071,540077,520075,540059,520054,520090,540094,540098,540080,540090,540086,540050,540097,540055,520047,540089,540070,540092,530071,530042,540088,540075,544253,540061,530038,530039,530040,580100,580040,540064,530043,550050,550045,550046,550054,520053,530061,520060,580050,540084,530041,530044,580060,580070,540065,550052,530060,540058,540085,540102,540073,540057,540095,544260,540110,540040,540082,540072,540062,540103,550061,550060,540060,540066,540067,540076,540068,580210,570050,540051,532005,530110,540045,540046,540091,580030,540069,540087,580180,540074&FromGasDate=%s&ToGasDate=%s",
               format(Sys.Date() - 365, "%d/%m/%Y"), 
               format(Sys.Date(), "%d/%m/%Y"))

# request content from url

rfc <- url |> 
  GET() |> 
  content()

# parse JSON to dataframe

print('Parsing data')

df <- do.call(rbind, 
              lapply(rfc$data$DailyProductionAndFlowList, 
                     function(x){
                       
                       x[sapply(x, is.null)] <- "NULL"
                       
                       as.data.frame(x, stringsAsFactors = FALSE)
                     }))

# some data cleaning

df <- df |> 
  transform(Date = as.Date(GasDate, format = "%Y-%m-%d"),
            HeldInStorage = as.numeric(HeldInStorage))

#extract storage data

# roma

roma <- df |> 
  subset(FacilityName == "Roma Underground Storage (RUGS)",
         select = c(Date, HeldInStorage)) |> 
  transform(Levels = round((HeldInStorage*100/50000), 1),
            facility="roma")

# iona

iona <- df |> 
  subset(FacilityName == "Iona Underground Gas Storage",
         select = c(Date, HeldInStorage)) |>
  transform(Levels = round((HeldInStorage*100/26000), 1),
            facility = "iona")

storage <- rbind(roma, iona)

#extract production data

longford <- df |> 
  subset(FacilityName=="Longford Gas Plant",
         select = c(Date, Supply)) |>
  transform(utilisation = round((Supply*100/1115), 1))

#extract pipeline data 

# msp

msp <- df |> 
  subset(FacilityName == "Moomba to Sydney Pipeline System" &
                      LocationName == "Moomba Hub",
         select = c(Date, TransferIn)) |> 
  transform(pipe = 'msp')

names(msp)[2] <- "flow"

# vni

vni <- df |> 
  subset(FacilityName == "Moomba to Sydney Pipeline System" &
                      LocationName=="Culcairn",
         select = c(Date, TransferOut)) |> 
  transform(pipe = "vni")

names(vni)[2] <- "flow"

# swqp

swqp <- df |> 
  subset(FacilityName == "South West Queensland Pipeline" &
                       LocationName == "Wallumbilla Hub") |>
  transform(flow = TransferIn - TransferOut, pipe = "swqp") |>
  subset(select = c(Date, flow, pipe))

pipeline <- rbind(swqp, msp, vni) |> 
  transform(flow = round(flow, 1))

# LNG production

# aplng

aplng <- df |> 
  subset(FacilityName == "APLNG Pipeline" &
                        LocationName=="Curtis Island") |>
  transform(flow = Demand, pipe = "aplng") |>
  subset(select = c(Date, flow, pipe))

# qclng

qclng <- df |> 
  subset(FacilityName == "Wallumbilla to Gladstone Pipeline" &
                         LocationName == "Curtis Island") |>
  transform(flow = Demand, pipe = "qclng") |>
  subset(select = c(Date, flow, pipe))

# glng

glng <- df |> 
  subset(FacilityName == "GLNG Gas Transmission Pipeline" &
                        LocationName == "Curtis Island") |>
  transform(flow = Demand, pipe = "glng") |>
  subset(select = c(Date, flow, pipe))

lng <- rbind(aplng, qclng, glng) |> 
  transform(flow = round(flow, 1))

#extract data for DWGM

print('Extracting DWGM data')

dwgm_1 <- read.csv("http://www.nemweb.com.au/REPORTS/CURRENT/VicGas/INT310_V4_PRICE_AND_WITHDRAWALS_1.CSV") |>
  transform(gas_date = as.Date(gas_date, format = "%d %b %Y")) |>
  subset(schedule_interval == 1)

# extract only data for last 2 weeks

dates <- sort(dwgm_1$gas_date, decreasing = TRUE)[1:14]

dwgm_1 <- dwgm_1 |>
  subset(gas_date %in% dates)

dwgm_2 <- read.csv("http://www.nemweb.com.au/REPORTS/CURRENT/VicGas/INT287_V4_GAS_CONSUMPTION_1.CSV") |>
  transform(gas_date=as.Date(gas_date, format = "%d %b %Y"),
            total_gas_used = round(total_gas_used/1000, 0)) |>
  # omit today's gas data since incomplete
  transform(total_gas_used = ifelse(gas_date==max(dates), NA, total_gas_used)) |>
  subset(gas_date %in% dates, select = c(gas_date, total_gas_used))

dwgm <- merge(dwgm_1, dwgm_2, by = "gas_date")

#extract GSH data 

print('Extracting GSH data')

url <- "https://nemweb.com.au/Reports/Current/GSH/GSH_Daily_Trans_Summary/"

links <- read_html(url) |>
  html_nodes("a") |>
  html_attr("href")

# remove links that are not zip

links <- links[grepl("\\.zip$", links)]

# get the most recent 7 days

matches <- gregexpr("_([0-9]{8})_", links)
dates <- regmatches(links, matches)

dates <- gsub("_", "", dates) |>
  as.Date(format = "%Y%m%d")

dates_ind <- which(dates %in% sort(dates, decreasing = TRUE)[1:7])

links <- links[dates_ind]

full_links <- paste("https://nemweb.com.au", links, sep = "")

# now download and unzip 

list_gsh <- lapply(full_links, function(x){
  
  temp <- tempfile()
  download.file(x, temp)
  
  matches <- gregexpr("([^/]+$)", x)
  filename <- regmatches(x, matches) |> unlist()
  filename <- gsub("zip", "CSV", filename)
  
  dat <- unz(temp, filename) |>
    read.table(skip=1, header = T, fill = TRUE, sep = ",")
  
  # exclude the last row and irrelevant columns
  
  dat <- dat[1:nrow(dat)-1, 6:ncol(dat) - 1]
  
  excludes <- paste0(c("HIGH", "LOW", "OPEN", "CLOSE"), "_PRICE")
  includes <- colnames(dat)[!colnames(dat) %in% excludes]
  
  dat |> subset(select = includes)
  
})

gsh <- do.call(rbind, list_gsh) |> 
  transform(CURRENTDATE = as.Date(CURRENTDATE),
            FROM_GAS_DATE = as.Date(FROM_GAS_DATE),
            TO_GAS_DATE = as.Date(TO_GAS_DATE))

gsh <- gsh[order(gsh$CURRENTDATE, decreasing = TRUE),]

# rename col names

names(gsh)[names(gsh) == "CURRENTDATE"] <- "DATE"
names(gsh)[names(gsh) == "VOLUME_WEIGHTED_AVERAGE_PRICE"] <- "VOLUME WEIGHTED AVERAGE PRICE"
names(gsh)[names(gsh) == "TOTAL_NUMBER_OF_TRADES"] <- "TOTAL_TRADES"
names(gsh)[names(gsh) == "TOTAL_QUANTITY"] <- "QUANTITY"

# now combine all dfs into a list

list_df <- list(storage, pipeline, lng, longford, dwgm, gsh)

# give names to elements in the list

names(list_df) <- c("storage", "pipeline", "lng", "longford", "dwgm", "gsh")

# now save it into an rds file

temp <- tempfile(fileext = '.rds')
saveRDS(list_df, temp)

# now upload to gdrive

print('Updating drive')
drive_upload(temp, name = "list_df.rds", overwrite = TRUE)

