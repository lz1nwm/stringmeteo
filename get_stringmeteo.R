library(rvest)
library(data.table)
library(ggplot2)
library(scales)
library(openxlsx2)
library(stringr)
library(lubridate)

add_new_dt <- function(dt.old, dt.new){
    
    #dt.new <- dt.all.n
    #dt.old <- dt.all
    
    # cols w/o data
    cols_on <- c('ddate', 'STATION_ID','STATION_NAME')
    cols_i <- names(dt.old)[ !(names(dt.old) %in% cols_on)]
    cols_d <- names(dt.old)
    
    # update values
    dt.old[dt.new, (cols_i) := mget(paste0("i.", cols_i)), on = (cols_on)]
    
    # filter old and merge with new
    dt.old <- rbindlist( list( dt.old[!dt.new, on = (cols_d)],
                               dt.new),
                         use.names = T)
    setkeyv(dt.old, cols_on)
    dt.old <- unique(dt.old)
}

clean_data <- function(df){
    df[, c('TEMP','TEMP_W') := tstrsplit( gsub("\\([^\\)]+\\)", "", X3), 'ºC') ]
    df[, 'TEMP_E' := regmatches(X3, gregexpr( "(?<=\\().+?(?=\\))", X3, perl = T)) ]
    df[, 'TEMP_E' := gsub('ºC','',TEMP_E) ]
    
    df[, X4 := gsub('---','ºC', X4)]
    df[, c('TEMP_MAX','TEMP_MIN') := tstrsplit( X4, 'ºC') ]
    
    df[, c('TEMP_DEW','HUMIDITY') := tstrsplit( X5, 'ºC') ]
    df[, HUMIDITY := gsub("^.*\\|([0-9]+)%\\|$", "\\1", HUMIDITY)]
    
    df[, WIND_DIRECTION := X6]
    df[, WIND_SPEED := gsub(" м/с", "", X7) ]
    df[, CLOUDS := X9]
    df[, VISIBILITY := X15]
    df[, RAIN_3  := str_extract(X16, "(.*(?=\\())")]
    df[, RAIN_3  := str_extract(RAIN_3, '\\d+(.\\d+)?')]
    df[, RAIN_12 := str_extract(X16, "(?<=\\().*(?=\\))")]
    df[, RAIN_12  := str_extract(RAIN_12, '\\d+(.\\d+)?')]
    df[, RAIN_24 := str_extract(X16, "(?<=\\|).*(?=\\|)")]
    df[, RAIN_24  := str_extract(RAIN_24, '\\d+(.\\d+)?')]
    df[, PRESSURE_1  := str_extract(X17, "(.*(?=\\/))")]
    df[, PRESSURE_2  := str_extract(X17, "((?<=\\/).*)")]
    
    cols <- c('TEMP','TEMP_W','TEMP_E', 'TEMP_MAX','TEMP_MIN', 
              'TEMP_DEW','HUMIDITY','WIND_SPEED','RAIN_12','RAIN_24',
              'PRESSURE_1','PRESSURE_2')
    df[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
    df[!is.na(RAIN_24)]
    df[, names(df)[names(df) %like% '^X'] := NULL]
}

Sys.setlocale("LC_CTYPE", "Bulgarian.utf8")

base_url <- 'https://www.stringmeteo.com/synop/bg_stday.php?'

load(file = './RData/bg_nimh.RData')
max(dt.all$ddate)

st <- wb_to_df('./RData/dict.xlsx') |> setDT()
dates <- seq.Date(as.Date('1999-09-01'), as.Date('2024-10-01'), by = 'month')
dates <- seq.Date(Sys.Date() %m-% months(1), Sys.Date(), by = 'month')
dates <- Sys.Date()

list_dt <- list()
#cty <- st[1,station_id]

library(cli)
cli_progress_bar(total = length(dates) * length(st[, station_id]))


for(cty in st[, station_id]){
    for(i in 1:length(dates) ){
        #cty <- '15614'
        #i <- dates[1]
        #print(paste(cty, as.Date(i)))
        
        
        yr <- year(dates[i])
        m <- month(dates[i])
        d <- mday(dates[i])
        
        if( !(paste(cty,i) %in% names(list_dt)) ){
            
            params <- paste0('year=',yr,'&month=',m,'&day=',d,'&city=',cty,'&int=31')
            dt <- read_html(paste0(base_url,params)) %>%
                html_element("table") %>% html_table()
            
            list_dt[[paste(cty,i)]] <- dt |> 
                setDT() |> 
                _[,1:18] |> 
                _[X1 %like% '\\d\\['] |> 
                _[, ddate := as.POSIXct(paste0(yr, X2), format = "%Y%d.%m.%H%M", tz = 'UTC')] |> 
                _[, c('X1','X2') := NULL] |> 
                _[, STATION_ID := cty ] |> 
                _[, STATION_NAME := st[station_id == cty, station_name] ]
            
            cli_progress_update()
            
            Sys.sleep( sample(1:2, 1) )
        }
        
    }
    #cli_progress_update()
}

dt.all.n <- rbindlist(list_dt, use.names = T)
dt.all.n <- clean_data(dt.all.n)

load(file = './RData/bg_nimh.RData')

nrow(dt.all)
max(dt.all$ddate)

dt.all <- add_new_dt(dt.all, dt.all.n)

nrow(dt.all)
max(dt.all$ddate)


save(dt.all, file = './RData/bg_nimh.RData')
