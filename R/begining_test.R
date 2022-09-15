library(httr)
library(jsonlite)
library(tidyverse)
library(sf)


api <- "https://api.purpleair.com/v1/keys"


api <- "https://api.purpleair.com/v1/keys -H \"X-API-Key: 0AD6F0F4-2488-11ED-B5AA-42010A800006"


# this does the https://community.purpleair.com/t/making-api-calls-with-the-purpleair-api/180
GET("https://api.purpleair.com/v1/keys", add_headers(`X-API-Key` = "0AD6F0F4-2488-11ED-B5AA-42010A800006"))




GET("https://api.purpleair.com/v1/sensors", 
    add_headers(
      .headers = c(`X-API-Key` = "0AD6F0F4-2488-11ED-B5AA-42010A800006", 
                   fields = "sensor_index,name,latitude,longitude,
                   location_type")))


GET("https://api.purpleair.com/v1/sensors&fields = sensor_index,name,latitude,longitude,location_type", 
    add_headers(.headers = c(`X-API-Key` = "0AD6F0F4-2488-11ED-B5AA-42010A800006")))


# Getting the names,
res <- GET("https://api.purpleair.com/v1/sensors",
query = list(fields = "name, latitude,longitude"), 
  add_headers(.headers = c(`X-API-Key` = "0AD6F0F4-2488-11ED-B5AA-42010A800006")))

content(res, "parsed") #list
content(res, "text") #json

#convert to sf

myJSON2TBL = function(resx){
  stopifnot(class(resx) == "response")
  
  api_outx <- fromJSON(content(resx, "text"))
  colnames(api_outx$data) <- api_outx$fields 
  
  channel_a = any(grepl("_a$", api_outx$fields)) # id channel a
  
  as_tibble(api_outx$data) %>%
    mutate(ParentID = sensor_index, ID = sensor_index, Lat = as.numeric(latitude), Lon = as.numeric(longitude),
           Label = name, DEVICE_LOCATIONTYPE = 
             if_else(as.logical(as.numeric(location_type)), "inside", "outside"),
           Hidden = ifelse(
             as.logical(as.numeric(private)),
             "true", "false"),
           isOwner = NA,
          AGE = Sys.time() - as.POSIXct(as.numeric(last_seen), 
                                                    origin = "1970-01-01"),
          Flag = as.numeric(
              case_when(
                channel_flags == "0" ~ FALSE,
                channel_flags == "1" & channel_a ~ TRUE,
                channel_flags == "1" & !channel_a ~ FALSE,
                channel_flags == "2" & !channel_a ~ TRUE,
                channel_flags == "2" & channel_a ~ FALSE,
                channel_flags == "3" ~ TRUE
            )
          ),
          
          A_H = NA,
          timeSinceModified = Sys.time() - as.POSIXct(as.numeric(last_modified), 
                                                      origin = "1970-01-01"),
          LastSeen = as.numeric(last_seen),
          last_modified = as.numeric(last_modified)
          
           ) %>%
    rename(
           THINGSPEAK_PRIMARY_ID = starts_with("primary_id"),
           THINGSPEAK_PRIMARY_ID_READ_KEY = starts_with("primary_key"),
           THINGSPEAK_SECONDARY_ID = starts_with("secondary_id"),
           THINGSPEAK_SECONDARY_ID_READ_KEY = starts_with( "secondary_key"),
           PM2.5_Value = pm2.5,
           pm25_10min = starts_with("pm2.5_10min"),
           pm25_30min = starts_with("pm2.5_30min"),
           pm25_1hr = starts_with("pm2.5_60min"),
           pm25_6hr = starts_with("pm2.5_6hour"),
           pm25_1day = starts_with("pm2.5_24hour"),
           pm25_1week = starts_with("pm2.5_1week"),
           Type =hardware,
           humidity = starts_with("humidity"),
           temp_f = starts_with("temperature"),
           pressure = starts_with("pressure")) %>%
    transmute(ID, ParentID, Label, DEVICE_LOCATIONTYPE, THINGSPEAK_PRIMARY_ID,           
               THINGSPEAK_PRIMARY_ID_READ_KEY,  THINGSPEAK_SECONDARY_ID,         
                THINGSPEAK_SECONDARY_ID_READ_KEY, Lat, Lon, 
           PM2_5Value = as.numeric(PM2.5_Value), v = as.numeric(PM2.5_Value), v1 = as.numeric(pm25_10min), v2 = as.numeric(pm25_30min), 
                v3 = as.numeric(pm25_1hr), v4 = as.numeric(pm25_6hr), v5 = as.numeric(pm25_1day), v6 = as.numeric(pm25_1week), 
                channel_flags, Flag, LastSeen, Type, Hidden, isOwner, humidity,
                  temp_f, pressure, AGE, LastSeen, A_H, lastModified = last_modified,
           timeSinceModified)
}



pA_sfx <- myJSON2TBL(res) %>%  drop_na %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)



AirSensor::initializeMazamaSpatialUtils()
setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") # SCAQMD sensors 

# ----- Load PAS ---------------------------------------------------------------

pas <- 
  pas_load(20220526, archival = TRUE) %>% 
  pas_filter(stateCode == "UT")

pas %>% pas_leaflet() # To pick a sensor


#Get latest humidity, pm2.5_10minute


fieldListA <- c("name", "primary_id_a", "primary_key_a", "secondary_id_a",
                "secondary_key_a", "temperature_a", "humidity_a", "pressure_a", 
                "pm2.5", "pm2.5_10minute_a", "pm2.5_30minute_a", "pm2.5_60minute_a",
                "pm2.5_6hour_a", "pm2.5_24hour_a", "pm2.5_1week_a", 
                "latitude","longitude", "location_type", "last_seen",
                "hardware", "private", "last_seen", "channel_flags", "last_modified")

res <- GET("https://api.purpleair.com/v1/sensors",
           query = list(fields = paste(fieldListA, collapse = ",")), 
           add_headers(.headers = c(`X-API-Key` = "0AD6F0F4-2488-11ED-B5AA-42010A800006")))


myJSON2TBL(res) -> dfx

dfxx <- pas_enhanceData(dfx, "US")
