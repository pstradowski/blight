Sys.setlocale("LC_MESSAGES", "en_US.utf8")
Sys.setlocale("LC_ALL", "en_US.utf8")
#data_path <- '~/datasci_course_materials/capstone/blight/data/'
data_path <- '/home/pawel/Documents/datasci_course_materials/capstone/blight/data/'
detroit.311 <- read.csv(paste0(data_path, "detroit-311.csv"))
violations <- read.csv(paste0(data_path, "detroit-blight-violations.csv"))
crime <- read.csv(paste0(data_path, "detroit-crime.csv"))
demolitions <- read.delim(paste0(data_path, "detroit-demolition-permits.tsv"))
load("boundary.RData")
library("dplyr")
library("leaflet")
library("tidyr")
library("fuzzyjoin")
library("magrittr")
library("geohash")

parse_addr <- function(df, col){
  df %>% 
    separate_(col, into=c("addr", "loc"), sep="\\(") %>%
    tidyr::extract(loc, into = c("lat", "lon"), 
                   regex = "(\\d+\\.\\d+),\\s(\\-*\\d+\\.\\d+)", perl = TRUE) %>%
    mutate(lat=as.numeric(lat), lon=as.numeric(lon)) %>%
    mutate(addr = sub("\n.+", "", addr)) 
}

enrich <- function(df) {
  df %>% filter(lat<max(boundary$lat), 
                lat > min(boundary$lat), 
                lon > min(boundary$lon), 
                lon < max(boundary$lon)) %>%
    mutate(geohash = gh_encode(lat, lon, 8)) %>%
    mutate(gh6 = substr(geohash, 1, 6),
           gh7 = substr(geohash, 1, 7))
}

violations %<>% parse_addr("ViolationAddress") %>% 
  enrich() 

crime %<>% rename(lat = LAT, lon = LON, addr = ADDRESS) %>% 
  enrich() 

demolitions %<>% parse_addr("site_location") %>% 
  enrich() 

detroit.311 %<>% rename(lon = lng) %>%
  enrich()
blight <-demolitions %>% distinct(geohash) %>%
  select(geohash, gh6, gh7, lat, lon) %>%
  mutate(blighted = 1) 


all_events <- crime %>% select(geohash, gh6, gh7, lat, lon) %>%
  bind_rows(violations %>% select(geohash, gh6, gh7, lat, lon)) %>%
  distinct(geohash) 

non_blight <- anti_join(all_events, blight, by="gh7") %>%
  select(geohash, gh6, gh7, lat, lon) %>%
  mutate(blighted = 0)

data_set <- non_blight %>%
  sample_n(nrow(blight), replace = FALSE) %>% 
  bind_rows(blight)

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Haversine formula (hf)
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

feat1 <- violations  %>% select(geohash, gh6, gh7, lat, lon) %>%
  inner_join(data_set, by="gh7") %>%
  mutate(dist = gcd.hf(lon.x, lat.x, lon.y, lat.y))
