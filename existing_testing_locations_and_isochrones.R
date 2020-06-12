library(tidyverse)
library(sf)

health_centers <- read_csv('./data/CHD Health Center addresses with zip_geocoded.csv')
aws.s3::s3saveRDS(health_centers, "s3://geomarker/covid_testing_locations/health_centers.rds")

library(openrouteservice)
# download isochrones from ORS
get_isochrones <- function(x, data, trans_mode, time_sec) {
  ors_isochrones(data[x, c('lon', 'lat')],
                 profile = trans_mode,
                 range = time_sec,          # 900 seconds = 15 minutes
                 output = "sf") %>%
    st_transform(3735)
}


health_center_isochrones_15min_drive_list <- CB::mappp(1:nrow(health_centers),
                                                       ~get_isochrones(.x, data = health_centers,
                                                                       trans_mode = 'driving-car',
                                                                       time_sec = 900))

health_center_isochrones_10min_walk_list <- CB::mappp(1:nrow(health_centers),
                                                      ~get_isochrones(.x, data = health_centers,
                                                                      trans_mode = 'foot-walking',
                                                                      time_sec = 600))


# bind into one sf data.frame
unionize_isochrones <- function(iso_list) {
  iso <- iso_list[[1]]
  for (i in 2:length(iso_list)) {
    iso <- rbind(iso, iso_list[[i]])
  }
  return(st_union(iso))
}

health_center_isochrones_15min_drive <- unionize_isochrones(health_center_isochrones_15min_drive_list)
health_center_isochrones_10min_walk <- unionize_isochrones(health_center_isochrones_10min_walk_list)
mapview::mapview(health_center_isochrones_15min_drive)
mapview::mapview(health_center_isochrones_10min_walk)

aws.s3::s3saveRDS(health_center_isochrones_15min_drive, "s3://geomarker/covid_testing_locations/health_center_isochrones_15min_drive.rds")
aws.s3::s3saveRDS(health_center_isochrones_10min_walk, "s3://geomarker/covid_testing_locations/health_center_isochrones_10min_walk.rds")

health_center_isochrones_10min_walk <- aws.s3::s3readRDS("s3://geomarker/covid_testing_locations/health_center_isochrones_10min_walk.rds")

mapview::mapview(d_neigh, zcol = 'frac_coverage') + mapview::mapview(health_center_isochrones_10min_walk)
aws.s3::s3saveRDS(d_neigh, "s3://geomarker/covid_testing_locations/covid_neighborhood_data.rds")

test_sites <- read_csv('./data/Testing Locations_geocoded.csv')
aws.s3::s3saveRDS(test_sites, "s3://geomarker/covid_testing_locations/test_sites.rds")

test_sites <- filter(test_sites, !is.na(lat))

test_sites %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  mapview::mapview()

test_sites_hc <- st_intersection(test_sites %>%
                                   st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
                                   st_transform(5072),
                                 tigris::counties(state = 'ohio') %>%
                                   filter(NAME == "Hamilton") %>%
                                   st_transform(5072))

mapview::mapview(test_sites_hc[30,])

test_sites_hc <- test_sites %>%
  filter(address %in% test_sites_hc$address)
aws.s3::s3saveRDS(test_sites_hc, "s3://geomarker/covid_testing_locations/test_sites_hc.rds")

library(openrouteservice)
# download isochrones from ORS
get_isochrones <- function(x, data, trans_mode, time_sec) {
  ors_isochrones(data[x, c('lon', 'lat')],
                 profile = trans_mode,
                 range = time_sec,          # 900 seconds = 15 minutes
                 output = "sf") %>%
    st_transform(3735)
}


test_sites_isochrones_15min_drive_list <- CB::mappp(1:nrow(test_sites),
                                                       ~get_isochrones(.x, data = test_sites,
                                                                       trans_mode = 'driving-car',
                                                                       time_sec = 900))

test_sites_isochrones_10min_walk_list <- CB::mappp(1:nrow(test_sites),
                                                      ~get_isochrones(.x, data = test_sites,
                                                                      trans_mode = 'foot-walking',
                                                                      time_sec = 600))

aws.s3::s3saveRDS(test_sites_isochrones_10min_walk_list, "s3://geomarker/covid_testing_locations/test_sites_isochrones_15min_drive_list.rds")
aws.s3::s3saveRDS(test_sites_isochrones_10min_walk_list, "s3://geomarker/covid_testing_locations/test_sites_isochrones_10min_walk_list.rds")


hc_test_sites_isochrones_15min_drive_list <- CB::mappp(1:nrow(test_sites_hc),
                                                    ~get_isochrones(.x, data = test_sites_hc,
                                                                    trans_mode = 'driving-car',
                                                                    time_sec = 900))

hc_test_sites_isochrones_10min_walk_list <- CB::mappp(1:nrow(test_sites_hc),
                                                   ~get_isochrones(.x, data = test_sites_hc,
                                                                   trans_mode = 'foot-walking',
                                                                   time_sec = 600))


# bind into one sf data.frame
unionize_isochrones <- function(iso_list) {
  iso <- iso_list[[1]]
  for (i in 2:length(iso_list)) {
    iso <- rbind(iso, iso_list[[i]])
  }
  return(st_union(iso))
}

test_sites_isochrones_15min_drive <- unionize_isochrones(hc_test_sites_isochrones_15min_drive_list)
test_sites_isochrones_10min_walk <- unionize_isochrones(hc_test_sites_isochrones_10min_walk_list)
mapview::mapview(test_sites_isochrones_15min_drive)
mapview::mapview(test_sites_isochrones_10min_walk)

aws.s3::s3saveRDS(test_sites_isochrones_15min_drive, "s3://geomarker/covid_testing_locations/test_sites_isochrones_15min_drive.rds")
aws.s3::s3saveRDS(test_sites_isochrones_10min_walk, "s3://geomarker/covid_testing_locations/test_sites_isochrones_10min_walk.rds")

