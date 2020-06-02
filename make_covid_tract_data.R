library(tidyverse)
library(sf)

hc_tracts <- tigris::tracts(state = "Ohio", county = "Hamilton")

d_tract <- read_csv("./data/PHDGP_ACS2014_2018_tract.csv") %>%
  mutate(GEOID = as.character(GEOID))

dep_index <- 'https://github.com/cole-brokamp/dep_index/raw/master/ACS_deprivation_index_by_census_tracts.rds' %>%
  url() %>%
  gzcon() %>%
  readRDS() %>%
  as_tibble()

d_tract <- d_tract %>%
  left_join(dep_index, by = c('GEOID' = 'census_tract_fips'))

d_tests <- read_csv("./data/CCHMC 05.23.2020.csv")

names(d_tests)

tests_by_tract <- d_tests %>%
  mutate(FIPS = as.character(FIPS)) %>%
  group_by(FIPS, `Result Determination`) %>%
  summarize(positive_tests = n())

tests_by_tract <- tests_by_tract %>%
  ungroup() %>%
  group_by(FIPS) %>%
  mutate(total_tests = sum(positive_tests)) %>%
  filter(`Result Determination` == "POSITIVE") %>%
  select(-`Result Determination`)

d_tract <- d_tract %>%
  left_join(tests_by_tract, by = c('GEOID' = 'FIPS'))

d_tract <- hc_tracts %>%
  left_join(d_tract, by = "GEOID")

# v <- tidycensus::load_variables(2018, "acs5")

pop_age <- tidycensus::get_acs(geography = 'tract',
                               variables = c("B01001_003", "B01001_004", "B01001_005",
                                             "B01001_006", "B01001_007", "B01001_008",
                                             "B01001_009", "B01001_010", "B01001_011",
                                             "B01001_012", "B01001_013", "B01001_014",
                                             "B01001_015", "B01001_016", "B01001_017",
                                             "B01001_018", "B01001_019", "B01001_020",
                                             "B01001_021", "B01001_022", "B01001_023",
                                             "B01001_024", "B01001_025",
                                             "B01001_027", "B01001_028", "B01001_029",
                                             "B01001_030", "B01001_031", "B01001_032",
                                             "B01001_033", "B01001_034", "B01001_035",
                                             "B01001_036", "B01001_037", "B01001_038",
                                             "B01001_039", "B01001_040", "B01001_041",
                                             "B01001_042", "B01001_043", "B01001_044",
                                             "B01001_045", "B01001_046", "B01001_047",
                                             "B01001_048", "B01001_049"),
                               state = "OH", county = "Hamilton")

pop_age <- pop_age %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable,
              values_from = estimate) %>%
  mutate(pop_under_18 = B01001_003 + B01001_004 + B01001_005 + B01001_006 +
                        B01001_027 + B01001_028 + B01001_029 + B01001_030,
         pop_over_50 = B01001_016 + B01001_017 + B01001_018 + B01001_019 + B01001_020 +
                       B01001_021 + B01001_022 + B01001_023 + B01001_024 + B01001_025 +
                       B01001_040 + B01001_041 + B01001_042 + B01001_043 + B01001_044 +
                       B01001_045 + B01001_046 + B01001_047 + B01001_048 + B01001_049,
         pop_over_65 = B01001_020 + B01001_021 + B01001_022 + B01001_023 + B01001_024 + B01001_025 +
                       B01001_044 + B01001_045 + B01001_046 + B01001_047 + B01001_048 + B01001_049,
         pop_over_80 = B01001_024 + B01001_025 + B01001_048 + B01001_049) %>%
  select(GEOID, pop_under_18, pop_over_50, pop_over_65, pop_over_80)

d_tract <- d_tract %>%
  right_join(pop_age, by = 'GEOID')

mapview::mapview(d_tract)

tract_to_neigh <- readRDS('tract_to_neighborhood.rds')

d_neigh <- d_tract %>%
  mutate(crowding_num = crowding * pop_total,
         percColor_num = percColor * pop_total,
         ice_num = ICEwnhinc * pop_total) %>%
  left_join(tract_to_neigh, by = c('GEOID' = 'fips_tract_id')) %>%
  group_by(neighborhood) %>%
  summarize_at(vars(ALAND, pop_total:pop_api, positive_tests:pop_over_80,
                    crowding_num, percColor_num, ice_num),
               sum, na.rm = TRUE)

d_neigh_avgs <- d_tract %>%
  st_drop_geometry() %>%
  left_join(tract_to_neigh, by = c('GEOID' = 'fips_tract_id')) %>%
  group_by(neighborhood) %>%
  summarize_at(vars(fraction_assisted_income:dep_index), mean, na.rm = TRUE)

d_neigh <- d_neigh %>%
  left_join(d_neigh_avgs, by = 'neighborhood') %>%
  mutate(positive_per_1000pop = positive_tests/pop_total * 1000,
         tests_per_1000pop = total_tests/pop_total * 1000,
         positive_per_tests = positive_tests/total_tests,
         pop_density = pop_total/ALAND*1000000,
         crowding = crowding_num/pop_total,
         percColor = percColor_num/pop_total,
         ICEwnhinc = ice_num/pop_total) %>%
  select(neighborhood,
         positive_per_1000pop, tests_per_1000pop, positive_per_tests,
         percColor, ICEwnhinc,
         pop_total, pop_under_18, pop_over_50, pop_over_65, pop_over_80,
         pop_density, crowding, fraction_assisted_income:dep_index)


saveRDS(d_neigh, "./data/covid_neighborhood_data.rds")
aws.s3::s3saveRDS(d_neigh, "s3://geomarker/covid_testing_locations/covid_neighborhood_data.rds")
d_neigh <- aws.s3::s3readRDS("s3://geomarker/covid_testing_locations/covid_neighborhood_data.rds")

d_tract <- d_tract %>%
  mutate(positive_per_1000pop = positive_tests/pop_total * 1000,
         tests_per_1000pop = total_tests/pop_total * 1000,
         positive_per_tests = positive_tests/total_tests,
         pop_density = pop_total/ALAND*1000000) %>%
  select(census_tract_fips = GEOID,
         positive_per_1000pop, tests_per_1000pop, positive_per_tests,
         percBlack, percHisp, percColor, ICEwnhinc,
         pop_total, pop_under_18, pop_over_50, pop_over_65, pop_over_80,
         pop_density, crowding, poverty, fraction_assisted_income:dep_index)



saveRDS(d_tract, "./data/covid_tract_data.rds")
aws.s3::s3saveRDS(d_tract, "s3://geomarker/covid_testing_locations/covid_tract_data.rds")


d <- read_csv("./data/ltcf_geocoded.csv")
aws.s3::s3saveRDS(ltcf, "s3://geomarker/covid_testing_locations/ltcf.rds")

nursing_homes <- read_csv('./data/Nursing Homes-Grid view_geocoded.csv')
aws.s3::s3saveRDS(nursing_homes, "s3://geomarker/covid_testing_locations/nursing_homes.rds")

prisons_jails <- read_csv('./data/Prisons_Jails-Grid view_geocoded.csv')
aws.s3::s3saveRDS(prisons_jails, "s3://geomarker/covid_testing_locations/prisons_jails.rds")

schools_reccenters <- read_csv('./data/covid_resource_schools_rec_centers_geocoded.csv')
aws.s3::s3saveRDS(schools_reccenters, "s3://geomarker/covid_testing_locations/schools_reccenters.rds")

shelters <- read_csv('./data/Shelter_Listings_geocoded.csv')
aws.s3::s3saveRDS(shelters, "s3://geomarker/covid_testing_locations/shelters.rds")

cmha <- read_csv('./data/cmha_addresses_geocoded.csv')
aws.s3::s3saveRDS(cmha, "s3://geomarker/covid_testing_locations/cmha.rds")

