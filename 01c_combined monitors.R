
# setwd("/uufs/chpc.utah.edu/common/home/brewer-delphi/aqs_inequality")

library(sp)
library(sf)
library(tmap)
library(INLA)
library(spdep)
library(dplyr)
library(stringr)
library(ggplot2)
library(SpatialEpi)


## little bit of cleaning
acs <- read.csv("/Users/brenna/Downloads/7020_data_count_poll.csv")

acs$geoid <- str_pad(acs$geoid, width = 12, pad = "0", side = "left")

acs_geom <- st_read("/Users/brenna/Downloads/7020_geom.shp") |>
  filter(!duplicated(geoid))

# merge by geoid
acs <- merge(acs_geom, acs, by = "geoid")

# get area, to calculate population density
acs$area <- st_area(acs)
acs$area_km <- acs$area / 1000
acs$pop_density <- acs$total / acs$area_km

# tm_shape(acs) +
#   tm_polygons(col = "pop_density", lwd = 0)

# drop empty geometries
acs <- acs[which(st_is_empty(acs) == FALSE), ]
# 
# # drop areas with no population
empty_areas <- acs[which(is.na(acs$hisp_p)), ]
acs <- acs[which(!acs$geoid %in% empty_areas$geoid), ]

# scale 0-100
acs <- acs |>
  mutate(hisp_perc = ifelse(is.na(hisp_p), 0, hisp_p*100)) |>
  mutate(black_perc = ifelse(is.na(black_p), 0, black_p*100)) |>
  mutate(aian_perc = ifelse(is.na(aian_p), 0, aian_p*100)) |>
  mutate(asian_perc = ifelse(is.na(asian_p), 0, asian_p*100)) |>
  mutate(nhpi_perc = ifelse(is.na(nhpi_p), 0, nhpi_p*100)) |>
  mutate(other_perc = ifelse(is.na(other_p), 0, other_p*100)) |>
  mutate(tom_perc = ifelse(is.na(tom_p), 0, tom_p*100)) |>
  mutate(state = str_sub(geoid, start = 0, end = 2)) |>
  mutate(total_1k = total / 1000) |>
  # logging
  mutate(hisp_log = log(hisp_p + 0.01)) |>
  mutate(black_log = log(black_p + 0.01)) |>
  mutate(aian_log = log(aian_p + 0.01)) |>
  mutate(asian_log = log(asian_p + 0.01)) |>
  mutate(nhpi_log = log(nhpi_p + 0.01)) |>
  mutate(other_log = log(other_p + 0.01)) |>
  mutate(tom_log = log(tom_p + 0.01))

acs$n_total_u <- sum(acs$n_upper_pm, acs$n_upper_co,
                     acs$n_upper_so2, acs$n_upper_no2,
                     acs$n_upper_pb, acs$n_upper_o3)

# geom
g <- inla.read.graph(filename = "/Users/brenna/Downloads/map.adj")

acs$idarea <- 1:nrow(acs)
acs$idarea_2 <- 1:nrow(acs)

E <- expected(
  population = acs$total,
  cases = acs$n_total_u,
  n.strata = 1
)

prior <- list(
  phi = list(
    prior = "pc",
    param = 1.000)
)

ncounties <- length(unique(acs$geoid))
countiesE <- rep(unique(acs$geoid))

dE <- data.frame(county = countiesE, 
                 E = E)
d <- merge(acs, dE, by.x = "geoid", by.y = "county")

# let's model
combined_l <- inla(n_total_u ~ hisp_log + black_log + 
                     aian_log + asian_log + nhpi_log + 
                     other_log + tom_log,
                   f(idarea, model = "bym2", graph = g,
                     hyper = prior), 
                   E = E,
                   data = d, family = "nbinomial",
                   control.inla = list(int.strategy = "eb"),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   verbose = TRUE)

summary(combined_l)


inla_results_pois[["inla_co_u"]] <- inla(n_upper_co ~ hisp_log + black_log + 
                                           aian_log + asian_log + nhpi_log + 
                                           other_log + tom_log,
                                         f(idarea, model = "bym2", graph = g,
                                           hyper = prior), 
                                         E = E,
                                         data = d, family = "nbinomial",
                                         control.inla = list(int.strategy = "eb"),
                                         control.compute = list(dic = TRUE, waic = TRUE),
                                         verbose = TRUE)