
setwd("/uufs/chpc.utah.edu/common/home/u1136804/ondemand")

## setup
library(sp)
library(sf)
# library(tmap)
library(INLA)
# library(Matrix)
# library(MatrixModels)
library(spdep)
library(dplyr)
# library(moments)
library(ggplot2)
library(stringr)
# library(regclass)

## data

acs <- read.csv("data/7020_data_count.csv")

acs$geoid <- str_pad(acs$geoid, width = 12, pad = "0", side = "left")

acs_geom <- st_read("data/7020_geom.shp")
# merge by geoid
acs <- merge(acs_geom, acs, by = "geoid")

# drop empty geometries
acs <- acs[which(st_is_empty(acs) == FALSE), ]
# drop areas with no population
empty_areas <- acs[which(is.na(acs$hisp_p)), ]
acs <- acs[which(!acs$geoid %in% empty_areas$geoid), ]

# fix poverty (some have 0)
acs$pov_p <- ifelse(is.na(acs$pov_p), 0 , acs$pov_p)

# scale 0-100
acs <- acs |>
  mutate(hisp_perc = hisp_p*100) |>
  mutate(black_perc = black_p*100) |>
  mutate(aian_perc = aian_p*100) |>
  mutate(asian_perc = asian_p*100) |>
  mutate(nhpi_perc = nhpi_p*100) |>
  mutate(other_perc = other_p*100) |>
  mutate(tom_perc = tom_p*100) |>
  mutate(pov_perc = pov_p*100)
acs <- acs |>
  mutate(hisp_p_log = log(hisp_perc + min(acs$hisp_perc[which(acs$hisp_perc > 0)]))) |>
  mutate(black_p_log = log(black_perc + min(acs$black_perc[which(acs$black_perc > 0)]))) |>
  mutate(aian_p_log = log(aian_perc + min(acs$aian_perc[which(acs$aian_perc > 0)]))) |>
  mutate(asian_p_log = log(asian_perc + min(acs$asian_perc[which(acs$asian_perc > 0)]))) |>
  mutate(nhpi_p_log = log(nhpi_perc + min(acs$nhpi_perc[which(acs$nhpi_perc > 0)]))) |>
  mutate(other_p_log = log(other_perc + min(acs$other_perc[which(acs$other_perc > 0)]))) |>
  mutate(tom_p_log = log(tom_perc + min(acs$tom_perc[which(acs$tom_perc > 0)]))) |>
  mutate(pov_p_log = log(pov_perc + min(acs$pov_perc[which(acs$pov_perc > 0)]))) |>
  mutate(total_1k = total / 1000)

acs$state <- str_sub(acs$geoid, start = 0, end = 2)

acs$total_centered <- acs$total_1k - mean(acs$total_1k)
acs$total_1k_log <- log(acs$total_1k)

## spatial structure
nb <- poly2nb(acs)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")

acs$idarea <- 1:nrow(acs)
acs$idarea_2 <- 1:nrow(acs)


## models

inla_results_pois <- list()

system.time(
  inla_results_pois[["inla_co_l"]] <- inla(n_lower_co ~ hisp_p_log + black_p_log + 
                                             aian_p_log + asian_p_log + nhpi_p_log + 
                                             other_p_log + tom_p_log + total_1k + 
                                             f(idarea, model = "bym2", graph = g, scale.model = TRUE) +
                                             f(idarea_2, model = "iid"), 
                                           data = acs, family = "poisson",
                                           control.inla= list(int.strategy = "eb"),
                                           control.compute = list(dic = TRUE, waic = TRUE),
                                           control.predictor = list(compute = TRUE))
)
inla_results_pois[["inla_co_u"]] <- inla(n_upper_co ~ hisp_p_log + black_p_log + 
                                           aian_p_log + asian_p_log + nhpi_p_log + 
                                           other_p_log + tom_p_log + total_1k_log +
                                           f(idarea, model = "bym2", graph = g, scale.model = TRUE) +
                                           f(idarea_2, model = "iid"), 
                                         data = acs, family = "poisson",
                                         control.inla= list(int.strategy = "eb"),
                                         control.compute = list(dic = TRUE, waic = TRUE),
                                         control.predictor = list(compute = TRUE))

inla_results_pois[["inla_no2_l"]] <- inla(n_lower_no2 ~ hisp_p_log + black_p_log + 
                                            aian_p_log + asian_p_log + nhpi_p_log + 
                                            other_p_log + tom_p_log + total_1k_log +
                                            f(idarea, model = "bym2", graph = g, scale.model = TRUE) +
                                            f(idarea_2, model = "iid"), 
                                          data = acs, family = "poisson",
                                          control.inla= list(int.strategy = "eb"),
                                          control.compute = list(dic = TRUE, waic = TRUE),
                                          control.predictor = list(compute = TRUE))
inla_results_pois[["inla_no2_u"]] <- inla(n_upper_no2 ~ hisp_p_log + black_p_log + 
                                            aian_p_log + asian_p_log + nhpi_p_log + 
                                            other_p_log + tom_p_log + total_1k_log +
                                            f(idarea, model = "bym2", graph = g, scale.model = TRUE) +
                                            f(idarea_2, model = "iid"), 
                                          data = acs, family = "poisson",
                                          control.inla= list(int.strategy = "eb"),
                                          control.compute = list(dic = TRUE, waic = TRUE),
                                          control.predictor = list(compute = TRUE))

inla_results_pois[["inla_pm_l"]] <- inla(n_lower_pm ~ hisp_p_log + black_p_log + 
                                           aian_p_log + asian_p_log + nhpi_p_log + 
                                           other_p_log + tom_p_log + total_1k_log +
                                           f(idarea, model = "bym2", graph = g, scale.model = TRUE) +
                                           f(idarea_2, model = "iid"), 
                                         data = acs, family = "poisson",
                                         control.inla= list(int.strategy = "eb"),
                                         control.compute = list(dic = TRUE, waic = TRUE),
                                         control.predictor = list(compute = TRUE))
inla_results_pois[["inla_pm_u"]] <- inla(n_upper_pm ~ hisp_p_log + black_p_log + 
                                           aian_p_log + asian_p_log + nhpi_p_log + 
                                           other_p_log + tom_p_log + total_1k_log +
                                           f(idarea, model = "bym2", graph = g, scale.model = TRUE) +
                                           f(idarea_2, model = "iid"), 
                                         data = acs, family = "poisson",
                                         control.inla= list(int.strategy = "eb"),
                                         control.compute = list(dic = TRUE, waic = TRUE),
                                         control.predictor = list(compute = TRUE))

inla_results_pois[["inla_pb_l"]] <- inla(n_lower_pb ~ hisp_p_log + black_p_log + 
                                           aian_p_log + asian_p_log + nhpi_p_log + 
                                           other_p_log + tom_p_log + total_1k_log +
                                           f(idarea, model = "bym2", graph = g, scale.model = TRUE) +
                                           f(idarea_2, model = "iid"), 
                                         data = acs, family = "poisson",
                                         control.inla= list(int.strategy = "eb"),
                                         control.compute = list(dic = TRUE, waic = TRUE),
                                         control.predictor = list(compute = TRUE))
inla_results_pois[["inla_pb_u"]] <- inla(n_upper_pb ~ hisp_p_log + black_p_log + 
                                           aian_p_log + asian_p_log + nhpi_p_log + 
                                           other_p_log + tom_p_log + total_1k_log +
                                           f(idarea, model = "bym2", graph = g, scale.model = TRUE) +
                                           f(idarea_2, model = "iid"), 
                                         data = acs, family = "poisson",
                                         control.inla= list(int.strategy = "eb"),
                                         control.compute = list(dic = TRUE, waic = TRUE),
                                         control.predictor = list(compute = TRUE))

inla_results_pois[["inla_so2_l"]] <- inla(n_lower_so2 ~ hisp_p_log + black_p_log + 
                                            aian_p_log + asian_p_log + nhpi_p_log + 
                                            other_p_log + tom_p_log + total_1k_log +
                                            f(idarea, model = "bym2", graph = g, scale.model = TRUE) +
                                            f(idarea_2, model = "iid"), 
                                          data = acs, family = "poisson",
                                          control.inla= list(int.strategy = "eb"),
                                          control.compute = list(dic = TRUE, waic = TRUE),
                                          control.predictor = list(compute = TRUE))
inla_results_pois[["inla_so2_u"]] <- inla(n_upper_so2 ~ hisp_p_log + black_p_log + 
                                            aian_p_log + asian_p_log + nhpi_p_log + 
                                            other_p_log + tom_p_log + total_1k_log +
                                            f(idarea, model = "bym2", graph = g, scale.model = TRUE) +
                                            f(idarea_2, model = "iid"), 
                                          data = acs, family = "poisson",
                                          control.inla= list(int.strategy = "eb"),
                                          control.compute = list(dic = TRUE, waic = TRUE),
                                          control.predictor = list(compute = TRUE))

inla_results_pois[["inla_o3_l"]] <- inla(n_lower_o3 ~ hisp_p_log + black_p_log + 
                                           aian_p_log + asian_p_log + nhpi_p_log + 
                                           other_p_log + tom_p_log + total_1k_log +
                                           f(idarea, model = "bym2", graph = g, scale.model = TRUE) +
                                           f(idarea_2, model = "iid"), 
                                         data = acs, family = "poisson",
                                         control.inla= list(int.strategy = "eb"),
                                         control.compute = list(dic = TRUE, waic = TRUE),
                                         control.predictor = list(compute = TRUE))
inla_results_pois[["inla_o3_u"]] <- inla(n_upper_o3 ~ hisp_p_log + black_p_log + 
                                           aian_p_log + asian_p_log + nhpi_p_log + 
                                           other_p_log + tom_p_log + total_1k_log +
                                           f(idarea, model = "bym2", graph = g, scale.model = TRUE) +
                                           f(idarea_2, model = "iid"), 
                                         data = acs, family = "poisson",
                                         control.inla= list(int.strategy = "eb"),
                                         control.compute = list(dic = TRUE, waic = TRUE),
                                         control.predictor = list(compute = TRUE))

## saving
saveRDS(inla_results_pois, file = "data/inla_results.rds")


