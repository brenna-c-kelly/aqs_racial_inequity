
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

# get area, to calculate population density
acs$area <- st_area(acs)
acs$area_km <- acs$area / 1000
acs$pop_density <- acs$total / acs$area_km

tm_shape(acs) +
  tm_polygons(col = "pop_density", lwd = 0)

# merge by geoid
acs <- merge(acs_geom, acs, by = "geoid")

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

## spatial setup
# nb <- poly2nb(acs)
# nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "/Users/brenna/Downloads/map.adj")

acs$idarea <- 1:nrow(acs)
acs$idarea_2 <- 1:nrow(acs)


acs$pop_wt_poll <- acs$total_1k * acs$n_sources

d$SIR <- d$n_lower_co / d$E
tm_shape(d) +
  tm_polygons(col = "SIR", lwd = 0, style = "cont")
summary(d$SIR)

## model(s)
# the outcomes are a combination of n_ (count) lower/upper (range of measurement scale), and the pollutant
inla_results_pois <- list()

# inla(n_lower_o3 ~ hisp_perc + black_perc + 
#        aian_perc + asian_perc + nhpi_perc + 
#        other_perc + tom_perc + offset(total_1k) + 
#        f(idarea, model = "bym2", graph = g), 
#      data = acs, family = "poisson",
#      control.inla = list(int.strategy = "eb"),
#      control.compute = list(dic = TRUE, waic = TRUE))

start = Sys.time()

prior <- list(
  phi = list(
    prior = "pc",
    param = 1.000)
)

acs$total_1k_poll <- acs$total_1k + acs$n_sources

E <- expected(
  population = acs$total,
  cases = acs$n_lower_co,
  n.strata = 1
)

# plot(aggregate(acs$n_upper_co, by = list(acs$n_sources), FUN = mean))
# plot(aggregate(acs$n_upper_no2, by = list(acs$n_sources), FUN = mean))
# plot(aggregate(acs$n_upper_o3, by = list(acs$n_sources), FUN = mean))
# plot(aggregate(acs$n_upper_pb, by = list(acs$n_sources), FUN = mean))
# plot(aggregate(acs$n_upper_pm, by = list(acs$n_sources), FUN = mean))
# plot(aggregate(acs$n_upper_so2, by = list(acs$n_sources), FUN = mean))
# 
# plot(acs$total, acs$n_lower_co)

Y <- acs$hisp / sum(acs$hisp)
X <- acs$white / sum(acs$white)
ID <- 0.5 * sum(abs(Y - X))
round(ID, 3)
library(MLID)
install.packages("MLID")
mlm <- lmer(n_upper_o3 ~ 0 + (1|hisp) + (1|black) + (1|aian) + (1|asian) +
              (1|nhpi) + (1|other) + (1|tom), data=acs)
summary(mlm)


ncounties <- length(unique(acs$geoid))
countiesE <- rep(unique(acs$geoid))

dE <- data.frame(county = countiesE, 
                 E = E)
d <- merge(acs, dE, by.x = "geoid", by.y = "county")

inla_results_pois[["inla_co_l"]] <- inla(n_lower_co ~ hisp_log + black_log + 
                                           aian_log + asian_log + nhpi_log + 
                                           other_log + tom_log,
                                         f(idarea, model = "bym2", graph = g,
                                           hyper = prior), 
                                         E = E,
                                         data = d, family = "nbinomial",
                                         control.inla = list(int.strategy = "eb"),
                                         control.compute = list(dic = TRUE, waic = TRUE),
                                         verbose = FALSE)
E <- expected(
  population = acs$total,
  cases = acs$n_upper_co,
  n.strata = 1
)

ncounties <- length(unique(acs$geoid))
countiesE <- rep(unique(acs$geoid))

dE <- data.frame(county = countiesE, 
                 E = E)
d <- merge(acs, dE, by.x = "geoid", by.y = "county")

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
E <- expected(
  population = acs$total,
  cases = acs$n_lower_no2,
  n.strata = 1
)

ncounties <- length(unique(acs$geoid))
countiesE <- rep(unique(acs$geoid))

dE <- data.frame(county = countiesE, 
                 E = E)
d <- merge(acs, dE, by.x = "geoid", by.y = "county")

inla_results_pois[["inla_no2_l"]] <- inla(n_lower_no2 ~ hisp_log + black_log + 
                                           aian_log + asian_log + nhpi_log + 
                                           other_log + tom_log,
                                         f(idarea, model = "bym2", graph = g,
                                           hyper = prior), 
                                         E = E,
                                         data = d, family = "nbinomial",
                                         control.inla = list(int.strategy = "eb"),
                                         control.compute = list(dic = TRUE, waic = TRUE),
                                         verbose = TRUE)
E <- expected(
  population = acs$total,
  cases = acs$n_upper_no2,
  n.strata = 1
)

ncounties <- length(unique(acs$geoid))
countiesE <- rep(unique(acs$geoid))

dE <- data.frame(county = countiesE, 
                 E = E)
d <- merge(acs, dE, by.x = "geoid", by.y = "county")

inla_results_pois[["inla_no2_u"]] <- inla(n_upper_no2 ~ hisp_log + black_log + 
                                           aian_log + asian_log + nhpi_log + 
                                           other_log + tom_log,
                                         f(idarea, model = "bym2", graph = g,
                                           hyper = prior), 
                                         E = E,
                                         data = d, family = "nbinomial",
                                         control.inla = list(int.strategy = "eb"),
                                         control.compute = list(dic = TRUE, waic = TRUE),
                                         verbose = TRUE)
E <- expected(
  population = acs$total,
  cases = acs$n_lower_pm,
  n.strata = 1
)

ncounties <- length(unique(acs$geoid))
countiesE <- rep(unique(acs$geoid))

dE <- data.frame(county = countiesE, 
                 E = E)
d <- merge(acs, dE, by.x = "geoid", by.y = "county")

inla_results_pois[["inla_pm_l"]] <- inla(n_lower_pm ~ hisp_log + black_log + 
                                           aian_log + asian_log + nhpi_log + 
                                           other_log + tom_log,
                                         f(idarea, model = "bym2", graph = g,
                                           hyper = prior), 
                                         E = E,
                                         data = d, family = "nbinomial",
                                         control.inla = list(int.strategy = "eb"),
                                         control.compute = list(dic = TRUE, waic = TRUE),
                                         verbose = TRUE)
E <- expected(
  population = acs$total,
  cases = acs$n_upper_pm,
  n.strata = 1
)

ncounties <- length(unique(acs$geoid))
countiesE <- rep(unique(acs$geoid))

dE <- data.frame(county = countiesE, 
                 E = E)
d <- merge(acs, dE, by.x = "geoid", by.y = "county")

inla_results_pois[["inla_pm_u"]] <- inla(n_upper_pm ~ hisp_log + black_log + 
                                           aian_log + asian_log + nhpi_log + 
                                           other_log + tom_log,
                                         f(idarea, model = "bym2", graph = g,
                                           hyper = prior), 
                                         E = E,
                                         data = d, family = "poisson",
                                         control.inla = list(int.strategy = "eb"),
                                         control.compute = list(dic = TRUE, waic = TRUE),
                                         verbose = FALSE)
E <- expected(
  population = acs$total,
  cases = acs$n_lower_pb,
  n.strata = 1
)

ncounties <- length(unique(acs$geoid))
countiesE <- rep(unique(acs$geoid))

dE <- data.frame(county = countiesE, 
                 E = E)
d <- merge(acs, dE, by.x = "geoid", by.y = "county")

# summary(glm.nb(n_lower_pb ~ 1, data = acs))

inla_results_pois[["inla_pb_l"]] <- inla(n_lower_pb ~ hisp_log + black_log + 
                                           aian_log + asian_log + nhpi_log + 
                                           other_log + tom_log,
                                         f(idarea, model = "bym2", graph = g,
                                           hyper = prior), 
                                         E = E,
                                         data = d, family = "nbinomial",
                                         control.inla = list(int.strategy = "eb"),
                                         control.compute = list(dic = TRUE, waic = TRUE),
                                         verbose = TRUE)
E <- expected(
  population = acs$total,
  cases = acs$n_upper_pb,
  n.strata = 1
)

ncounties <- length(unique(acs$geoid))
countiesE <- rep(unique(acs$geoid))

dE <- data.frame(county = countiesE, 
                 E = E)
d <- merge(acs, dE, by.x = "geoid", by.y = "county")

inla_results_pois[["inla_pb_u"]] <- inla(n_upper_pb ~ hisp_log + black_log + 
                                           aian_log + asian_log + nhpi_log + 
                                           other_log + tom_log,
                                         f(idarea, model = "bym2", graph = g,
                                           hyper = prior),
                                         #E = E, 
                                         data = d, family = "nbinomial",
                                         control.inla = list(int.strategy = "eb"),
                                         control.compute = list(dic = TRUE, waic = TRUE),
                                         verbose = FALSE)
E <- expected(
  population = acs$total,
  cases = acs$n_lower_so2,
  n.strata = 1
)

ncounties <- length(unique(acs$geoid))
countiesE <- rep(unique(acs$geoid))

dE <- data.frame(county = countiesE, 
                 E = E)
d <- merge(acs, dE, by.x = "geoid", by.y = "county")

inla_results_pois[["inla_so2_l"]] <- inla(n_lower_so2 ~ hisp_log + black_log + 
                                           aian_log + asian_log + nhpi_log + 
                                            other_log + tom_log,
                                          f(idarea, model = "bym2", graph = g,
                                            hyper = prior), 
                                          E = E,
                                          data = d, family = "nbinomial",
                                          control.inla = list(int.strategy = "eb"),
                                          control.compute = list(dic = TRUE, waic = TRUE),
                                          verbose = TRUE)
E <- expected(
  population = acs$total,
  cases = acs$n_upper_so2,
  n.strata = 1
)

ncounties <- length(unique(acs$geoid))
countiesE <- rep(unique(acs$geoid))

dE <- data.frame(county = countiesE, 
                 E = E)
d <- merge(acs, dE, by.x = "geoid", by.y = "county")

inla_results_pois[["inla_so2_u"]] <- inla(n_upper_so2 ~ hisp_log + black_log + 
                                            aian_log + asian_log + nhpi_log + 
                                            other_log + tom_log,
                                          f(idarea, model = "bym2", graph = g,
                                            hyper = prior), 
                                          E = E,
                                          data = d, family = "nbinomial",
                                          control.inla = list(int.strategy = "eb"),
                                          control.compute = list(dic = TRUE, waic = TRUE),
                                          verbose = TRUE)
E <- expected(
  population = acs$total,
  cases = acs$n_lower_o3,
  n.strata = 1
)

ncounties <- length(unique(acs$geoid))
countiesE <- rep(unique(acs$geoid))

dE <- data.frame(county = countiesE, 
                 E = E)
d <- merge(acs, dE, by.x = "geoid", by.y = "county")

inla_results_pois[["inla_o3_l"]] <- inla(n_lower_o3 ~ hisp_log + black_log + 
                                           aian_log + asian_log + nhpi_log + 
                                           other_log + tom_log,
                                         f(idarea, model = "bym2", graph = g,
                                           hyper = prior), 
                                         E = E,
                                         data = d, family = "nbinomial",
                                         control.inla = list(int.strategy = "eb"),
                                         control.compute = list(dic = TRUE, waic = TRUE),
                                         verbose = TRUE)
E <- expected(
  population = acs$total,
  cases = acs$n_upper_o3,
  n.strata = 1
)

ncounties <- length(unique(acs$geoid))
countiesE <- rep(unique(acs$geoid))

dE <- data.frame(county = countiesE, 
                 E = E)
d <- merge(acs, dE, by.x = "geoid", by.y = "county")

inla_results_pois[["inla_o3_u"]] <- inla(n_upper_o3 ~ hisp_log + black_log + 
                                           aian_log + asian_log + nhpi_log + 
                                           other_log + tom_log,
                                         f(idarea, model = "bym2", graph = g,
                                           hyper = prior), 
                                         E = E,
                                         data = d, family = "nbinomial",
                                         control.inla = list(int.strategy = "eb"),
                                         control.compute = list(dic = TRUE, waic = TRUE),
                                         verbose = TRUE)

end = Sys.time()

end - start

summary(inla_results_pois[["inla_pm_l"]])


saveRDS(inla_results_pois, file = "data/inla_results_pois.rds")

list.files()

## these run with a warning about the number of threads needed
# null model
runs_w_warning <- inla(n_lower_o3 ~ 1,
                       data = acs, family = "poisson",
                       control.inla = list(int.strategy = "eb"),
                       control.compute = list(dic = TRUE, waic = TRUE),
                       control.predictor = list(compute = TRUE))
# offset
runs_w_warning <- inla(n_lower_o3 ~ offset(total),
                       data = acs, family = "poisson",
                       control.inla = list(int.strategy = "eb"),
                       control.compute = list(dic = TRUE, waic = TRUE),
                       control.predictor = list(compute = TRUE))
# desired covariates and offset
runs_w_warning <- inla(n_lower_o3 ~ hisp_perc + black_perc + 
                         aian_perc + asian_perc + nhpi_perc + 
                         other_perc + tom_perc,# + offset(total_1k), # + 
                       #f(idarea, model = "bym2", graph = g) +
                       #f(idarea_2, model = "iid"), 
                       data = acs, family = "poisson",
                       control.inla = list(int.strategy = "eb"),
                       control.compute = list(dic = TRUE, waic = TRUE),
                       control.predictor = list(compute = TRUE))

## these give excessive warnings about threads and (maybe) never finish running






# covariates and spatial fx
system.time(doesnt_run <- inla(n_lower_o3 ~ hisp_perc + black_perc + 
                                 aian_perc + asian_perc + nhpi_perc + 
                                 other_perc + tom_perc + offset(total_1k) + 
                                 f(idarea, model = "bym2", graph = g), 
                               data = acs, family = "poisson",
                               control.inla = list(int.strategy = "eb"),
                               control.compute = list(dic = TRUE, waic = TRUE)))


doesnt_run <- inla(n_lower_o3 ~ hisp_perc + black_perc + 
                     aian_perc + asian_perc + nhpi_perc + 
                     other_perc + tom_perc + total_1k + 
                     f(idarea, model = "bym2", graph = g) +
                     f(idarea_2, model = "iid"), 
                   data = acs, family = "poisson",
                   control.inla = list(int.strategy = "eb"),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(compute = TRUE))

# no covariates, w/ spatial fx
doesnt_run <- inla(n_lower_o3 ~ 1 + 
                     f(idarea, model = "bym2", graph = g) +
                     f(idarea_2, model = "iid"), 
                   data = acs, family = "poisson",
                   control.inla = list(int.strategy = "eb"),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.predictor = list(compute = TRUE))