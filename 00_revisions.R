
## cleaning
# mons <- read.csv("/Users/brenna/Downloads/aqs_monitors-3.csv")

mons <- mons |>
  filter(Parameter.Code %in% c(44201, # o3
                               42401, # so2
                               42101, # co
                               42602, # no2
                               88101, # pm2.5
                               81102, 86101, # pm10
                               14129, 85129, 12128)) # lead

mons$lat_lon <- paste0(mons$Latitude, mons$Longitude)

mons$criteria <- case_when(mons$Parameter.Code %in% c(14129, 85129, 12128) ~ "pb",
                           mons$Parameter.Code == 42101 ~ "co",
                           mons$Parameter.Code %in% c(88101, 81102, 86101) ~ "pm",
                           mons$Parameter.Code == 42602 ~ "no2",
                           mons$Parameter.Code == 44201 ~ "o3",
                           mons$Parameter.Code == 42401 ~ "so2")

mons <- mons |>
  filter(!is.na(criteria))
mons$criteria <- as.factor(mons$criteria)

mons$last_year <- str_sub(mons$Last.Sample.Date, start = 0, end = 4)
mons$Last.Sample.Date <- ymd(mons$Last.Sample.Date)

###############
# missingness
###############

table(mons$Measurement.Scale.Definition, mons$criteria)

# if there was a measurement within the past 5 years; sites are evaluated every 5 years
#### any year, for gif
# mons <- mons |>
#   filter(last_year %in% c("", "2023", "2022", "2021", "2020", "2019"))

# imputing lat/long based on site number
mons$State.Code <- str_pad(mons$State.Code, pad = "0", width = 2, side = "left")
mons$County.Code <- str_pad(mons$County.Code, pad = "0", width = 3, side = "left")
mons$Site.Number <- str_pad(mons$Site.Number, pad = "0", width = 4, side = "left")

mons$site_code <- paste0(mons$State.Code, mons$County.Code, mons$Site.Number)

missing_sites <- mons[which(is.na(mons$Latitude)), "site_code"]
missing_sites <- mons[which(mons$site_code %in% missing_sites), ]

# the sites with missing lat/lon have addresses
# google maps to the rescue
missing_sites[, c("Address", "State.Name", "site_code")]

# making a dataframe for merge
missing_site_locs <- data.frame(Latitude_m = c(41.759142851119755, 28.847295692743412, 27.997397528059278, 44.301764986058686,
                                               46.33560595983247, 43.636043773953546, 43.61948993912684, 43.579846480926776,
                                               43.60106825076201, 43.631329594263555, 43.61519809470876, 43.25579488107274,
                                               45.95800312479897, 37.27107085242156, 43.636043773953546, 43.636043773953546,
                                               43.636043773953546, 43.636043773953546),
                                Longitude_m = c(-72.6812059865072, -82.47762724105901, -82.68835723156158, -69.76804768078843,
                                                -86.84501913332537, -83.83589061574071, -84.19734680224849, -84.24246288690706,
                                                -84.20426022923483, -84.21565144483327, -84.23534194236433, -86.2416138858149,
                                                -86.2464834021623, -79.94489398712126, -83.83589061574071, -83.83589061574071,
                                                -83.83589061574071, -83.83589061574071),
                                site_code = c("090030019", "120170001", "121032002", "230110015",
                                              "260030901", "260170901", "261110901", "261110904",
                                              "261110905", "261110907", "261110910", "261210905",
                                              "261530901", "511610010", "260170902", "260170903", 
                                              "260170904", "260170905"))


mons <- merge(mons, missing_site_locs, by = c("site_code"), all.x = TRUE)
mons <- mons |>
  filter(!is.na(Latitude)) # remove those with missing lat/lon outside of what was cleaned for past 5 years

mons$Latitude <- ifelse(is.na(mons$Latitude),
                        mons$Latitude_m,
                        mons$Latitude)
mons$Longitude <- ifelse(is.na(mons$Longitude),
                         mons$Longitude_m,
                         mons$Longitude)

# AEA will be used for visualization, but distance-preserving projections like Mercator are needed for buffer calculations
aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"
mons <- st_as_sf(mons, coords = c("Longitude", "Latitude"),
                 crs = 4326, agr = "constant")## |>
#st_transform(3857) # mercator
mons <- st_transform(mons, st_crs(aea))


## monitor duration (?)




## temporal patterns of placement


mons$first_year <- mons$First.Year.of.Data

mons <- mons |>
  filter(!is.na(first_year)) |>
  filter(!is.na(last_year))

mons_year <- split(mons, mons$first_year)


## plots
states <- st_read("/Users/brenna/Documents/School/Coursework/2023 Spring, Summer/GEOG 6960/points, us/us states/states.shp")

states <- states |>
  filter(NAME != "Puerto Rico")
mons <- mons |>
  filter(State.Name != "Puerto Rico")

mons$pollutant <- ifelse(mons$criteria == "co" ~ "CO",
                         mons$criteria == "no2" ~ expression("NO"[2]))
                         # mons$criteria == "o3" ~ "NO{subsc('1')}",
                         # mons$criteria == "pb" ~ "Pb",
                         # mons$criteria == "pm" ~ "PM",
                         # mons$criteria == "so2" ~ "NO{subsc('1')}")

aq_pal <- c("#00a99d", "#ebb140", "#ff2908", 
            "#c58058", "#436042", "#4f5dbc")

years <- data.frame(end = seq(from = 1957, to = 2023))

aq_labs <- c("CO", expression("NO"[2]), expression("O"[3]), "Pb", "PM", expression("SO"[2]))

pdf(file = "JAMA/gif/aqs.pdf")
for(i in 1:nrow(years)) { # nrow(years)
  
  mons_plot <- mons |>
    filter(first_year <= years[i, 1] & 
             last_year >= years[i, 1])
  
  if("pm" %in% unique(mons_plot$criteria) == FALSE) {
    plt_pm <- tm_shape(states) +
      tm_polygons(col = "white", border.col = "black", lwd = 0.25) +
      tm_layout(frame = FALSE, legend.position = c(0.35, 0.2),
                title = "PM", title.position = c(0.65, 0.55), title.size = 1,
                main.title = paste0("EPA Monitors in ", years[i, 1]),
                main.title.position = "center")
  }
  else(plt_pm <- tm_shape(states) +
         tm_polygons(col = "white", border.col = "black", lwd = 0.25) +
         tm_shape(mons_plot[which(mons_plot$criteria == "pm"), ]) + #mons_year[[50]]) +
         tm_dots(col = "#436042", size = 0.05, alpha = 0.4, legend.show = FALSE) +
         tm_layout(frame = FALSE, legend.position = c(0.35, 0.2),
                   title = "PM", title.position = c(0.65, 0.55),
                   main.title = paste0("EPA Monitors in ", years[i, 1]),
                   title.size = 1))
 
  if("o3" %in% unique(mons_plot$criteria) == FALSE) {
    plt_o3 <- tm_shape(states) +
      tm_polygons(col = "white", border.col = "black", lwd = 0.25) +
      tm_layout(frame = FALSE, legend.position = c(0.35, 0.2),
                title = expression("O"[3]), title.position = c(0.65, 0.55),
                title.size = 1)
  }
  else(plt_o3 <- tm_shape(states) +
         tm_polygons(col = "white", border.col = "black", lwd = 0.25) +
         tm_shape(mons_plot[which(mons_plot$criteria == "o3"), ]) + #mons_year[[50]]) +
         tm_dots(col = "#ff2908", size = 0.05, alpha = 0.4, legend.show = FALSE) +
         tm_layout(frame = FALSE, legend.position = c(0.35, 0.2),
                   title = expression("O"[3]), title.position = c(0.65, 0.55),
                   title.size = 1))
  
  if("so2" %in% unique(mons_plot$criteria) == FALSE) {
    plt_so2 <- tm_shape(states) +
      tm_polygons(col = "white", border.col = "black", lwd = 0.25) +
      tm_layout(frame = FALSE, legend.position = c(0.35, 0.2),
                title = expression("SO"[2]), title.position = c(0.65, 0.55),
                title.size = 1)
  }
  else(plt_so2 <- tm_shape(states) +
         tm_polygons(col = "white", border.col = "black", lwd = 0.25) +
         tm_shape(mons_plot[which(mons_plot$criteria == "so2"), ]) + #mons_year[[50]]) +
         tm_dots(col = "#4f5dbc", size = 0.05, alpha = 0.4, legend.show = FALSE) +
         tm_layout(frame = FALSE, legend.position = c(0.35, 0.2),
                   title = expression("SO"[2]), title.position = c(0.65, 0.55),
                   title.size = 1))
  
  if("no2" %in% unique(mons_plot$criteria) == FALSE) {
    plt_no2 <- tm_shape(states) +
      tm_polygons(col = "white", border.col = "black", lwd = 0.25) +
      tm_layout(frame = FALSE, legend.position = c(0.35, 0.2),
                title = expression("NO"[2]), title.position = c(0.65, 0.55),
                title.size = 1)
  }
  else(plt_no2 <- tm_shape(states) +
         tm_polygons(col = "white", border.col = "black", lwd = 0.25) +
         tm_shape(mons_plot[which(mons_plot$criteria == "no2"), ]) + #mons_year[[50]]) +
         tm_dots(col = "#ebb140", size = 0.05, alpha = 0.4, legend.show = FALSE) +
         tm_layout(frame = FALSE, legend.position = c(0.35, 0.2),
                   title = expression("NO"[2]), title.position = c(0.65, 0.55),
                   title.size = 1))
  
  if("pb" %in% unique(mons_plot$criteria) == FALSE) {
    plt_pb <- tm_shape(states) +
      tm_polygons(col = "white", border.col = "black", lwd = 0.25) +
      tm_layout(frame = FALSE, legend.position = c(0.35, 0.2),
                title = "Pb", title.position = c(0.65, 0.55),
                title.size = 1)
  }
  else(plt_pb <- tm_shape(states) +
         tm_polygons(col = "white", border.col = "black", lwd = 0.25) +
         tm_shape(mons_plot[which(mons_plot$criteria == "pb"), ]) + #mons_year[[50]]) +
         tm_dots(col = "#c58058", size = 0.05, alpha = 0.4, legend.show = FALSE) +
         tm_layout(frame = FALSE, legend.position = c(0.35, 0.2),
                   title = "Pb", title.position = c(0.65, 0.55),
                   title.size = 1))
  
  if("co" %in% unique(mons_plot$criteria) == FALSE) {
    plt_co <- tm_shape(states) +
      tm_polygons(col = "white", border.col = "black", lwd = 0.25) +
      tm_layout(frame = FALSE, legend.position = c(0.35, 0.2),
                title = "CO", title.position = c(0.65, 0.55),
                title.size = 1)
  }
  else(plt_co <- tm_shape(states) +
         tm_polygons(col = "white", border.col = "black", lwd = 0.25) +
         tm_shape(mons_plot[which(mons_plot$criteria == "co"), ]) + #mons_year[[50]]) +
         tm_dots(col = "#00a99d", size = 0.05, alpha = 0.4, legend.show = FALSE) +
         tm_layout(frame = FALSE, legend.position = c(0.35, 0.2),
                   title = "CO", title.position = c(0.65, 0.55),
                   title.size = 1))
  
  
  print(
    tmap_arrange(plt_pm, plt_o3, plt_so2,
                 plt_no2, plt_pb, plt_co)
  )
  
}
dev.off()


# tm_shape(states) +
#   tm_polygons(col = "white", border.col = "black", lwd = 0.5) +
#   tm_shape(mons_plot) + #mons_year[[50]]) +
#   tm_dots(col = "criteria", palette = aq_pal, 
#           size = 0.15, alpha = 0.6, legend.show = FALSE) +
#   tm_add_legend(title = "Pollutant", type = "symbol", 
#                 labels = aq_labs, col = aq_pal, shape = 19) +
#   tm_layout(frame = FALSE, legend.position = c(0.35, 0.2),
#             title = paste0("EPA Monitors in ", years[i, 1]),
#             title.position = c(0.55, 0.55))


#############################
# maps of race and ethnicity
#############################
library(sf)
library(tmap)
library(dplyr)
library(stringr)
library(tidycensus)

county_race <- get_acs(geography = "county",
                       variable = c("B03002_001", # total pop, race denom
                                    "B03002_012", # hisp
                                    "B03002_003", # white
                                    "B03002_004", # black
                                    "B03002_005", # aian
                                    "B03002_006", # asian
                                    "B03002_007", # nhpi
                                    "B03002_008", # other
                                    "B03002_009"),# tom
                       geometry = TRUE,
                       year = 2021,
                       output = "wide")

# "#436042", "#ff2908", "#4f5dbc", "#ebb140", "#c58058", "#00a99d"

county_race <- county_race |>
  rename(total = B03002_001E,
         hisp = B03002_012E,
         white = B03002_003E,
         black = B03002_004E,
         aian = B03002_005E,
         asian = B03002_006E,
         nhpi = B03002_007E,
         other = B03002_008E,
         tom = B03002_009E) |>
  filter(total > 0) |>
  mutate(hisp_p = 100 * hisp / total) |>
  mutate(hisp_p = ifelse(is.na(hisp_p), 0, hisp_p)) |>
  mutate(hisp_p_log = log(hisp_p + st_drop_geometry(min(acs[which(acs$hisp_p > 0), "hisp_p"])))) |>
  mutate(white_p = 100 * white / total) |>
  mutate(white_p = ifelse(is.na(white_p), 0, white_p)) |>
  mutate(white_p_log = log(white_p + st_drop_geometry(min(acs[which(acs$white_p > 0), "white_p"])))) |>
  mutate(black_p = 100 * black / total) |>
  mutate(black_p = ifelse(is.na(black_p), 0, black_p)) |>
  mutate(black_p_log = log(black_p + st_drop_geometry(min(acs[which(acs$black_p > 0), "black_p"])))) |>
  mutate(aian_p = 100 * aian / total) |>
  mutate(aian_p = ifelse(is.na(aian_p), 0, aian_p)) |>
  mutate(aian_p_log = log(aian_p + st_drop_geometry(min(acs[which(acs$aian_p > 0), "aian_p"])))) |>
  mutate(asian_p = 100 * asian / total) |>
  mutate(asian_p = ifelse(is.na(asian_p), 0, asian_p)) |>
  mutate(asian_p_log = log(asian_p + st_drop_geometry(min(acs[which(acs$asian_p > 0), "asian_p"])))) |>
  mutate(nhpi_p = 100 * nhpi / total) |>
  mutate(nhpi_p = ifelse(is.na(nhpi_p), 0, nhpi_p)) |>
  mutate(nhpi_p_log = log(nhpi_p + st_drop_geometry(min(acs[which(acs$nhpi_p > 0), "nhpi_p"])))) |>
  mutate(other_p = 100 * other / total) |>
  mutate(other_p = ifelse(is.na(other_p), 0, other_p)) |>
  mutate(other_p_log = log(other_p + st_drop_geometry(min(acs[which(acs$other_p > 0), "other_p"])))) |>
  mutate(tom_p = 100 * tom / total) |>
  mutate(tom_p = ifelse(is.na(tom_p), 0, tom_p)) |>
  mutate(tom_p_log = log(tom_p + st_drop_geometry(min(acs[which(acs$tom_p > 0), "tom_p"]))))


county_race <- county_race |>
  mutate(state = str_sub(GEOID, start = 0, end = 2)) |>
  filter(state != "72")
  
# col_white_col <- colorRampPalette(c(color_x, white, color_y), space = "rgb",
#                                   interpolate = "linear", alpha = FALSE)

aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"

county_race <- st_transform(county_race, crs = aea)

map_hisp <- tm_shape(county_race) +
  tm_polygons(col = "hisp_p", style = "fisher", 
              palette = "magma", lwd = 0,
              title = "% Hispanic \nor Latinx",
              legend.is.portrait = TRUE) +
  tm_layout(frame = FALSE,
            # legend.outside.position = "bottom",
            legend.outside.size = 0.35,
            legend.outside = TRUE)
map_white <- tm_shape(county_race) +
  tm_polygons(col = "white_p", style = "fisher", 
              palette = "magma", lwd = 0,
              title = "% White",
              legend.is.portrait = TRUE) +
  tm_layout(frame = FALSE,
            # legend.outside.position = "bottom",
            legend.outside.size = 0.35,
            legend.outside = TRUE)
map_black <- tm_shape(county_race) +
  tm_polygons(col = "black_p", style = "fisher", 
              palette = "magma", lwd = 0,
              title = "% Black or African \nAmerican",
              legend.is.portrait = TRUE) +
  tm_layout(frame = FALSE,
            # legend.outside.position = "bottom",
            legend.outside.size = 0.35,
            legend.outside = TRUE)
map_aian <- tm_shape(county_race) +
  tm_polygons(col = "aian_p", style = "fisher", 
              palette = "magma", lwd = 0,
              title = "% AIAN",
              legend.is.portrait = TRUE) +
  tm_layout(frame = FALSE,
            # legend.outside.position = "bottom",
            legend.outside.size = 0.35,
            legend.outside = TRUE)
map_asian <- tm_shape(county_race) +
  tm_polygons(col = "asian_p", style = "fisher", 
              palette = "magma", lwd = 0,
              title = "% Asian",
              legend.is.portrait = TRUE) +
  tm_layout(frame = FALSE,
            # legend.outside.position = "bottom",
            legend.outside.size = 0.35,
            legend.outside = TRUE)
map_nhpi <- tm_shape(county_race) +
  tm_polygons(col = "nhpi_p", style = "fisher", 
              palette = "magma", lwd = 0,
              title = "% NHPI",
              legend.is.portrait = TRUE) +
  tm_layout(frame = FALSE,
            # legend.outside.position = "bottom",
            legend.outside.size = 0.35,
            legend.outside = TRUE)
map_other <- tm_shape(county_race) +
  tm_polygons(col = "other_p", style = "fisher", 
              palette = "magma", lwd = 0,
              title = "% Some Other Race",
              legend.is.portrait = TRUE) +
  tm_layout(frame = FALSE,
            # legend.outside.position = "bottom",
            legend.outside.size = 0.35,
            legend.outside = TRUE)
map_tom <- tm_shape(county_race) +
  tm_polygons(col = "tom_p", style = "fisher", 
              palette = "magma", lwd = 0,
              title = "% Two or More Races",
              legend.is.portrait = TRUE) +
  tm_layout(frame = FALSE,
            # legend.outside.position = "bottom",
            legend.outside.size = 0.35,
            legend.outside = TRUE)

tmap_arrange(map_aian, map_asian, map_black,
             map_hisp, map_nhpi, map_other,
             map_tom, map_white, ncol = 2)

#################
#  population 
#################
library(sf)
library(tmap)
library(stringr)

acs <- read.csv("data/7020_data_count.csv")
acs$geoid <- str_pad(acs$geoid, width = 12, pad = "0", side = "left")

acs_geom <- st_read("data/7020_geom.shp")

# merge by geoid
acs <- merge(acs_geom, acs, by = "geoid")

# drop empty geometries
acs <- acs[which(st_is_empty(acs) == FALSE), ]

# map of population variability

# deciles
pop_breaks <- quantile(acs$total, seq(from = 0, to = 1, by = 0.1))
# clean labels
break_labels = paste("≤", as.character(round(pop_breaks, 2)[-1]))
# map with deciles
pdf(file = "population.pdf")
tm_shape(acs, proj = aea) +
  tm_polygons(col = "total", palette = "viridis", breaks = pop_breaks,
              labels = break_labels, lwd = 0, title = "Population") +
  tm_layout(frame = FALSE)
dev.off()



##############
# duration 
##############

library(dplyr)
library(ggplot2)

aqs_19 <- read.csv("/Users/brenna/Downloads/annual_conc_by_monitor_2019.csv")
aqs_20 <- read.csv("/Users/brenna/Downloads/annual_conc_by_monitor_2020.csv")
aqs_21 <- read.csv("/Users/brenna/Downloads/annual_conc_by_monitor_2021-3.csv")
aqs_22 <- read.csv("/Users/brenna/Downloads/annual_conc_by_monitor_2022-2.csv")
aqs_23 <- read.csv("/Users/brenna/Downloads/annual_conc_by_monitor_2023-2.csv")
aqs_24 <- read.csv("/Users/brenna/Downloads/annual_conc_by_monitor_2024.csv")

aqs <- rbind(aqs_19, aqs_20, aqs_21, 
             aqs_22, aqs_23, aqs_24)

aqs$criteria <- case_when(aqs$Parameter.Code %in% c(14129, 85129, 12128) ~ "pb",
                          aqs$Parameter.Code == 42101 ~ "co",
                          aqs$Parameter.Code %in% c(88101, 81102, 86101) ~ "pm",
                          aqs$Parameter.Code == 42602 ~ "no2",
                          aqs$Parameter.Code == 44201 ~ "o3",
                          aqs$Parameter.Code == 42401 ~ "so2")
aqs <- aqs |>
  filter(!is.na(criteria))

ggplot(aqs) + 
  geom_boxplot(aes(x = Year, y = Observation.Percent, 
                   group = Year)) +
  facet_wrap(facets = "criteria")

aqs$State.Code <- str_pad(aqs$State.Code, width = 2, pad = "0", side = "left")
aqs$County.Code <- str_pad(aqs$County.Code, width = 3, pad = "0", side = "left")
aqs$Site.Num <- str_pad(aqs$Site.Num, width = 4, pad = "0", side = "left")
aqs$site_code <- paste0(aqs$State.Code, aqs$County.Code, aqs$Site.Num)

site_obs <- aggregate(aqs$Observation.Percent, by = list(aqs$site_code, aqs$Parameter.Code), FUN = mean) |>
  rename(site_code = Group.1,
         Parameter.Code = Group.2,
         obs_perc = x)



########################
# buffers by duration
########################
library(sf)
library(INLA)
library(dplyr)
library(spdep)
library(tidycensus)
# plotting the buffers
us_states <- get_acs(geography = "state",
                     variables = c("B03002_001"),
                     geometry = TRUE,
                     year = 2021)
us_states <- st_transform(us_states, aea)

head(regional_buffer_upper_by_criteria$o3)

method <- read.csv("JAMA/methods_all.csv")



o3 <- rbind(regional_buffer_lower_by_criteria$o3,
            micro_buffer_upper_by_criteria$o3,
            middle_buffer_upper_by_criteria$o3,
            neighborhood_buffer_upper_by_criteria$o3,
            urban_buffer_upper_by_criteria$o3)

o3_method <- merge(o3, method[, c("Method.Code", "Recording.Mode", "Parameter.Code")], 
                   by.x = c("Last.Method.Code", "Parameter.Code"), 
                   by.y = c("Method.Code", "Parameter.Code"), all.x = TRUE)


so2 <- rbind(regional_buffer_lower_by_criteria$so2,
            micro_buffer_upper_by_criteria$so2,
            middle_buffer_upper_by_criteria$so2,
            neighborhood_buffer_upper_by_criteria$so2,
            urban_buffer_upper_by_criteria$so2)

so2_method <- merge(so2, method[, c("Method.Code", "Recording.Mode", "Parameter.Code")], 
                   by.x = c("Last.Method.Code", "Parameter.Code"), 
                   by.y = c("Method.Code", "Parameter.Code"), all.x = TRUE)


pm <- rbind(regional_buffer_lower_by_criteria$pm,
            micro_buffer_upper_by_criteria$pm,
            middle_buffer_upper_by_criteria$pm,
            neighborhood_buffer_upper_by_criteria$pm,
            urban_buffer_upper_by_criteria$pm)

pm_method <- merge(pm, method[, c("Method.Code", "Recording.Mode", "Parameter.Code")], 
                   by.x = c("Last.Method.Code", "Parameter.Code"), 
                   by.y = c("Method.Code", "Parameter.Code"), all.x = TRUE)


pb <- rbind(regional_buffer_lower_by_criteria$pb,
            micro_buffer_upper_by_criteria$pb,
            middle_buffer_upper_by_criteria$pb,
            neighborhood_buffer_upper_by_criteria$pb,
            urban_buffer_upper_by_criteria$pb)

pb_method <- merge(pb, method[, c("Method.Code", "Recording.Mode", "Parameter.Code")], 
                   by.x = c("Last.Method.Code", "Parameter.Code"), 
                   by.y = c("Method.Code", "Parameter.Code"), all.x = TRUE)


no2 <- rbind(regional_buffer_lower_by_criteria$no2,
            micro_buffer_upper_by_criteria$no2,
            middle_buffer_upper_by_criteria$no2,
            neighborhood_buffer_upper_by_criteria$no2,
            urban_buffer_upper_by_criteria$no2)

no2_method <- merge(no2, method[, c("Method.Code", "Recording.Mode", "Parameter.Code")], 
                   by.x = c("Last.Method.Code", "Parameter.Code"), 
                   by.y = c("Method.Code", "Parameter.Code"), all.x = TRUE)


co <- rbind(regional_buffer_lower_by_criteria$co,
            micro_buffer_upper_by_criteria$co,
            middle_buffer_upper_by_criteria$co,
            neighborhood_buffer_upper_by_criteria$co,
            urban_buffer_upper_by_criteria$co)

co_method <- merge(co, method[, c("Method.Code", "Recording.Mode", "Parameter.Code")], 
                   by.x = c("Last.Method.Code", "Parameter.Code"), 
                   by.y = c("Method.Code", "Parameter.Code"), all.x = TRUE)


table(o3_method$Recording.Mode)
table(is.na(o3_method$Recording.Mode))
prop.table(table(is.na(o3_method$Recording.Mode)))

table(pm_method$Recording.Mode)
table(is.na(pm_method$Recording.Mode))
prop.table(table(is.na(pm_method$Recording.Mode)))

table(no2_method$Recording.Mode)
table(is.na(no2_method$Recording.Mode))
prop.table(table(is.na(no2_method$Recording.Mode)))

table(so2_method$Recording.Mode)
table(is.na(so2_method$Recording.Mode))
prop.table(table(is.na(so2_method$Recording.Mode)))

table(pb_method$Recording.Mode)
table(is.na(pb_method$Recording.Mode))
prop.table(table(is.na(pb_method$Recording.Mode)))

table(co_method$Recording.Mode)
table(is.na(co_method$Recording.Mode))
prop.table(table(is.na(co_method$Recording.Mode)))




########################
# sensitivity analysis
########################

method <- read.csv("JAMA/methods_all.csv")

pm <- rbind(regional_buffer_upper_by_criteria$pm,
            micro_buffer_upper_by_criteria$pm,
            middle_buffer_upper_by_criteria$pm,
            neighborhood_buffer_upper_by_criteria$pm,
            urban_buffer_upper_by_criteria$pm)

pm_method <- merge(pm, method[, c("Method.Code", "Recording.Mode", "Parameter.Code")], 
                   by.x = c("Last.Method.Code", "Parameter.Code"), 
                   by.y = c("Method.Code", "Parameter.Code"), all.x = TRUE)

pm_cont <- pm_method[which(pm_method$Recording.Mode == "Continuous"), ]
pm_int <- pm_method[which(pm_method$Recording.Mode == "Intermittent"), ]

# centroids
centroids <- read.csv("/Users/brenna/Documents/School/Research/aqs-inequities/aqs_inequities/CenPop2020_Mean_BG.csv")
names(centroids) <- tolower(names(centroids))

centroids <- centroids |>
  filter(statefp != 72)

centroids <- st_as_sf(centroids, coords = c("longitude", "latitude"), 
                      crs = 4326, agr = "constant")

# creating block group fips for join
centroids$geoid <- paste0(str_pad(centroids$statefp, pad = "0", width = 2, side = "left"),
                          str_pad(centroids$countyfp, pad = "0", width = 3, side = "left"),
                          str_pad(centroids$tractce, pad = "0", width = 6, side = "left"),
                          centroids$blkgrpce)

centroids <- st_transform(centroids, st_crs(mons))

measured_pm_upper <- st_join(centroids, pm_cont, join = st_within) |>
  filter(!is.na(criteria)) |>
  select(geoid, Measurement.Scale, 
         Measurement.Scale.Definition, 
         criteria)

count_pm_upper <- data.frame(t(table(measured_pm_upper$geoid)))[, 2:3] |>
  rename("geoid" = Var2) |>
  rename("n_upper_pm" = Freq)

# acs
acs <- read.csv("data/7020_data_count.csv")

acs$geoid <- str_pad(acs$geoid, width = 12, pad = "0", side = "left")

acs <- acs |>
  select(!n_upper_pm)

acs_merge <- acs |>
  merge(count_pm_upper, by = "geoid", all.x = TRUE) |>
  mutate(n_upper_pm = ifelse(is.na(n_upper_pm), 0, n_upper_pm))

## regression
acs_geom <- st_read("data/7020_geom.shp")

# merge by geoid
acs <- merge(acs_geom, acs_merge, by = "geoid")
# drop empty geometries
acs <- acs[which(st_is_empty(acs) == FALSE), ]
# drop areas with no population
empty_areas <- acs[which(is.na(acs$hisp_p)), ]
acs <- acs[which(!acs$geoid %in% empty_areas$geoid), ]
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

# inla prep
library(INLA)
library(spdep)
library(SpatialEpi)

# nb <- poly2nb(acs)
# nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")

acs$idarea <- 1:nrow(acs)
acs$idarea_2 <- 1:nrow(acs)

# results
inla_results_nbin <- list()

prior <- list(
  phi = list(
    prior = "pc",
    param = 1.000)
)

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

start = Sys.time()
inla_results_nbin[["inla_pm_u"]] <- inla(n_upper_pm ~ hisp_p_log + black_p_log + 
                                           aian_p_log + asian_p_log + nhpi_p_log + 
                                           other_p_log + tom_p_log +
                                           f(idarea, model = "bym2", graph = g,
                                             hyper = prior), 
                                         E = E,
                                         data = d, family = "nbinomial",
                                         control.inla = list(int.strategy = "eb"),
                                         control.compute = list(dic = TRUE, waic = TRUE),
                                         verbose = TRUE)
end = Sys.time()
end - start
saveRDS(inla_results_nbin, file = "data/inla_results_rev_pmcont.rds")




############
# modify T1
############
# median / IQR rather than mean / SD
# difference T1 test than t-test
library(tableone)

acs <- read.csv("data/7020_data_count.csv")
acs <- acs[which(st_is_empty(acs) == FALSE), ]
empty_areas <- acs[which(is.na(acs$hisp_p)), ]
acs <- acs[which(!acs$geoid %in% empty_areas$geoid), ]

acs <- acs |>
  mutate(n_upper_pm_bin = ifelse(n_upper_pm > 0, 1, 0)) |>
  mutate(n_upper_no2_bin = ifelse(n_upper_no2 > 0, 1, 0)) |>
  mutate(n_upper_so2_bin = ifelse(n_upper_so2 > 0, 1, 0)) |>
  mutate(n_upper_co_bin = ifelse(n_upper_co > 0, 1, 0)) |>
  mutate(n_upper_o3_bin = ifelse(n_upper_o3 > 0, 1, 0)) |>
  mutate(n_upper_pb_bin = ifelse(n_upper_pb > 0, 1, 0)) |>
  mutate(aian_perc = aian_p * 100) |>
  mutate(asian_perc = asian_p * 100) |>
  mutate(black_perc = black_p * 100) |>
  mutate(hisp_perc = hisp_p * 100) |>
  mutate(nhpi_perc = nhpi_p * 100) |>
  mutate(tom_perc = tom_p * 100) |>
  mutate(white_perc = white_p * 100) |>
  mutate(other_perc = other_p * 100)

## Vector of variables to summarize
myVars <- c("total", "aian_perc", "asian_perc", 
            "black_perc", "hisp_perc", "nhpi_perc", 
            "tom_perc", "white_perc", "other_perc")

## Create a TableOne object

outcomes <- c("n_upper_pm_bin", 
              "n_upper_no2_bin", 
              "n_upper_so2_bin", 
              "n_upper_co_bin", 
              "n_upper_o3_bin", 
              "n_upper_pb_bin")

tabl_no2 <- CreateTableOne(vars = myVars, data = acs, 
                           strata = "n_upper_no2_bin")

print(tabl_no2, nonnormal = TRUE, formatOptions = list(big.mark = ","), 
      quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(tab1_o3, file = "tab1_pb.csv")

tabl_no2

aggregate(acs$aian_perc, by = list(acs$n_upper_pm_bin), FUN = IQR)
quantile(acs$aian_perc, 0.75)


wilcox.test(mpg ~ am, data=mtcars) 



#####################
# regional analysis
#####################
# code block groups by EPA region
# - map of EPA region density per capita
# - model EPA region as a random effect
# code block groups by state
# - map of EPA region density per capita
# - model state as a random effect
library(sf)
library(tmap)
library(dplyr)
library(tidyr)
library(stringr)
library(tidycensus)

aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"

# acs
acs_df <- read.csv("data/7020_data_count.csv")
acs_df$geoid <- str_pad(acs_df$geoid, width = 12, pad = "0", side = "left")

acs_geom <- st_read("data/7020_geom_distinct.shp")

# merge by geoid
acs_merge <- merge(acs_geom, acs_df, by = "geoid")
# drop empty geometries
acs <- acs_merge[which(st_is_empty(acs_merge) == FALSE), ]
# drop areas with no population
empty_areas <- acs[which(is.na(acs$hisp_p)), ]
acs <- acs[which(!acs$geoid %in% empty_areas$geoid), ]

acs$state_geoid <- str_sub(acs$geoid, start = 0, end = 2)
acs$county_geoid <- str_sub(acs$geoid, start = 0, end = 5)

# epa
epa <- read.csv("JAMA/epa_regions.csv")
names(epa) <- tolower(names(epa))
epa <- epa |>
  mutate(county_geoid = paste0(state.code, 
                        str_pad(county.code, width = 3, pad = "0", side = "left"))) |>
  select(county_geoid, epa.region) |>
  distinct()

acs_epa <- merge(acs, epa, by.x = "county_geoid", by.y = "county_geoid")
# acs_epa_sf <- merge(acs_geom, acs_epa, by = "geoid")
mons_clean <- st_read("data/mons_clean.shp")

acs_epa_sf <- st_transform(acs_epa, st_crs(mons_clean))

mons_epa <- st_join(mons_clean, acs_epa_sf, join = st_intersects)

mons_epa_agg <- mons_epa |> 
  count(county_geoid, criteri) |>
  rename(mon_count = n)

mons_epa_denoms <- acs |>
  group_by(county_geoid) |>
  summarize_at(c("total", "hisp", "black", 
                 "aian", "asian", "nhpi", 
                 "other", "tom"), sum) |>
  st_drop_geometry()

head(mons_epa_denoms)

# us state boundaries
us_county <- get_acs(geography = "county",
                     variables = c("B03002_001"),
                     geometry = TRUE,
                     year = 2021)
us_county <- st_transform(us_county, aea)

mons_epa_state <- merge(us_county, st_drop_geometry(mons_epa_agg), 
                        by.x = "GEOID", by.y = "county_geoid", all.x = TRUE)
# get by capita per state
mons_epa_state_dat <- merge(mons_epa_state, mons_epa_denoms, 
                            by.x = "GEOID", by.y = "county_geoid")
mons_epa_state_dat <- mons_epa_state_dat |>
  pivot_wider(names_from = "criteri", values_from = "mon_count")
mons_epa_state_dat <- mons_epa_state_dat |>
  mutate(o3 = ifelse(is.na(o3), 0, o3)) |>
  mutate(so2 = ifelse(is.na(so2), 0, so2)) |>
  mutate(no2 = ifelse(is.na(no2), 0, no2)) |>
  mutate(pb = ifelse(is.na(pb), 0, pb)) |>
  mutate(co = ifelse(is.na(co), 0, co)) |>
  mutate(pm = ifelse(is.na(pm), 0, pm)) |>
  mutate(o3_per_cap = ifelse(is.na(o3), 0, 1000 * o3 / total)) |>
  mutate(so2_per_cap = ifelse(is.na(so2), 0, 1000 * so2 / total)) |>
  mutate(pb_per_cap = ifelse(is.na(pb), 0, 1000 * pb / total)) |>
  mutate(pm_per_cap = ifelse(is.na(pm), 0, 1000 * pm / total)) |>
  mutate(co_per_cap = ifelse(is.na(co), 0, 1000 * co / total)) |>
  mutate(no2_per_cap = ifelse(is.na(no2), 0, 1000 * no2 / total))

"#436042"
"#ff2908"
"#4f5dbc"
"#ebb140"
"#c58058"
"#00a99d"



pm_pal <- colorRampPalette(c("white", "#436042"), space = "rgb",
                           interpolate = "linear", alpha = FALSE)
# mons_epa_state_dat$o3_per_cap <- ifelse(mons_epa_state_dat$nhpi == 0, 0, mons_epa_state_dat$pm_per_cap)

# breaks
# calculate deciles
aqs_breaks <- quantile(mons_epa_state_dat$o3_per_cap,
           seq(from = 0, to = 1, by = 0.1))
# clean labels
break_labels = paste("≤", as.character(round(pub_breaks, 2)[-1] |>
                                         str_pad(width = 4, side = "right", pad = "0")))
min(st_drop_geometry(mons_epa_state_dat[which(mons_epa_state_dat$pm_per_cap > 0), "pm_per_cap"]))

tm_shape(mons_epa_state_dat) +
  tm_polygons(col = "pm_per_cap", style = "jenks", lwd = 0, palette = pm_pal(5))
tm_shape(mons_epa_state_dat) +
  tm_polygons(col = "o3", style = "jenks", lwd = 0, palette = "viridis")
tm_shape(mons_epa_state_dat) +
  tm_polygons(col = "no2", style = "jenks", lwd = 0, palette = "viridis")
tm_shape(mons_epa_state_dat) +
  tm_polygons(col = "so2", style = "jenks", lwd = 0, palette = "viridis")
tm_shape(mons_epa_state_dat) +
  tm_polygons(col = "pb", style = "jenks", lwd = 0, palette = "viridis")
tm_shape(mons_epa_state_dat) +
  tm_polygons(col = "co", style = "cont", lwd = 0, palette = "Spectral", 
              midpoint = mean(mons_epa_state_dat$co))




##############
# county size
##############
# get county size and compare to block group size
# state geometry
us_county <- get_acs(geography = "county",
                     variables = c("B03002_001"),
                     geometry = TRUE,
                     year = 2021)
us_county <- st_transform(us_county, aea)

library(units)


county_race$area <- st_area(county_race)
county_race$area_sqkm <- set_units(county_race$area, km^2)
mean(county_race$area_sqkm)

acs$area <- st_area(acs)
acs$area_sqkm <- set_units(acs$area, km^2)
mean(acs$area_sqkm)


################
## create maps showing monitors per capita by state
################

us_states <- get_acs(geography = "state",
                     variables = c("B03002_001"),
                     geometry = TRUE,
                     year = 2021)
us_states <- st_transform(us_states, aea)
mons_clean <- st_read("data/mons_clean.shp")
mons_clean <- st_transform(mons_clean, st_crs(us_states))
mons_state <- st_join(mons_clean, us_states, join = st_intersects)

# get count
mons_state_agg <- mons_state |> 
  count(Stat_Cd, criteri) |>
  rename(mon_count = n) |>
  st_drop_geometry()

# epa regions
epa <- read.csv("JAMA/epa_regions.csv")
names(epa) <- tolower(names(epa))
epa <- epa |>
  select(state.code, epa.region) |>
  distinct()
head(epa)

mons_epa <- merge(us_states, mons_state_agg, by.x = "GEOID", by.y = "Stat_Cd")
mons_epa <- merge(mons_epa, epa, by.x = "GEOID", by.y = "state.code")

library(units)

mons_epa$state_area_sqkm <- st_area(mons_epa)
mons_epa$state_area_sqkm <- set_units(mons_epa$state_area_sqkm, km^2)
mons_epa$state_area_100km <- 100 * mons_epa$state_area_sqkm
mons_epa$state_area_100km <- drop_units(mons_epa$state_area_100km)


"#436042"
"#ff2908"
"#4f5dbc"
"#ebb140"
"#c58058"
"#00a99d"

# class interval choices
library(ggplot2)
ggplot(mons_epa_pm, aes(x = mon_count)) +
  geom_histogram()

mons_epa_pm <- mons_epa[which(mons_epa$criteri == "pm"), ] |>
  mutate(mons_per_capita = 100000 * mon_count / estimate) |>
  mutate(monitor_density = 10000 * mon_count / state_area_100km)
mons_epa_no2 <- mons_epa[which(mons_epa$criteri == "no2"), ] |>
  mutate(mons_per_capita = 100000 * mon_count / estimate) |>
  mutate(monitor_density = 10000 * mon_count / state_area_100km)
mons_epa_so2 <- mons_epa[which(mons_epa$criteri == "so2"), ] |>
  mutate(mons_per_capita = 100000 * mon_count / estimate) |>
  mutate(monitor_density = 10000 * mon_count / state_area_100km)
mons_epa_pb <- mons_epa[which(mons_epa$criteri == "pb"), ] |>
  mutate(mons_per_capita = 100000 * mon_count / estimate) |>
  mutate(monitor_density = 10000 * mon_count / state_area_100km)
mons_epa_co <- mons_epa[which(mons_epa$criteri == "co"), ] |>
  mutate(mons_per_capita = 100000 * mon_count / estimate) |>
  mutate(monitor_density = 10000 * mon_count / state_area_100km)
mons_epa_o3 <- mons_epa[which(mons_epa$criteri == "o3"), ] |>
  mutate(mons_per_capita = 100000 * mon_count / estimate) |>
  mutate(monitor_density = 10000 * mon_count / state_area_100km)

pm_pal <- colorRampPalette(c("white", "#436042"), space = "rgb",
                           interpolate = "linear", alpha = FALSE) # "#EAEDEA"

mons_epa_pm$monitor_density_log <- log(mons_epa_pm$monitor_density)

tmap_mode(mode = "view")
tm_shape(mons_epa_pm) +
  tm_polygons(col = "monitor_density_log", palette = "viridis", style = "cont", lwd = 0)

hist(log(mons_epa_pm$monitor_density))



acs_epa$epa.region <- as.factor(acs_epa$epa.region)

mons_plt_dat <- mons_epa_state_dat |>
  # st_drop_geometry() |>
  group_by(Stat_Cd) |>
  summarize_at(c("n_upper_pm", "n_upper_no2", "n_upper_so2",
                 "n_upper_co", "n_upper_o3", "n_upper_pb"), sum)


mons_plt_state <- mons_plt_dat[, c("Stat_Cd", )]

mons_plt_dat$pm_


##########
# area
##########
# - get average area of a county
# - get average area of a block group
# - compare spatial resolution










length(unique(mons$site_code))
nrow(mons)

aqs_o3$State.Code <- str_pad(aqs_o3$State.Code, width = 2, pad = "0", side = "left")
aqs_o3$County.Code <- str_pad(aqs_o3$County.Code, width = 3, pad = "0", side = "left")
aqs_o3$Site.Num <- str_pad(aqs_o3$Site.Num, width = 4, pad = "0", side = "left")

aqs_o3 <- merge(o3, site_obs, 
                by.x = c("site_code", "Parameter.Code"),
                by.y = c("site_code", "Parameter.Code"),
                all.x = TRUE)

prop.table(table(is.na(aqs_o3$obs_perc)))

# setdiff(o3$site_code, aqs_o3$site_code)

table(is.na(aqs_o3$obs_perc), aqs_o3$State.Code)

Sample.Duration
Valid.Day.Count
Completeness.Indicator
Observation.Count
Null.Data.Count


aqs_o3$obs_proportion <- aqs_o3$Observation.Percent / 100

o3_pal <- colorRampPalette(c("#FFC9C1", "#ff2908"), space = "rgb",
                           interpolate = "linear", alpha = FALSE)

## O3
o3_map <- tm_shape(us_states, proj = aea) +
  tm_polygons(col = "white", lwd = 0.5, border.col = "black") +
  tm_shape(aqs_o3) +
  tm_polygons(col = "obs_perc", palette = o3_pal(5), alpha = 0.5, lwd = 0) +
  tm_shape(micro_buffer_upper_by_criteria$o3) +
  tm_polygons(col = "#ff2908", alpha = 0.25, lwd = 1) +
  tm_shape(middle_buffer_upper_by_criteria$o3) +
  tm_polygons(col = "#ff2908", alpha = 0.25, lwd = 1) +
  tm_shape(neighborhood_buffer_upper_by_criteria$o3) +
  tm_polygons(col = "#ff2908", alpha = 0.25, lwd = 1) +
  tm_shape(urban_buffer_upper_by_criteria$o3) +
  tm_polygons(col = "#ff2908", alpha = 0.25, lwd = 0)

## PM
pm_map <- tm_shape(us_states, proj = aea) +
  tm_polygons(col = "white", lwd = 0.5, border.col = "black") +
  tm_shape(regional_buffer_upper_by_criteria$pm) +
  tm_polygons(col = "#436042", alpha = 0.25, lwd = 0) +
  tm_shape(micro_buffer_upper_by_criteria$pm) +
  tm_polygons(col = "#436042", alpha = 0.25, lwd = 1) +
  tm_shape(middle_buffer_upper_by_criteria$pm) +
  tm_polygons(col = "#436042", alpha = 0.25, lwd = 1) +
  tm_shape(neighborhood_buffer_upper_by_criteria$pm) +
  tm_polygons(col = "#436042", alpha = 0.25, lwd = 1) +
  tm_shape(urban_buffer_upper_by_criteria$pm) +
  tm_polygons(col = "#436042", alpha = 0.25, lwd = 0)

## NO2
no2_map <- tm_shape(us_states, proj = aea) +
  tm_polygons(col = "white", lwd = 0.5, border.col = "black") +
  tm_shape(regional_buffer_upper_by_criteria$no2) +
  tm_polygons(col = "#ebb140", alpha = 0.35, lwd = 0) +
  tm_shape(micro_buffer_upper_by_criteria$no2) +
  tm_polygons(col = "#ebb140", alpha = 0.35, lwd = 1) +
  tm_shape(middle_buffer_upper_by_criteria$no2) +
  tm_polygons(col = "#ebb140", alpha = 0.35, lwd = 1) +
  tm_shape(neighborhood_buffer_upper_by_criteria$no2) +
  tm_polygons(col = "#ebb140", alpha = 0.35, lwd = 1) +
  tm_shape(urban_buffer_upper_by_criteria$no2) +
  tm_polygons(col = "#ebb140", alpha = 0.35, lwd = 0)

## SO2
so2_map <- tm_shape(us_states, proj = aea) +
  tm_polygons(col = "white", lwd = 0.5, border.col = "black") +
  tm_shape(regional_buffer_upper_by_criteria$so2) +
  tm_polygons(col = "#4f5dbc", alpha = 0.25, lwd = 0) +
  tm_shape(micro_buffer_upper_by_criteria$so2) +
  tm_polygons(col = "#4f5dbc", alpha = 0.25, lwd = 1) +
  tm_shape(middle_buffer_upper_by_criteria$so2) +
  tm_polygons(col = "#4f5dbc", alpha = 0.25, lwd = 1) +
  tm_shape(neighborhood_buffer_upper_by_criteria$so2) +
  tm_polygons(col = "#4f5dbc", alpha = 0.25, lwd = 1) +
  tm_shape(urban_buffer_upper_by_criteria$so2) +
  tm_polygons(col = "#4f5dbc", alpha = 0.25, lwd = 0)

## Pb
pb_map <- tm_shape(us_states, proj = aea) +
  tm_polygons(col = "white", lwd = 0.5, border.col = "black") +
  #tm_shape(regional_buffer_upper_by_criteria$pb) +
  #tm_polygons(col = "#c58058", alpha = 0.25, lwd = 0) +
  tm_shape(micro_buffer_upper_by_criteria$pb) +
  tm_polygons(col = "#c58058", alpha = 0.35, lwd = 1) +
  tm_shape(middle_buffer_upper_by_criteria$pb) +
  tm_polygons(col = "#c58058", alpha = 0.35, lwd = 1) +
  tm_shape(neighborhood_buffer_upper_by_criteria$pb) +
  tm_polygons(col = "#c58058", alpha = 0.35, lwd = 1) +
  tm_shape(urban_buffer_upper_by_criteria$pb) +
  tm_polygons(col = "#c58058", alpha = 0.35, lwd = 1)

## CO
co_map <- tm_shape(us_states, proj = aea) +
  tm_polygons(col = "white", lwd = 0.5, border.col = "black") +
  tm_shape(regional_buffer_upper_by_criteria$co) +
  tm_polygons(col = "#00a99d", alpha = 0.35, lwd = 0) +
  tm_shape(micro_buffer_upper_by_criteria$co) +
  tm_polygons(col = "#00a99d", alpha = 0.35, lwd = 1) +
  tm_shape(middle_buffer_upper_by_criteria$co) +
  tm_polygons(col = "#00a99d", alpha = 0.35, lwd = 1) +
  tm_shape(neighborhood_buffer_upper_by_criteria$co) +
  tm_polygons(col = "#00a99d", alpha = 0.35, lwd = 1) +
  tm_shape(urban_buffer_upper_by_criteria$co) +
  tm_polygons(col = "#00a99d", alpha = 0.35, lwd = 0)

# current.mode <- tmap_mode("plot")
tmap_arrange(o3_map, pm_map, so2_map, 
             no2_map, pb_map, co_map,
             nrow = 2, ncol = 3)





