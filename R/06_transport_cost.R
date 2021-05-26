### computation of transport cost matrices for truck, train and ship based on logistic network ############

library(sf)
library(dplyr)
library(mapview)
library(raster)
library(fasterize)
library(gdistance)

# should results be written to file?
write = TRUE

# load data -------------------------------------------------------------------

# MU polygons
GEO_MUN_SOY <- readRDS("intermediate_data/GEO_MUN_SOY_02.rds")

# IBGE logistic network
ibge2014 <- st_read("input_data/geo/IBGE_logistic_network/rodovia_2014.shp",stringsAsFactors = FALSE)
# road_structural <- st_read("input_data/geo/IBGE_logistic_network/eixo_rodoviario_estruturante_2014.shp",stringsAsFactors = FALSE)
# rail <- st_read("input_data/geo/IBGE_logistic_network/ferrovias_2014.shp", stringsAsFactors = FALSE)
# river <- st_read("input_data/geo/IBGE_logistic_network/hidrovias_linha_2014.shp", stringsAsFactors = FALSE) 
# ports_dry <- st_read("input_data/geo/IBGE_logistic_network/porto_seco_2014.shp", stringsAsFactors = FALSE)
# ports <- st_read("input_data/geo/IBGE_logistic_network/portos_2014.shp", stringsAsFactors = FALSE)

# Victoria dataset
dnit2010 <- st_read("input_data/geo/road_network_dnit2010.gpkg",stringsAsFactors = FALSE)

# MU capitals
MUN_capitals <- readRDS("intermediate_data/MUN_capitals.rds")

# inspect data
#mapview(list(road, rail, river, GEO_MUN_SOY))
#mapview(list(road, road_structural))
#mapview(dnit2010)


# rasterize road network -----------------------------------------------------------

# create blank raster with desired resolution and extent of Brazil
# NOTE: resoltution of 5 km is chosen for now to keep computational requirements manageable
rast_temp <- raster(ext = extent(GEO_MUN_SOY), resolution = 5000, crs = 5880, )
writeRaster(rast_temp,"intermediate_data/rast_temp.tif", overwrite = TRUE, format="GTiff") 

# create raster covering Brazilian territory
# using fasterize, which is much faster than rasterize but only works for polygons
Brazil_rast <- fasterize(GEO_MUN_SOY, rast_temp, field = NULL)
# Brazil_rast <- buffer(Brazil_rast, width = 5000, doEdge = TRUE)


# rasterize dnit2010
# this requires using the gdal utils (there seems to be no better option for linestrings)
# first project road data to EPSG 5880 and write to file
dnit2010_sirgas <- st_transform(dnit2010, crs = 5880)
st_write(dnit2010_sirgas, dsn = "intermediate_data/dnit2010_sirgas.gpkg", driver = "GPKG", delete_dsn = TRUE)
# define file source and target extent for rasterization
src <- "intermediate_data/dnit2010_sirgas.gpkg"
ext <- as.character(st_bbox(GEO_MUN_SOY))
# rsterize from file to file with resolution of 5000m
gdal_utils('rasterize', src, "intermediate_data/dnit2010_rasterized.tif", options = c("-tr", "5000", "5000", "-a", "cost", "-te", ext, "-a_nodata", "NA", "-at"))
# load in rasterized file
dnit2010_rast <- raster("intermediate_data/dnit2010_rasterized.tif")
#mapview(dnit2010_rast, maxpixels = 833860)


## rasterize  IBGE data

#  project road data to EPSG 5880 
ibge2014_sirgas <- st_transform(ibge2014, crs = 5880)

# remove planned roads, travessias and roads with type "NA"
ibge2014_sirgas <- filter(ibge2014_sirgas, !TipoPNV %in% c("Planejada", "Travessia"))
ibge2014_sirgas <- filter(ibge2014_sirgas, !is.na(TipoPNV))

# add road type classes analogous to Victoria
paved <- c("Duplicada", "Em obras de duplicação", "Pavimentada")
unpaved <- c("Em obras de pavimentação", "Implantada", "Em obras de implantação", "Leito Natural")
ibge2014_sirgas <- mutate(ibge2014_sirgas, type = ifelse(TipoPNV %in% paved, "paved", "unpaved"), .after = TipoPNV)

# assign cost values/driving speeds
ibge2014_sirgas <- mutate(ibge2014_sirgas, speed = ifelse(type == "paved", 1, ifelse(type == "unpaved", 2, 3)))

# write to file
st_write(ibge2014_sirgas, dsn = "intermediate_data/ibge2014_sirgas.gpkg", driver = "GPKG", delete_dsn = TRUE)

# define file source and target extent for rasterization
src <- "intermediate_data/ibge2014_sirgas.gpkg"
ext <- as.character(st_bbox(GEO_MUN_SOY))
# rasterize from file to file with resolution of 5000m
gdal_utils('rasterize', src, "intermediate_data/ibge2014_rasterized.tif", options = c("-tr", "5000", "5000", "-a", "speed", "-te", ext, "-a_nodata", "NA", "-at"))
# load in rasterized file
ibge2014_rast <- raster("intermediate_data/ibge2014_rasterized.tif")
#mapview(ibge2014_rast, maxpixels = 833860)

# create cost raster -------------------------------------------------------------------------------------------

# the cost raster should be the dnit2010 raster, with all non-network cells within Brazil set to the maximum cost value
road_cost2010 <- dnit2010_rast
road_cost2010[is.na(dnit2010_rast) & Brazil_rast == 1] <- 50

# # optional: if other cost values are desired:
# # define costs
# cost <- c("no_network" = 50, "unpaved" = 32, "paved" = 16)
# # modify raster accordingly
# road_cost2010[dnit2010_rast == 16] <- cost["paved"]
# road_cost2010[dnit2010_rast == 32] <- cost["unpaved"]
# road_cost2010[is.na(dnit2010_rast) & Brazil_rast == 1] <- cost["no_network"]

# option 2: use average speed instead of trespassing cost
road_speed2010 <- dnit2010_rast
# define average speed (in km/h)
speed <- c("paved" = 50, "unpaved" = 20, "no_network" = 10)
#transform km/h into meters/h
speed <- speed*1000
# modify raster accordingly
road_speed2010[dnit2010_rast == 16] <- speed["paved"]
road_speed2010[dnit2010_rast == 32] <- speed["unpaved"]
road_speed2010[is.na(dnit2010_rast) & Brazil_rast == 1] <- speed["no_network"]
mapview(road_speed2010, maxpixels = 833860)

# use IBGE data
road_speed2014 <- ibge2014_rast
# modify raster accordingly
road_speed2014[ibge2014_rast == 1] <- speed["paved"]
road_speed2014[ibge2014_rast == 2] <- speed["unpaved"]
road_speed2014[is.na(ibge2014_rast) & Brazil_rast == 1] <- speed["no_network"]
mapview(road_speed2014, maxpixels = 833860)


# compute least cost paths between MU capitals --------------------------------------------------------------------------

# this uses gDistance

# create transition matrix based on cost raster
#transition <- transition(x = road_cost2010, transitionFunction = function(x) 1/mean(x), directions=8)
transition <- transition(x = road_speed2014, transitionFunction = function(x) mean(x), directions=8)

# geoCorrect (see gDistance documentation)
transition_corr <- geoCorrection(transition, type = "c")
# compute matrix of least-cost distances between MU capitals
road_cost_mat <- costDistance(transition_corr, st_coordinates(MUN_capitals)[1:10,])

# check an exemplary pair of MUs
A <- MUN_capitals[1,]
B <-MUN_capitals[1337,]
AtoB <- shortestPath(transition_corr, as_Spatial(A), as_Spatial(B), output = "SpatialLines")
#road_cost_crop  <- crop(road_cost2010, AtoB) 
#plot(road_cost_crop)
#plot(road_speed2014)
#plot(AtoB, add = TRUE)
plot(as_Spatial(MUN_capitals[c(1,1337),]), add = TRUE)
cost_mat_AtoB <- costDistance(transition_corr, as_Spatial(MUN_capitals[1:1337,]))

m1 <- mapview(road_speed2014, maxpixels =  799850) #, st_as_sf(AtoB)
m2 <- mapview(AtoB, color = 'cyan') 
m3 <- mapview(list(A,B), zcol = "nm_mun", col.regions = list("yellow", "green"))

m1+m2+m3


# export results

if(write){
  saveRDS(road_cost_mat, file = "intermediate_data/road_cost_mat.RDS")
}
