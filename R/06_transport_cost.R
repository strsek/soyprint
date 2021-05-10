### computation of transport cost matrices for truck, train and ship based on logistic network ############

library(sf)
library(dplyr)
library(mapview)
library(raster)
library(fasterize)
library(gdistance)

# should results be written to file?
write = FALSE

# load data -------------------------------------------------------------------

# MU polygons
GEO_MUN_SOY <- readRDS("intermediate_data/GEO_MUN_SOY_02.rds")

# # IBGE logistic network
# road <- st_read("input_data/geo/IBGE_logistic_network/rodovia_2014.shp",stringsAsFactors = FALSE)
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
mapview(dnit2010)


# rasterize dnit2010 data -----------------------------------------------------------

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
# rsterize from file to file
gdal_utils('rasterize', src, "intermediate_data/dnit2010_rasterized.tif", options = c("-tr", "5000", "5000", "-a", "cost", "-te", ext, "-a_nodata", "NA", "-at"))
# load in rasterized file
dnit2010_rast <- raster("intermediate_data/dnit2010_rasterized.tif")
mapview(dnit2010_rast, maxpixels = 833860)

# create cost raster -------------------------------------------------------------------------------------------

# the cost raster should be the dnit2010 raster, with all non-network cells within Brazil set to the maximum cost value
road_cost2010 <- dnit2010_rast
road_cost2010[is.na(dnit2010_rast) & Brazil_rast == 1] <- 50

# # optional: if other cost values are desired:
# # define costs
# costval <- c("no_network" = 50, "unpaved" = 36, "paved" = 16)
# # modifiy raster accordingly
# road_cost2010[dnit2010_rast == 16] <- costval["paved"]
# road_cost2010[dnit2010_rast == 36] <- costval["unpaved"]
# road_cost2010[is.na(dnit2010_rast) & GEO_MUN_SOY_rast_sirgas == 1] <- costval["no_network"]


# compute least cost paths between MU capitals --------------------------------------------------------------------------

# this uses gDistance

# create transition matrix based on cost raster
transition <- transition(x = road_cost2010, transitionFunction = function(x) 1/mean(x), directions=8)
# geocorrect (see gDistance documentation)
transition_corr <- geoCorrection(transition, type = "c")
# compute matrix of least-cost distances between MU capitals
cost_mat <- costDistance(transition_corr, st_coordinates(MUN_capitals)[1:10,])

# check an exemplary pair of MUs
AtoB <- shortestPath(transition_corr, as_Spatial(MUN_capitals[1,]), as_Spatial(MUN_capitals[5000,]), output = "SpatialLines")
road_cost_crop  <- crop(road_cost2010, AtoB) 
plot(road_cost_crop)
#plot(road_cost2010)
plot(AtoB, add = TRUE)
plot(as_Spatial(MUN_capitals[c(1,5000),]), add = TRUE)
cost_mat_AtoB <- costDistance(transition_corr, as_Spatial(MUN_capitals[1:2,]))

m1 <- mapview(road_cost2010, maxpixels =  799850) #, st_as_sf(AtoB)
m2 <- mapview(AtoB, color = 'cyan') 

m1+m2
