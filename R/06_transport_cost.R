### computation of transport cost matrices for truck, train and ship based on logistic network ############

library(sf)
library(dplyr)
library(dodgr)
library(mapview)

# should results be written to file?
write = FALSE

# load  data
# MU polygons
GEO_MUN_SOY <- readRDS("intermediate_data/GEO_MUN_SOY_02.rds")
# IBGE logistic network
road <- st_read("input_data/geo/IBGE_logistic_network/rodovia_2014.shp",stringsAsFactors = FALSE)
road_structural <- st_read("input_data/geo/IBGE_logistic_network/eixo_rodoviario_estruturante_2014.shp",stringsAsFactors = FALSE)
rail <- st_read("input_data/geo/IBGE_logistic_network/ferrovias_2014.shp", stringsAsFactors = FALSE)
river <- st_read("input_data/geo/IBGE_logistic_network/hidrovias_linha_2014.shp", stringsAsFactors = FALSE) 
ports_dry <- st_read("input_data/geo/IBGE_logistic_network/porto_seco_2014.shp", stringsAsFactors = FALSE)
ports <- st_read("input_data/geo/IBGE_logistic_network/portos_2014.shp", stringsAsFactors = FALSE)

mapview(list(road, rail, river, GEO_MUN_SOY))
mapview(list(road, road_structural))

##### roads        
# remove "planned roads"
roads <- road[road$TipoPNV != "Planejada",]
