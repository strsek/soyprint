############ transport optimization within R, using the transport package ###################

library(transport)
library(openxlsx)
library(dplyr)
library(abind)

write = TRUE

# load modified version of the transport function
#source("R/function_library.R")

# load data
SOY_MUN <- readRDS("intermediate_data/SOY_MUN_fin.RDS")
MUN_capital_dist <- readRDS("intermediate_data/MUN_capital_dist.rds")
MUN_road_dist <- readRDS("intermediate_data/MUN_road_dist.rds")
MUN_road_dist_osm_mf <- readRDS("intermediate_data/MUN_road_dist_osm_mf.rds")
MUN_road_dist_osm_nn <- readRDS("intermediate_data/MUN_road_dist_osm_nn.rds")

dist <- list(euclid = MUN_capital_dist, IBGE = MUN_road_dist, OSM_mf = MUN_road_dist_osm_mf, OSM_nn = MUN_road_dist_osm_nn)

flows <- lapply(dist, function(d){
  MUN_dist <- d
  class(MUN_dist) <- "numeric"

  a_bean <- SOY_MUN$excess_supply_bean
  b_bean <- SOY_MUN$excess_use_bean
  names(a_bean) <- SOY_MUN$co_mun
  names(b_bean) <- SOY_MUN$co_mun
  
  product <- c("bean", "oil", "cake")
  
  
  MUN_transport <- sapply(product, function(x){
    suppliers <- SOY_MUN$co_mun[pull(SOY_MUN, paste0("excess_supply_",x))>0]
    demanders <- SOY_MUN$co_mun[pull(SOY_MUN, paste0("excess_use_",x))>0]
    solve <- transport(a = pull(SOY_MUN, paste0("excess_supply_",x)), b = pull(SOY_MUN, paste0("excess_use_",x)), costm = MUN_dist, method = "networkflow", fullreturn=TRUE, threads=4)
    cat("total cost ", x, ": ", solve$cost, "\n")
    sol <- solve$default
    sol$co_orig <- suppliers[sol$from]
    sol$co_dest <- demanders[sol$to]
    sol$product <- x
    sol <- sol[,c("co_orig", "co_dest", "product", "mass")]
    names(sol)[4] <- "value"
    return(sol)
    }, 
    simplify = FALSE, USE.NAMES = TRUE)
  
  #flows_euclid <- abind(MUN_transport,along = 1, force.array = FALSE)
  flows <- abind(MUN_transport, along = 1, force.array = FALSE)
  return(flows)
})


if (write){
  saveRDS(flows_road, file = "intermediate_data/flows_road.rds")
  saveRDS(flows, file = "intermediate_data/flows_list.rds")
  
}

