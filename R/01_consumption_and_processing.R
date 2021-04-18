
######## Estimation of MU consumption and processing use items #########

library(dplyr)
library(sf)

# load data 
CBS_SOY <- readRDS("intermediate_data/CBS_SOY.rds")
SOY_MUN <- readRDS("intermediate_data/SOY_MUN_00.rds")
GEO_MUN_SOY <- readRDS("intermediate_data/GEO_MUN_SOY_00.rds")

# should results be written to file?
write = TRUE


# reference values from FAO ----------------------------------------------------------------------------------------------------

## derive reference values for MU total processing estimation

# average operation days of processing facilities
proc_days <- CBS_SOY["bean", "processing"]/sum(SOY_MUN$proc_cap, na.rm = TRUE)
ref_days <- CBS_SOY["oil", "production"]/sum(SOY_MUN$ref_cap, na.rm = TRUE)

# conversion factor from soybean to cake and oil
cake_conv <- CBS_SOY["cake", "production"]/CBS_SOY["bean", "processing"]
oil_conv <- CBS_SOY["oil", "production"]/CBS_SOY["bean", "processing"]
# note: values are relatively consistent with Smalling et. al (2008)[75% and 20%] and Dei (2011)[79% / 19%]


## derive reference values for MU consumption estimation

# per capita consumption (bean & oil)
tot_pop <- sum(SOY_MUN$population, na.rm = TRUE)
food_per_cap <- CBS_SOY$food/tot_pop
names(food_per_cap) <- rownames(CBS_SOY)

# per capita other use (oil)
other_per_cap <- CBS_SOY$other/tot_pop
names(other_per_cap) <- rownames(CBS_SOY)

# share of seed use in total soybean production (or supply)
seed_use_share <- CBS_SOY["bean", "seed"] / sum(SOY_MUN$prod_bean, na.rm = TRUE) #  or /CBS_SOY["production", "soybean"]  
# seed_use_share_supp <- CBS_SOY["seed", "soybean"] / CBS_SOY["Domestic supply quantity", "soybean"] 	


# estimation of MU processing quantities and consumption items ---------------------------------------------------------

## MU processing quantities

# annual bean processing quantity
SOY_MUN$proc_bean <- SOY_MUN$proc_cap*proc_days
sum(SOY_MUN$proc_bean, na.rm = T) == CBS_SOY["bean", "processing"]

# annual cake and production (through conversion factor from annual processing quantity)
SOY_MUN$prod_oil <- SOY_MUN$proc_bean*oil_conv
SOY_MUN$prod_cake <- SOY_MUN$proc_bean*cake_conv
sum(SOY_MUN$prod_oil, na.rm = T) == CBS_SOY["oil","production"]
sum(SOY_MUN$prod_cake, na.rm = T) == CBS_SOY["cake","production"]


## MU consumption items

# human soybean consumption (proxy: population)
SOY_MUN$food_bean <- SOY_MUN$population*food_per_cap["bean"]
sum(SOY_MUN$food_bean, na.rm = T) == CBS_SOY["bean", "food"]

# human soy oil consumption (proxy: population)
SOY_MUN$food_oil <- SOY_MUN$population*food_per_cap["oil"]
sum(SOY_MUN$food_oil, na.rm = T) == CBS_SOY["oil", "food"]

# other use of oil (proxy: population)
SOY_MUN$other_oil <- SOY_MUN$population*other_per_cap["oil"]
sum(SOY_MUN$other_oil, na.rm = T) == CBS_SOY["oil", "other"]

# seed use (proxy: soybean production)
SOY_MUN$seed_bean <- SOY_MUN$prod_bean*seed_use_share
sum(SOY_MUN$seed_bean, na.rm = T) == CBS_SOY["bean", "seed"]

# stock addition (proxy: grain storage capacity)
store_cap_tot <- sum(SOY_MUN$storage_cap, na.rm = T) # total storage capacity
SOY_MUN$stock_bean <- (SOY_MUN$storage_cap/store_cap_tot)*CBS_SOY["bean","stock_addition"]
sum(SOY_MUN$stock_bean, na.rm = T) == CBS_SOY["bean", "stock_addition"]


##### append results to GEO MUN file
newcols <- SOY_MUN %>% dplyr::select(c(co_mun, proc_bean:stock_bean)) 
GEO_MUN_SOY <- GEO_MUN_SOY %>% left_join(newcols, by = "co_mun")

# write to file
if (write == TRUE){
  saveRDS(SOY_MUN, file = "intermediate_data/SOY_MUN_01.rds")
  saveRDS(GEO_MUN_SOY, file = "intermediate_data/GEO_MUN_SOY_01.rds")
  # st_write(GEO_MUN_SOY, "intermediate_data/GEO_MUN_SOY.gpkg", driver = "GPKG", overwrite=TRUE, delete_dsn=TRUE)
}
