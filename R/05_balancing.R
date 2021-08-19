
########### Final adjustments to municipality-level data to balance supply with demand #########################

library(dplyr)

write = TRUE

# load data -----------------------------------------------------------------------------------
SOY_MUN <- readRDS("intermediate_data/SOY_MUN_03.rds")
GEO_MUN_SOY <- readRDS("intermediate_data/GEO_MUN_SOY_03.rds")
CBS_SOY <- readRDS("intermediate_data/CBS_SOY.rds")
EXP_MUN_SOY <- readRDS("intermediate_data/EXP_MUN_SOY.rds")
IMP_MUN_SOY <- readRDS("intermediate_data/IMP_MUN_SOY.rds")
btd_imp_BRA_soy <- readRDS("intermediate_data/btd_exp_BRA_soy.rds")
btd_imp_BRA_soy <- readRDS("intermediate_data/btd_imp_BRA_soy.rds")

CBS_SOY <- mutate(CBS_SOY, total_supply = production + import, total_use = export + food + feed + seed + processing + other + stock_addition)

###### NOTE: here the harmonized and re-export-free trade data will have to be taken into account

# clear small imbalances in CBS by adding difference between total supply an demand to "storage"
CBS_SOY$stock_addition <- CBS_SOY$stock_addition + (CBS_SOY$total_supply - CBS_SOY$total_use)
CBS_SOY$stock_withdrawal <- -CBS_SOY$stock_addition
CBS_SOY <- mutate(CBS_SOY, total_supply = production + import, total_use = export + food + feed + seed + processing + other + stock_addition)
CBS_SOY <- CBS_SOY[c("bean", "oil", "cake"),]

# add storage columns for oil and cake in the MU table and allocate according to cake/oil production
SOY_MUN <- SOY_MUN %>% mutate(stock_oil = CBS_SOY["oil","stock_addition"]*prod_oil/sum(prod_oil), stock_cake = CBS_SOY["cake","stock_addition"]*prod_cake/sum(prod_cake), .after=stock_bean)

# compare aggregate MU data with target FAO/FABIO data
# create version of table containing only CBS items
SOY_MUN_fin <- SOY_MUN %>% dplyr::select(prod_bean, prod_oil, prod_cake, imp_bean, imp_oil, imp_cake, exp_bean, exp_oil, exp_cake, food_bean, food_oil, feed_bean, feed_cake, seed_bean, other_oil, proc_bean, stock_bean:stock_cake)
SOY_agg <- colSums(SOY_MUN_fin)

SOY_agg <- as.data.frame(cbind("MUN" = SOY_agg, "FAO" = c(CBS_SOY$production, CBS_SOY$import, CBS_SOY$export, CBS_SOY$food[1:2], CBS_SOY$feed[c(1,3)], CBS_SOY$seed[1], CBS_SOY$other[2], CBS_SOY$processing[1], CBS_SOY$stock_addition)))
SOY_agg$ratio <- SOY_agg$FAO/SOY_agg$MUN 

# NOTE: trade ratios should be 1 already! (no re-scaling here)

# rescale MU level data
SOY_MUN_fin <- as.data.frame(t(t(SOY_MUN_fin)*SOY_agg$ratio))

# check for balance, adding columns for total supply and demand of each product
SOY_agg$MUN_fin <- colSums(SOY_MUN_fin)
SOY_agg$check <- SOY_agg$MUN_fin == SOY_agg$FAO

SOY_MUN_fin  <- SOY_MUN_fin %>%  mutate(total_supply_bean = prod_bean + imp_bean, 
                                        total_supply_oil = prod_oil + imp_oil, 
                                        total_supply_cake = prod_cake + imp_cake, 
                                        total_use_bean = exp_bean + food_bean + feed_bean + seed_bean + proc_bean + stock_bean,
                                        total_use_oil = exp_oil + food_oil + other_oil + stock_oil,
                                        total_use_cake = exp_cake + feed_cake + stock_cake)

sum(SOY_MUN_fin$total_supply_bean) == sum(SOY_MUN_fin$total_use_bean)
sum(SOY_MUN_fin$total_supply_oil) == sum(SOY_MUN_fin$total_use_oil)
sum(SOY_MUN_fin$total_supply_cake) == sum(SOY_MUN_fin$total_use_cake)


# add columns for excess supply and demand of each product
SOY_MUN_fin <- mutate(SOY_MUN_fin, 
                  excess_supply_bean = ifelse(total_supply_bean - total_use_bean > 0, total_supply_bean - total_use_bean, 0), 
                  excess_supply_oil =  ifelse(total_supply_oil -  total_use_oil > 0,  total_supply_oil -  total_use_oil, 0),
                  excess_supply_cake = ifelse(total_supply_cake - total_use_cake > 0, total_supply_cake - total_use_cake, 0),
                  excess_use_bean = ifelse(total_use_bean - total_supply_bean > 0, total_use_bean - total_supply_bean, 0), 
                  excess_use_oil =  ifelse(total_use_oil -  total_supply_oil > 0,  total_use_oil -  total_supply_oil, 0),
                  excess_use_cake = ifelse(total_use_cake - total_supply_cake > 0, total_use_cake - total_supply_cake, 0))

# add columns for domestic use of each product
SOY_MUN_fin <- mutate(SOY_MUN_fin, 
                      domestic_use_bean =  total_use_bean - exp_bean,
                      domestic_use_oil  =  total_use_oil -  exp_oil ,
                      domestic_use_cake =  total_use_cake - exp_cake)

# add MU identifiers back again
SOY_MUN_fin <- bind_cols(SOY_MUN[,1:4], SOY_MUN_fin)

# merge with GEO file
GEO_MUN_SOY_fin <- right_join(GEO_MUN_SOY, dplyr::select(SOY_MUN_fin, c(co_mun, total_supply_bean:excess_use_cake)), by = "co_mun")


# re-scale MU exports/imports files to match CBS values as well
# EXP_MUN_SOY <- mutate(EXP_MUN_SOY, 
#                      export     = ifelse(product == "soybean", export*SOY_agg["exp_bean","ratio"],      ifelse(product == "soy_oil", export*SOY_agg["exp_oil","ratio"],     export*SOY_agg["exp_cake","ratio"] )),
#                      export_dol = ifelse(product == "soybean", export_dol*SOY_agg["exp_bean","ratio"],  ifelse(product == "soy_oil", export_dol*SOY_agg["exp_oil","ratio"], export_dol*SOY_agg["exp_cake","ratio"] )))
# 
# IMP_MUN_SOY <- mutate(IMP_MUN_SOY, 
#                      import     = ifelse(product == "soybean", import*SOY_agg["imp_bean","ratio"],      ifelse(product == "soy_oil", import*SOY_agg["imp_oil","ratio"],     import*SOY_agg["imp_cake","ratio"] )),
#                      import_dol = ifelse(product == "soybean", import_dol*SOY_agg["imp_bean","ratio"],  ifelse(product == "soy_oil", import_dol*SOY_agg["imp_oil","ratio"], import_dol*SOY_agg["imp_cake","ratio"] )))

# export data -------------------------------------------------------------------
if(write){
  saveRDS(SOY_MUN_fin, file = "intermediate_data/SOY_MUN_fin.rds")
  saveRDS(GEO_MUN_SOY_fin, file = "intermediate_data/GEO_MUN_SOY_fin.rds")
  saveRDS(CBS_SOY, file = "intermediate_data/CBS_SOY.rds")
  write.csv2(CBS_SOY, file = "intermediate_data/CBS_SOY.rds")
  saveRDS(EXP_MUN_SOY, file = "intermediate_data/EXP_MUN_SOY_cbs.rds")
  saveRDS(IMP_MUN_SOY, file = "intermediate_data/IMP_MUN_SOY_cbs.rds")
}
