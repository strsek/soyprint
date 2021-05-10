
############ transport allocation using EUclidean distance ############################

library(openxlsx)
library(dplyr)
library(reshape2)
library(gdxrrw)

#### set GAMS directory according to location where GAMS is installed on your PC ###
# igdx("C:/GAMS/34")

write = FALSE

# load data
SOY_MUN <- readRDS("intermediate_data/SOY_MUN_fin.rds")
MUN_dist <- readRDS("intermediate_data/MUN_dist.rds")
class(MUN_dist) <- "numeric"

# prepare data for GAMS --------------------------------------------------------------

# total supply and demand per MU for each product
supply <- dplyr::select(SOY_MUN, c(co_mun, total_supply_bean:total_supply_cake)) %>% rename("a" = "co_mun", "bean" = "total_supply_bean", "oil" = "total_supply_oil", "cake" = "total_supply_cake") 
demand <- dplyr::select(SOY_MUN, c(co_mun, total_use_bean:total_use_cake)) %>% rename("a" = "co_mun", "bean" = "total_use_bean", "oil" = "total_use_oil", "cake" = "total_use_cake")

# excess supply and demand
SOY_MUN <- mutate(SOY_MUN, excess_supply_bean = ifelse(total_supply_bean - total_use_bean > 0, total_supply_bean - total_use_bean, 0), 
                           excess_supply_oil =  ifelse(total_supply_oil -  total_use_oil > 0,  total_supply_oil -  total_use_oil, 0),
                           excess_supply_cake = ifelse(total_supply_cake - total_use_cake > 0, total_supply_cake - total_use_cake, 0),
                           excess_use_bean = ifelse(total_use_bean - total_supply_bean > 0, total_use_bean - total_supply_bean, 0), 
                           excess_use_oil =  ifelse(total_use_oil -  total_supply_oil > 0,  total_use_oil -  total_supply_oil, 0),
                           excess_use_cake = ifelse(total_use_cake - total_supply_cake > 0, total_use_cake - total_supply_cake, 0))

excess_supply <- dplyr::select(SOY_MUN, c(co_mun, excess_supply_bean:excess_supply_cake)) %>% rename("a" = "co_mun", "bean" = "excess_supply_bean", "oil" = "excess_supply_oil", "cake" = "excess_supply_cake")
excess_demand <- dplyr::select(SOY_MUN, c(co_mun, excess_use_bean:excess_use_cake)) %>% rename("a" = "co_mun", "bean" = "excess_use_bean", "oil" = "excess_use_oil", "cake" = "excess_use_cake")

# restricted tables only containing positive values of excess supply and demand
#excess_supply_pos <- excess_supply[(excess_supply$bean + excess_supply$oil + excess_supply$cake)>0,]
#excess_demand_pos <- excess_demand[(excess_demand$bean + excess_demand$oil + excess_demand$cake)>0,]
  
# preliminary cost matrix
# set zeroes to small positive value and then set diagonal to zero again
MUN_dist[MUN_dist == 0] <- 10000
diag(MUN_dist) <- 0

# generate fake cost matrices for truck, train and ship
cost_truck <- MUN_dist/1000000
cost_train <- MUN_dist/2000000
cost_ship <- MUN_dist/3000000

npaths <- c(1:length(MUN_dist))
# set random elements of train and ship cost to very large value to simulate non-existing paths
# [sample(npaths,5000000)] <- 99999999999999999999999
# cost_ship[sample(npaths,5000000)] <- 99999999999999999999999

#cost_mat <- rbind(cost_truck, cost_train, cost_ship)
#cost_mat <- cbind(rownames(cost_mat), cost_mat)
#cost_mat <- cbind(c(rep("truck", nrow(SOY_MUN)), rep("train", nrow(SOY_MUN)), rep("ship", nrow(SOY_MUN))), cost_mat)

# reshape to long format
cost_truck <- cost_truck %>% melt(varnames = c("a","b")) %>% rename("truck" = "value") 
cost_train <- cost_train %>% melt(varnames = c("a","b")) %>% rename("train" = "value") 
cost_ship <- cost_ship %>% melt(varnames = c("a","b")) %>% rename("ship" = "value") 

transport_cost <- cost_truck %>% left_join(cost_train, by = c("a", "b")) %>% left_join(cost_ship, by = c("a", "b"))
# restricted sample only containing only MUs with excess supply (a) and demand (b)
#transport_cost_pos <- filter(transport_cost, a %in% excess_supply_pos$a, b %in% excess_demand_pos$a)

# sheet with only municipality codes
MUN <- dplyr::select(SOY_MUN, co_mun:nm_mun)

# write to file ------------------------------
if (write){
  write.xlsx(MUN, file = "GAMS/MUN_codes.xlsx", col.names = F, row.names = F, sheetName = "MUN_codes")
  write.xlsx(demand, file = "GAMS/demand.xlsx", row.names = F, sheetName = "demand")
  write.xlsx(supply, file = "GAMS/supply.xlsx", row.names = F, sheetName = "supply")
  write.xlsx(excess_demand, file = "GAMS/excess_demand.xlsx", row.names = F, sheetName = "demand")
  write.xlsx(excess_supply, file = "GAMS/excess_supply.xlsx", row.names = F, sheetName = "supply")
  write.csv(transport_cost, file = "GAMS/transport_cost.csv", row.names	= F)
  #write.xlsx(transport_cost, file = "GAMS/transport_cost.xlsx", row.names = F, col.names = T)
  #write.xlsx(excess_supply_pos, file ="GAMS/excess_supply_pos.xlsx",row.names = F, sheetName = "supply")
  #write.xlsx(excess_demand_pos, file ="GAMS/excess_demand_pos.xlsx",row.names = F, sheetName = "demand")
  #write.csv(transport_cost_pos, file = "GAMS/transport_cost_pos.csv", row.names	= F)
}
