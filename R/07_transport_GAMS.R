
############ transport allocation using EUclidean distance ############################

library(openxlsx)
library(dplyr)
library(reshape2)
library(gdxrrw)

#### set GAMS directory according to location where GAMS is installed on your PC ###
igdx("C:/GAMS/34")

write = FALSE

# load data
SOY_MUN <- readRDS("intermediate_data/SOY_MUN_fin.rds")
MUN_capital_dist <- readRDS("intermediate_data/MUN_capital_dist.rds")
MUN_center_dist <- readRDS("intermediate_data/MUN_center_dist.rds")
class(MUN_capital_dist) <- "numeric"

# prepare data for GAMS --------------------------------------------------------------

# total supply and demand per MU for each product
supply <- dplyr::select(SOY_MUN, c(co_mun, total_supply_bean:total_supply_cake)) %>% rename("a" = "co_mun", "bean" = "total_supply_bean", "oil" = "total_supply_oil", "cake" = "total_supply_cake") 
demand <- dplyr::select(SOY_MUN, c(co_mun, total_use_bean:total_use_cake)) %>% rename("a" = "co_mun", "bean" = "total_use_bean", "oil" = "total_use_oil", "cake" = "total_use_cake")

# excess supply and demand
excess_supply <- dplyr::select(SOY_MUN, c(co_mun, excess_supply_bean:excess_supply_cake)) %>% rename("a" = "co_mun", "bean" = "excess_supply_bean", "oil" = "excess_supply_oil", "cake" = "excess_supply_cake")
excess_demand <- dplyr::select(SOY_MUN, c(co_mun, excess_use_bean:excess_use_cake)) %>% rename("a" = "co_mun", "bean" = "excess_use_bean", "oil" = "excess_use_oil", "cake" = "excess_use_cake")

# restricted tables only containing non-zero values of excess supply and demand
excess_supply_pos <- excess_supply[(excess_supply$bean + excess_supply$oil + excess_supply$cake)>0,]
excess_demand_pos <- excess_demand[(excess_demand$bean + excess_demand$oil + excess_demand$cake)>0,]
  
# preliminary cost matrix: Euclidean distance of capitals or central points
MUN_dist <- MUN_capital_dist
#MUN_center_dist[MUN_dist == 0] <- 10000
#diag(MUN_center_dist) <- 0
#MUN_dist <- MUN_center_dist

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

# for optimization without modal split: use only MUN_dist as cost
MUN_dist_long <- melt(MUN_dist, varnames = c("a","b"))

# sheet with only municipality codes
MUN <- dplyr::select(SOY_MUN, co_mun:nm_mun)

# write to file ------------------------------
if (write){
  write.xlsx(MUN, file = "GAMS/MUN_codes.xlsx", col.names = F, row.names = F, sheetName = "MUN_codes")
  write.csv(MUN_dist_long, file = "GAMS/MUN_dist_long.csv", row.names = F)
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



# read and process results -------------------------------------------------------
transport_sol <- rgdx("GAMS/transport_sol.gdx", requestList = list(name = "xsoytransport") )
flows <- as.data.frame(transport_sol$val)
colnames(flows) <- c(paste0(transport_sol$domains,"_num"), "value")
uels <- transport_sol$uels[[1]]
flows$co_orig <- as.numeric(uels[flows$a_num])
flows$co_dest <- as.numeric(uels[flows$b_num])
flows$mode <- uels[flows$mode_num]
flows$product <- uels[flows$product_num]
flows <- flows[,c(6:9,5)]

# simple version without modal split
transport_sol_simple <- rgdx("GAMS/transport_sol_simple.gdx", requestList = list(name = "xsoytransport") )
flows_simple <- as.data.frame(transport_sol_simple$val)
colnames(flows_simple) <- c(paste0(transport_sol_simple$domains,"_num"), "value")
uels <- transport_sol_simple$uels[[1]]
flows_simple$co_orig <- as.numeric(uels[flows_simple$a_num])
flows_simple$co_dest <- as.numeric(uels[flows_simple$b_num])
flows_simple$product <- uels[flows_simple$product_num]
flows_simple <- flows_simple[,c(5:7,4)]
#total cost
transport_sol_simple_cost <- rgdx("GAMS/transport_sol_simple.gdx", requestList = list(name = "xtotalcost") )
transport_sol_simple_cost <- transport_sol_simple_cost$val

# compare solution from simple model in GAMS and R
all.equal(arrange(flows_simple, by=value), arrange(flows_R, by = value))
flows_comp <- full_join(flows_R, flows_simple, by = c("co_orig", "co_dest", "product"))
flows_comp <- mutate(flows_comp, diff = value.x - value.y)
flows_comp[is.na(flows_comp)] <- 0
colSums(select(flows_comp, value.x, value.y))

# export
saveRDS(flows, file = "intermediate_data/transport_flows.rds")
