##### Benchmark comparison of subnational supply chain results with TRASE data #########

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)

write = TRUE

options(scipen = 9999)

# load data ----------

SOY_MUN <- readRDS("intermediate_data/SOY_MUN_fin.rds")

# trase data for Brazilian soy
trase <- read.csv("input_data/BRAZIL_SOY_2.5.1_TRASE.csv")
# country name ISO correspondence
trase_names <- read.csv2("input_data/trase_names.csv", fileEncoding="UTF-8-BOM")
#flows <- readRDS("intermediate_data/transport_flows.rds")
# supply chain results
#source_to_export_euclid <- readRDS("intermediate_data/source_to_export.RDS")
#source_to_export_road <- readRDS("intermediate_data/source_to_export_road.RDS")
source_to_export_list <- readRDS("intermediate_data/source_to_export_list.RDS")


regions <- readRDS("intermediate_data/regions.rds")
regions_btd <- distinct(regions, CO_BTD, ISO_BTD) %>% arrange(CO_BTD) 


## prepare TRASE data ----------------------------------------------

# add ISO codes to trase flows
trase <- left_join(trase, trase_names, by = "COUNTRY") %>% relocate(ISOA3, .after = COUNTRY)

# remove columns not needed
trase_mun <- trase %>% dplyr::select(STATE, MUNICIPALITY, COUNTRY, ISOA3, ECONOMIC.BLOC, SOY_EQUIVALENT_TONNES, TRASE_GEOCODE, LAND_USE_HA) %>% 
                       rename("nm_state" = "STATE", "nm_mun" = "MUNICIPALITY", "nm_dest" = "COUNTRY", "co_dest" = "ISOA3", "gr_dest" = "ECONOMIC.BLOC", 
                              "exp_tot" = "SOY_EQUIVALENT_TONNES", "co_mun_trase" = "TRASE_GEOCODE", "landuse" = "LAND_USE_HA")%>%
                       mutate(co_mun_trase = as.numeric(substr(co_mun_trase, 4, nchar(co_mun_trase)))) %>% replace_na(list(co_mun_trase = 9999999)) %>%
                       relocate(co_mun_trase, .before=nm_mun)

# aggregate destination countries that belong to ROW in FABIO
trase_mun <- trase_mun %>% left_join(dplyr::select(regions, CO_PAIS_ISOA3, ISO_BTD), by = c("co_dest" = "CO_PAIS_ISOA3"))%>%
  relocate(ISO_BTD, .after = co_dest) %>% rename(to_code = ISO_BTD)

trase_mun <- trase_mun %>% group_by(nm_state, co_mun_trase, nm_mun, to_code) %>% 
  summarise(exp_tot = sum(exp_tot, na.rm = TRUE), landuse = sum(landuse, na.rm = TRUE), .groups = "drop")


# check if MU codes from trase match IBGE codes from SOY_MUN
mun_code_comp <- dplyr::select(trase_mun, co_mun_trase, nm_mun) %>% distinct(co_mun_trase, nm_mun) %>%
  left_join(dplyr::select(SOY_MUN, co_mun, nm_mun), by=c("co_mun_trase" = "co_mun")) %>%
  mutate(nm_mun.x = iconv(nm_mun.x, from = 'UTF-8', to = 'ASCII//TRANSLIT'), nm_mun.y = iconv(nm_mun.y, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>% 
  mutate(check = nm_mun.x==nm_mun.y) # looks good!

trase_mun <- trase_mun %>% rename(co_mun = co_mun_trase) #%>% left_join(select(SOY_MUN, c(co_mun, co_state)), by = "co_mun") %>% relocate(co_state, .before = nm_state)


## match TRASE with own results ------------------

#flow_list <- list(euclid = source_to_export_euclid, road = source_to_export_road)

#for (res in names(flow_list)){
  
results_list <- sapply(names(source_to_export_list), function(fl){

# translate flows of oil and cake into soybean equivalents and aggregate
# NOTE: we need to use OUR OWN equivalence factor here, which comes from the national average conversion factor from soybean to cake and oil according to FAO CBS! (see script 01_consumption_and_processing)
results_mun_agg <- source_to_export_list[[fl]] %>% mutate(value = ifelse(item_code != "bean", value*1.023466, value)) %>% #1.031
  group_by(from_code, to_code) %>% summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
  mutate(from_code = as.numeric(from_code)) %>%
  rename("co_mun" = "from_code", !!str_c(fl) := "value")
}, USE.NAMES = TRUE, simplify = FALSE)

results_df <- results_list %>% reduce(full_join, by = c("co_mun", "to_code")) 

# join trase data with own results
comp_mun <- trase_mun %>% full_join(results_df, by = c("co_mun", "to_code")) %>%
  rename(trase = exp_tot)
  

# add state codes and missing names from join
comp_mun <- comp_mun %>% dplyr::select(-c(nm_mun, nm_state)) %>% left_join(SOY_MUN[,1:4], by = "co_mun") %>%
  relocate(nm_mun, .after = co_mun) %>% relocate(c(co_state, nm_state), .before = co_mun)

# add state codes from SOY_MUN (redundant)
# comp_mun <- comp_mun %>% left_join(dplyr::select(SOY_MUN, c(co_mun, co_state)), by = "co_mun") %>% relocate(co_state, .before = nm_state)

# change NAs to zero
#comp_mun <- comp_mun %>% replace_na(list(trase = 0, own = 0))
comp_mun[,6:ncol(comp_mun)][is.na(comp_mun[,6:ncol(comp_mun)])] <- 0

# add country group from FABIO
regions_btd <- filter(regions, ISO_BTD != "ROW") %>% distinct(CO_BTD, ISO_BTD, region)
comp_mun <- comp_mun %>% left_join(dplyr::select(regions_btd, c(ISO_BTD, region)), by = c("to_code" = "ISO_BTD")) %>%
  rename(to_region = region) %>% relocate(to_region, .after = to_code) %>%
  # separate China
  mutate(to_region = ifelse(to_code == "CHN", "China", to_region)) %>%
  mutate(to_region = ifelse(to_code == "ROW", "ROW", to_region))

# drop land-use (optional)
comp_mun <- dplyr::select(comp_mun, -landuse)


# aggregate by state
comp_state <- comp_mun %>% group_by(co_state, to_code, to_region) %>% 
  summarise(across(c("trase", names(results_list)), sum, na.rm = TRUE), .groups = "drop")
  #  summarise(trase = sum(trase, na.rm = TRUE), own = sum(own, na.rm = T), .groups = "drop")

# comp_state <- mutate(comp_state, diff = own-trase, rel_diff = (own-trase)/trase)
# comp_state$rel_diff[!is.finite(comp_state$rel_diff)] <- 1
# 


# compare production with trase flows 
# check outliers

# aggregate by region
comp_state_by_region <- comp_mun %>% group_by(co_state, to_region) %>% 
  summarise(across(c("trase", names(results_list)), sum, na.rm = TRUE), .groups = "drop")

# total exports by destination region
exp_by_dest <- comp_mun %>% group_by(to_code) %>% 
  summarise(across(c("trase", names(results_list)), sum, na.rm = TRUE), .groups = "drop")

exp_by_dest_region <- comp_mun %>% group_by(to_region) %>% 
  summarise(across(c("trase", names(results_list)), sum, na.rm = TRUE), .groups = "drop")

# # export sums
# exp_total <- colSums(filter(exp_by_dest, to_code != "BRA")[,2:3])
# exp_total["diff"] <- exp_total[2] - exp_total[1]
# 
# flow_total <- colSums(exp_by_dest[,2:3])
# flow_total["diff"] <- flow_total[2] - flow_total[1]

# total flows by MU
comp_mun_total <- comp_mun %>% group_by(co_state, nm_state, co_mun, nm_mun) %>% summarise(across(c("trase", names(results_list)), sum, na.rm = TRUE), .groups = "drop")
# add soy production
comp_mun_total <- full_join(comp_mun_total, dplyr::select(SOY_MUN, c(co_mun, prod_bean)), by = "co_mun") # prod_oil, prod_cake
# change oil and cake into soybean equivalents
#comp_mun_total <- mutate(comp_mun_total, prod_oil = prod_oil*1.031, prod_cake = prod_cake*1.031) %>% rename("prod_oil_eq" = "prod_oil", "prod_cake_eq" = "prod_cake")
# add missing names
comp_mun_total <- comp_mun_total %>% dplyr::select(-c(nm_mun, co_state, nm_state)) %>% left_join(SOY_MUN[,1:4], by = "co_mun") %>%
  relocate(nm_mun, .after = co_mun) %>% relocate(c(co_state, nm_state), .before = co_mun)
# change NA to zero
comp_mun_total <- comp_mun_total %>% replace_na(list(trase = 0, own = 0, prod_bean = 0, prod_oil = 0, prod_cake = 0))

#comp_mun_total <- mutate(comp_mun_total, prod_diff = own-prod_bean)

#return(list(comp_mun = comp_mun, comp_mun_total=comp_mun_total, comp_state=comp_state, comp_state_by_region=comp_state_by_region, exp_by_dest=exp_by_dest, exp_by_dest_region=exp_by_dest_region))
#})

# add columns for differences to trase
comp_mun <- comp_mun %>% mutate(across(names(results_list), .fns = list(diff = ~ .-trase)))
comp_state <- comp_state %>% mutate(across(names(results_list), .fns = list(diff = ~ .-trase)))
comp_state_by_region <- comp_state_by_region %>% mutate(across(names(results_list), .fns = list(diff = ~ .-trase)))
#comp_mun_total <- comp_mun_total %>% mutate(across(names(results_list), .fns = list(diff = ~ .-trase)))


# total differences
(diff_mun <- dplyr::select(comp_mun, contains("diff")) %>% abs() %>% colSums())
(diff_state <- dplyr::select(comp_state, contains("diff")) %>% abs() %>% colSums())
(diff_state_by_region <- dplyr::select(comp_state_by_region, contains("diff")) %>% abs() %>% colSums())


#comp_mun_total <- comp_mun_total %>% mutate(across(names(results_list), .fns = list(diff = ~ .-trase)))

#############

# comp_mun_euclid <- comp_mun
# comp_state_euclid  <- comp_state
# comp_mun_total_euclid <- comp_mun_total
# 
# comp_mun_road <- comp_mun
# comp_state_road  <- comp_state
# comp_mun_total_road <- comp_mun_total

# comp_mun_all <- left_join(results_list$euclid$comp_mun, dplyr::select(results_list$road$comp_mun, !trase), by = colnames(results_list$euclid$comp_mun)[1:6]) %>%
#   rename(euclid = own.x, road = own.y)
# comp_mun_all <- mutate(comp_mun_all, 
#                        diff_own = euclid - road,
#                        diff_euclid = (trase - euclid),
#                        diff_road = (trase - road))
# 
# sum(abs(comp_mun_all$diff_euclid), na.rm = T)
# sum(abs(comp_mun_all$diff_road), na.rm = T)
# 
# # aggregate by state
# comp_state_all <- comp_mun_all %>% group_by(co_state, to_code, to_region) %>% 
#   summarise(trase = sum(trase, na.rm = TRUE), euclid = sum(euclid, na.rm = T), road = sum(road, na.rm = T), .groups = "drop")
# comp_state_all <- mutate(comp_state_all, 
#                        diff_own = euclid - road,
#                        diff_euclid = (trase - euclid),
#                        diff_road = (trase - road))
# 
# # aggregate state results by destination region
# comp_state_by_region_all <- comp_mun_all %>% group_by(co_state, to_region) %>% 
#   summarise(trase = sum(trase, na.rm = TRUE), euclid = sum(euclid, na.rm = T), road = sum(road, na.rm = T), .groups = "drop")
# comp_state_by_region_all <- mutate(comp_state_by_region_all, 
#                          diff_own = euclid - road,
#                          diff_euclid = (trase - euclid),
#                          diff_road = (trase - road))
# 
# 
# sum(abs(comp_state_all$diff_euclid), na.rm = T)
# sum(abs(comp_state_all$diff_road), na.rm = T)
# 
# comp_mun_total_all <- left_join(results_list$euclid$comp_mun_total, dplyr::select(results_list$road$comp_mun_total, !c(trase, prod_oil, prod_cake, prod_bean)), by = colnames(results_list$euclid$comp_mun_total)[-c(5:9)]) %>%
#   rename(euclid = own.x, road = own.y) %>% relocate(road, .after = euclid)
# 
# sum(comp_mun_total_all$euclid)
# sum(comp_mun_total_all$road)

# # compare results
# mape_state <- mean(comp_state$rel_diff)
# cor(comp_state$trase, comp_state$own)

# plot data
ggplot(comp_state, aes(x=trase, y = euclid))+
  geom_point(color = "darkgreen")+
  geom_abline(slope = 1) + 
  geom_abline(slope = 1.05, linetype="dashed", color = "blue")+
  geom_abline(slope = 0.95, linetype="dashed", color = "blue")+
  geom_abline(slope = 1.5, linetype="dashed", color = "cyan")+
  geom_abline(slope = 0.5, linetype="dashed", color = "cyan")


ggplot(comp_mun, aes(x=trase, y = IBGE), color = "blue")+
  geom_point(color = "red")+
  geom_abline(slope = 1) + 
  geom_abline(slope = 1.05, linetype="dashed", color = "blue")+
  geom_abline(slope = 0.95, linetype="dashed", color = "blue")+
  geom_abline(slope = 1.5, linetype="dashed", color = "cyan")+
  geom_abline(slope = 0.5, linetype="dashed", color = "cyan")


if (write){
  write.csv2(comp_mun, "intermediate_data/comp_mun.csv")
  write.csv2(comp_state, "intermediate_data/comp_state.csv")
  write.csv2(comp_state_by_region, "intermediate_data/comp_state_by_region.csv")
  write.csv2(comp_mun_total, "intermediate_data/comp_mun_total.csv")
}

