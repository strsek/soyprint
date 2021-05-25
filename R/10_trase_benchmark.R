##### Benchmark comparison of subnational supply chain results with TRASE data #########

library(dplyr)
library(tidyr)

SOY_MUN <- readRDS("intermediate_data/SOY_MUN_fin.rds")

# trase data for Brazilian soy
trase <- read.csv("input_data/BRAZIL_SOY_2.5.1_TRASE.csv")
# country name ISO correspondence
trase_names <- read.csv2("input_data/trase_names.csv", fileEncoding="UTF-8-BOM")
#flows <- readRDS("intermediate_data/transport_flows.rds")
# supply chain results
source_to_export <- readRDS("intermediate_data/source_to_export.RDS")

regions <- readRDS("intermediate_data/regions.rds")
regions_btd <- distinct(regions, CO_BTD, ISO_BTD) %>% arrange(CO_BTD) 

## TRASE data ----------------------------------------------

# add ISO codes to trase flows
trase <- left_join(trase, trase_names, by = "COUNTRY") %>% relocate(ISOA3, .after = COUNTRY)

# remove columns not needed
trase_mun <- trase %>% select(STATE, MUNICIPALITY, COUNTRY, ISOA3, ECONOMIC.BLOC, SOY_EQUIVALENT_TONNES, TRASE_GEOCODE, LAND_USE_HA) %>% 
                       rename("nm_state" = "STATE", "nm_mun" = "MUNICIPALITY", "nm_dest" = "COUNTRY", "co_dest" = "ISOA3", "gr_dest" = "ECONOMIC.BLOC", 
                              "exp_tot" = "SOY_EQUIVALENT_TONNES", "co_mun_trase" = "TRASE_GEOCODE", "landuse" = "LAND_USE_HA")%>%
                       mutate(co_mun_trase = as.numeric(substr(co_mun_trase, 4, nchar(co_mun_trase)))) %>% replace_na(list(co_mun_trase = 9999999)) %>%
                       relocate(co_mun_trase, .before=nm_mun)

# aggregate destination countries that belong to ROW in FABIO
trase_mun <- trase_mun %>% left_join(select(regions, CO_PAIS_ISOA3, ISO_BTD), by = c("co_dest" = "CO_PAIS_ISOA3"))%>%
  relocate(ISO_BTD, .after = co_dest) %>% rename(to_code = ISO_BTD)

trase_mun <- trase_mun %>% group_by(nm_state, co_mun_trase, nm_mun, to_code) %>% 
  summarise(exp_tot = sum(exp_tot, na.rm = TRUE), landuse = sum(landuse, na.rm = TRUE), .groups = "drop")


# check if MU codes from trase match IBGE codes from SOY_MUN
mun_code_comp <- select(trase_mun, co_mun_trase, nm_mun) %>% distinct(co_mun_trase, nm_mun) %>%
  left_join(select(SOY_MUN, co_mun, nm_mun), by=c("co_mun_trase" = "co_mun")) %>%
  mutate(nm_mun.x = iconv(nm_mun.x, from = 'UTF-8', to = 'ASCII//TRANSLIT'), nm_mun.y = iconv(nm_mun.y, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>% 
  mutate(check = nm_mun.x==nm_mun.y) # looks good!

trase_mun <- trase_mun %>% rename(co_mun = co_mun_trase) #%>% left_join(select(SOY_MUN, c(co_mun, co_state)), by = "co_mun") %>% relocate(co_state, .before = nm_state)

## own results ------------------

# translate flows of oil and cake into soybean equivalents and aggregate
results_mun_agg <- source_to_export %>% mutate(value = ifelse(item_code != "bean", value*1.031, value)) %>% 
  group_by(from_code, to_code) %>% summarise(value = sum(value, na.rm = T), .groups = "drop") %>%
  mutate(from_code = as.numeric(from_code))

# join trase data with own results

comp_mun <- trase_mun %>% full_join(results_mun_agg, by = c("co_mun" = "from_code", "to_code")) %>%
  rename(trase = exp_tot, own = value)

# add state codes from SOY_MUN
comp_mun <- comp_mun %>% left_join(select(SOY_MUN, c(co_mun, co_state)), by = "co_mun") %>% relocate(co_state, .before = nm_state)

# add country group from FABIO
comp_mun <- comp_mun %>% left_join(select(regions, c(ISO_BTD, region)), by = c("to_code" = "ISO_BTD")) %>%
  rename(to_region = region) %>% relocate(to_region, .after = to_code) %>%
  # separate China
  mutate(to_region = ifelse(to_code == "CHN", "China", to_region)) %>%
  mutate(to_region = ifelse(to_code == "ROW", "ROW", to_region))


# aggregate by state
comp_state <- comp_mun %>% group_by(co_state, to_code) %>% 
  summarise(trase = sum(trase, na.rm = TRUE), own = sum(own, na.rm = T), .groups = "drop")

comp_state <- mutate(comp_state, diff = own-trase, rel_diff = (own-trase)/trase)
comp_state$rel_diff[!is.finite(comp_state$rel_diff)] <- 1

# compare results
mape_state <- mean(comp_state$rel_diff)
cor(comp_state$trase, comp_state$own)

# plot data
ggplot(comp_state, aes(x=trase, y = own, color = co_state))+
  geom_point()

# aggregate by region
comp_region <- comp_mun %>% group_by(co_state, to_region) %>% 
  summarise(trase = sum(trase, na.rm = TRUE), own = sum(own, na.rm = T), .groups = "drop")

dest_mun <- comp_mun %>% group_by(to_region) %>% 
  summarise(trase = sum(trase, na.rm = TRUE), own = sum(own, na.rm = T), .groups = "drop")


# match trase country names with ISO codes


