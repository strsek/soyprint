
###### Harmonize municipality exports and imports with FAO bilateral trade data for 2013 ############

library(dplyr)
library(tidyr)
library(janitor)
library(openxlsx)

write <- FALSE

# load data ------------------------------------------------------------------------------------

EXP_MUN_SOY <- readRDS("intermediate_data/EXP_MUN_SOY_00.rds")
IMP_MUN_SOY <- readRDS("intermediate_data/IMP_MUN_SOY_00.rds")

btd <- readRDS("input_data/FABIO/btd_bal.rds")
cbs <- readRDS("input_data/FABIO/cbs_full.rds")

# countries contained in COMEX
COMEX_regions <- read.csv2(file = "input_data/PAIS_COMEX.csv", header = TRUE, stringsAsFactors = F)
# full sample of countries contained in FAO
FAO_regions <- read.csv(file = "input_data/FABIO/FAO_regions_full.csv", stringsAsFactors = F)
# countries contained in FABIO
FABIO_regions <- read.xlsx("input_data/FABIO/FABIO_regions.xlsx", colNames = TRUE)
#load("intermediate_data/FAO_areas.RData")


# harmonize country sample and codes of COMEX and FAO -------------------------------------------------------

### match COMEX country codes/names with FAO system used in FABIO

# set "ZZZ" region ISOA3 code from COMEX to "ROW" as used in FAO
COMEX_regions$CO_PAIS_ISOA3[COMEX_regions$CO_PAIS_ISOA3 == "ZZZ"] <- "ROW"
COMEX_regions$CO_PAIS_ISON3[COMEX_regions$CO_PAIS_ISON3 == 898] <- 999

# the FAO regions table contains duplicates of countries, but with different ISO codes: detect them and remove the ones not used in FABIO
FAO_regions_dup <- FAO_regions %>% get_dupes(iso3c)
# remove codes 351 (China), 62 (Ethiopia PDR) and 206 (Sudan, former) 
FAO_regions <- filter(FAO_regions, !code %in% c(351,62,206))

# match COMEX with FAO by ISO3 code
regions <- full_join(COMEX_regions, FAO_regions, by = c("CO_PAIS_ISOA3" = "iso3c")) 
# unmatched regions (small islands etc.) can be assigned to ROW 
regions$code[is.na(regions$code)] <- 999

# rename "code" into "CO_FAO" and "CO_PAIS" into "CO_COMEX"
regions <- regions %>% rename("CO_FAO" = "code", "CO_COMEX" = "CO_PAIS") %>% relocate (CO_FAO, .after = CO_COMEX)

# create separate column with FAO code, but setting it to the ROW code in case the region is not part of the BTD and therefore FABIO
btd_regions <- unique(btd$from_code)
all.equal(FABIO_regions$FAO.Code, btd_regions)
regions <- regions %>% mutate(btd = ifelse(CO_FAO %in% btd_regions, TRUE, FALSE)) %>% mutate(CO_BTD = ifelse(btd == TRUE, CO_FAO, 999)) %>% relocate (CO_BTD, .after = CO_FAO) 
regions <- regions %>% mutate(ISO_BTD = ifelse(CO_BTD == 999, "ROW", CO_PAIS_ISOA3)) %>% relocate(ISO_BTD, .after = CO_BTD)

# extract relevant COMEX country codes from municipality soy trade data (--> get rid of irrelevant countries)
#COMEX_codes_rel <- unique(c(EXP_MUN_SOY$co_destin, IMP_MUN_SOY$co_origin))
#regions_rel <- filter(regions, CO_PAIS %in% COMEX_codes_rel)


# harmonize MU trade data with FABIO ------------------------------------------------------------------

### aggregate exports/imports to/from FABIO ROW countries

# add btd country codes
EXP_MUN_SOY <- EXP_MUN_SOY %>% left_join(regions[,c(1,3:4)], by = c("co_destin" = "CO_COMEX")) %>% rename("to_code" = "CO_BTD", "to_name" = "ISO_BTD") %>% relocate(to_code:to_name, .after = nm_destin)
IMP_MUN_SOY <- IMP_MUN_SOY %>% left_join(regions[,c(1,3:4)], by = c("co_origin" = "CO_COMEX")) %>% rename("from_code" = "CO_BTD", "from_name" = "ISO_BTD") %>% relocate(from_code:from_name, .after = nm_origin)

# aggregate
EXP_MUN_SOY <- EXP_MUN_SOY %>% group_by(across(c(-export, - export_dol, - co_destin, - nm_destin))) %>% 
                               summarise(export = sum(export, na.rm = TRUE), export_dol = sum(export_dol, na.rm = TRUE), .groups = "drop") %>% 
                               ungroup() 

IMP_MUN_SOY <- IMP_MUN_SOY %>% group_by(across(c(-import, - import_dol, - co_origin, - nm_origin))) %>% 
                               summarise(import = sum(import, na.rm = TRUE), import_dol = sum(import_dol, na.rm = TRUE), .groups = "drop") %>% 
                               ungroup() 

### compare aggregate MU trade values with FAO BTD 

# filter soy products trade of Brazil from btd
# soybeans (2555), soybean oil (2571), soybean cake (2590)
btd_exp_BRA_soy <- filter(btd, from_code == 21, item_code %in% c(2555, 2571, 2590))
btd_imp_BRA_soy <- filter(btd, to_code == 21, item_code %in% c(2555, 2571, 2590))

# add item codes to MU trade data
EXP_MUN_SOY <- EXP_MUN_SOY %>% mutate (item_code = ifelse(product == "soybean", 2555, ifelse(product == "soy_oil", 2571, 2590))) %>% relocate(item_code, .after = product)
IMP_MUN_SOY <- IMP_MUN_SOY %>% mutate (item_code = ifelse(product == "soybean", 2555, ifelse(product == "soy_oil", 2571, 2590))) %>% relocate(item_code, .after = product)


## check how well data already matches:

# aggregate municipality trade to national values
EXP_NAT_SOY <- EXP_MUN_SOY %>% group_by(product, item_code, to_code, to_name) %>% 
                               summarise(export = sum(export, na.rm = TRUE), export_dol = sum(export_dol, na.rm = TRUE), .groups = "drop") %>% 
                               ungroup() 

IMP_NAT_SOY <- IMP_MUN_SOY %>% group_by(product, item_code, from_code, from_name) %>% 
                               summarise(import = sum(import, na.rm = TRUE), import_dol = sum(import_dol, na.rm = TRUE), .groups = "drop") %>% 
                               ungroup() 

# add column with corresponding BTD trade data
EXP_NAT_SOY <- EXP_NAT_SOY %>% full_join(btd_exp_BRA_soy[,c(2,4:5)], by = c("item_code" = "item_code", "to_code" = "to_code")) %>% rename("export_btd" = "value")

IMP_NAT_SOY <- IMP_NAT_SOY %>% full_join(btd_imp_BRA_soy[,c(2:3,5)], by = c("item_code" = "item_code", "from_code" = "from_code")) %>% rename("import_btd" = "value")

# add product names for trade that was not registered in comex 
EXP_NAT_SOY = mutate(EXP_NAT_SOY, product = ifelse(item_code == 2555, "soybean", ifelse(item_code == 2571, "soy_oil", "soy_cake")))
IMP_NAT_SOY = mutate(IMP_NAT_SOY , product = ifelse(item_code == 2555, "soybean", ifelse(item_code == 2571, "soy_oil", "soy_cake")))
# add country codes names to trade that was not registered in comex
EXP_NAT_SOY = EXP_NAT_SOY %>% left_join(FABIO_regions[,c(1,3)], by = c("to_code" = "FAO.Code")) %>% mutate(to_name = ISO) %>% dplyr::select(!ISO)
IMP_NAT_SOY = IMP_NAT_SOY %>% left_join(FABIO_regions[,c(1,3)], by = c("from_code" = "FAO.Code")) %>% mutate(from_name = ISO) %>% dplyr::select(!ISO)

# EXP_NAT_SOY = EXP_NAT_SOY %>% rename("to_code" = "co_btd", "to_name" = "nm_btd" )

# check overall consitency of trade sums
colSums(EXP_NAT_SOY[,c(5:7)], na.rm = TRUE)
colSums(EXP_MUN_SOY[,c(10:11)], na.rm = TRUE)
colSums(IMP_NAT_SOY[,c(5:7)], na.rm = TRUE)
colSums(IMP_MUN_SOY[,c(10:11)], na.rm = TRUE)


# check country sums
#EXP_SOY <- EXP_MUN_SOY %>% group_by(product) %>% summarise(export = sum(export, na.rm = T), export_dol = sum(export_dol, na.rm = T))
EXP_SOY <- EXP_NAT_SOY %>% group_by(product) %>% summarise(export = sum(export, na.rm = T), export_dol = sum(export_dol, na.rm = T), export_btd = sum(export_btd, na.rm = T))
#IMP_SOY <- IMP_MUN_SOY %>% group_by(product) %>% summarise(import = sum(import, na.rm = T), import_dol = sum(import_dol, na.rm = T))
IMP_SOY <- IMP_NAT_SOY %>% group_by(product) %>% summarise(import = sum(import, na.rm = T), import_dol = sum(import_dol, na.rm = T), import_btd = sum(import_btd, na.rm = T))


# export data --------------------------------------

if (write == TRUE){
  
  # export data for Martin
   saveRDS(EXP_MUN_SOY, file = "intermediate_data/EXP_MUN_SOY.rds")
   saveRDS(IMP_MUN_SOY, file = "intermediate_data/IMP_MUN_SOY.rds")
   saveRDS(EXP_NAT_SOY, file = "intermediate_data/EXP_NAT_SOY.rds")
   saveRDS(IMP_NAT_SOY, file = "intermediate_data/IMP_NAT_SOY.rds")
   saveRDS(regions, file = "intermediate_data/regions.rds")
   write.csv2(regions, file = "intermediate_data/regions.csv")
   saveRDS(btd_exp_BRA_soy, file = "intermediate_data/btd_exp_BRA_soy.rds")
   saveRDS(btd_imp_BRA_soy, file = "intermediate_data/btd_imp_BRA_soy.rds")
  
}