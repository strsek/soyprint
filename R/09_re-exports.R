### re-allocation of re-exports, including the sub-national inter-municipality trade ########

library("data.table")
library("Matrix")
library(dplyr)
library(tidyr)
library(tibble)

SOY_MUN <- readRDS("intermediate_data/SOY_MUN_fin.rds")

# BTD ---------------------------------------------------------------------

btd <- readRDS("input_data/FABIO/btd_bal.rds")
cbs <- readRDS("input_data/FABIO/cbs_full.rds")

items <- unique(cbs$item_code)
regions <- readRDS("intermediate_data/regions.rds")
regions_btd <- distinct(regions, CO_BTD, ISO_BTD) %>% arrange(CO_BTD)
soy_items <- c("bean" = 2555, "oil" = 2571, "cake" = 2590)
areas <- sort(unique(cbs$area_code))

## sub-national trade

# imports and exports
exp <- readRDS("intermediate_data/EXP_MUN_SOY.rds")
imp <- readRDS("intermediate_data/IMP_MUN_SOY.rds")

# intra-municipal trade
flows <- readRDS("intermediate_data/transport_flows_R.rds")

setwd("~/BOKU/Thesis/Data/FABIO/fabio")
source("R/01_tidy_functions.R")


# Prepare reallocation of re-exports --------------------------------------

### extend bilateral soy trade data with sub-national trade beteen Brazilian MUs

btd_soy <- filter(btd, item_code %in% c(2555, 2571, 2590))

# bring MU exports in same format as btd
btd_MUN_exp <- exp %>% select(co_mun, item_code, to_code, export) %>%
  rename("from_code" = "co_mun", "value" = "export") %>%
  relocate(to_code, .after = from_code) %>% relocate(item_code, .before = from_code)

# bring MU imports in same format as btd
btd_MUN_imp <- imp %>% select(co_mun, item_code, from_code, import) %>%
  rename("to_code" = "co_mun", "value" = "import") %>%
  relocate(to_code, .after = from_code) %>% relocate(item_code, .before = from_code)

# bring intra-MU trade in same format as btd
btd_MUN_intra <- flows %>% mutate(item_code = ifelse(product=="bean", 2555, ifelse(product == "oil", 2571,2590))) %>% select(!product) %>%
  rename("from_code" = "co_orig", "to_code" = "co_dest") %>%
  relocate(item_code, .before = from_code)

# append to btd table (after removing BRA)
btd_soy_extended <- btd_soy %>% filter(from_code != 21 & to_code !=21) %>% select(!year) %>%
  bind_rows(btd_MUN_exp, btd_MUN_imp,btd_MUN_intra)
  

# Create a structure to map importers to exporters per item (+ targets)

# extend FABIO btd regions by disaggregating Brazil into its municipalities
regions_extended <- regions_btd %>% filter(ISO_BTD != "BRA") %>% bind_rows(setNames(SOY_MUN[,1:2], names(regions_btd)))
regions_code <- regions_extended$CO_BTD

mapping_templ <- data.table(
  from_code = rep(regions_code, each = length(regions_code)),
  to_code = rep(regions_code, times =  length(regions_code)))

# mapping_templ <- data.table(
#   from_code = rep(regions_code, each = length(regions_code), times = length(soy_items)),
#   to_code = rep(regions_code, times =  length(regions_code) * length(soy_items)),
#   item_code = rep(soy_items, each = length(regions_code) ^ 2))

# assign btd data to mapping
mapping <- lapply(soy_items, function(x){
  btd_item <- filter(btd_soy_extended, item_code == x) %>% select(!item_code)
  map <- left_join(mapping_templ, btd_item, by = c("from_code", "to_code")) %>% replace_na(list(value = 0))})

# restructure into matrices per item
mapping_reex <- lapply(mapping, function(x){
  pivot_wider(x, names_from = to_code, values_from = value) %>% column_to_rownames("from_code") %>% as("Matrix")})

# mapping_reex2 <- lapply(mapping, function(x){
#   with(x, sparseMatrix(i=dense_rank(from_code), j = dense_rank(to_code), x=value, dimnames=list(regions_code, regions_code)))})

# mapping_reex3 <- sapply(soy_items, function(x){
#   btd_item <- filter(btd_soy_extended, item_code == x) %>% select(!item_code)
#   with(btd_item, sparseMatrix(i=match(from_code, regions_code), j = match(from_code, regions_code), x=value, dimnames=list(regions_code, regions_code)))})

## extend CBS with sub-national data
cbs_soy <- cbs %>% filter(item_code %in% soy_items & year == 2013)
cbs_soy[, dom_use := na_sum(feed, food, losses, other, processing, seed, stock_addition, balancing, unspecified)]
cbs_soy[, total_use := na_sum(dom_use, exports)]

# reshape SOY_MUN so that soy products go into separate rows
#SOY_MUN_bean <- SOY_MUN %>% select(c(co_mun:nm_mun, ends_with("bean"))) %>% rename_with(~ sub("_bean$", "", .x), everything()) %>% mutate(item_code = 2555, .after = nm_mun)
#SOY_MUN_oil <-  SOY_MUN %>% select(c(co_mun:nm_mun, ends_with("oil")))  %>% rename_with(~ sub("_oil$", "", .x),  everything()) %>% mutate(item_code = 2571, .after = nm_mun)
#SOY_MUN_cake <- SOY_MUN %>% select(c(co_mun:nm_mun, ends_with("cake"))) %>% rename_with(~ sub("_cake$", "", .x), everything()) %>% mutate(item_code = 2590, .after = nm_mun)
#SOY_MUN_long <- bind_rows(SOY_MUN_bean, SOY_MUN_oil, SOY_MUN_cake) %>% rename(area_code = co_mun,
#                                                                              area = nm_mun,
#                                                                              production = prod, 
#                                                                              imports = imp, 
#                                                                              exports = exp, 
#                                                                              processing = proc,
#                                                                              stock_addition = stock, 
#                                                                              dom_use = domestic_use)


SOY_MUN_long <- SOY_MUN %>% 
  pivot_longer(cols = prod_bean:domestic_use_cake, names_to = c(".value", "product"), names_pattern = "(.+)_(.+$)") %>%
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(item_code = ifelse(product=="bean", 2555, ifelse(product == "oil", 2571,2590)), .before = product) %>%
  rename(area_code = co_mun,
         area = nm_mun,
         production = prod, 
         imports = imp, 
         exports = exp, 
         processing = proc,
         stock_addition = stock, 
         dom_use = domestic_use)


# extend CBS with long SOY_MUN table
cbs_soy_extended <- cbs_soy %>% select(!c(item, year, stock_withdrawal)) %>% filter(area_code != 21) %>%
  bind_rows(select(SOY_MUN_long, !c(co_state:nm_state, product, starts_with("excess")))) %>%
  mutate_all(~replace(., is.na(.), 0))



## re-allocate re-exports

#reex <- list()
#for(j in soy_items) {

reex <- lapply(soy_items, function(x){    
data <- merge(data.table(area_code = regions_code),
              cbs_soy_extended[item_code == x, .(area_code, production, dom_use, total_use, dom_share = production / total_use)],
              by = "area_code", all = TRUE) %>%
  mutate_all(~replace(., is.na(.), 0))

denom <- data$total_use
denom[denom == 0] <- 1
mat <- mapping_reex[[names(soy_items)[soy_items == x]]]

# divide bilateral trade matrix by total use (domestic use + exports) along columns --> technical coefficient matrix use/total use
# i.e. what is the share of soy from country x in the total use of country y
mat <- t(t(mat) / denom)
# Leontief inverse (I - A)^-1: how much soy from country x is needed for an extra unit of use in y
mat <- diag(nrow(mat)) - mat
mat <- solve(mat, sparse = TRUE) # make sure to use sparse = TRUE so that the matrix remains in compressed dgC format!
# mat <- Matrix(mat, sparse = TRUE)

# remove re-exports
mat <- mat * data$dom_share # multiplying L with share of domestic production in total use
mat <- t(t(mat) * data$dom_use) # again, along columns
colnames(mat) <- rownames(mat)
#dimnames(mat) <- list(regions_code[regions_code != 21], regions_code[regions_code != 21])

return(mat)
#reex[[names(soy_items)[soy_items == j]]] <- mat
})

### TO CHECK: negative values in trade matrix?
# --> due to input coefficients >>1 (trade mismatches!)

# reshape to long format

# system.time(
# btd_final1 <- lapply(soy_items, function(x){
#   out <- data.table(item_code = x, from_code = regions_code, as.matrix(reex[[names(soy_items)[soy_items == x]]]))
#   out <- pivot_longer(out, cols = !c(item_code, from_code), names_to = "to_code", values_to = "value")
#   
# }) 
# )
# 
# system.time(
# btd_final2 <- lapply(reex, function(x){
#   out <- data.table(from_code = regions_code, as.matrix(x))
#   out <- pivot_longer(out, cols = !from_code, names_to = "to_code", values_to = "value")
# }) 
# )
# 
# system.time(
# btd_final3 <- sapply(soy_items, function(x) {
#  out <- reex[[names(soy_items)[soy_items == x]]]
#  out <- data.table(from_code = regions_code, as.matrix(out))
#  out <- melt(out, id.vars = c("from_code"), variable.name = "to_code", variable.factor = FALSE)
#  out[, .(item_code = as.integer(x),
#          from_code = as.integer(from_code), 
#          to_code = as.integer(to_code), value)]
# }, USE.NAMES = TRUE, simplify = FALSE)
# )
# 
# system.time(
# btd_final4 <- lapply(reex, function(x) {
#   out <- data.table(from_code = regions_code, as.matrix(x))
#   out <- melt(out, id.vars = c("from_code"), variable.name = "to_code", variable.factor = FALSE)
#   out[, .(from_code = as.integer(from_code), 
#           to_code = as.integer(to_code), value)]
# })
# )

system.time(
btd_final5 <- lapply(soy_items, function(x) {
  # convert to triplet form
  m <- reex[[names(soy_items)[soy_items == x]]]
  m <- as(m, "dgTMatrix")
  # convert to data frame: convert to 1-based indexing (see https://stackoverflow.com/questions/52662748/from-sparsematrix-to-dataframe)
  df <- data.frame(i=as.integer(rownames(m)[m@i + 1]), j=as.integer(colnames(m)[m@j + 1]), x=m@x)
  names(df) <- c("from_code", "to_code", "value")
  df <- mutate(df, item_code = x, .before = from_code)
  })
)

# all.equal(btd_final1$cake$value, btd_final2$cake$value)
# all.equal(btd_final2$cake$value, btd_final3$cake$value)
# 
# b1 <- filter(btd_final1$bean, value>0) %>% arrange(value) %>% relocate(item_code, .before = from_code) %>% mutate(to_code = as.numeric(to_code))
# b3 <- filter(btd_final3$bean, value>0) %>% arrange(value) %>% relocate(item_code, .before = from_code) %>% mutate(to_code = as.numeric(to_code))
# b5 <- filter(btd_final5$bean, value>0) %>% arrange(value)
# 
# all.equal(b1,b3, check.attributes = FALSE)
# 
# all.equal(b3,b5)

btd_final <- bind_rows(btd_final5)


# Store the balanced table -----------------------------------------------
saveRDS(btd_final, "intermediate_data/btd_final.rds")
