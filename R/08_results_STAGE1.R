### subnational supply chain to exports results STAGE 1 ###

library(dplyr)
library(data.table)
library(tibble)
library(tidyr)
library(abind)
library(Matrix.utils)


write = TRUE

flows_GAMS <- readRDS("intermediate_data/flows_GAMS.rds")
flows_R <- readRDS("intermediate_data/flows_R.rds")
GEO_MUN_SOY <- readRDS("intermediate_data/GEO_MUN_SOY_fin.rds")
SOY_MUN <- readRDS("intermediate_data/SOY_MUN_fin.rds")
EXP_MUN_SOY <- readRDS("intermediate_data/EXP_MUN_SOY.rds")
IMP_MUN_SOY <- readRDS("intermediate_data/IMP_MUN_SOY.rds")

co_mun <- SOY_MUN$co_mun
product <- c("bean", "oil", "cake")

# create a transport matrix for each product
#flow_mat <- matrix(NA, nrow = nrow(SOY_MUN), ncol = nrow(SOY_MUN), dimnames = list(SOY_MUN$co_mun, SOY_MUN$co_mun))

# Create a structure to map importers to exporters per item (+ targets)
mapping_templ <- data.table(
  co_orig = rep(co_mun, each = length(co_mun), times = length(product)),
  co_dest = rep(co_mun, times = length(co_mun) * length(product)),
  product = rep(product, each = length(co_mun) ^ 2))

# join template with transport values
flow_long <- left_join(mapping_templ, flows_R, by = c("co_orig", "co_dest", "product")) %>% replace_na(list(value = 0))

# put data into a list of separate wide-format matrices for each product
flow_wide <- sapply(product, function(x){
  filter(flow_long, product == x) %>% dplyr::select(!product) %>% pivot_wider(names_from = co_dest, values_from = value) %>% column_to_rownames("co_orig") %>% as("Matrix")
  }, USE.NAMES = TRUE, simplify = FALSE)

# add back self-supply to the diagonals
flow_wide_full <- sapply(product, function(x){
  mat <- flow_wide[[x]]
  diag(mat) <- as.numeric(pull(SOY_MUN, paste0("total_supply_",x)) - pull(SOY_MUN, paste0("excess_supply_",x)))
  return(as(mat, "Matrix"))}, USE.NAMES = TRUE, simplify = FALSE)

# check whether row and column sums add up to total supply and demand
lapply(product, function(x){
  all.equal(rowSums(flow_wide_full[[x]]), pull(SOY_MUN, paste0("total_supply_",x), name = "co_mun")) &
  all.equal(colSums(flow_wide_full[[x]]), pull(SOY_MUN, paste0("total_use_",x), name = "co_mun"))})

# 
flow_long_full <- abind(lapply(product, function(x){
  summ <- summary(flow_wide_full[[x]])
  df <- data.frame(co_orig = co_mun[summ$i], co_dest = co_mun[summ$j], product = x, value = summ$x)}), along = 1)

#flow_long_full <- abind(lapply(product, function(x){as.data.frame(as.matrix(flow_wide_full[[x]])) %>% pivot_longer(everything(), names_to = "co_dest", values_to = "vlaue")}), along = 1)	


## bring Exports into wide format

EXP_MUN_SOY <- mutate(EXP_MUN_SOY, product = ifelse(product == "soybean", "bean", ifelse(product == "soy_oil", "oil", "cake")))
  
destin <- unique(EXP_MUN_SOY$to_name)

exp_templ <- data.frame(
  co_orig = rep(co_mun, each = length(destin), times = length(product)),
  co_dest = rep(destin, times = length(co_mun) * length(product)),
  product = rep(product, each = length(destin) * length(co_mun)))

exp_long  <- left_join(exp_templ, dplyr::select(EXP_MUN_SOY, c(co_mun, product, to_name, export)), by = c("co_orig" = "co_mun", "co_dest" = "to_name", "product" = "product")) %>% replace_na(list(export = 0))

# put data into a list of separate wide-format matrices for each product
exp_wide <- sapply(product, function(x){
  filter(exp_long, product == x) %>% dplyr::select(!product) %>% pivot_wider(names_from = co_dest, values_from = export) %>% column_to_rownames("co_orig") %>% as("Matrix")
}, USE.NAMES = TRUE, simplify = FALSE)


# map MU sources to export destinations by multiplying the MU flow matrix in relative terms with the export matrix for each product
flow_wide_rel <- lapply(flow_wide_full, function(x){ rel <- t(t(x)/colSums(x)); rel[is.na(rel)] <- 0; return(rel)})

source_to_export <- sapply(product, function(x){flow_wide_rel[[x]] %*% exp_wide[[x]]}, USE.NAMES = TRUE, simplify = FALSE)
 

# finally, take into account that some of the MU level supply is imported by multiplying rows by "domestic supply share" of each MU
source_to_export <- sapply(product, function(x){
  dom_share <- (pull(SOY_MUN, paste0("prod_",x))/pull(SOY_MUN, paste0("total_supply_",x)))
  dom_share[is.na(dom_share)] <- 0
  source_to_export[[x]] * dom_share}, USE.NAMES = TRUE, simplify = FALSE)

sapply(exp_wide, sum, na.rm = T)
sapply(source_to_export, sum, na.rm = T)

# bring back into long format
source_to_export_df <- lapply(product, function(x) {
  # convert to triplet form
  m <- source_to_export[[x]]
  m <- as(m, "dgTMatrix")
  # convert to data frame: convert to 1-based indexing (see https://stackoverflow.com/questions/52662748/from-sparsematrix-to-dataframe)
  df <- data.frame(i=(rownames(m)[m@i + 1]), j=(colnames(m)[m@j + 1]), x=m@x)
  names(df) <- c("from_code", "to_code", "value")
  df <- mutate(df, item_code = x, .before = from_code)
}) 

source_to_export_fin <- bind_rows(source_to_export_df)


# export results
if (write){
  saveRDS(source_to_export_fin, "intermediate_data/source_to_export.RDS")
}
