
#### "trase-level" results: flows from MU of origin to country of first import ####

library(dplyr)
library(data.table)
library(tibble)
library(tidyr)
library(abind)
library(Matrix.utils)

write = FALSE

#flows_GAMS <- readRDS("intermediate_data/flows_GAMS_simple.rds")
#flows_euclid <- readRDS("intermediate_data/flows_R.rds")
#flows_road <- readRDS("intermediate_data/flows_road.rds")
flows_euclid <- readRDS("intermediate_data/flows_R.rds")
bs_files <- list.files("./GAMS/bs_res", pattern="*.rds", full.names=F)
flows_bs <- lapply(bs_files, function(file){
  readRDS(paste0("./GAMS/bs_res/", file))
})
names(flows_bs) <- gsub(".rds","",bs_files)
 
flows <- c(flows_euclid, flows_bs)
rm(flows_bs)

#GEO_MUN_SOY <- readRDS("intermediate_data/GEO_MUN_SOY_fin.rds")
SOY_MUN <- readRDS("intermediate_data/SOY_MUN_fin.rds")
EXP_MUN_SOY <- readRDS("intermediate_data/EXP_MUN_SOY_cbs.rds")
IMP_MUN_SOY <- readRDS("intermediate_data/IMP_MUN_SOY_cbs.rds")

co_mun <- SOY_MUN$co_mun
product <- c("bean", "oil", "cake")


# bring exports into wide format

EXP_MUN_SOY <- mutate(EXP_MUN_SOY, 
                      product = ifelse(product == "soybean", 
                                       "bean", 
                                       ifelse(product == "soy_oil", "oil", "cake")))

destin <- unique(EXP_MUN_SOY$to_name)
destin <- c("BRA", destin)
destin <- sort(destin)

# template to contain all origins and destinations
exp_templ <- data.frame(
  co_orig = rep(co_mun, each = length(destin), times = length(product)),
  co_dest = rep(destin, times = length(co_mun) * length(product)),
  product = rep(product, each = length(destin) * length(co_mun)))

exp_long  <- left_join(exp_templ, dplyr::select(EXP_MUN_SOY, c(co_mun, product, to_name, export)), 
                       by = c("co_orig" = "co_mun", "co_dest" = "to_name", "product" = "product")) %>% 
  replace_na(list(export = 0))

# add domestic consumption as "exports to Brazil"
#all.equal(exp_long$co_orig[exp_long$co_dest == "BRA" & exp_long$product == "bean"], SOY_MUN$co_mun) 
# NOTE: for bean, remove processing use from domestic use as it is no final use in our soy supply chain
exp_long$export[exp_long$co_dest == "BRA" & exp_long$product == "bean"] <- 
  SOY_MUN$domestic_use_bean - SOY_MUN$proc_bean # (SOY_MUN$prod_oil + SOY_MUN$prod_cake)
exp_long$export[exp_long$co_dest == "BRA" & exp_long$product == "oil"]  <- SOY_MUN$domestic_use_oil
exp_long$export[exp_long$co_dest == "BRA" & exp_long$product == "cake"] <- SOY_MUN$domestic_use_cake


# put data into a list of separate wide-format matrices for each product
exp_wide <- sapply(product, function(x){
  filter(exp_long, product == x) %>% 
    dplyr::select(!product) %>% 
    pivot_wider(names_from = co_dest, values_from = export) %>% 
    column_to_rownames("co_orig") %>% 
    as("Matrix")
}, USE.NAMES = TRUE, simplify = FALSE)



# Create a structure to map importers to exporters per item (+ targets)
mapping_templ <- data.table(
  co_orig = rep(co_mun, each = length(co_mun), times = length(product)),
  co_dest = rep(co_mun, times = length(co_mun) * length(product)),
  product = rep(product, each = length(co_mun) ^ 2))

rm(EXP_MUN_SOY, IMP_MUN_SOY, exp_templ, exp_long)

#ncores <- detectCores() - 1
#clust <- makeCluster(ncores)
#clusterExport(cl = clust, c("mapping_templ", "exp_wide", "co_mun", "destin", "product", "flows"))
#clusterEvalQ(clust, {library(dplyr); library(data.table); library(tibble); library(tidyr); library(abind);library(Matrix.utils)})
#source_2_export <- parLapply(clust, flows, function(x){

#source_2_export <- lapply(flows, function(x){

system.time(
source_2_export <- mclapply(flows, function(x){  

  # join template with transport values
  #flow_long <- left_join(mapping_templ, x, by = c("co_orig", "co_dest", "product")) %>% 
  #  replace_na(list(value = 0))
  
  # put data into a list of separate wide-format matrices for each product

  #flow_wide <- sapply(product, function(x){
  #  filter(flow_long, product == x) %>% dplyr::select(!product) %>%
  #    pivot_wider(names_from = co_dest, values_from = value) %>% 
  #    column_to_rownames("co_orig") %>% 
  #    as("Matrix")
  #  }, USE.NAMES = TRUE, simplify = FALSE)
  
  
  #flow_wide <- sapply(product, function(x){
  #  df <- filter(flow_long, product == x, value != 0) %>% dplyr::select(!product)
  #  mat <- with(df, sparseMatrix(i = match(co_orig, co_mun), # dense_rank(co_orig), 
  #                               j = match(co_dest, co_mun), # dense_rank(co_dest), 
  #                               x = value, 
  #                               dims = c(length(co_mun), length(co_mun)),
  #                               dimnames = list(co_mun,co_mun))) # list(sort(unique(co_orig)), sort(unique(co_dest)))))
  #}, USE.NAMES = TRUE, simplify = FALSE)
  
  ## WE dont't even need the long mapping! We can create full sparse matrix directly!
  flow_wide <- sapply(product, function(prod){
    df <- filter(x, product == prod, value != 0) %>% dplyr::select(!product)
    mat <- with(df, sparseMatrix(i = match(co_orig, co_mun), # dense_rank(co_orig), 
                                 j = match(co_dest, co_mun), # dense_rank(co_dest), 
                                 x = value, 
                                 dims = c(length(co_mun), length(co_mun)),
                                 dimnames = list(co_mun,co_mun))) # list(sort(unique(co_orig)), sort(unique(co_dest)))))
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
  
  # bring back into long format
  flow_long_full <- abind(lapply(product, function(x){
    summ <- summary(flow_wide_full[[x]])
    df <- data.frame(co_orig = co_mun[summ$i], 
                     co_dest = co_mun[summ$j], 
                     product = x, value = summ$x)}), 
    along = 1)
  
  #flow_long_full <- abind(lapply(product, function(x){as.data.frame(as.matrix(flow_wide_full[[x]])) %>% pivot_longer(everything(), names_to = "co_dest", values_to = "vlaue")}), along = 1)	
  
  # map MU sources to export destinations by multiplying the MU flow matrix in relative terms with the export matrix for each product
  
  # create a sub-national flow "input coefficient matrix" by dividing each column by the column sum (= total use)
  # flow_wide_rel <- lapply(flow_wide_full, 
  #                         function(x){ 
  #                           rel <- t(t(x)/colSums(x)) 
  #                           rel[is.na(rel)] <- 0 
  #                           return(rel)})
  
  flow_wide_rel <- lapply(flow_wide_full, 
                          function(x){ 
                            rel <- x 
                            rel@x <- rel@x / rep.int(colSums(rel), diff(rel@p))
                            #rel[is.na(rel)] <- 0 
                            return(rel)})
  

  # map sources to exports by multiplying the flow coefficient matrix with the export matrix 
  # this entails the implicit assumption that all uses of soy products in a MU (exports, processing, domestic consumption) have the same spatial source structure (proportionality assumption common in IO)
  source_to_export <- sapply(product, function(x){
    flow_wide_rel[[x]] %*% exp_wide[[x]]}, 
    USE.NAMES = TRUE, simplify = FALSE)
  
  ##option2: use exp_wide_rel
  #exp_wide_rel <- lapply(exp_wide, function(x){ rel <- x/rowSums(x); rel[is.na(rel)] <- 0; return(rel)})
  #source_to_export2 <- sapply(product, function(x){flow_wide_full[[x]] %*% exp_wide_rel[[x]]}, USE.NAMES = TRUE, simplify = FALSE)
  ##NOTE: it should not matter which option is used 
  #--> sums of flow_wide and exp_wide need to match!
  # currently there are still slight discrepancies because the export in SOY_MUN are adapted to match the CBS data and EXP_MUN_SOY not yet!
  # after harmonizing the two the issue should be resolved
  # --> ~~NO!, because flow_wide_full for beans contains also flows destined for processing, while "exports to Brazil" have been cleared by that
  
  # check if results match total exports
  #sapply(flow_wide_full, sum, na.rm = T)
  #sapply(exp_wide, sum, na.rm = T)
  #sapply(source_to_export, sum, na.rm = T)
  Map(function(x,y){all.equal(sum(x),sum(y))}, source_to_export, exp_wide)
  
  # take into account that some of the MU level supply is imported by multiplying rows by "domestic supply share" of each MU
  dom_share <- sapply(product, function(x){
    dom_share <- (pull(SOY_MUN, paste0("prod_",x))/pull(SOY_MUN, paste0("total_supply_",x)))
    dom_share[is.na(dom_share)] <- 0
    return(dom_share)
    }, USE.NAMES = TRUE, simplify = TRUE) %>% as.data.frame() %>% `rownames<-`(SOY_MUN$co_mun)
  
  source_to_export <- sapply(product, function(x){
    source_to_export[[x]] * dom_share[[x]]}, 
    USE.NAMES = TRUE, simplify = FALSE)
  
  # sapply(source_to_export, sum, na.rm = T) 
  
  # finally, map oil and cake exports back to the origin of soybean production... 
  # ..by multiplying the bean flow coefficient matrix with the export matrices of oil and cake
  # this again assumes that all uses of beans in a MU (export, processing ...) share the same spatial source structure
  # the result is in turn corrected by the domestic bean supply share of each MU to remove bean imports at the source
  source_to_export[2:3] <- lapply(source_to_export[2:3], function(x){
    (flow_wide_rel$bean %*% x) * dom_share$bean}) 
  
  sapply(source_to_export, sum, na.rm = T) 
  
  # bring back into long format
  source_to_export_df <- lapply(product, function(x) {
    # convert to triplet form
    m <- source_to_export[[x]]
    m <- as(m, "dgTMatrix")
    # convert to data frame: convert to 1-based indexing (see https://stackoverflow.com/questions/52662748/from-sparsematrix-to-dataframe)
    df <- data.frame(i=(rownames(m)[m@i + 1]), 
                     j=(colnames(m)[m@j + 1]), 
                     x=m@x, 
                     stringsAsFactors = FALSE)
    names(df) <- c("from_code", "to_code", "value")
    df <- mutate(df, item_code = x, .before = from_code)
  }) 
  
  # bind results
  source_to_export_fin <- bind_rows(source_to_export_df)
  
  return(source_to_export_fin)
  
}, mc.cores = 12)

#stopCluster(clust)
)


# export results
if (write){
  #saveRDS(source_2_export, "intermediate_data/source_to_export_list.RDS")
  saveRDS(source_2_export, "intermediate_data/source_to_export_list.RDS")
}

rm(list = ls())