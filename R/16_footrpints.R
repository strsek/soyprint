### calculate footprints ###

library(Matrix)
library(data.table)

LA_mass <- readRDS("intermediate_data/FABIO/2013_L_mass.rds")
LB_mass <- readRDS("intermediate_data/FABIO/2013_B_inv_mass.rds")
LA_value <- readRDS("intermediate_data/FABIO/2013_L_value.rds")
LB_value <- readRDS("intermediate_data/FABIO/2013_B_inv_value.rds")
X <- readRDS("intermediate_data/FABIO/X.rds")
X <- X
YA <- readRDS("intermediate_data/FABIO/Y_hybrid.rds")
YA <- YA$`2013`
load("/mnt/nfs_fineprint/tmp/exiobase/pxp/2013_Y.RData")
YB <- as(Y, "sparseMatrix"); rm(Y)
load("/mnt/nfs_fineprint/tmp/exiobase/Y.codes.RData")
load("/mnt/nfs_fineprint/tmp/exiobase/pxp/IO.codes.RData")
E <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/E.rds")
E <- E$`2013`
l_mun <- read.csv("input_data/soy_areas_mapbiomas.csv") 
cbs <- readRDS("intermediate_data/FABIO/cbs_final.rds")
areas <- unique(cbs[,.(area_code, area)])
areas_mun <- areas[area_code > 1000,]

# prepare land-use data ------------------------------------------------------------------------

# extend E by municipality land use
l_mun <- as.data.table(l_mun)[,.(year,area,class,co_mun)]
setnames(l_mun, "area", "landuse")
l_mun <- merge(areas_mun, l_mun[class == 39,], by.x = "area_code", by.y = "co_mun", all.x = TRUE)
l_mun[is.na(landuse), `:=`(landuse = 0)]
l_mun[, `:=`(item_code = 2555, class = NULL, year = NULL)]
l_mun <- merge(l_mun, cbs[,.(area_code, item_code, production)], by = c("item_code", "area_code"))
#l_mun <- merge(l_mun, as.data.table(SOY_MUN)[,.(co_mun, area_plant)], by.x = "area_code", by.y = "co_mun")

# intensity:
#l_mun[,landuse := landuse/production]
#l_mun[!is.finite(landuse), landuse := 0]
#l_mun[, production := NULL]

soy_E <- unique(E[item_code %in% c(2555, 2571, 2590), .(item_code, item, comm_code, comm_group, group)])
E_mun <- as.data.table(expand_grid(area_code = areas_mun$area_code, item_code = c(2555, 2571, 2590))) 
E_mun <- merge(E_mun, soy_E, by = "item_code")
E_mun[, area := areas$area[match(area_code, areas$area_code)]]
E_mun <- merge(E_mun, l_mun, by = c("area_code", "area", "item_code"), all.x = TRUE)
E_mun[is.na(landuse), landuse := 0]

E <- rbind(E, E_mun, fill=TRUE)
E <- E[area_code %in% unique(cbs$area_code),]
E <- E[! (item_code %in% c(2555, 2571, 2590) & area_code == 21),]
E[is.na(E)] <- 0
# ensure correct order
setkey(E, area_code, comm_code)


# compute demand-driven production and footprints ------------------------------------

# aggregate final demand categories
# for YA
colnames(YA) <- sub("_.*", "", colnames(YA))
sum_mat <- as(sapply(unique(colnames(YA)),"==",colnames(YA)), "Matrix")*1
YA_agg <- YA %*% sum_mat 
# for YB
dimnames(YB) <- list(paste0(IO.codes$Country.Code,"_",IO.codes$Product.Code),Y.codes$`Region Name`)
sum_mat <- as(sapply(unique(colnames(YB)),"==",colnames(YB)), "Matrix")*1
YB_agg <- YB %*% sum_mat 
  
# calculate production embodied in final demand impulse
PA_mass  <- LA_mass  %*% YA_agg
PA_value <- LA_value %*% YA_agg
PB_mass  <- LB_mass  %*% YB_agg
PB_value <- LB_value %*% YB_agg

# calculate municipal land-use footprints
l <- as.vector(E$landuse / X)
l[!is.finite(l)] <- 0
FA_mass  <- l*PA_mass
FA_value <- l*PA_value
FB_mass  <- l*PB_mass 
FB_value <- l*PB_value

P_mass  <- list(PA_mass,  PB_mass)
P_value <- list(PA_value, PB_value)
F_mass  <- list(FA_mass,  FB_mass)
F_value <- list(FA_value, FB_value)

# Store results -----------------------
saveRDS(P_mass , "results/P_mass.rds")
saveRDS(P_value, "results/P_value.rds")
saveRDS(F_mass , "results/F_mass.rds")
saveRDS(F_value, "results/F_value.rds")

