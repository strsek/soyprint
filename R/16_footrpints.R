### calculate footprints ###

library(Matrix)
library(data.table)

L <- readRDS("intermediate_data/FABIO/2013_L_mass.rds")
X <- readRDS("intermediate_data/FABIO/X.rds")
X <- X
Y <- readRDS("intermediate_data/FABIO/Y.rds")
Y <- Y$`2013`
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
colnames(Y) <- sub("_.*", "", colnames(Y))
sum_mat <- as(sapply(unique(colnames(Y)),"==",colnames(Y)), "Matrix")*1
Y_agg <- Y %*% sum_mat 
  
# calculate production embodied in final demand impulse
P <- L %*% Y_agg

# calculate municipal land-use footprints
l <- as.vector(E$landuse / X)
l[!is.finite(l)] <- 0
FP <- l*P


# Store results -----------------------
saveRDS(P, "results/P.rds")
saveRDS(FP, "results/FP.rds")

