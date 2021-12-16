## pixel visulizations

library(sf)
library(mapview)

soy_vect <- st_read("input_data/geo/MapBiomas/soy_vect_simplified.gpkg")
GEO_MUN_SOY <- readRDS("intermediate_data/GEO_MUN_SOY_fin.rds")
soy_vect_rough <- st_read("input_data/geo/MapBiomas/soy_vect_defl.shp")
soy_vect_mt <- st_read("input_data/geo/MapBiomas/soy_vect_mt_s0005.gpkg")



# intersect
soy_vect_rough <- st_transform(soy_vect_rough, crs = 5880)
soy_vect_mt <- st_transform(soy_vect_mt, crs = 5880) %>% st_make_valid()

#soy_mun_vect <- st_intersection(soy_vect_rough,GEO_MUN_SOY)
soy_mun_vect1 <- st_intersection(GEO_MUN_SOY, soy_vect_rough)
soy_mun_vect_mt <- st_intersection(filter(GEO_MUN_SOY, nm_state == "MT"), soy_vect_mt)

mapview(soy_mun_vect)

#PIX_MUN_SOY <- soy_mun_vect1 %>% group_by(nm_mun) %>% summarise_all(first) 
PIX_MUN_SOY <- aggregate(soy_mun_vect1, list(soy_mun_vect1$co_mun),  function(x) x[1])
PIX_MUN_SOY_mt <- aggregate(soy_mun_vect_mt, list(soy_mun_vect_mt$co_mun),  function(x) x[1])


# merge GEO_MUN with export results for each destination
mod <- names(comp_mun)[7:13] ; names(mod)<-mod
comp_mun_bymod <- lapply(mod, function(m){pivot_wider(comp_mun[,c(names(comp_mun)[1:5], m)], names_from = to_code, values_from = m) %>% replace(is.na(.), 0)})
comp_mun_bymod_prod <- lapply(comp_mun_bymod, function(m){tab <- m; tab[,5:ncol(tab)] <- tab[,5:ncol(tab)]/rowSums(tab[,5:ncol(tab)], na.rm = TRUE); return(tab)})

PIX_MUN_SOY_bymod <- lapply(comp_mun_bymod_prod, function(x){
  right_join(PIX_MUN_SOY, x) %>% replace(is.na(.),0)
  })

PIX_MUN_SOY_bymod_mt <- lapply(comp_mun_bymod_prod, function(x){
  x_mt <- filter(x, nm_state == "MT")
  right_join(PIX_MUN_SOY, x_mt) %>% replace(is.na(.),0)
})


### plot

gg_funct(PIX_MUN_SOY_bymod$intermod_nn, title = "Land-use probablilty for exports to Germany", variable = "DEU", cutoff = -0.1)
gg_funct(PIX_MUN_SOY_bymod$intermod_nn, title = "Land-use probablilty for exports to China", variable = "CHN", cutoff = -0.1, unit = "probability")
ggsave(filename = "BRA_CHN_1.png", scale = 2, dpi = 600)

gg_funct_state(PIX_MUN_SOY_bymod_mt$intermod_nn, state = "MT", title = "Mato Grosso \n Land-use probablilty for exports to China", variable = "CHN", cutoff = -0.1, unit = "probability")
ggsave(filename = "MT_CHN.png")

