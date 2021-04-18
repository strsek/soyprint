######## comparative graphs of municipality soy data #####

library(ggplot2)
library(viridis)
library(mapview)
library(tmap)
library(sf)
library(leafsync)
library(patchwork)

load("GEO_MUN_SOY.Rdata")
GEO_states <- st_read("GEO-Brazil_boundaries/gadm36_BRA_shp/gadm36_BRA_1.shp", stringsAsFactors = FALSE)


## plot data 

# with ggplot & patchwork

plot_funct <- function(variable, title,...){
  thesubset <- GEO_MUN_SOY[!is.na(GEO_MUN_SOY[,paste0(variable)])[,1],]
  thedata <- st_drop_geometry(thesubset)
  theplot <- ggplot(thesubset, aes(fill = thedata[,paste0(variable)]), size = 0.01) +
                geom_sf(color = "transparent")+            
                scale_fill_viridis(direction = -1, na.value = "transparent", name = paste(title), ...) + 
                geom_sf(data = GEO_states, fill = "transparent", color = "grey", size = 0.1)+
                theme_void() # or minimal
  return(theplot)
}
#

g_prod <- plot_funct(variable = "prod_t", title = "Soy production in tons")
g_exp_tot <- plot_funct(variable = "ex_tot_t", title = "Soy exports in t \n (all soy products)")
g_exp_bean <- plot_funct(variable = "ex_bean_t", title = "Soy bean exports in t")
g_exp_oil <- plot_funct(variable = "ex_oil_t", title = "Soy oil exports in t")
g_exp_cake <- plot_funct(variable = "ex_cake_t", title = "Soy cake exports in t")
g_proc <- plot_funct(variable = "proc_total", title = "Processing facilities \n (active & inactive)")
g_refbot <- plot_funct(variable = "ref_total", title = "Refining and bottling facilities \n (active & inactive)")
g_proc_cap <- plot_funct(variable = "proc_cap", title = "Processing capacity \n (tons/day)")
g_ref_cap <- plot_funct(variable = "ref_cap", title = "Refining capacity \n (tons/day)")
g_bot_cap <- plot_funct(variable = "bot_cap", title = "Bottling capacity \n (tons/day)")
g_cattle <- plot_funct(variable = "cattle", title = "Cattle (units)", trans = "log")
g_pig <- plot_funct(variable = "pig", title = "Pigs (units)", trans = "log")
g_poultry <- plot_funct(variable = "poultry", title = "Poultry (units)", trans = "log")
g_sheep <- plot_funct(variable = "sheep", title = "Sheep (units)", trans = "log")
g_pop <- plot_funct(variable = "population", title = "Population", trans = "log")


comparison <- (g_prod | g_exp_tot | g_exp_bean | g_exp_oil | g_exp_cake ) / (g_proc | g_refbot | g_proc_cap | g_ref_cap | g_bot_cap) / (g_cattle | g_pig | g_poultry | g_sheep | g_pop)
ggsave(plot = comparison, filename = "comparison.pdf", device = "pdf", width = 60, height = 30, units = "cm")
ggsave(plot = comparison, filename = "comparison.png", device = "png", width = 60, height = 30, units = "cm")



##with mapview (would be cool, but very heavy, only manageable for individual layers)

#mapviewOptions(platform = "mapdeck", fgb = TRUE)
#m_prod <- mapview(GEO_MUN_SOY, zcol = "prod_t", map.types = "CartoDB.Positron") 
#m_exp_t <- mapview(GEO_MUN_SOY, zcol = "ex_tot_t", map.types = "CartoDB.Positron")
#m_proc_fac <- mapview(GEO_MUN_SOY, zcol = "proc_total", map.types = "CartoDB.Positron")
#m_refbot_fac <- mapview(GEO_MUN_SOY, zcol = "ref_total", map.types = "CartoDB.Positron")
#sync(m_prod, m_exp_t, m_proc_fac, m_refbot_fac)


##with tmap (slow)

#tmap_mode("plot")
#tm_prod <- tm_shape(GEO_MUN) + tm_polygons("prod_quantity", convert2density = TRUE)
#tm_exp_t <- tm_shape(GEO_MUN) + tm_polygons("total_export_t", convert2density = TRUE)
#tm_proc_fac <- tm_shape(GEO_MUN) + tm_polygons("proc_total", convert2density = TRUE)
#tm_refbot_fac <- tm_shape(GEO_MUN) + tm_polygons("refbot_total", convert2density = TRUE)
#tmap_arrange(tm_prod, tm_exp_t, tm_proc_fac, tm_refbot_fac)

#tm_shape(GEO_MUN_SOY) +
#  tm_polygons(c("prod_t", "ex_tot_t")) +
#  tm_facets(ncol = 2)


##with base plot (not useful)

#GEO_states <- st_read("GEO-Brazil_boundaries/gadm36_BRA_shp/gadm36_BRA_1.shp", stringsAsFactors = FALSE)
##par(mfrow = c(2,2))
#plot(GEO_MUN_SOY["prod_t"], lty = 0) #  breaks = "quantile", reset = FALSE
#plot(st_geometry(GEO_states), add = TRUE)
#plot(GEO_MUN_SOY["ex_tot_t"], lty = 0) #  breaks = "quantile"
#plot(st_geometry(GEO_states), add = TRUE)
#plot(GEO_MUN_SOY["proc_total"], lty = 0) #  breaks = "quantile"
#plot(st_geometry(GEO_states), add = TRUE)
#plot(GEO_MUN_SOY["ref_total"], lty = 0) #  breaks = "quantile"
#plot(st_geometry(GEO_states), add = TRUE)#