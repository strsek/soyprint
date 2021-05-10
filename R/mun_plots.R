######## comparative graphs of municipality soy data #####

library(ggplot2)
library(viridis)
library(mapview)
library(tmap)
library(sf)
library(leafsync)
library(patchwork)

write = FALSE

# load data
GEO_MUN_SOY <- readRDS("intermediate_data/GEO_MUN_SOY_03.rds")
GEO_states <- st_read("input_data/geo/GADM_boundaries/gadm36_BRA_1.shp", stringsAsFactors = FALSE)
SOY_MUN <- readRDS("intermediate_data/SOY_MUN_fin.rds")


# plot data ------------------------

plot_funct <- function(variable, title, unit, ...){
  thesubset <- GEO_MUN_SOY[SOY_MUN[,paste0(variable)]>0,]
  thedata <- st_drop_geometry(thesubset)
  theplot <- ggplot(thesubset, aes(fill = pull(thedata, variable)), size = 0.01) +
                labs(title = title)+
                geom_sf(color = "transparent")+            
                scale_fill_viridis(direction = -1, na.value = "transparent", name = paste(unit)) + 
                geom_sf(data = GEO_states, fill = "transparent", color = "grey", size = 0.1)+
                theme_void()+ # or minimal
                theme(plot.title = element_text(hjust = 0.5), plot.margin = margin(0.5, 1, 0.5, 1, "cm"))
  return(theplot)
}
#

g_prod_bean <- plot_funct(variable = "prod_bean", title = "Soybean production", unit = "tons")
g_prod_oil  <- plot_funct(variable = "prod_oil", title = "Soy oil  production", unit = "tons")
g_prod_cake <- plot_funct(variable = "prod_cake", title = "Soy cake production", unit = "tons")

g_exp_bean <- plot_funct(variable = "exp_bean", title = "Soy bean exports", unit = "tons")
g_exp_oil <-  plot_funct(variable = "exp_oil", title = "Soy oil exports", unit = "tons")
g_exp_cake <- plot_funct(variable = "exp_cake", title = "Soy cake exports", unit = "tons")

g_imp_bean <- plot_funct(variable = "imp_bean", title = "Soy bean imports", unit = "tons")
g_imp_oil  <- plot_funct(variable = "imp_oil",  title =  "Soy oil imports", unit = "tons")
g_imp_cake <- plot_funct(variable = "imp_cake", title = "Soy cake imports", unit = "tons")

g_food_bean <- plot_funct(variable = "food_bean", title = "Soy bean food use", unit = "tons")
g_food_oil  <- plot_funct(variable = "food_oil",  title =  "Soy oil food use", unit = "tons")

g_proc_bean <-  plot_funct(variable = "proc_bean", title = "Soy bean processing use", unit = "tons")
g_feed_bean <-  plot_funct(variable = "feed_bean",  title =  "Soy bean feed use", unit = "tons")
g_feed_cake <-  plot_funct(variable = "feed_cake",  title =  "Soy cake feed use", unit = "tons")

g_seed_bean  <-  plot_funct(variable = "seed_bean",  title =  "Soy bean seed use", unit = "tons")
g_other_oil  <-  plot_funct(variable = "other_oil",  title =  "Soy oil other use", unit = "tons")
g_stock_bean <-  plot_funct(variable = "stock_bean",  title =  "Soy bean storage addition", unit = "tons")

#g_proc_cap <- plot_funct(variable = "proc_cap", title = "Processing capacity \n (tons/day)")
#g_ref_cap  <- plot_funct(variable = "ref_cap", title = "Refining capacity \n (tons/day)")
#g_bot_cap  <- plot_funct(variable = "bot_cap", title = "Bottling capacity \n (tons/day)")
#g_cattle   <- plot_funct(variable = "cattle", title = "Cattle (units)", trans = "log")
#g_pig      <- plot_funct(variable = "pig", title = "Pigs (units)", trans = "log")
#g_poultry  <- plot_funct(variable = "poultry", title = "Poultry (units)", trans = "log")
#g_sheep    <- plot_funct(variable = "sheep", title = "Sheep (units)", trans = "log")
#g_pop      <- plot_funct(variable = "population", title = "Population", trans = "log")


# create lattices with patchwork ----------------

#comparison <- (g_prod | g_exp_tot | g_exp_bean | g_exp_oil | g_exp_cake ) / (g_proc | g_refbot | g_proc_cap | g_ref_cap | g_bot_cap) / (g_cattle | g_pig | g_poultry | g_sheep | g_pop)
comparison <- (g_prod_bean | g_prod_oil | g_prod_cake | g_exp_bean | g_exp_oil | g_exp_cake ) / 
              (g_imp_bean  | g_imp_oil  | g_imp_cake  | g_food_bean| g_food_oil| g_seed_bean) / 
              (g_feed_bean | g_feed_cake| g_other_oil | g_stock_bean)

comparison_bean <- (g_prod_bean | g_imp_bean | g_exp_bean | g_proc_bean | g_food_bean | g_feed_bean |g_seed_bean | g_stock_bean)
comparison_oil  <- (g_prod_oil  | g_imp_oil  | g_exp_oil  | g_food_oil  | g_other_oil)
comparison_cake <- (g_prod_cake | g_imp_cake | g_exp_cake | g_feed_cake)

if(write){
ggsave(plot = comparison, filename = "comparison.png", device = "png", width = 60, height = 30, units = "cm")
ggsave(plot = comparison_bean, filename = "comparison_bean.png", device = "png",  width = 96, height = 10, units = "cm")
ggsave(plot = comparison_oil , filename = "comparison_oil.png", device = "png", width = 60, height = 10, units = "cm")
ggsave(plot = comparison_cake, filename = "comparison_cake.png", device = "png", width = 48, height = 10, units = "cm")
}

# plot with mapview (ool, but very heavy: only manageable for individual layers)

#mapviewOptions(platform = "mapdeck", fgb = TRUE)
#m_prod <- mapview(GEO_MUN_SOY, zcol = "prod_t", map.types = "CartoDB.Positron") 
#m_exp_t <- mapview(GEO_MUN_SOY, zcol = "ex_tot_t", map.types = "CartoDB.Positron")
#m_proc_fac <- mapview(GEO_MUN_SOY, zcol = "proc_total", map.types = "CartoDB.Positron")
#m_refbot_fac <- mapview(GEO_MUN_SOY, zcol = "ref_total", map.types = "CartoDB.Positron")
#sync(m_prod, m_exp_t, m_proc_fac, m_refbot_fac)

