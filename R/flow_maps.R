# flow maps for transport flows

library(dplyr)
library(sf)
library(ggplot2)
library(ggthemes)
library(viridis)
library(ggforce)

flows <- readRDS("intermediate_data/transport_flows.rds")
SOY_MUN <- readRDS("intermediate_data/SOY_MUN_fin.rds")
GEO_MUN_SOY <- readRDS("intermediate_data/GEO_MUN_SOY_fin.rds")
MUN_capitals <- readRDS("intermediate_data/MUN_capitals.rds")
states <- st_read("input_data/geo/GADM_boundaries/gadm36_BRA_1.shp")

# drop geometries and prepare for plot
MUN_capitals_df <- st_drop_geometry(MUN_capitals)

# add coordinates of origins and destinations
flows <- flows %>% left_join(MUN_capitals_df[,c(1,3:4)], by = c("co_orig" = "co_mun")) %>% rename("orig_lon" = "LONG", "orig_lat" = "LAT")
flows <- flows %>% left_join(MUN_capitals_df[,c(1,3:4)], by = c("co_dest" = "co_mun")) %>% rename("dest_lon" = "LONG", "dest_lat" = "LAT")

# summarize modes and create separate frames for each product
flows <- flows %>% group_by(co_orig,co_dest,product,orig_lon,orig_lat,dest_lon,dest_lat) %>% summarise(value = sum(value, na.rm = T), .groups = "drop")
flows_bean <- filter(flows, product == "bean")
flows_oil <- filter(flows, product == "oil")
flows_cake <- filter(flows, product == "cake")

# check aggregate flow data
flows_agg <- group_by(flows, co_orig, product) %>% summarise(value = sum(value, na.rm = T), .groups = "drop")

# plot with ggplot

flow <- flows_bean

ggplot(data = states) +
  geom_sf(color = "grey", fill= "transparent", size = 0.1)+
  geom_sf(data = GEO_MUN_SOY[GEO_MUN_SOY$excess_supply_bean>0,], aes(fill = excess_supply_bean), color = "transparent", size = 0.01)+
  scale_fill_viridis(direction = -1, option = "D", na.value = "transparent", name = "Excess soybean supply \n in tons")+
  #scale_fill_gradient(low = "white", high = "darkgreen", na.value = "transparent", name = "Excess soybean supply")+
  #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below
  geom_segment(data = flow[which(flow$value>1000),], aes(x=orig_lon, y=orig_lat, xend=dest_lon, yend=dest_lat, alpha=value), col="darkblue")+
  #geom_link(data = flows_bean[which(flows_bean$value>10),], aes(x=orig_lon, y=orig_lat, xend=dest_lon, yend=dest_lat, alpha=value, colour = stat(index)), lineend = "round")+
  #scale_colour_gradient(low = "red", high = "green") +
  #Here is the magic bit that sets line transparency - essential to make the plot readable
  scale_alpha_continuous(range = c(0.05, 0.45), guide = "none")+
  #Set black background, ditch axes and fix aspect ratio
  #theme(panel.background = element_rect(fill='transparent',colour='transparent'))+quiet+coord_equal()+
  #theme(axis.text = element_blank(),
  #      axis.text.y = element_blank(),
  #      axis.ticks = element_blank(),
  #      rect = element_blank(),
  #      panel.background = element_rect(fill='darkgrey',colour='darkgrey'))+
  theme_map()+
  #theme(legend.position = "none")+
  coord_sf(crs = st_crs("+init=epsg:4326"))

ggsave(filename = "flows_bean.png")


flow <- flows_oil

ggplot(data = states) +
  geom_sf(color = "grey", fill= "transparent", size = 0.1)+
  geom_sf(data = GEO_MUN_SOY[GEO_MUN_SOY$excess_supply_oil>0,], aes(fill = excess_supply_oil), color = "transparent", size = 0.01)+
  scale_fill_viridis(direction = -1, option = "A", na.value = "transparent", name = "Excess soy oil supply \n in tons")+
  #scale_fill_gradient(low = "white", high = "darkgreen", na.value = "transparent", name = "Excess soybean supply")+
  #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below
  geom_segment(data = flow[which(flow$value>1000),], aes(x=orig_lon, y=orig_lat, xend=dest_lon, yend=dest_lat, alpha=value), col="firebrick")+
  #scale_colour_gradient(low = "red", high = "green") +
  #Here is the magic bit that sets line transparency - essential to make the plot readable
  scale_alpha_continuous(range = c(0.05, 0.45), guide = "none")+
  #Set black background, ditch axes and fix aspect ratio
  #theme(panel.background = element_rect(fill='transparent',colour='transparent'))+quiet+coord_equal()+
  #theme(axis.text = element_blank(),
  #      axis.text.y = element_blank(),
  #      axis.ticks = element_blank(),
  #      rect = element_blank(),
  #      panel.background = element_rect(fill='darkgrey',colour='darkgrey'))+
  theme_map()+
  #theme(legend.position = "none")+
  coord_sf(crs = st_crs("+init=epsg:4326"))

ggsave(filename = "flows_oil.png")


flow <- flows_cake

ggplot(data = states) +
  geom_sf(color = "grey", fill= "transparent", size = 0.1)+
  geom_sf(data = GEO_MUN_SOY[GEO_MUN_SOY$excess_supply_cake>0,], aes(fill = excess_supply_cake), color = "transparent", size = 0.01)+
  scale_fill_viridis(direction = -1, option = "C", na.value = "transparent", name = "Excess soy cake supply \n in tons")+
  #scale_fill_gradient(low = "white", high = "darkgreen", na.value = "transparent", name = "Excess soybean supply")+
  #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below
  geom_segment(data = flow[which(flow$value>1000),], aes(x=orig_lon, y=orig_lat, xend=dest_lon, yend=dest_lat, alpha=value), col="purple4")+
  #geom_link(data = flows_bean[which(flows_bean$value>10),], aes(x=orig_lon, y=orig_lat, xend=dest_lon, yend=dest_lat, alpha=value, colour = stat(index)), lineend = "round")+
  #scale_colour_gradient(low = "red", high = "green") +
  #Here is the magic bit that sets line transparency - essential to make the plot readable
  scale_alpha_continuous(range = c(0.05, 0.45), guide = "none")+
  #Set black background, ditch axes and fix aspect ratio
  #theme(panel.background = element_rect(fill='transparent',colour='transparent'))+quiet+coord_equal()+
  #theme(axis.text = element_blank(),
  #      axis.text.y = element_blank(),
  #      axis.ticks = element_blank(),
  #      rect = element_blank(),
  #      panel.background = element_rect(fill='darkgrey',colour='darkgrey'))+
  theme_map()+
  #theme(legend.position = "none")+
  coord_sf(crs = st_crs("+init=epsg:4326"))

ggsave(filename = "flows_cake.png")


# general MU level plots

plot_funct <- function(variable, title,...){
  thesubset <- GEO_MUN_SOY[pull(SOY_MUN, variable)>0,]#GEO_MUN_SOY[!is.na(GEO_MUN_SOY[,paste0(variable)])[,1],]
  thedata <- st_drop_geometry(thesubset)
  theplot <- ggplot(thesubset, aes(fill = pull(thedata, variable)), size = 0.01) +
    geom_sf(color = "transparent")+            
    scale_fill_viridis(direction = -1, na.value = "transparent", name = paste(title), ...) + 
    #scale_fill_gradient(low = "white", high = "firebrick")+
    geom_sf(data = states, fill = "transparent", color = "grey", size = 0.1)+
    theme_void() # or minimal
  return(theplot)
}

plot_funct(variable = "excess_supply_cake", title = "Excess soy cake supply in tons")
# ggsave(filename = "excess_supply_cake.png")
