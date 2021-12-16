### soyprint function library ###


# ggplot function filtering filtering polygons with positive values or above a defined threshold
gg_funct <- function(GEO, variable, title = NA, unit = NA, cutoff = 0, pal = "viridis", ...){
  thesubset <- GEO[pull(st_drop_geometry(GEO), variable)>cutoff,]
  thedata <- st_drop_geometry(thesubset)
  theplot <- ggplot(thesubset, aes(fill = pull(thedata, variable))) +
    labs(title = ifelse(!is.na(title), title, paste(variable, dest)))+
    geom_sf(color = "transparent", size = 0.0) +            #color = "transparent"
    scale_fill_viridis(direction = -1, na.value = "transparent", name = paste(unit), option = pal) + 
    #scale_fill_gradient(low = "mintcream", high = "darkgreen", na.value = "transparent", name = paste(unit)) + 
    geom_sf(data = GEO_states, fill = "transparent", color = "darkgrey", size = 0.8)+
    theme_void()+ # or minimal
    theme(plot.title = element_text(hjust = 0.5), plot.margin = margin(0.5, 1, 0.5, 1, "cm")#,
        #    panel.background = element_rect(fill='transparent', color='transparent'), #transparent panel bg)
        #    plot.background = element_rect(fill='transparent', color='transparent'), #transparent plot bg
        #    panel.grid.major = element_blank(), #remove major gridlines
        #    panel.grid.minor = element_blank(), #remove minor gridlines
        #    legend.background = element_rect(fill='transparent', color='transparent'), #transparent legend bg
        #    legend.box.background = element_rect(fill='transparent', color='transparent') #transparent legend panel
          )
  return(theplot)
}

gg_funct_state <- function(GEO, variable, title = NA, unit = NA, cutoff = 0, state = "MT", ...){
  thesubset <- GEO[pull(st_drop_geometry(GEO), variable)>cutoff,]
  thedata <- st_drop_geometry(thesubset)
  theplot <- ggplot(thesubset, aes(fill = pull(thedata, variable)), size = 0.01) +
    labs(title = ifelse(!is.na(title), title, paste(variable, dest)))+
    geom_sf(color = "transparent")+            
    scale_fill_viridis(direction = -1, na.value = "transparent", name = paste(unit)) + 
    geom_sf(data = filter(GEO_states, nm_state == state), fill = "transparent", color = "grey", size = 0.3)+
    theme_void()+ # or minimal
    theme(plot.title = element_text(hjust = 0.5), plot.margin = margin(0.5, 1, 0.5, 1, "cm"))
  return(theplot)
}

gg_funct_state2 <- function(GEO, variable, title = NA, unit = NA, cutoff = 0, state = "MT", ...){
  thesubset <- GEO[pull(st_drop_geometry(GEO), variable)>cutoff,]
  thedata <- st_drop_geometry(thesubset)
  theplot <- ggplot(thesubset, aes(fill = pull(thedata, variable)), size = 0.01) +
    labs(title = ifelse(!is.na(title), title, paste(variable, dest)))+
    geom_sf(color = "transparent")+            
    scale_fill_viridis(direction = -1, na.value = "transparent", name = paste(unit),
                       breaks=c(0,1),labels=c("low","high"),
                       limits=c(0,1)) + 
    geom_sf(data = filter(GEO_states, nm_state == state), fill = "transparent", color = "grey", size = 0.1)+
    theme_void()+ # or minimal
    theme(plot.title = element_text(hjust = 0.5), plot.margin = margin(0.5, 1, 0.5, 1, "cm"))
  return(theplot)
}


# mapview function filtering polygons with positive values or above a defined threshold
mv_funct <- function(GEO, variable, cutoff = 0){
  thesubset <- GEO[pull(st_drop_geometry(GEO), variable)>cutoff,]
  map <- mapview(thesubset, zcol = variable, na.alpha = 0, layer.name = paste(variable, dest))
  return(map)
}

# function to export synced mapview plots (see https://github.com/r-spatial/mapview/issues/35)
save_tags <- function (tags, file, selfcontained = F, libdir = "./lib") 
{
  if (is.null(libdir)) {
    libdir <- paste(tools::file_path_sans_ext(basename(file)), 
                    "_files", sep = "")
  }
  htmltools::save_html(tags, file = file, libdir = libdir)
  if (selfcontained) {
    if (!htmlwidgets:::pandoc_available()) {
      stop("Saving a widget with selfcontained = TRUE requires pandoc. For details see:\n", 
           "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
    }
    htmlwidgets:::pandoc_self_contained_html(file, file)
    unlink(libdir, recursive = TRUE)
  }
  return(file)
}
