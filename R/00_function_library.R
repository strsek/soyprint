### soyprint function library ###

library(ggplot2)
library(ggpointdensity)


# ggplot function filtering filtering polygons with positive values or above a defined threshold
gg_funct <- function(GEO, variable, title = NA, unit = NA, cutoff = 0, pal = "viridis", limits = NULL, ...){
  thesubset <- GEO[pull(st_drop_geometry(GEO), variable)>cutoff,]
  thedata <- st_drop_geometry(thesubset)
  theplot <- ggplot(thesubset, aes(fill = pull(thedata, variable))) +
    labs(title = ifelse(!is.na(title), title, paste(variable, dest)))+
    geom_sf(color = "transparent", size = 0.0) +            #color = "transparent"
    scale_fill_viridis(limits = limits, direction = -1, na.value = "transparent", name = paste(unit), option = pal) + 
    #scale_fill_gradient(low = "mintcream", high = "darkgreen", na.value = "transparent", name = paste(unit)) + 
    geom_sf(data = GEO_states, fill = "transparent", color = "darkgrey", size = 0.4)+
    theme_void()+ # or minimal
    theme(plot.title = element_text(hjust = 0.5), plot.margin = margin(0.2, 0.0, 0.2, 0.0, "cm")#,
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


## nicely named confidence intervals of a matrix returned as df
library(gmodels)
library(dplyr)

ci_funct <- function(x, level = 95, margin = 1, stats = c("lower", "upper", "mean", "se")){
     t(apply(as.matrix(x), margin, function(mat) {ci(mat, confidence = level/100)})) %>% 
     as.data.frame() %>% 
    # rename(!!paste0("upper",level) := `CI upper`, 
    #        !!paste0("lower",level) := `CI lower`, 
    #        !!paste0("mean",level) := Estimate, 
    #        !!paste0("sd",level) := `Std. Error`) %>%
     rename("upper" = `CI upper`, 
            "lower" = `CI lower`, 
            "mean" = Estimate, 
            "se" = `Std. Error`) %>%
     dplyr::select(one_of(stats))  %>%
     rename_with(~str_c(.,level), everything())
    #rename(across(.fns = function(x){paste0(x,level)}))
}


## define function for density coloring of scatterplots (based on https://slowkow.com/notes/ggplot2-color-by-density/)
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}


## scatter plots with pearsons r
scatter_funct <- function(data, x,y, title = NULL, colgroup = to_region, legend = TRUE, xlab = "", ylab = "TRASE", col = "firebrick", alph = 0.5){
  xsym <- rlang::sym(x)
  ysym <- rlang::sym(y)
  data <- filter(data, !!xsym > 0 | !!ysym > 0 )
  scatter <- ggplot(data, aes(x=!!xsym, y=!!ysym)) +
    geom_point(size=1.8, fill = col, alpha = alph, shape = 21, stroke = 0, aes(label1 = co_mun, label2 = to_code )) + #  ifelse(is.na(colgroup), "blue", !!rlang::sym(colgroup))) # aes(label1 = co_mun, label2 = to_code )
    geom_abline(slope = 1, alpha = 0.6, linetype = "dashed", color = "darkgray", size = 0.7) +
    geom_abline(slope = cor(data[[x]], data[[y]]), alpha = 0.8, color = "seagreen", size = 0.7) +
    labs(title = title, y = ylab, x = xlab) + 
    scale_color_d3("category20",  alpha = 0.4)  +   
    theme_minimal() +
    theme(legend.position = ifelse(legend, "right", "none")) +
    #scale_x_continuous(labels = scales::scientific) +
    stat_cor(method = "pearson", color = "seagreen", 
             p.accuracy = 0.001,
             label.x.npc = 0.5, label.y.npc = 0.1,
             size = 3)
}

scatter_funct_group <- function(data, x,y, title = NULL, legend = TRUE, xlab = "", ylab = "TRASE"){
  xsym <- rlang::sym(x)
  ysym <- rlang::sym(y)
  data <- filter(data, !!xsym > 0 | !!ysym > 0 )
  scatter <- ggplot(data, aes(x=!!xsym, y=!!ysym)) +
    geom_point(size=1, aes(fill = to_region, label1 = co_mun, label2 = to_code ), alpha = 0.4, shape = 21, stroke = 0) + #  ifelse(is.na(colgroup), "blue", !!rlang::sym(colgroup)))
    geom_abline(slope = 1, alpha = 0.5, linetype = "dashed", color = "gray", size = 0.7) +
    geom_abline(slope = cor(data[[x]], data[[y]]), alpha = 0.8, color = "seagreen", size = 1) +
    labs(title = title, y = ylab, x = xlab) + 
    scale_color_d3("category20",  alpha = 0.4)  +   
    theme_minimal() +
    labs(title = title, y = ylab, x = xlab) + 
    theme(legend.position = ifelse(legend, "right", "none")) +
    scale_x_continuous(labels = scales::scientific) +
    stat_cor(method = "pearson", color = "seagreen", 
             p.accuracy = 0.001,
             label.x.npc = 0.6, label.y.npc = "bottom")
}


scatter_funct_dens <- function(data, x,y, title = NULL, legend = TRUE, xlab = "", ylab = "TRASE", col = "firebrick"){
  xsym <- rlang::sym(x)
  ysym <- rlang::sym(y)
  data <- filter(data, !!xsym > 0 | !!ysym > 0 )
  scatter <- ggplot(data, aes(x=!!xsym, y=!!ysym)) +
    #geom_point(size=1, fill = col, aes(label1 = co_mun, label2 = to_code ), alpha = 0.4, shape = 21) + #  ifelse(is.na(colgroup), "blue", !!rlang::sym(colgroup)))
    geom_pointdensity(size = 1, alpha = 0.4, adjust = 4) + 
    geom_abline(slope = 1, alpha = 0.5, linetype = "dashed", color = "gray", size = 0.7) +
    geom_abline(slope = cor(data[[x]], data[[y]]), alpha = 0.8, color = "seagreen", size = 1) +
    labs(title = title, y = ylab, x = xlab) + 
    #scale_color_d3("category20",  alpha = 0.4)  +  
    scale_color_viridis()+#trans = "pseudo_log"
    theme_minimal() +
    theme(legend.position = ifelse(legend, "right", "none")) +
    #scale_x_continuous(labels = scales::scientific) +
    stat_cor(method = "pearson", color = "seagreen", 
             p.accuracy = 0.0001,
             label.x.npc = 0.5, label.y.npc = "bottom")
}

scatter_funct_dens2 <- function(data, x,y, bwx = NA, bwy = NA, n = 100, alph = 0.4, title = NULL, legend = TRUE, xlab = "", ylab = "TRASE", col = "firebrick", rsize = 3){
  if(any(is.na(bwx), is.na(bwy))) stop("bw has to be defined")
  xsym <- rlang::sym(x)
  ysym <- rlang::sym(y)
  data <- filter(data, !!xsym > 0 | !!ysym > 0 )
  data$density <- get_density(data[[x]], data[[y]], n = n, h = c(bwx, bwy))
  scatter <- ggplot(data, aes(x=!!xsym, y=!!ysym)) +
    geom_point(size=1, aes(fill = density, label1 = co_mun, label2 = to_code ), alpha = alph, shape = 21, stroke = 0) + #  ifelse(is.na(colgroup), "blue", !!rlang::sym(colgroup)))
    #geom_pointdensity(size = 1, alpha = 0.4, adjust = 4) + 
    geom_abline(slope = 1, alpha = 0.5, linetype = "dashed", color = "gray", size = 0.7) +
    geom_abline(slope = cor(data[[x]], data[[y]]), alpha = 0.8, color = "seagreen", size = 0.8) +
    labs(title = title, y = ylab, x = xlab) + 
    #scale_color_d3("category20",  alpha = 0.4)  +  
    scale_fill_viridis(trans = "pseudo_log", option = "plasma")+#
    theme_minimal() +
    theme(legend.position = ifelse(legend, "right", "none")) +
    #scale_x_continuous(labels = scales::scientific) +
    stat_cor(method = "pearson", color = "seagreen", 
             p.accuracy = 0.0001,
             label.x.npc = 0.6, label.y.npc = 0.05,
             size = rsize)
}

