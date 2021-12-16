library(Matrix)
library(data.table)
library(sf)
library(dplyr)
library(tidyr)
library(raster)
library(fasterize)
library(mapview)
library(gdalUtilities)
library(parallel)
library(ggplot2)
library(rasterVis)
library(viridis)

# prepare footprint results ----------------------------------

regions <- fread("input_data/FABIO/inst/regions_full.csv")

# load production footprints
P <- readRDS("results/P.rds")

# load MU polygons and project to WGS84
GEO_MUN_SOY <- readRDS("intermediate_data/GEO_MUN_SOY_fin.rds") %>% st_transform(crs = 4326)

# helper function (add to library)
is.finite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) (is.finite(x)))
}

# join production footprints for soybeans with MU polygons
P_soy_mun <- P[as.numeric(sub("_.*", "", rownames(P)))>1000 & sub(".*_", "", rownames(P)) == "c021",]
P_soy_mun <- as.data.frame(as.matrix(P_soy_mun))
# transform into integer probabilities
P_soy_mun <- P_soy_mun/GEO_MUN_SOY$prod_bean # make sure they sum up to one!
P_soy_mun[!is.finite(P_soy_mun)] <- 0
P_soy_mun <- round(P_soy_mun*100)
# change column names to ISO codes
colnames(P_soy_mun) <- regions$iso3c[match(colnames(P_soy_mun), regions$code)]
# join with polygons
P_soy_mun <- mutate(P_soy_mun, co_mun = as.numeric(sub("_.*", "", rownames(P_soy_mun)))) %>% relocate(co_mun)
GEO_MUN_P <- dplyr::select(GEO_MUN_SOY, c(co_mun:nm_state, prod_bean)) %>%
  left_join(P_soy_mun, by = "co_mun") # or simply to cbind, as rows already math (but this is more safe)


# prepare land-use tiles --------------------------------------

# load tiles in a list
tile_names = list.files("input_data/geo/mb_tiles/",pattern="^.*.tif$")
tile_paths = paste0("input_data/geo/mb_tiles/",tile_names)
tiles <- lapply(tile_paths, raster)
names(tiles) <- tile_names

# create function to write probability values from transport model in to soy pixels for each MU
# TODO: if a sub-region is selected (e.g. MT), make sure only those tiles are processes and written to file which overlap with the region
burn_rast <- function(rast, poly, class, value, file, zeroes = TRUE, subarea = FALSE){
  # if the target class is not contained in the raster or it does not overlap with the polygon extent, stop
  if(!class %in% rast[] | !ifelse(subarea, st_intersects(st_as_sfc(st_bbox(poly)), st_as_sfc(st_bbox(rast)), sparse = F), TRUE)) {
    return() # next # out <- NULL stop("no soy in tile") 
  } else {
    # optional: extract required class --> not needed if raster is already (0/1)
    rast <- clamp(rast, class, class, useValues = FALSE) # rast[rast != class] <- NA ; rast[!is.na(rast)] <- 1
    # optional: round if needed
    poly <- mutate(poly, !!rlang::sym(value) := round(!!rlang::sym(value)))  # value_var <- rlang::sym(value)
    if(!zeroes) poly <- filter(poly, !!rlang::sym(value) > 0 )
    # rasterize
    out <- fasterize::fasterize(poly, rast, field = value) # we could also create a raster brick by value/destination with "by"
    # keep only soy pixels
    out <- out * rast
    # if a file name was specified write and load from file, otherwise load in memory (not suggested)
    if (missing(file)) {
      return(out)
    } else {
      writeRaster(out, filename=file, format="GTiff", datatype="INT1U", overwrite=TRUE)
      raster(file) 
    }
  }
}

# test for one tile
# prob_tile1 <- burn_rast(rast = tiles[[1]], poly = GEO_MUN_SOY, value = "co_state", class = 1, file = "prob_tile1.tif")
# mapview(prob_tile1)

# select final demand region
reg <- "CHN"
# create directory
ifelse(!dir.exists(paste0("results/mb_tiles/",reg)), dir.create(paste0("results/mb_tiles/",reg)), "Folder exists already")

# apply to tile list
system.time(
prob_tiles <- mclapply(names(tiles), function(x) {
  burn_rast(rast = tiles[[x]], 
            poly = GEO_MUN_P, 
            value = reg, class = 1, zeroes = FALSE,
            file = paste0("results/mb_tiles/",reg,"/",x))
  }, mc.cores = 12)
)

# build a vrt
gdalUtilities::gdalbuildvrt(gdalfile = sapply(prob_tiles, filename), output.vrt = paste0("results/mb_tiles/",reg,"/prob.vrt"))
prob_vrt <- raster(paste0("results/mb_tiles/",reg,"/prob.vrt"))
mapview(prob_vrt, na.color = "transparent")

# resample: two options

# # 1: with gdal: saves resampled tiles to file
# # NOTE: this is not strictly necessary, as we can also create a resampled vrt out of the original tiles
# gdal_resample <- function(r, factor, outdir, method = 'near', load = FALSE) {
#   
#   #Geometry attributes
#   t1 <- c(xmin(r), ymin(r), 
#           xmax(r), ymax(r))
#   res <- res(r) * factor
#   
#   #Temporal files
#   fname <- sub("^.+/", "", filename(r))
#   inname <- sub(paste0(getwd(),"/"), "", filename(r)) #paste0(indir,fname)
#   outname <- paste0(outdir,fname)
# 
#   #GDAL time!
#   gdalUtilities::gdalwarp(srcfile = inname, dstfile = outname, 
#            tr = res, te = t1, r = method, overwrite = TRUE)
#   
#   if (load) {
#     resample_raster = raster(outname)
#     return(resample_raster)
#   } else {
#     cat("file", fname, "successfully resampled and saved to", outdir)
#     return()
#   }
# }
# 
# system.time(
# prob_tiles_agg <- mclapply(prob_tiles, gdal_resample, factor = 16, outdir = "results/mb_tiles/resampled/", load = TRUE, mc.cores = 12)
# )
# 
# gdalUtilities::gdalbuildvrt(gdalfile = sapply(prob_tiles_agg, filename), output.vrt = "results/mb_tiles/resampled/mb_test.vrt")
# prob_agg_vrt <- raster("results/mb_tiles/resampled/mb_test.vrt")
# 
# mapview(prob_agg_vrt, na.color = "transparent")


# 2: directly build a vrt from high-res tiles with desired resolution:
gdalUtilities::gdalbuildvrt(gdalfile = sapply(prob_tiles, filename), 
                                             output.vrt = paste0("results/mb_tiles/",reg,"/prob_agg.vrt"),
                                             r = "nearest", # TODO: what resampling method?
                                             tr = res(prob_tiles[[1]])*16)

prob_agg_vrt <- raster(paste0("results/mb_tiles/",reg,"/prob_agg.vrt"))
mapview(prob_agg_vrt, na.color = "transparent")# plot


# plot -----------------------------------------

GEO_states <- st_read("input_data/geo/GADM_boundaries/gadm36_BRA_1.shp", stringsAsFactors = FALSE)

# using solution from https://stackoverflow.com/questions/47116217/overlay-raster-layer-on-map-in-ggplot2-in-r/47190738#47190738

# function to reshape raster into data frame
# this is nice because it only retains information of non-NA cells and their position!
gplot_data <- function(x, maxpixels = 500000)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x))) 
  names(dat) <- c('value', 'variable')
  
  dat <- dplyr::as_tibble(data.frame(coords, dat))
  
  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]], 
                            by = c("value" = "ID"))
  }
  dat
}

prob_dat <- gplot_data(prob_agg_vrt, maxpixels = ncell(prob_agg_vrt)) %>% dplyr::filter(!is.na(value))

# plot
ggplot() +
  geom_sf(data = GEO_states, fill = "gray19", color = "lightgrey", size = 0.1) +
  geom_tile(data = dplyr::filter(prob_dat, !is.na(value)), 
            aes(x = x, y = y, fill = value) ) +
  #scale_fill_gradient("Land use\nprobability",
  #                    low = 'yellow', high = 'blue',
  #                    na.value = NA) +
  scale_fill_viridis()+
  coord_sf(datum = sf::st_crs(prob_agg_vrt)) +
  theme_void()
#coord_quickmap()
  
  
