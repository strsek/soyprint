##### Benchmark comparison / validation  #####

library(dplyr)
library(data.table)
library(Matrix)
library(tidyr)
library(ggplot2)
library(purrr)
library(sf)
library(viridis)
library(mapview)
library(sf)
library(leafsync)
library(patchwork)
library(ggsci)
library(gtools)
library(ggpubr)
library(Metrics)
library(xtable)


# load function library
source("R/00_function_library.R")

write = FALSE
options(scipen = 99999)

# load data ----------

SOY_MUN <- readRDS("intermediate_data/SOY_MUN_fin.rds")
GEO_MUN_SOY <- readRDS("intermediate_data/GEO_MUN_SOY_fin.rds")
GEO_states <- st_read("input_data/geo/GADM_boundaries/gadm36_BRA_1.shp", stringsAsFactors = FALSE) %>%  
  mutate(nm_state = substr(HASC_1, 4,5))
comp_list <- readRDS( "intermediate_data/comp_list.rds")
MUN_capital_dist <- readRDS("intermediate_data/MUN_capital_dist.rds")
class(MUN_capital_dist) <- "numeric"
EXP_NAT_wide <- readRDS("intermediate_data/EXP_NAT_wide.rds")
CBS_SOY <- readRDS("intermediate_data/CBS_SOY_bal.rds")

# extract only required columns from benchmark tables, renaming "mean" to "multimode"
comp_list <- lapply(comp_list, function(comp){dplyr::select(comp,co_state:multimode_mean) %>% rename(multimode = multimode_mean)})
# transfrom flows from tons to kilotons for plots
comp_list <- lapply(comp_list, function(comp){mutate(comp, across(trase:multimode, function(x){x/1000}))})
comp_mun <- comp_list$mun
comp_state <- comp_list$state


# maps of flow origins for selected destinations --------------------------

# select destination country
targets <- c("CHN", "BRA", "NLD", "ESP", "THA", "DEU", "KOR", "NOR")
for (dest in targets){

#dest <- "CHN"

# merge benchmark results with GEOdata
GEO_MUN_SOY_dest <- GEO_MUN_SOY %>% 
  left_join(filter(comp_mun, to_code == dest)) %>%
  mutate_all(~replace(., is.na(.), 0))
GEO_STATE_SOY_dest <- GEO_states %>% 
  left_join(filter(comp_state, to_code == dest)) %>% 
  mutate_all(~replace(., is.na(.), 0))

# # or by destination region
# dest <- "EU"
# # merge results with GEOdata
# GEO_MUN_SOY_dest <- GEO_MUN_SOY %>% left_join(filter(comp_mun_by_region, to_region == dest))# %>% replace(is.na(.), 0)
# GEO_STATE_SOY_dest <- GEO_states %>% left_join(filter(comp_state_by_region, to_region == dest))


# select results to compare
res <- c("trase", "euclid", "multimode", "downscale") 
names(res) <- res
resdata <- GEO_MUN_SOY_dest %>% st_drop_geometry() %>% dplyr::select(all_of(res)) 
resdata_state <- GEO_STATE_SOY_dest %>% st_drop_geometry() %>% dplyr::select(all_of(res)) 

# generate ggplots (using gg_funct from function library)
limits <- c(min(resdata[resdata>0]), max(resdata))
# NOTE: The cutoff allows to not plot flows below a certain level (in tons)
exp_dest <- lapply(res, gg_funct, 
                   GEO = GEO_MUN_SOY_dest, cutoff = 10, unit = "kilotons", title = '', pal = "plasma", limits = limits)
exp_dest <- map2(exp_dest, res, ~ .x + ggtitle(.y))
(wrap <- wrap_plots(exp_dest, nrow = ceiling(length(exp_dest)/2), guides = "collect"))
if(write) ggsave(paste0("results/map_benchmark_",dest,".png"), wrap, bg='transparent', scale = 1.5, width = 20,  height = 20, units = "cm")

}

limits_state <- c(min(resdata_state[resdata_state>0]), max(resdata_state))
exp_dest_state <- lapply(res, gg_funct, 
                         GEO = GEO_STATE_SOY_dest, unit = "tons", cutoff = 10, pal = "plasma", limits = limits_state)
exp_dest_state <- map2(exp_dest_state, res, ~ .x + ggtitle(.y))
(wrap_state <- wrap_plots(exp_dest_state, nrow = ceiling(length(exp_dest_state)/2), guides = "collect"))
if(write) ggsave(paste0("results/map_benchmark_state_",dest,".png"), wrap_state, bg='transparent', scale = 1.5, width = 30,  height = 10, units = "cm")

# mapview approach (using mv_funct from function library)
exp_dest_mv <- lapply(res, mv_funct,  GEO = GEO_MUN_SOY_dest, cutoff = 10)
(sync <- sync(exp_dest_mv, ncol = 2))
#save_tags(sync, paste0("results/benchmark_",dest,".html"), selfcontained=TRUE)

exp_dest_mv_state <- lapply(res, mv_funct,  GEO = GEO_STATE_SOY_dest,cutoff = 10)
(sync_state <- sync(exp_dest_mv_state, ncol = 2))
#save_tags(sync, paste0("results/benchmark_state_",dest,".html"), selfcontained=TRUE)



# compute focal sums/means -----------------------------------------------------------------------

# filter those MUs that produce soy --> these are the only MUs relevant for the comparison
SOY_MUN_prod <- SOY_MUN[SOY_MUN$prod_bean>0,]
GEO_MUN_SOY_prod  <- GEO_MUN_SOY[GEO_MUN_SOY$prod_bean>0,]
MUN_capital_dist_prod <- MUN_capital_dist[SOY_MUN$prod_bean>0, SOY_MUN$prod_bean>0]

# create (different spatial weights matrices)
# direct neighbors (queens contiguity)
w_q <- st_touches(GEO_MUN_SOY_prod, byid = FALSE, sparse = FALSE)*1
w_q <- as(w_q, "sparseMatrix")
dimnames(w_q) <- list(GEO_MUN_SOY_prod$co_mun, GEO_MUN_SOY_prod$co_mun)
diag(w_q) <- 1

# MUs in distance of max 100km
w_d100 <- MUN_capital_dist_prod
w_d100[MUN_capital_dist_prod >  100000] <- 0
w_d100[MUN_capital_dist_prod <= 100000] <- 1
w_d100 <- as(w_d100, "sparseMatrix")
diag(w_d100) <- 1

# MUs inverse distance
w_id <- 1/MUN_capital_dist_prod
w_id[!is.finite(w_id)] <- 0
w_id <- as(w_id, "sparseMatrix")
diag(w_id) <- rowSums(w_id)

# add column/row for unknown MU
c_unknown <- c(rep(0,nrow(GEO_MUN_SOY_prod)), 1)
add_unknown <- function(mat){mat <- rbind(mat, "9999999" = rep(0, ncol(mat))); mat <- cbind(mat, "9999999" = c_unknown)}
w_q     <- add_unknown(w_q)
w_d100  <- add_unknown(w_d100)
w_id <- add_unknown(w_id)

# row-standardize
w_q_s  <- w_q/rowSums(w_q)
w_d100_s  <- w_d100/rowSums(w_d100)
w_id_s <- w_id/rowSums(w_id)
w_list <- list(w_q, w_q_s, w_d100, w_d100_s, w_id_s)
names(w_list) <- w_nms <- c("sum_q", "mean_q", "sum_100", "mean_100", "mean_id" )


# create df containing all possible combinations of origins and destinations

# optional: remove the unknown origin flows in trase
comp_mun_known <- filter(comp_mun, co_mun != "9999999")
# how many flows do we lose?
sum(comp_mun$trase) - sum(comp_mun_known$trase) # a lot!

# sort destinations by aggregate export volume
comp_nat <- comp_mun_known %>% group_by(to_code, to_name) %>% summarise(across(trase:multimode, sum)) %>% arrange(desc(downscale))
# use only those that have exports in both trase and comex
dests <- comp_nat$to_code[comp_nat$trase>0 & comp_nat$downscale>0] ; names(dests) <- dests
# how many flows do we lose?
comp_nat_loss <- filter(comp_nat, !to_code %in% dests) %>% summarise(across(trase:multimode, ~ sum(.x)))
#dest_regs <- (unique(comp_mun[,c("to_code","to_region")])) %>% arrange(to_code)

comp_mun_long <- expand.grid(co_mun = SOY_MUN_prod$co_mun, # c(SOY_MUN_prod$co_mun, 9999999),
                             "to_code" = dests, stringsAsFactors = FALSE)
#comp_mun_long$to_region <- dest_regs$to_region[match(comp_mun_long$to_code, dest_regs$to_code)]
comp_mun_long <- merge(comp_mun_long, 
                       as.data.table(comp_mun), #[, .(co_mun, to_code, trase, multimode, euclid, downscale)]
                        by = c("co_mun", "to_code"), all.x = TRUE) %>%
                 dplyr::arrange(to_code) 
# NOTE: some NAs due destination LCA, which is only contained in TRASE --> not relevant for comparison, will be discarded below
#comp_mun_long[is.na(comp_mun_long)] <- 0
# separate by destination
comp_mun_long_dest <- sapply(dests, function(dest){
  filter(comp_mun_long, to_code == dest)}, 
  simplify = FALSE, USE.NAMES = TRUE)
#all.equal(as.character(comp_mun_long_dest$AGO$co_mun), rownames(w_d100_s))


## compute focal values
comp_mun_long_dest <- lapply(comp_mun_long_dest, function(comp){
  by_weight <- lapply(names(w_list), function(w){
    w_co  <- w_list[[w]][as.character(comp$co_mun), as.character(comp$co_mun)] # w[as.character(comp$co_mun), as.character(comp$co_mun)]
    comp_focal <- comp %>% dplyr::select(all_of(res)) %>% as("Matrix") 
    comp_focal <- as.data.frame(as.matrix(w_co %*% comp_focal))
    names(comp_focal) <- paste0(names(comp_focal),"_",w)
    return(comp_focal)
  })
  comp_focal <-  do.call("cbind", by_weight)
  comp_focal <- cbind(comp,comp_focal)
  return(comp_focal)
})



# remove countries that are only present in either trase or our results
#trase_dests <- unique(comp_mun_long$to_code[comp_mun_long$trase>0])
#comex_dests <- unique(comp_mun_long$to_code[comp_mun_long$euclid>0])
#(setdiff(trase_dests, comex_dests))
#comp_mun_long_dest <- comp_mun_long_dest[!names(comp_mun_long_dest) %in% setdiff(trase_dests, comex_dests)]

# bind to long list
comp_mun_long <- rbindlist(comp_mun_long_dest)
#comp_mun_long <- group_by(comp_mun_long, to_code) %>% mutate(exp_nat = sum(downscale, na.rm = T))
#comp_mun_long <- arrange(comp_mun_long, desc(exp_nat))
  

## scatterplots ---------------------------------------------------------------

models <- c("multimode", "euclid", "downscale"); names(models) = models
w_nms <- c("base", "mean_q")
names(w_nms) <- w_nms

# on global level
#layout <- '
#           AB
#           CD
#           EF
#'
scatter_global <- lapply(models, function(mod){
  scatter_base <- scatter_funct(comp_mun_long, paste(mod), "trase")
  scatter_mean_q <- scatter_funct(comp_mun_long, paste0(mod,"_mean_q"), "trase_mean_q")
  #scatter_sum_q  <- scatter_funct(comp_mun_long, paste0(mod,"_sum_q"), "trase_sum_q")
  #scatter_mean_100 <- scatter_funct(comp_mun_long, paste0(mod,"_mean_100"), "trase_mean_100")
  #scatter_sum_100  <- scatter_funct(comp_mun_long, paste0(mod,"_sum_100"), "trase_sum_100")
  #scatter_mean_id  <- scatter_funct(comp_mun_long, paste0(mod,"_mean_id"), "trase_mean_id")
  #return(list(base = scatter_base, 
  #            mean_q = scatter_mean_q, sum_q = scatter_sum_q, 
  #            mean_100 = scatter_mean_100, sum_100 = scatter_sum_100,
  #            mean_id = scatter_mean_id))
  #patch <- wrap_plots(A = scatter_base, B = scatter_mean_q, C = scatter_sum_q, D = scatter_mean_100, E = scatter_sum_100, F = scatter_mean_id, design = layout, guides = "collect")
  patch = scatter_base + scatter_mean_q + plot_layout(guides = 'collect')
  return(patch)
  })



scatter_global <- lapply(w_nms, function(w){
    lapply(models, function(mod){
      if(w != "base"){w = paste0("_",w)} else {w = ""}
      scatter <- scatter_funct(comp_mun_long, paste0(mod,w), paste0("trase",w), legend = FALSE,
                               ylab = "TRASE", xlab = mod,
                               col = "firebrick") 
      return(scatter) 
    })
  })

scatter_global <- lapply(models, function(mod){
    lapply(w_nms, function(w){
    if(w != "base"){w = paste0("_",w)} else {w = ""}
    scatter <- scatter_funct(comp_mun_long, paste0(mod,w), paste0("trase",w), legend = TRUE,
                             ylab = "TRASE", xlab = mod,
                             col = "firebrick",
                             alph = 0.4) 
    return(scatter) 
  })
})

scatter_global <- lapply(models, function(mod){
  lapply(w_nms, function(w){
    if(w != "base"){w = paste0("_",w)} else {w = ""}
    bw = ifelse(w == "",100, 50)
    scatter <- scatter_funct_dens2(comp_mun_long, paste0(mod,w), paste0("trase",w), legend = FALSE,
                             bwx = bw, bwy = bw,
                              ylab = "TRASE", xlab = mod,
                             col = "firebrick",
                             alph = 0.4) 
    return(scatter) 
  })
})


scatter_global <- lapply(w_nms, function(w){
    gg <- lapply(models, function(mod){
    if(w != "base"){w = paste0("_",w)} else {w = ""}
    bw = ifelse(w == "",100, 50)
    scatter <- scatter_funct_dens2(comp_mun_long, paste0(mod,w), paste0("trase",w), legend = FALSE,
                                   bwx = bw, bwy = bw,
                                   ylab = "TRASE", xlab = mod, rsize = 4,
                                   col = "firebrick",
                                   alph = 0.4) 
    return(scatter) 
  })
  wlab <- ggplot() + 
    annotate("text", x = 0.8, y = 0.5, size=5.5, label = ifelse(w == "base", "direct", "focal mean")) + 
    theme_void()
  gg <- c(lab = list(wlab), gg)
  wrap_plots(gg, nrow = 1) + plot_layout(widths = c(0.5,1,1,1))
})


#scatter_global$multimode$base <- scatter_global$multimode$base + labs(title = "direct") + theme(plot.title = element_text(hjust = 0.5))
#scatter_global$multimode$mean_q <- scatter_global$multimode$mean_q + labs(title = "focal mean") + theme(plot.title = element_text(hjust = 0.5))


#scat_wrap_global <- wrap_plots(c(scatter_global$multimode, scatter_global$euclid, scatter_global$downscale), nrow = 3)
scat_wrap_global <- scatter_global$base / scatter_global$mean_q
ggsave(scat_wrap_global, file = "results/scatter_global.png", height = 20, width = 38, units = "cm")

# by country

scatter_dest <- lapply(comp_mun_long_dest[c("BRA", "CHN", "NLD", "DEU")], function(comp_dest){
  scatter_list <- lapply(models, function(mod){
    scatter_base <- scatter_funct(comp_dest, paste(mod), "trase", legend = FALSE)
    scatter_mean_q <- scatter_funct(comp_dest, paste0(mod,"_mean_q"), "trase_mean_q", legend = FALSE)
    scatter_sum_q  <- scatter_funct(comp_dest, paste0(mod,"_sum_q"), "trase_sum_q", legend = FALSE)
    scatter_mean_100 <- scatter_funct(comp_dest, paste0(mod,"_mean_100"), "trase_mean_100", legend = FALSE)
    scatter_sum_100  <- scatter_funct(comp_dest, paste0(mod,"_sum_100"), "trase_sum_100", legend = FALSE)
    scatter_mean_id  <- scatter_funct(comp_mun_long, paste0(mod,"_mean_id"), "trase_mean_id", legend = FALSE)
    #return(list(base = scatter_base, 
    #            mean_q = scatter_mean_q, sum_q = scatter_sum_q, 
    #            mean_100 = scatter_mean_100, sum_100 = scatter_sum_100,
    #            mean_id = scatter_mean_id))
    patch <- wrap_plots(A = scatter_base, B = scatter_mean_q, C = scatter_sum_q, D = scatter_mean_100, E = scatter_sum_100, design = layout, guides = "collect")
    return(patch)
    })
})


w_nms <- c("", "mean_q")
names(w_nms) <- w_nms
targets <- c("CHN", "ESP", "NOR"); names(targets) <- targets
targets_long <- c("China", "Spain", "Norway"); names(targets_long) <- targets
target_cols <- c("darkblue", "firebrick", "darkmagenta"); names(target_cols) = targets
models <- c("multimode", "euclid", "downscale"); names(models) = models

scatter_dest <- lapply(targets, function(targ){
  comp_dest <- comp_mun_long_dest[[targ]]
  lapply(w_nms, function(w){
    gg <- lapply(models, function(mod){
      if(w != "base"){w = paste0("_",w)} else {w = ""}
      options(scipen = 9999) 
      scatter <- scatter_funct(comp_dest, paste0(mod,w), paste0("trase",w), 
                               col = target_cols[targ],
                               xlab = mod,
                               legend = FALSE) 
      return(scatter) 
    })
    destlab <- ggplot() + 
      annotate("text", x = 0.8, y = 0.5, size=6, label = targets_long[targ]) + 
      theme_void()
    gg <- c(lab = list(destlab), gg)
    wrap_plots(gg, nrow = 1) + plot_layout(widths = c(0.5,1,1,1))
  })
})

scatter_targets <-  scatter_dest$CHN$mean_q / scatter_dest$ESP$mean_q / scatter_dest$NOR$mean_q
#scatter_targets[[1]][[2]] <-  scatter_targets[[1]][[2]] + labs(title = 'China') + theme(plot.title = element_text(hjust = 0.5))
#scatter_targets[[2]][[2]] <-  scatter_targets[[2]][[2]] + labs(title = 'Spain') + theme(plot.title = element_text(hjust = 0.5))
#scatter_targets[[3]][[2]] <-  scatter_targets[[3]][[2]] + labs(title = 'Norway') + theme(plot.title = element_text(hjust = 0.5))

ggsave(scatter_targets, filename="results/scatter_targets.png", height = 15, width = 17.5, units = "cm", scale = 1.5)



## xy plots 
#comp_plot <- filter(comp_mun, nm_mun != "UNKNOWN")
#ggplot(comp_plot, aes(x=trase, y = multimode))+
#  geom_point(color = "darkgreen", alpha = 0.1)+
#  geom_abline(slope = 1) + 
#  geom_abline(slope = 1.05, linetype="dashed", color = "blue")+
#  geom_abline(slope = 0.95, linetype="dashed", color = "blue")+
#  geom_abline(slope = 1.5, linetype="dashed", color = "cyan")+
#  geom_abline(slope = 0.5, linetype="dashed", color = "cyan")


#comp_mun$density <- get_density(comp_mun_long$multimode, comp_mun_long$trase, n = 100)
#
#ggplot(comp_mun, aes(x=multimode, y = cv, color = density))+
#  geom_point(alpha = 0.4, shape = 1) + 
#  scale_color_viridis()+
#  labs(y = "coefficient of variation", "mean (tons)") +
#  theme_minimal()

# regressions

# globally
reg_global <- lapply(models, function(mod){
  m_base <- lm(as.formula(paste0("trase ~ ",mod)), data = comp_mun_long) %>% summary
  m_sum_q <- lm(as.formula(paste0("trase_sum_q ~ ",mod,"_sum_q")), data = comp_mun_long) %>% summary
  m_mean_q <- lm(as.formula(paste0("trase_mean_q ~ ",mod,"_mean_q")), data = comp_mun_long) %>% summary
  m_sum_100 <- lm(as.formula(paste0("trase_sum_100 ~ ",mod,"_sum_100")), data = comp_mun_long) %>% summary
  m_mean_100 <- lm(as.formula(paste0("trase_mean_100 ~ ",mod,"_mean_100")), data = comp_mun_long) %>% summary
  m_mean_id <- lm(as.formula(paste0("trase_mean_id ~ ",mod,"_mean_id")), data = comp_mun_long) %>% summary
  return(list(base = m_base$r.squared, mean_q = m_mean_q$r.squared, sum_q = m_sum_q$r.squared, mean_100 = m_mean_100$r.squared, sum_100 = m_sum_100$r.squared, mean_id = m_mean_id$r.squared)) 
})

dfs_global <- lapply(reg_global, data.frame, stringsAsFactors = FALSE)
dfs_global <- bind_rows(dfs_global)
rownames(dfs_global) <- names(models)
#dfs_global <- t(dfs_global)

# globally with country fixed effects
reg_global_fe <- lapply(models, function(mod){
  m_base <- lm(as.formula(paste0("trase ~ to_code + ",mod,":to_code")), data = comp_mun_long) %>% summary
  m_sum_q <- lm(as.formula(paste0("trase_sum_q ~ to_code + ",mod,"_sum_q : to_code")), data = comp_mun_long) %>% summary
  m_mean_q <- lm(as.formula(paste0("trase_mean_q ~ to_code + ",mod,"_mean_q : to_code")), data = comp_mun_long) %>% summary
  m_sum_100 <- lm(as.formula(paste0("trase_sum_100 ~ to_code + ",mod,"_sum_100 : to_code")), data = comp_mun_long) %>% summary
  m_mean_100 <- lm(as.formula(paste0("trase_mean_100 ~ to_code + ",mod,"_mean_100 : to_code")), data = comp_mun_long) %>% summary
  m_mean_id <- lm(as.formula(paste0("trase_mean_id ~ to_code + ",mod,"_mean_id : to_code")), data = comp_mun_long) %>% summary
  return(list(base = m_base, mean_q = m_mean_q, sum_q = m_sum_q, mean_100 = m_mean_100, sum_100 = m_sum_100, mean_id = m_mean_id)) 
})

reg_global_fe1 <- lapply(models, function(mod){
  m_base <- lm(as.formula(paste0("trase ~ ",mod,"*to_code")), data = comp_mun_long) %>% summary
  m_sum_q <- lm(as.formula(paste0("trase_sum_q ~ ",mod,"_sum_q * to_code")), data = comp_mun_long) %>% summary
  m_mean_q <- lm(as.formula(paste0("trase_mean_q ~ ",mod,"_mean_q * to_code")), data = comp_mun_long) %>% summary
  m_sum_100 <- lm(as.formula(paste0("trase_sum_100 ~ ",mod,"_sum_100 * to_code")), data = comp_mun_long) %>% summary
  m_mean_100 <- lm(as.formula(paste0("trase_mean_100 ~ ",mod,"_mean_100 * to_code")), data = comp_mun_long) %>% summary
  m_mean_id <- lm(as.formula(paste0("trase_mean_id ~ ",mod,"_mean_id * to_code")), data = comp_mun_long) %>% summary
  return(list(base = m_base, mean_q = m_mean_q, sum_q = m_sum_q, mean_100 = m_mean_100, sum_100 = m_sum_100, mean_id = m_mean_id)) 
})

# by destination
w_nms <- c("",w_nms)
names(w_nms) <- w_nms

reg_dest <- lapply(comp_mun_long_dest, function(comp_dest){
  lapply(w_nms, function(w){
    lapply(models, function(mod){
      if(w != "") w = paste0("_",w)
      m<- lm(as.formula(paste0("trase",w," ~ ",mod,w)), data = comp_dest) %>% summary
      return(m$r.squared) 
    })
  })
})

'm_base <- lm(as.formula(paste0("trase ~ ",mod)), data = comp_dest) %>% summary
m_sum_q <- lm(as.formula(paste0("trase_sum_q ~ ",mod,"_sum_q")), data = comp_dest) %>% summary
m_mean_q <- lm(as.formula(paste0("trase_mean_q ~ ",mod,"_mean_q")), data = comp_dest) %>% summary
m_sum_100 <- lm(as.formula(paste0("trase_sum_100 ~ ",mod,"_sum_100")), data = comp_dest) %>% summary
m_mean_100 <- lm(as.formula(paste0("trase_mean_100 ~ ",mod,"_mean_100")), data = comp_dest) %>% summary
m_mean_id <- lm(as.formula(paste0("trase_mean_id ~ ",mod,"_mean_id")), data = comp_mun_long) %>% summary
return(list(base = m_base$r.squared, mean_q = m_mean_q$r.squared, sum_q = m_sum_q$r.squared, mean_100 = m_mean_100$r.squared, sum_100 = m_sum_100$r.squared, mean_id = m_mean_id$r.squared)) 
'
reg_dfs <- lapply(reg_dest, data.frame, stringsAsFactors = FALSE)
reg_dfs <- bind_rows(reg_dfs)
rownames(reg_dfs) <- names(comp_mun_long_dest)
#dfs <- t(dfs)

# simple pearson correlation
# by destination
#pearson <- lapply(comp_mun_long_dest, function(comp_dest){
#  lapply(models, function(mod){
#    c_base <- cor(comp_dest$trase, comp_dest[mod])
#    c_sum_q <- lm(as.formula(paste0("trase_sum_q ~ ",mod,"_sum_q")), data = comp_dest) %>% summary
#    c_mean_q <- lm(as.formula(paste0("trase_mean_q ~ ",mod,"_mean_q")), data = comp_dest) %>% summary
#    c_sum_100 <- lm(as.formula(paste0("trase_sum_100 ~ ",mod,"_sum_100")), data = comp_dest) %>% summary
#    c_mean_100 <- lm(as.formula(paste0("trase_mean_100 ~ ",mod,"_mean_100")), data = comp_dest) %>% summary
#    c_mean_id <- lm(as.formula(paste0("trase_mean_id ~ ",mod,"_mean_id")), data = comp_mun_long) %>% summary
#    return(list(base = m_base, mean_q = m_mean_q, sum_q = m_sum_q, mean_100 = m_mean_100, sum_100 = m_sum_100, mean_id = m_mean_id)) 
#  })
#})

comp_mun_long <- as.data.frame(comp_mun_long)
pearson_global <- lapply(w_nms, function(w){
  lapply(models, function(mod){
    if(w != "base"){w = paste0("_",w)} else {w = ""}
    #coeff <- cor(comp_dest[paste0("trase",w)], comp_dest[paste0(mod,w)])
    test <- cor.test(x = unlist(comp_mun_long[,paste0("trase",w)]), y = comp_mun_long[,paste0(mod,w)], method = "pearson", conf.level = 0.95)
    result <- paste0(round(test$estimate,4), stars.pval(test$p.value)) # ,"(", round(test$conf.int[1],4),",", round(test$conf.int[2],4), ")"
    return(result) 
  })
})
pearson_global_df <- lapply(pearson_global, data.frame, stringsAsFactors = FALSE)
pearson_global_df <- bind_rows(pearson_global_df)
rownames(pearson_global_df) <- names(w_nms)


pearson_dest <- lapply(comp_mun_long_dest, function(comp_dest){
  #comp_dest <- as.data.frame(comp_dest)
  lapply(w_nms, function(w){
    lapply(models, function(mod){
      if(w != "") w = paste0("_",w)
      #coeff <- cor(comp_dest[paste0("trase",w)], comp_dest[paste0(mod,w)])
      test <- cor.test(x = unlist(comp_dest[paste0("trase",w)]), y = unlist(comp_dest[paste0(mod,w)]), method = "pearson", conf.level = 0.95)
      result <- paste0(round(test$estimate,4), stars.pval(test$p.value)) # ,"(", round(test$conf.int[1],4),",", round(test$conf.int[2],4), ")"
      return(result) 
    })
  })
})

pearson_df <- lapply(pearson_dest, data.frame, stringsAsFactors = FALSE)
pearson_df <- bind_rows(pearson_df)
#rownames(pearson_df) <- names(comp_mun_long_dest)
pearson_df <- mutate(pearson_df, to_code = names(comp_mun_long_dest), .before = multimode)

# add column for total exports in trase and my data
pearson_df <- comp_nat %>% dplyr::select(c(to_code:to_name, trase, multimode)) %>% 
  rename(exports_trase = trase, exports_model = multimode) %>%
  left_join(pearson_df, by = "to_code") %>%
  arrange(desc(exports_trase))
# transform to kilotons
pearson_df <- mutate(pearson_df, exports_trase = round(exports_trase/1000,3), exports_model = round(exports_model/1000,4))
# replace NA by -
pearson_df[is.na(pearson_df)] <- "-"

# add column for share of raw soy in exports
(equi_fact <- (CBS_SOY["bean", "processing"])/(CBS_SOY["cake", "production"] + CBS_SOY["oil", "production"]))
EXP_NAT_wide_eq <- EXP_NAT_wide %>%  mutate(oil = oil*equi_fact, cake = cake * equi_fact) %>%
  mutate(bean_share = bean/(bean+oil+cake))

pearson_df <- dplyr::left_join(pearson_df, EXP_NAT_wide_eq %>% dplyr::select(c(to_name, bean_share)), by = c("to_code" = "to_name"))
pearson_df <- pearson_df %>% relocate(bean_share, .after = exports_model)

# save to tex
# extract only columns we need
pearson_df_fin <- select(pearson_df, -c(to_name, starts_with("sum_"), ends_with("multimode"), contains("_id.")))
print(xtable(pearson_df_fin, caption = "Corrleation between different model approaches and TRASE by destination country",digits = 1), 
      file = "results/tables/pearson_dest.tex",
      include.rownames=FALSE)


# other statistics
comp_mun_dest_stats <- comp_mun_long %>% group_by(to_code) %>% 
  summarise(across(downscale:multimode, .fns = list(diff = ~ sum(abs(.-trase)), msle = ~ msle(trase,.), rmse = ~rmse(trase,.)), #mape = ~ mape(trase,.)
                   .names = "{.fn}_{.col}")) 
  