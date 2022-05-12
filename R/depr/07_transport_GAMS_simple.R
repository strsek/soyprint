
###### data processing for GAMS optimization model with gdxrrw ########

## NOTE: To run this script and the GAMS script that is called by it, you need
# - an in installation of GAMS with a PRO LICENSE
# - the GDXRRW package, which provides the interface functionality of R with GAMS

# the script prepares data for GAMS, calls the GAMS model and loads optimization results back in

library(openxlsx)
library(dplyr)
library(reshape2)
library(gdxrrw) # if not installed, tun install_github("GAMS-dev/gdxrrw/gdxrrw")
library(gdxdt)
library(tidyr)
library(tibble)

## set GAMS directory according to location where GAMS is installed on your PC ##
# see https://github.com/GAMS-dev/gdxrrw for further info
#igdx("C:/GAMS/34") 
igdx("~/gams37.1_linux_x64_64_sfx")

write = TRUE

# load data
SOY_MUN <- readRDS("intermediate_data/SOY_MUN_fin.rds")
load("intermediate_data/cost_matrices.Rdata")
load("intermediate_data/stations.Rdata")
load("intermediate_data/ports.Rdata")
load("intermediate_data/cargo_long.Rdata")


# prepare data for GAMS --------------------------------------------------------------

# change MU code to character for GAMS
SOY_MUN$co_mun <- as.character(SOY_MUN$co_mun)

# total supply and demand per MU for each product
supply <- dplyr::select(SOY_MUN, c(co_mun, total_supply_bean:total_supply_cake)) %>% 
  rename("a" = "co_mun", 
         "bean" = "total_supply_bean", 
         "oil" = "total_supply_oil", 
         "cake" = "total_supply_cake") 
demand <- dplyr::select(SOY_MUN, c(co_mun, total_use_bean:total_use_cake)) %>% 
  rename("a" = "co_mun", 
         "bean" = "total_use_bean", 
         "oil" = "total_use_oil", 
         "cake" = "total_use_cake")

# excess supply and demand
excess_supply <- dplyr::select(SOY_MUN, c(co_mun, excess_supply_bean:excess_supply_cake)) %>% 
  rename("a" = "co_mun", 
         "bean" = "excess_supply_bean", 
         "oil" = "excess_supply_oil", 
         "cake" = "excess_supply_cake")
excess_demand <- dplyr::select(SOY_MUN, c(co_mun, excess_use_bean:excess_use_cake)) %>% 
  rename("a" = "co_mun", 
         "bean" = "excess_use_bean", 
         "oil" = "excess_use_oil", 
         "cake" = "excess_use_cake")

# restricted tables only containing non-zero values of excess supply and demand
# not necesary, as GAMS can do this via domain restriction
# excess_supply_pos <- excess_supply[(excess_supply$bean + excess_supply$oil + excess_supply$cake)>0,]
# excess_demand_pos <- excess_demand[(excess_demand$bean + excess_demand$oil + excess_demand$cake)>0,]

# export & processing demand
export_processing <- mutate(SOY_MUN, exp_proc_bean = exp_bean + proc_bean) %>% 
  dplyr::select(c(co_mun, c(exp_proc_bean, exp_oil, exp_cake))) %>% 
  rename("a" = "co_mun", 
         "bean" = "exp_proc_bean", 
         "oil" = "exp_oil", 
         "cake" = "exp_cake")

# products
products <- c("bean","oil", "cake")


# put data in gdx-conformable format -------------------

## sets: products, MUs, train stations, ports
product_lst <- list(name='product',  type = 'set', uels=list(products), ts='products')#, te = matrix(ncol = 1, SOY_MUN$nm_mun))
a_lst <-   list(name='a',  type = 'set', uels=list(SOY_MUN$co_mun),ts='municipalities')#, te = matrix(ncol = 1, SOY_MUN$nm_mun))
w1_list <- list(name='w1', type = 'set', uels=list(ports_orig$cdi_tuaria),ts='origin ports')
w2_list <- list(name='w2', type = 'set', uels=list(ports_dest$cdi_tuaria),ts='destination ports')
r1_list <- list(name='r1', type = 'set', uels=list(stations_orig$CodigoTres),ts='origin stations')#, te =  matrix(ncol = 1, stations_orig$name))
r2_list <- list(name='r2', type = 'set', uels=list(stations_dest$CodigoTres),ts='destination stations')#, te =  matrix(ncol = 1, stations_dest$name))

## parameters: supply, demand, export+processing demand, transport costs, route capacities
# Note that excess supply and demand is used
supply <- list(name='supply',val=as.matrix(excess_supply[,2:4]), uels=list(excess_supply$a, products),
               dim=2, domains = c("a", "product"), form='full',type='parameter',ts='demand quantities')

demand  <- list(name='demand',val=as.matrix(excess_demand[,2:4]), uels=list(excess_demand$a, products),
                dim=2, domains = c("a", "product"), form='full',type='parameter',ts='demand quantities')

exp_proc <- list(name='exp_proc',val=as.matrix(export_processing[,2:4]), uels=list(export_processing$a, products),
                 dim=2,domains = c("a", "product"), form='full',type='parameter',ts='export and processing demand')

# costs matrices
C_a_b <- list(name='C_a_b',val=road_cost_MUN, uels=dimnames(road_cost_MUN),
              dim=2, domains = c("a", "b"), form='full',type='parameter',
              ts='road transportation costs per tkm between all MUs')

C_a_w1 <- list(name='C_a_w1',val=road_cost_MUN_port, uels=dimnames(road_cost_MUN_port),
               dim=2, domains = c("a", "w1"), form='full',type='parameter',
               ts='road transportation costs per tkm between MUs and origin ports')

C_a_r1  <- list(name='C_a_r1',val=road_cost_MUN_stat, uels=dimnames(road_cost_MUN_stat),
               dim=2, domains = c("a", "r1"), form='full',type='parameter',
               ts='road transportation costs per tkm between MUs and origin stations')

C_w2_b <- list(name='C_w2_b',val=road_cost_port_MUN, uels=dimnames(road_cost_port_MUN),
               dim=2, domains = c("w2", "b"), form='full',type='parameter',
               ts='road transportation costs per tkm between MUs and origin ports')

C_r2_b <- list(name='C_r2_b',val=road_cost_stat_MUN, uels=dimnames(road_cost_stat_MUN),
               dim=2, domains = c("r2", "b"), form='full',type='parameter',
               ts='road transportation costs per tkm between MUs and origin ports')

C_r1_r2 <- list(name='C_r1_r2',val=rail_cost, uels=dimnames(rail_cost),
               dim=2, domains = c("r1", "r2"), form='full',type='parameter',
               ts='rail transportation costs per tkm between rail terminals')

C_w1_w2 <- list(name='C_w1_w2',val=water_cost, uels=dimnames(water_cost),
               dim=2, domains = c("w1", "w2"), form='full',type='parameter',
               ts='water transportation costs per tkm between ports')

# capacity constraints for rail and water
cap_r <- cargo_rail_long %>% 
  rename (r1 = orig, r2 = dest, value = volume) %>% 
  mutate(across(r1:product,as.factor))
attr(cap_r,'symName') <- 'cap_r';
attr(cap_r,'ts') <- 'transportation capacities between all stations';
attr(cap_r,'domains') <- c("r1", "r2", "product")

cap_w <- cargo_water_long %>% 
  rename (w1 = orig, w2 = dest, value = volume) %>% 
  mutate(across(w1:product,as.factor))
attr(cap_w,'symName') <- 'cap_w';
attr(cap_w,'ts') <- 'transportation capacities between all ports';
attr(cap_w,'domains') <- c("w1", "w2", "product")


# write to gdx file ------------------------------
if (write){
  wgdx.lst("GAMS/GAMS_data.gdx", 
           list(product_lst, a_lst, 
                w1_list, w2_list, r1_list, r2_list, 
                supply, demand, exp_proc, 
                cap_w, cap_r, 
                C_a_b, C_a_w1, C_a_r1, C_w2_b, C_r2_b, C_r1_r2, C_w1_w2))
}



# run GAMS model -----------------------------------------------------------------------

# define file path of .gms file in correct format
gms <- paste0(getwd(),"/GAMS/transport_model_Brazil_intermod.gms")
isWindows <- ("mingw32" == R.Version()$os)
if (isWindows) gms <- gsub("/","\\",gms,fixed=TRUE)
# input file
ingdx <- paste0(getwd(),"/GAMS/GAMS_data.gdx")
if (isWindows) ingdx <- gsub("/","\\",ingdx,fixed=TRUE)
# output file
outgdx <- paste0(getwd(),"/GAMS/GAMS_sol.gdx")
if (isWindows) outgdx <- gsub("/","\\",outgdx,fixed=TRUE)

# RUN model
gams(paste0(gms, " --INPUT=", ingdx, " --OUTPUT=", outgdx))



# after GAMS optimization: read and process results ---------------------------------------------

#sol <- rgdx("GAMS/GAMS_sol.gdx", requestList = list(name = "xtotalcost") )
X_a_b <-   readgdx("GAMS/GAMS_sol.gdx", "X_a_b")
X_a_r1 <-  readgdx("GAMS/GAMS_sol.gdx", "X_a_r1")
X_a_w1 <-  readgdx("GAMS/GAMS_sol.gdx", "X_a_w1")
X_r1_r2 <- readgdx("GAMS/GAMS_sol.gdx", "X_r1_r2")
X_w1_w2 <- readgdx("GAMS/GAMS_sol.gdx", "X_w1_w2")
X_r2_b <-  readgdx("GAMS/GAMS_sol.gdx", "X_r2_b")
X_w2_b <-  readgdx("GAMS/GAMS_sol.gdx", "X_w2_b")
totalcost <- rgdx("GAMS/GAMS_sol.gdx", requestList = list(name = "xtotalcost"))$val

## compile flows from source MU to target MU
 
# get flows into wide format matrices per product
X_a_b_wide <- sapply(products, function(x){
  filter(X_a_b, product == x) %>% dplyr::select(!product) %>%
    pivot_wider(names_from = b, values_from = value)  %>% 
    arrange(a) %>% column_to_rownames("a") %>% 
    dplyr::select(as.character(sort(as.numeric(names(.))))) %>%
    replace(is.na(.), 0) %>% as("matrix")
}, USE.NAMES = TRUE, simplify = FALSE)

X_a_r1_wide <- sapply(products, function(x){
  filter(X_a_r1, product == x) %>% dplyr::select(!product) %>%
    pivot_wider(names_from = r1, values_from = value)  %>% 
    arrange(a) %>% column_to_rownames("a") %>% 
    dplyr::select(sort(names(.))) %>%
    replace(is.na(.), 0) %>% as("matrix")
}, USE.NAMES = TRUE, simplify = FALSE)

X_r1_r2_wide <- sapply(products, function(x){
  filter(X_r1_r2, product == x) %>% dplyr::select(!product) %>% 
    pivot_wider(names_from = r2, values_from = value) %>% 
    arrange(r1) %>% column_to_rownames("r1") %>% 
    dplyr::select(sort(names(.))) %>%
    replace(is.na(.), 0) %>% as("matrix")
}, USE.NAMES = TRUE, simplify = FALSE)

X_r2_b_wide <- sapply(products, function(x){
  filter(X_r2_b, product == x) %>% dplyr::select(!product) %>% 
    pivot_wider(names_from = b, values_from = value) %>% 
    arrange(r2) %>% column_to_rownames("r2") %>% 
    dplyr::select(sort(names(.))) %>%
    replace(is.na(.), 0) %>% as("matrix")
}, USE.NAMES = TRUE, simplify = FALSE)


X_a_w1_wide <- sapply(products, function(x){
  filter(X_a_w1, product == x) %>% dplyr::select(!product) %>% 
    pivot_wider(names_from = w1, values_from = value) %>% 
    arrange(a) %>% column_to_rownames("a") %>% 
    dplyr::select(sort(names(.))) %>%
    replace(is.na(.), 0) %>% as("matrix")
}, USE.NAMES = TRUE, simplify = FALSE)

X_w1_w2_wide <- sapply(products, function(x){
  filter(X_w1_w2, product == x) %>% dplyr::select(!product) %>% 
    pivot_wider(names_from = w2, values_from = value) %>% 
    arrange(w1) %>% column_to_rownames("w1") %>% 
    dplyr::select(sort(names(.))) %>%
    replace(is.na(.), 0) %>% as("matrix")
}, USE.NAMES = TRUE, simplify = FALSE)

X_w2_b_wide <- sapply(products, function(x){
  filter(X_w2_b, product == x) %>% dplyr::select(!product) %>% 
    pivot_wider(names_from = b, values_from = value) %>% 
    arrange(w2) %>% column_to_rownames("w2") %>% 
    dplyr::select(sort(names(.))) %>%
    replace(is.na(.), 0) %>% as("matrix")
}, USE.NAMES = TRUE, simplify = FALSE)

# obtain rail and water flows from origin to destination MU
X_a_b_r_wide <- Map(function(x,y,z){ 
  t(t(x)/colSums(x)) %*% t(t(y)/colSums(y)) %*% z }, 
  X_a_r1_wide, X_r1_r2_wide, X_r2_b_wide)
X_a_b_w_wide <- Map(function(x,y,z){ 
  t(t(x)/colSums(x)) %*% t(t(y)/colSums(y)) %*% z }, 
  X_a_w1_wide, X_w1_w2_wide, X_w2_b_wide)

# back to long
X_a_b_r <- do.call(rbind, lapply(products, function(x){
  as.data.frame.table(X_a_b_r_wide[[x]], stringsAsFactors = FALSE) %>% 
    rename(co_orig = Var1, co_dest = Var2, value_rail = Freq) %>% 
    mutate(product = x, .after = co_dest)})) %>% 
  filter(value_rail > 0) %>% mutate(across(co_orig:co_dest, as.numeric))

X_a_b_w <- do.call(rbind, lapply(products, function(x){
  as.data.frame.table(X_a_b_w_wide[[x]], stringsAsFactors = FALSE) %>% 
    rename(co_orig = Var1, co_dest = Var2, value_water = Freq) %>% 
    mutate(product = x, .after = co_dest)})) %>% 
  filter(value_water > 0) %>% mutate(across(co_orig:co_dest, as.numeric))

# merge flows from all modes
X_a_b_all <- rename(X_a_b, co_orig = a, co_dest = b, value_road = value) %>% 
  mutate(co_orig = as.numeric(co_orig), co_dest = as.numeric(co_dest)) %>%
  full_join(X_a_b_r, by = c("co_orig", "co_dest", "product")) %>% 
  full_join(X_a_b_w, by = c("co_orig", "co_dest", "product")) %>%
  replace(is.na(.), 0) %>%
  mutate(value_total = value_road + value_rail + value_water)

X_a_b_tot <- dplyr::select(X_a_b_all, -(value_road:value_water)) %>% 
  rename(value = value_total)

## wide-format aggregation
## # add to road flows to obtain total MU-MU flows
## X_a_b_tot_wide <- Map(function(x,y,z){
##   row <- sort(unique(c(rownames(x), rownames(y), rownames(z))))
##   col <- sort(unique(c(colnames(x), colnames(y), colnames(z))))
##   a <- matrix(0, length(row), length(col), dimnames = list(row, col) )
##   a[rownames(x), colnames(x)] <- a[rownames(x), colnames(x)]+x
##   a[rownames(y), colnames(y)] <- a[rownames(y), colnames(y)]+y
##   a[rownames(z), colnames(z)] <- a[rownames(z), colnames(z)]+z
##   return(a)}, 
## X_a_b_wide, X_a_b_r_wide, X_a_b_w_wide)
## 
## # back into long format
## X_a_b_tot <- do.call(rbind, lapply(products, function(x){
##   as.data.frame.table(X_a_b_tot_wide[[x]], stringsAsFactors = FALSE) %>% 
##   rename(co_orig = Var1, co_dest = Var2, value = Freq) %>% mutate(product = x, .after = co_dest)
##   })) %>% filter(value > 0) %>% mutate(across(co_orig:co_dest, as.numeric))


## # old model with overall modal split constraint and hypothetic transport cost for rail and water
## transport_sol <- rgdx("GAMS/transport_sol.gdx", requestList = list(name = "xsoytransport") )
## flows <- as.data.frame(transport_sol$val)
## colnames(flows) <- c(paste0(transport_sol$domains,"_num"), "value")
## uels <- transport_sol$uels[[1]]
## flows$co_orig <- as.numeric(uels[flows$a_num])
## flows$co_dest <- as.numeric(uels[flows$b_num])
## flows$mode <- uels[flows$mode_num]
## flows$product <- uels[flows$product_num]
## flows <- flows[,c(6:9,5)]
## 
## # simple version without modal split
## transport_sol_simple <- rgdx("GAMS/transport_sol_simple.gdx", requestList = list(name = "xsoytransport") )
## flows_simple <- as.data.frame(transport_sol_simple$val)
## colnames(flows_simple) <- c(paste0(transport_sol_simple$domains,"_num"), "value")
## uels <- transport_sol_simple$uels[[1]]
## flows_simple$co_orig <- as.numeric(uels[flows_simple$a_num])
## flows_simple$co_dest <- as.numeric(uels[flows_simple$b_num])
## flows_simple$product <- uels[flows_simple$product_num]
## flows_simple <- flows_simple[,c(5:7,4)]
## #total cost
## transport_sol_simple_cost <- rgdx("GAMS/transport_sol_simple.gdx", requestList = list(name = "xtotalcost") )
## transport_sol_simple_cost <- transport_sol_simple_cost$val

# compare solution from simple model in GAMS and R
# all.equal(arrange(flows_simple, by=value), arrange(flows_R, by = value))
# flows_comp <- full_join(flows_R, flows_simple, by = c("co_orig", "co_dest", "product"))
# flows_comp <- mutate(flows_comp, diff = value.x - value.y)
# flows_comp[is.na(flows_comp)] <- 0
# colSums(select(flows_comp, value.x, value.y))

# export
if (write) {
  saveRDS(X_a_b_tot, file = "intermediate_data/X_a_b_tot.rds")
  #saveRDS(flows, file = "intermediate_data/flows_GAMS.rds")
  #saveRDS(flows_simple, file = "intermediate_data/flows_GAMS_simple.rds")
}

# clear environment
rm(list = ls())
gc()
