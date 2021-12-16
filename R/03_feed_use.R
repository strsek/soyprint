
####### Feed use estimation, using estimated animal numbers by livestock system and FAO feed ratios ############

library(dplyr)
library(openxlsx)

# should results be written to file ?
write = TRUE

# load data ---------
SOY_MUN <- readRDS("intermediate_data/SOY_MUN_02.rds")
GEO_MUN_SOY <- readRDS("intermediate_data/GEO_MUN_SOY_02.rds")
feed_ratios <- read.xlsx("input_data/Feed_ratios_FAO.xlsx", sheet = 2)
CBS_SOY <- read.csv2("intermediate_data/CBS_SOY.csv")#readRDS("intermediate_data/CBS_SOY.rds")


# prepare feed ratios --------------

# compute average soy feed use per animal and year in tons, using dry matter content of 88% for both bean and cake (EMBRAPA) -------------
dm_content <- c("bean" = 0.88, "cake" = 0.88)
# dry matter intake per animal and year
feed_ratios <- mutate(feed_ratios, bean_dm = DM*bean/100, cake_dm = DM*cake/100)
# total (wet matter) intake per animal and year in kg and tons
feed_ratios <- mutate(feed_ratios, bean_kg = bean_dm / dm_content["bean"], cake_kg = cake_dm / dm_content["cake"])
feed_ratios <- mutate(feed_ratios, bean_t = bean_kg/1000, cake_t = cake_kg/1000)


# compute feed use for each MU by specie ------------------------

## soybeans
#soybean_feed_t <- sapply(feed_ratios$system_name, FUN = function(x){feed_ratios$soybean_t[feed_ratios$system_name == x]*SOY_MUN[,x]})
bean_feed_t <- t(t(SOY_MUN[,feed_ratios$system_name]) * feed_ratios$bean_t)

## soy cake
#soy_cake_feed_t <- sapply(feed_ratios$system_name, FUN = function(x){feed_ratios$soy_cake_t[feed_ratios$system_name == x]*SOY_MUN[,x]})
cake_feed_t <- t(t(SOY_MUN[,feed_ratios$system_name]) * feed_ratios$cake_t)

### compare with FAO national feed use aggregates and rescale to match these values
sum(SOY_MUN$feed_bean)
CBS_SOY["bean", "feed"]

sum(SOY_MUN$feed_cake)
CBS_SOY["cake", "feed"]

bean_feed_t_fin <- bean_feed_t*(CBS_SOY["bean", "feed"]/sum(bean_feed_t))
cake_feed_t_fin <- cake_feed_t*(CBS_SOY["cake", "feed"]/sum(cake_feed_t))

## aggregate feed use to groups corresponding to FABIO livestock sectors
groups <- c("Cattle", "Dairy", "Cattle", "Dairy", "Cattle", 
            "Buffaloes", "Dairy", "Buffaloes", "Dairy", 
            "chicken_byd", "Eggs", "Poultry Birds", 
            "Pigs", "Pigs", "Pigs")


bean_feed_group <- as.data.frame(t(rowsum(t(bean_feed_t_fin), groups)))
cake_feed_group <- as.data.frame(t(rowsum(t(cake_feed_t_fin), groups)))

# allocate backyard chicken feed to meat and eggs according to national numbers of layers vs. broilers
# TODO: check with MB
layershare <- sum(SOY_MUN$chicken_lay)/(sum(SOY_MUN$chicken_lay)+sum(SOY_MUN$chicken_bro))
bean_feed_group <- mutate(bean_feed_group, Eggs = Eggs + layershare*chicken_byd, `Poultry Birds`  = `Poultry Birds` + (1-layershare)*chicken_byd) %>% select(-chicken_byd)
cake_feed_group <- mutate(cake_feed_group, Eggs = Eggs + layershare*chicken_byd, `Poultry Birds`  = `Poultry Birds` + (1-layershare)*chicken_byd) %>% select(-chicken_byd)
# allocate dairy to milk and butter
buttershare <- Z_bra_soy["Soyabeans", "Butter, Ghee"]/sum(Z_bra_soy["Soyabeans", c("Milk - Excluding Butter", "Butter, Ghee")])
bean_feed_group <- mutate(bean_feed_group, `Milk - Excluding Butter` = (1-buttershare)*Dairy, `Butter, Ghee` = buttershare*Dairy) %>% select(-Dairy)
cake_feed_group <- mutate(cake_feed_group, `Milk - Excluding Butter` = (1-buttershare)*Dairy, `Butter, Ghee` = buttershare*Dairy) %>% select(-Dairy)

soy_use[names(bean_feed_group), "Soyabean Cake MUN"] <- unlist(cake_feed_group)
soy_use[names(cake_feed_group), "Soyabean Cake MUN"] <- unlist(cake_feed_group)
colnames(soy_use)[5:7] <- paste(colnames(soy_use)[5:7], "FABIO")
rownames(soy_use) <- NULL


### add total soybean and cake feed use per MU to the main table
SOY_MUN <- mutate(SOY_MUN, feed_bean = rowSums(bean_feed_t_fin), feed_cake = rowSums(cake_feed_t_fin))

# re-scale to fit FAO data
#SOY_MUN$feed_bean <- SOY_MUN$feed_bean * CBS_SOY["bean", "feed"]/sum(SOY_MUN$feed_bean)
#SOY_MUN$feed_cake <- SOY_MUN$feed_cake * CBS_SOY["cake", "feed"]/sum(SOY_MUN$feed_cake)

all.equal(sum(SOY_MUN$feed_bean),CBS_SOY["bean", "feed"])
all.equal(sum(SOY_MUN$feed_cake),CBS_SOY["cake", "feed"])

# remove animal numbers from main sheet (no longer needed)
#SOY_MUN <- select(SOY_MUN, -c(cattle:quail))

# add new columns to GEO dataset
GEO_MUN_SOY <- left_join(GEO_MUN_SOY, SOY_MUN[,c("co_mun","feed_bean", "feed_cake")], by="co_mun")

# add back MU codes to feed data
bean_feed_t_fin <- as.data.frame(bean_feed_t_fin) %>% `rownames<-`(SOY_MUN$co_mun) # mutate(co_mun = SOY_MUN$co_mun, .before = cattle_gra_meat)
cake_feed_t_fin <- as.data.frame(cake_feed_t_fin) %>% `rownames<-`(SOY_MUN$co_mun) #mutate(co_mun = SOY_MUN$co_mun, .before = cattle_gra_meat)

# export data -----------------------
if (write){
  saveRDS(bean_feed_t_fin, file = "intermediate_data/bean_feed_t.rds")
  saveRDS(cake_feed_t_fin, file = "intermediate_data/cake_feed_t.rds")
  saveRDS(SOY_MUN, file = "intermediate_data/SOY_MUN_03.rds")
  saveRDS(GEO_MUN_SOY, file = "intermediate_data/GEO_MUN_SOY_03.rds")
}  
