
####### Feed use estimation, using estimated animal numbers by livestock system and FAO feed ratios ############

library(dplyr)
library(openxlsx)

# should results be written to file ?
write = TRUE

# load data ---------
SOY_MUN <- readRDS("intermediate_data/SOY_MUN_02.rds")
GEO_MUN_SOY <- readRDS("intermediate_data/GEO_MUN_SOY_02.rds")
feed_ratios <- read.xlsx("input_data/Feed_ratios_FAO.xlsx", sheet = 2)
CBS_SOY <- readRDS("intermediate_data/CBS_SOY.rds")


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


### add total soybean and cake feed use per MU to the main table
SOY_MUN <- mutate(SOY_MUN, feed_bean = rowSums(bean_feed_t), feed_cake = rowSums(cake_feed_t))

### compare with FOA national feed use aggregates and rescale to match these values
sum(SOY_MUN$feed_bean)
CBS_SOY["bean", "feed"]

sum(SOY_MUN$feed_cake)
CBS_SOY["cake", "feed"]

# re-scale to fit FOA data
SOY_MUN$feed_bean <- SOY_MUN$feed_bean * CBS_SOY["bean", "feed"]/sum(SOY_MUN$feed_bean)
SOY_MUN$feed_cake <- SOY_MUN$feed_cake * CBS_SOY["cake", "feed"]/sum(SOY_MUN$feed_cake)

all.equal(sum(SOY_MUN$feed_bean),CBS_SOY["bean", "feed"])
all.equal(sum(SOY_MUN$feed_cake),CBS_SOY["cake", "feed"])

# remove animal numbers from main sheet (no longer needed)
#SOY_MUN <- select(SOY_MUN, -c(cattle:quail))

# add new columns to GEO dataset
GEO_MUN_SOY <- left_join(GEO_MUN_SOY, SOY_MUN[,c("co_mun","feed_bean", "feed_cake")], by="co_mun")

# export data -----------------------
if (write){
  saveRDS(SOY_MUN, file = "intermediate_data/SOY_MUN_03.rds")
  saveRDS(GEO_MUN_SOY, file = "intermediate_data/GEO_MUN_SOY_03.rds")
}  
