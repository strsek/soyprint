##### Nesting of subnational data into FABIO #########

library(dplyr)
library(Matrix)

# read FABIO data
Z_list <- readRDS("input_data/FABIO/FABIO_exp/Z_mass.rds")
#Z_v <- readRDS("input_data/FABIO/FABIO_exp/Z_value.rds")
Y_list <- readRDS("input_data/FABIO/FABIO_exp/Y.rds")
X_list <- readRDS("input_data/FABIO/FABIO_exp/X.rds")
L <- readRDS("input_data/FABIO/FABIO_exp/2013_L_mass.rds")
regions <- read.csv("input_data/FABIO/FABIO_exp/regions.csv")
items <- read.csv("input_data/FABIO/FABIO_exp/items.csv")

# MU data
SOY_MUN <- readRDS("intermediate_data/SOY_MUN_fin.rds")

# feed use data
bean_feed_t <- readRDS("intermediate_data/bean_feed_t.rds")
cake_feed_t <- readRDS("intermediate_data/cake_feed_t.rds")

# CBS
CBS_SOY <- readRDS("intermediate_data/CBS_SOY.rds")

Z <- Z_list$`2013`
#Zv_2013 <- Z_v$`2013`
X <- X_list[,"2013"]
Y <- Y_list$`2013`

rm(Z_list,Y_list,X_list)


# domestic use structure -------------------------------------------------

# extract Brazilian domestic block from Z
bra_ind <- seq((which(regions$name == "Brazil")-1)*nrow(items)+1,(which(regions$name == "Brazil"))*nrow(items)) #which(grepl("Soyabean", items$item))
Z_bra <- as.matrix(Z[bra_ind,bra_ind]) 
dimnames(Z_bra) <- list(items$item, items$item)
# soy rows
Z_bra_soy <- Z_bra[grepl("Soya", rownames(Z_bra)),]

# transform to df
soy_use <- as.data.frame(t(Z_bra_soy))
soy_use <- cbind(items[,c(2:3, 5,7)], soy_use)

# compare with CBS
CBS_SOY <- readRDS("intermediate_data/CBS_SOY.rds")
CBS_FABIO <- CBS_SOY; CBS_FABIO[CBS_FABIO!=0] <- 0
CBS_FABIO["bean", "seed"] <- soy_use["Soyabeans", "Soyabeans"]
CBS_FABIO["bean", "processing"] <- soy_use["Soyabean Oil", "Soyabeans"] + soy_use["Soyabean Cake", "Soyabeans"]
CBS_FABIO["bean", "feed"] <- sum(soy_use[grepl("Livestock", soy_use$group),"Soyabeans"])
CBS_FABIO["cake", "feed"] <- sum(soy_use[grepl("Livestock", soy_use$group),"Soyabean Cake"])


# add own estimates to FABIO soy use
soy_use <- mutate(soy_use, "Soyabeans MUN" = 0, "Soyabean Oil MUN" = 0, "Soyabean Cake MUN" = 0)

(cake_conv <- CBS_SOY["cake", "production"]/CBS_SOY["bean", "processing"])
(oil_conv <- CBS_SOY["oil", "production"]/CBS_SOY["bean", "processing"])
(proc_loss <- 1-cake_conv-oil_conv)
(equi_fact <- 1/(cake_conv+oil_conv))

soy_use["Soyabeans", "Soyabeans MUN"] <- sum(SOY_MUN$seed_bean)
soy_use["Soyabean Oil", "Soyabeans MUN"]  <- sum(SOY_MUN$proc_bean)*oil_conv*equi_fact
soy_use["Soyabean Cake", "Soyabeans MUN"] <- sum(SOY_MUN$proc_bean)*cake_conv*equi_fact

# feed: groups for allocation to FABIO
cake_feed_tot <- colSums(cake_feed_t)
bean_feed_tot <- colSums(bean_feed_t)
#groups <- sub("_.*", "", names(cake_feed_tot))
#groups[grepl("dair", names(cake_feed_tot))] <- "dairy"
groups <- c("Cattle", "Dairy", "Cattle", "Dairy", "Cattle", 
            "Buffaloes", "Dairy", "Buffaloes", "Dairy", 
            "chicken_byd", "Eggs", "Poultry Birds", 
            "Pigs", "Pigs", "Pigs")


# aggregate
cake_feed_group <- as.data.frame(t(rowsum(cake_feed_tot, groups)))
bean_feed_group <- as.data.frame(t(rowsum(bean_feed_tot, groups)))
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

soy_use_BRA <- soy_use %>% select(-c(item_code, group, comm_group)) %>% rename(using_sector = item)

# extract final demand block of Brazil
bra_ind_Y <- startsWith(Y@Dimnames[[2]], "21_")
Y_bra <- as.matrix(Y[bra_ind, bra_ind_Y])
rownames(Y_bra) <- items$item
Y_bra_soy <- Y_bra[grepl("Soya", rownames(Y_bra)),]


# adapt FABIO values to our estimates -------------------------------
Z_bra_corr <- Z_bra
Y_bra_corr <- Y_bra

Z_bra_corr[grepl("Soya", rownames(Z_bra_corr)),] <- 0
Y_bra_corr[grepl("Soya", rownames(Y_bra_corr)),] <- 0

# seed use
Z_bra_corr["Soyabeans", "Soyabeans"] <- sum(SOY_MUN$seed_bean)

# processing
Z_bra_corr["Soyabeans", "Soyabean Oil"]  <- sum(SOY_MUN$proc_bean)*oil_conv*equi_fact
Z_bra_corr["Soyabeans", "Soyabean Cake"] <- sum(SOY_MUN$proc_bean)*cake_conv*equi_fact

# feed use
Z_bra_corr["Soyabeans", names(bean_feed_group)] <- unlist(bean_feed_group)
Z_bra_corr["Soyabean Cake", names(cake_feed_group)] <- unlist(cake_feed_group)
# set poultry-egg soy trade to 0
Z_bra_corr["Poultry Birds", "Eggs"] <- 0
# TODO: validate with MB that all other species use no soy

# other use (Y)
Y_bra_corr["Soyabean Oil", "21_other"] <- sum(SOY_MUN$other_oil)

# food use (Y)
Y_bra_corr["Soyabeans", "21_food"] <- sum(SOY_MUN$food_bean)
Y_bra_corr["Soyabean Oil", "21_food"] <- sum(SOY_MUN$food_oil)

# stock addition (Y)
Y_bra_corr["Soyabeans", "21_stock_addition"] <- sum(SOY_MUN$stock_bean)
Y_bra_corr["Soyabean Oil", "21_stock_addition"] <- sum(SOY_MUN$stock_oil)
Y_bra_corr["Soyabean Cake", "21_stock_addition"] <- sum(SOY_MUN$stock_cake)
# TODO: check if artificial cake and oil stock change should rather be added to balancing item

# check sums
all.equal(sum(Z_bra_corr["Soyabeans",]), sum(select(SOY_MUN, c(seed_bean, proc_bean, feed_bean))))
all.equal(sum(Z_bra_corr["Soyabean Cake",]), sum(SOY_MUN$feed_cake))

all.equal(sum(Z_bra_corr["Soyabeans",]) + sum(Y_bra_corr["Soyabeans",]), sum(SOY_MUN$prod_bean) + sum(SOY_MUN$imp_bean) - sum(SOY_MUN$exp_bean))
all.equal(sum(Z_bra_corr["Soyabean Cake",]) + sum(Y_bra_corr["Soyabean Cake",]), sum(SOY_MUN$prod_cake) + sum(SOY_MUN$imp_cake) - sum(SOY_MUN$exp_cake))
all.equal(sum(Z_bra_corr["Soyabean Oil",]) + sum(Y_bra_corr["Soyabean Oil",]), sum(SOY_MUN$prod_oil) + sum(SOY_MUN$imp_oil) - sum(SOY_MUN$exp_oil))

# add results back to full Z matrix
Z_corr <- Z
Z_corr[bra_ind,bra_ind] <- Z_bra_corr
Y_corr <- Y
Y_corr[bra_ind,bra_ind_Y] <- Y_bra_corr


# export & import -------------------------------------------------------

# TODO with final FABIO_exp


# save updated IO data

if(write){
  saveRDS(Z_corr, file ="intermediate_data/Z_corr.rds") 
  saveRDS(Y_corr, file ="intermediate_data/Y_corr.rds")
  }