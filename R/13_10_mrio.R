
library("Matrix")


# MRIO Table ---

mr_sup_m <- readRDS("intermediate_data/FABIO/mr_sup_mass.rds")
mr_sup_v <- readRDS("intermediate_data/FABIO/mr_sup_value.rds")
mr_use <- readRDS("intermediate_data/FABIO/mr_use.rds")

# check for conformity
all.equal(rownames(mr_sup_m$`2013`), colnames(mr_use$`2013`))
all.equal(colnames(mr_sup_m$`2013`), rownames(mr_use$`2013`))

# Mass
trans_m <- lapply(mr_sup_m, function(x) {
  # TODO: ask why this is transformed to a normal matrix? --> not feasible for large matrix
  #out <- as.matrix(x / rowSums(x))
  # --> make use of sparse matrix indexing: see https://stackoverflow.com/questions/39284774/column-rescaling-for-a-very-large-sparse-matrix-in-r
  # option 1:
  #out_t <- t(x)
  #out_t@x <- out_t@x / rep.int(colSums(out_t), diff(out_t@p))
  #out <- t(out_t)
  # option 2:
  out <- x
  out@x <- out@x / rowSums(out)[(out@i+1)]
   
  #out[!is.finite(out)] <- 0 # See Issue #75
  #return(as(out, "Matrix"))
  if(sum(is.na(out))) stop("NAs contained")
  return(out)
})

# TODO: mr_use and trans_m (derived from mr_sup) are not conformable: inner dimensions need to fit --> done
Z_m <- mapply(function(x, y) {
  x %*% y
}, x = mr_use, y = trans_m)

Z_m <- lapply(Z_m, round)


# Value
trans_v <- lapply(mr_sup_v, function(x) {
  #out <- as.matrix(x / rowSums(x))
  #out[!is.finite(out)] <- 0 # See Issue #75
  
  out <- x
  out@x <- out@x / rowSums(out)[(out@i+1)]
  
  #return(as(out, "Matrix"))
  if(sum(is.na(out))) stop("NAs contained")
  return(out)
})

Z_v <- mapply(function(x, y) {
  x %*% y
}, x = mr_use, y = trans_v)

Z_v <- lapply(Z_v, round)



# Rebalance row sums in Z and Y -----------------------------------------

library(data.table)
regions <- fread("input_data/FABIO/inst/regions_full.csv")
regions <- regions[cbs==TRUE]
items <- fread("input_data/FABIO/inst/items_full.csv")
nrcom <- nrow(items)
Y <- readRDS("intermediate_data/FABIO/Y.rds")

# Rebalance row sums for each year
for(i in seq_along(Z_m)){

  X <- rowSums(Z_m[[i]]) + rowSums(Y[[i]])

  # TODO: CHANGED --> verify
  #for(j in which(X < 0)){
  for(j in names(which(X < 0))){
    #reg <- j %/% nrcom + 1 
    reg <- sub("_.*", "",j)
    #Y[[i]][j, paste0(regions[reg, code], "_balancing")] <-
    #  Y[[i]][j, paste0(regions[reg, code], "_balancing")] - X[j]
    Y[[i]][j, paste0(reg, "_balancing")] <-
      Y[[i]][j, paste0(reg, "_balancing")] - X[j]
  }

}

# TODO: why is balancing item adjusted with Z_m and not Z_v?

# Derive total output X ---------------------------------------------

X <- mapply(function(x, y) {
  rowSums(x) + rowSums(y)
}, x = Z_m, y = Y)



# Store X, Y, Z variables
saveRDS(Z_m, "intermediate_data/FABIO/Z_mass.rds")
saveRDS(Z_v, "intermediate_data/FABIO/Z_value.rds")
saveRDS(Y, "intermediate_data/FABIO/Y.rds")
saveRDS(X, "intermediate_data/FABIO/X.rds")




# redistribute balancing over all uses proportionally ---------------------------------------------
# TODO: do we need this? Not adapted yet

regions <- fread("input_data/FABIO/inst/regions_full.csv")
regions <- regions[cbs==TRUE]
items <- fread("input_data/FABIO/inst/items_full.csv")
nrcom <- nrow(items)
nrreg <- nrow(regions)
nrfd <- ncol(Y[[1]])/nrreg
i=28
for(i in seq_along(Z_m)){
  reg=1
  for(reg in seq_len(nrow(regions))){
    z_range <- (nrcom*(reg-1)+1):(nrcom*reg)
    y_range <- (nrfd*(reg-1)+1):(nrfd*reg)
    Z_sum <- rowSums(Z_m[[i]][, z_range])
    Y_sum <- rowSums(Y[[i]][, y_range])
    balancing <- as.vector(Y[[i]][, grepl("balancing", colnames(Y[[i]]))][, reg])
    balancing <- balancing / as.vector(Z_sum + Y_sum - balancing)
    balancing[!is.finite(balancing)] <- 0
    Z_m[[i]][, z_range] <- Z_m[[i]][, z_range] * (1 + balancing)
    Z_v[[i]][, z_range] <- Z_v[[i]][, z_range] * (1 + balancing)
    Y[[i]][, y_range] <- Y[[i]][, y_range] * (1 + balancing)
  }
}

saveRDS(Z_m, "/mnt/nfs_fineprint/tmp/fabio/v2/Z_mass_b.rds")
saveRDS(Z_v, "/mnt/nfs_fineprint/tmp/fabio/v2/Z_value_b.rds")
saveRDS(Y, "/mnt/nfs_fineprint/tmp/fabio/v2/Y_b.rds")

