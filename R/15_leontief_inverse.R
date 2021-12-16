### Leontief inverse ###

# what is changed:
# - nothing

library("data.table")
library("Matrix")

write = TRUE

# Leontief inverse ---

prep_solve <- function(year, Z, Y, X,
                       adj_X = FALSE, adj_A = TRUE, adj_diag = FALSE) {

  if(adj_X) {X <- X + 1e-10}

  A <- Matrix(0, nrow(Z), ncol(Z))
  idx <- X != 0
  # CHNAGED index here
  idx <- c(1:nrow(A))[idx]
  A[, idx] <- t(t(Z[, idx]) / X[idx])
  # TODO: use sparse matrix hack here as well
  #A <- Z
  #A[, idx]@x <- A[, idx]@x / rep.int(X[idx], diff(A[, idx]@p))
  if(adj_A) {A[A < 0] <- 0}

  if(adj_diag) {diag(A)[diag(A) == 1] <- 1 - 1e-10}
  L <- .sparseDiagonal(nrow(A)) - A

  lu(L) # Computes LU decomposition and stores it in L

  L_inv <- solve(L, tol = .Machine[["double.eps"]])
  dimnames(L_inv) <- dimnames(Z)

  return(L_inv)
}

##
years <- seq(2013, 2013)
years_singular <- c(1986,1994,2002,2009)

Z_m <- readRDS("intermediate_data/FABIO/Z_mass.rds")
Z_v <- readRDS("intermediate_data/FABIO/Z_value.rds")
Y <- readRDS("intermediate_data/FABIO/Y.rds")
X <- readRDS("intermediate_data/FABIO/X.rds")


for(year in years){

  print(year)

  adjust <- ifelse(year %in% years_singular, TRUE, FALSE)

  L <- prep_solve(year = year, Z = Z_m[[as.character(year)]],
                  Y = Y[[as.character(year)]], X = X[, as.character(year)],
                  adj_diag = adjust)
  if (write) saveRDS(L, paste0("intermediate_data/FABIO/", year, "_L_mass.rds"))

  L <- prep_solve(year = year, Z = Z_v[[as.character(year)]],
                  Y = Y[[as.character(year)]], X = X[, as.character(year)],
                  adj_diag = adjust)
  if (write) saveRDS(L, paste0("intermediate_data/FABIO/", year, "_L_value.rds"))

}


