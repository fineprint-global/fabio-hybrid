################
# invert FABIO
# aggregate products
################

# Years to calculate hybridised FABIO for
years <- 1994:1986

# invert <- function(year) {
require(Matrix) # Necessary for forked processes

for(year in years){
  Z <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Z.rds"))
  X <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_X.rds"))
  
  print(paste(year,"read"))
  
  A <- t(t(Z)/X)
  A[!is.finite(A)] <- 0
  A[A<0] <- 0
  diag(A)[diag(A)==1] <- 0.999999
  
  L <- diag(nrow(A))-A
  L <- solve(L, tol = 1.0e-22)
  
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/", year, "_L.rds"))
  
  print(paste(year,"solved"))
}

