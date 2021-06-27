################################
# invert B
################################
# Block matrix inversion:
# B^-1 = -(A - BD^-1C)^-1 BD^-1
# for C = 0 -->  B^-1 = -A^-1 BD^-1

library(Matrix)

year=2013
years <- 1986:2013
losses="losses/"

for(losses in c("losses/","")){
  for(year in years){
    print(paste0(losses,year))
    
    if(year<1995){
      load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/1995_L.RData"))
      load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/1995_x.RData"))
    } else {
      load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/", year, "_x.RData"))
      load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/", year, "_L.RData"))
    }
    
    D_inv <- L
    rm(L); gc()
    
    B <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/v2/hybrid/", year, "_B.rds"))
    B <- t(t(B)/x)
    B[!is.finite(B)] <- 0
    B[B<0] <- 0
    B <- 0-B
    
    A_inv <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/v2/", losses, year, "_L_mass.rds"))
    B_inv <- -A_inv %*% B %*% D_inv
    saveRDS(B_inv, paste0("/mnt/nfs_fineprint/tmp/fabio/v2/hybrid/", losses, year, "_B_inv_mass.rds"))
    
    A_inv <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/v2/", losses, year, "_L_value.rds"))
    B_inv <- -A_inv %*% B %*% D_inv
    saveRDS(B_inv, paste0("/mnt/nfs_fineprint/tmp/fabio/v2/hybrid/", losses, year, "_B_inv_value.rds"))
    
  }
}

