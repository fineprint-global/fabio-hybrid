################################
# invert B
################################
# Block matrix inversion:
# B^-1 = -(A - BD^-1C)^-1 BD^-1
# for C = 0 -->  B^-1 = -A^-1 BD^-1

library(Matrix)
library(qs2)

vers <- "v2" # "v1.1", v1.2
hybrid_model <- "gloria" #"exio"
years <- 2010:2023
versions <- c("","losses/")
versions = versions[1]
# version = versions[1] # TODO: rename, is not ideal for debugging

for(version in versions){
  for(year in years){
    print(paste0(version,year))
    print(Sys.time())
    
    if (hybrid_model == "exio") {
      if(year<1995){
        load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/1995_L.RData"))
        load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/1995_x.RData"))
      } else if(year>2016) {
        load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/2016_L.RData"))
        load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/2016_x.RData"))
      } else {
        load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/", year, "_x.RData"))
        load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/", year, "_L.RData"))
      }
      
      D_inv <- L
      rm(L); gc()
    }
    
    if (hybrid_model == "gloria") {
      D_inv <- qs_read(paste0("~/Hybrid-Conc/Fabio_Hybrid_Output/Gloria_L_Matrices/L_", year, ".qs2"))
      x <- qs_read(paste0("/mnt/nfs_fineprint/tmp/gloria/v060-compiled/IOTs_basic_prices/X/X_", year, ".qs2"))
    }
    
    B <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",vers,"/hybrid/", hybrid_model, "/", version, year, "_B.rds"))
    
    # Ensure the matrix is sparse
    # D_inv <- as(D_inv, "sparseMatrix")
    
    # print("Division")
    # print(Sys.time())
    # B <- t(t(B)/x)
    # 
    # print("Filtering")
    # print(Sys.time())
    # B[!is.finite(B)] <- 0
    # B[B<0] <- 0
    # B <- 0-B
    
    # sparse-optimized version of the calulation above
    print("Division")
    print(Sys.time())
    
    # Column-scale B by 1/x, sparse-safe
    x_inv <- 1 / x
    x_inv[!is.finite(x_inv)] <- 0
    B <- B %*% Diagonal(x = x_inv)
    
    # Verify sparsity was preserved
    stopifnot(inherits(B, "CsparseMatrix"))
    
    print("Filtering")
    print(Sys.time())
    
    # Operate on nonzero values only (sparse-safe)
    B@x[!is.finite(B@x)] <- 0
    B@x[B@x < 0] <- 0
    B <- drop0(B)
    
    # Negate (sparse-safe: just flips signs of @x)
    B <- -B
    
    print("Load A inv")
    print(Sys.time())
    A_inv <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",vers,"/", version, year, "_L_mass.rds"))
    
    print("second calculation (mass)")
    print(Sys.time())
    B_inv <- -A_inv %*% B %*% D_inv
    
    rm(A_inv)
    gc(verbose = FALSE)
    
    print("saving")
    print(Sys.time())
    saveRDS(B_inv, paste0("~/Hybrid-Conc/Fabio_Hybrid_Output/Hybrid_Inv_Matrices/",vers,"/hybrid/", hybrid_model, "/", version, year, "_B_inv_mass.rds"))
    
    rm(B_inv)
    gc(verbose = FALSE)
    
    print("third calculation (value)")
    print(Sys.time())
    A_inv <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",vers,"/", version, year, "_L_value.rds"))
    B_inv <- -A_inv %*% B %*% D_inv
    
    print("saving")
    print(Sys.time())
    saveRDS(B_inv, paste0("~/Hybrid-Conc/Fabio_Hybrid_Output/Hybrid_Inv_Matrices/",vers, "/hybrid/", hybrid_model, "/", version, year, "_B_inv_value.rds"))
    
    print("Finished:")
    print(paste0(version,year))
    print(Sys.time())
    
    rm(B_inv, A_inv, B, D_inv)
    gc(verbose = FALSE)
  }
}

