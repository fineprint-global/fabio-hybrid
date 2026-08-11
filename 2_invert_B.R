################################
# invert B
# Performance note: it is critical to check that matrix multiplications are appropriately done as sparse x sparse or dense x dense format. Sparse x dense => dense implicit conversion is slow on the server. 
# Explicit sparse -> dense conversions are thus added due to these prerformance concerns. Implicit conversion/sparse-first version of relevant code is kept for reference
# execution time is expected to be 3-5 min/year (mass/value versions combined) on the server, down from ~22:30 before
################################
# Formula: Block matrix inversion:
# B^-1 = -(A - BD^-1C)^-1 BD^-1
# for C = 0 -->  B^-1 = -A^-1 BD^-1

library(Matrix)
library(qs2)

vers <- "v2" # "v1.1", v1.2
hybrid_model <- "gloria" #"exio"
years <- 2010:2023
versions <- c("","losses/")
versions = versions[1]

for(version in versions){
  for(year in years){
    print(paste0(version,year))
    print(Sys.time())
    
    if (hybrid_model == "exio") {
      D_inv <- readRDS(paste0("/mnt/nfs_fineprint/tmp/exiobase/v3.10/pxp/IOT_", year, "_pxp/L.rds"))
      x <- readRDS(paste0("/mnt/nfs_fineprint/tmp/exiobase/v3.10/pxp/IOT_", year, "_pxp/x.rds"))
    }
    
    if (hybrid_model == "gloria") {
      D_inv <- qs_read(paste0("~/Hybrid-Conc/Fabio_Hybrid_Output/Gloria_L_Matrices/L_", year, ".qs2"))
      x <- qs_read(paste0("/mnt/nfs_fineprint/tmp/gloria/v060-compiled/IOTs_basic_prices/X/X_", year, ".qs2"))
    }
    
    B <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",vers,"/hybrid/", hybrid_model, "/", version, year, "_B.rds"))
    
    # Original implicit formula
    # print(Sys.time())
    # B <- t(t(B)/x)
    # 
    # print("Filtering")
    # print(Sys.time())
    # B[!is.finite(B)] <- 0
    # B[B<0] <- 0
    # B <- 0-B
    
    # sparse-optimized version of the calculation above
    print("Division")
    print(Sys.time())
    
    # Column-scale B by 1/x, sparse-safe
    x_inv <- 1 / x
    x_inv[!is.finite(x_inv)] <- 0
    B <- B %*% Diagonal(x = x_inv)
    stopifnot(inherits(B, "CsparseMatrix")) # Verify sparsity was preserved
    
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
    
    print("Matrix multiplication (mass)")
    print(Sys.time())
    
    # Original formula: B_inv <- -A_inv %*% B %*% D_inv .. with added explicit sparse => dense conversion
    B_inv <- as(-A_inv %*% B, "denseMatrix") %*% D_inv
    
    # this performs the following without allocating a new object:
    # A_inv_mult_B <- as.matrix(A_inv %*% B)
    # B_inv <- -(A_inv_mult_B %*% D_inv)
   
    rm(A_inv)
    gc(verbose = FALSE)
    
    print("saving")
    print(Sys.time())
    saveRDS(B_inv, paste0("~/Hybrid-Conc/Fabio_Hybrid_Output/Hybrid_Inv_Matrices/",vers,"/hybrid/", hybrid_model, "/", version, year, "_B_inv_mass.rds"))
    
    rm(B_inv)
    gc(verbose = FALSE)
    
    print("Matrix multiplication (value)")
    print(Sys.time())
    A_inv <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",vers,"/", version, year, "_L_value.rds"))
    B_inv <- as(-A_inv %*% B, "denseMatrix") %*% D_inv
    
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

