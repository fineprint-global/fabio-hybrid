library(Matrix)
library(parallel)
library(qs2)

vers <- "v2" # "v1.1", v1.2
hybrid_model <- "gloria" #"exio"

# Matrices necessary
sup  <- read.csv(paste0("inst/fabio-", hybrid_model, "_sup_",vers,".csv"))
use  <- read.csv(paste0("inst/fabio-", hybrid_model, "_use_",vers,".csv"))
conc <- read.csv(paste0("inst/fabio-", hybrid_model, "_reg_",vers,".csv"))

as_matrix <- function(x) {
  y <- as.matrix(x[5:ncol(x)])
  y_rowSums <- rowSums(y, na.rm = TRUE)
  if(!all(x[["Total"]] == y_rowSums)) warning("Total does not match rowSums - please check concordance tables")
  y[is.na(y)] <- 0
  dimnames(y) <- NULL
  Matrix(y)
}

if (hybrid_model == "exio") {
  conc$FABIO_code <- 1:nrow(conc)
  conc$EXIOBASE_code[is.na(conc$EXIOBASE_code)] <- 50 # Move NAs to extra column to drop, allocate at some point
  
  Sup <- as_matrix(sup)
  Use <- as_matrix(use)
  Cou_NA <- sparseMatrix(i = conc$FABIO_code, j = conc$EXIOBASE_code) * 1
  Cou <- Cou_NA[, 1:49] # Remove 50th column of countries missing in EXIOBASE
}

if (hybrid_model == "gloria") {
  
  conc <- conc[order(conc$FABIO_area_code), ]
  conc$GLORIA_region_index <- 1:nrow(conc)
  # conc$EXIOBASE_code[is.na(conc$EXIOBASE_code)] <- 50 # Move NAs to extra column to drop, allocate at some point
  
  Sup <- as_matrix(sup)
  Use <- as_matrix(use)
  Cou <- sparseMatrix(i = conc$FABIO_area_code, j = conc$GLORIA_region_index) * 1
}

# Function to create the hybrid part for a certain year
hybridise <- function(year, Sup, Use, Cou, Y_all) {
  
  require(Matrix) # Necessary for forked processes

  # Read EXIOBASE Z
  if (hybrid_model == "exio") {
    if(year<1995){
      load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/1995_Z.RData"))
    } else if(year>2016) {
      load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/2016_Z.RData"))
    } else {
      load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/", year, "_Z.RData"))
    }
    
    # Calculate Tech matrices for the 49 EXIO countries
    Tec <- vector("list", 49)
    for(i in 1:49) {
      tmp <- Matrix(0, nrow = 200, ncol = 200)
      for(j in 1:49)
        tmp <- tmp + Z[(1 + 200 * (j - 1)):(200 * j), 
                       (1 + 200 * (i - 1)):(200 * i)]
      Tec[[i]] <- tmp 
    }
    
  }
  
  # Read Gloria Z
  if (hybrid_model == "gloria") {
    Z <- qs_read(paste0("/mnt/nfs_fineprint/tmp/gloria/v060-compiled/IOTs_basic_prices/Z/Z_", year, ".qs2")) 
  
    # Gloria dimensions
    n_regions <- 164
    n_sectors <- 120
    # Sanity check: 164 * 120 = 19,680
    stopifnot(nrow(Z) == n_regions * n_sectors)
    
    # Calculate Tech matrices for the 164 Gloria regions
    Tec <- vector("list", n_regions)
    for (i in 1:n_regions) {
      tmp <- Matrix(0, nrow = n_sectors, ncol = n_sectors)
      for (j in 1:n_regions)
        tmp <- tmp + Z[(1 + n_sectors * (j - 1)):(n_sectors * j), 
                       (1 + n_sectors * (i - 1)):(n_sectors * i)]
      Tec[[i]] <- tmp
    }
  
    # Tec processing depends on matrix dimensions, this could even be separated out as global vars.
    # Z for EXIOBASE has dims 9800 * 9800, which is porcessed in 200*200 (sector x sector) blocks
    # Z for Gloria   has dims 19680 19680
    # 200 sectors * 49 regions = 9,800
    # for Gloria, it would be 120 sectors * 164 regions = 19680
    # in FABIO, there are 180 regions * 123 sectors (so some splitting logic would map presumably)
  }
  
  
  
  # Y comes from FABIO, can thus stay the same; 192 seems to be the old FABIO country count (so division is to normalize it to 1, needs to depend on version)
  
  
  # read FABIO Y
  Y <- Y_all[[as.character(year)]]
  
  # Get the columns of Y containing the other use category to allocate
  Oth <- Y[, grep("other$", colnames(Y))]
  rm(Y, Z)
  
  # Match FABIO countries with EXIOBASE countries and restructure the Other use matrix
  Oth <- Oth %*% Cou
  nprod <- nrow(Oth) / 192
  
  # Create matrix for sector matching
  T <- vector("list", 49)
  for(i in 1:49) {
    T[[i]] <- Sup[1:nprod,] %*% Tec[[i]] * Use[1:nprod,]
    T[[i]] <- T[[i]] / rowSums(T[[i]])
    T[[i]][is.na(T[[i]])] <- 0
  }
  
  # Compute the hybrid part from Oth and T
  B <- Matrix(0, nrow = 192 * nprod, ncol = 49 * 200, sparse = TRUE)
  for(i in 1:49) {
    B[, (1 + 200 * (i - 1)):(200 * i)] <-
      do.call(rbind, replicate(192, T[[i]], simplify = FALSE)) * Oth[, i]
  }
  
  B
}




# Execute -----------------------------------------------------------------

# Select fabio version or run loop
versions <- c("", "losses/")
version <- versions[1]

for(version in versions){
  Y_all <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",vers,"/", version, "Y.rds"))
  
  # Setup to process in parallel
  n_cores <- parallel::detectCores() - 2
  cl <- parallel::makeCluster(n_cores)
  
  # Years to calculate hybridised FABIO for
  years <- 1986:2020
  
  output <- parallel::parLapply(cl, years, hybridise, Sup, Use, Cou, Y_all)
  
  for(i in seq_along(output)) {
    saveRDS(output[[i]], 
            paste0("/mnt/nfs_fineprint/tmp/fabio/",vers,"/hybrid/", version, years[[i]], "_B.rds"))
  }
  
  parallel::stopCluster(cl)
  
  rm(cl, Y_all, n_cores, years, output); gc()
  
  # # Alternatively run a loop
  # for(year in years){
  #   saveRDS(hybridise(year, Sup, Use, Cou, Y_all), 
  #           paste0("/mnt/nfs_fineprint/tmp/fabio/",vers,"/hybrid/", version, year, "_B.rds"))
  # }
}


