library(Matrix)
library(parallel)

# Matrices necessary
# sup <- readODS::read_ods("/mnt/nfs_fineprint/tmp/fabio/v2/hybrid/fabio-exiobase.ods", sheet = 1, skip = 1)
# use <- readODS::read_ods("/mnt/nfs_fineprint/tmp/fabio/v2/hybrid/fabio-exiobase.ods", sheet = 2, skip = 1)
# conc <- readODS::read_ods("/mnt/nfs_fineprint/tmp/fabio/v2/hybrid/fabio-exiobase.ods", sheet = 3)
sup <- read.csv("fabio-exio_sup.csv")
use <- read.csv("fabio-exio_use.csv")
conc <- read.csv("fabio-exio_conc.csv")
conc$FABIO_code <- 1:nrow(conc)
# Move NAs to extra column to drop, allocate at some point
conc$EXIOBASE_code[is.na(conc$EXIOBASE_code)] <- 50

as_matrix <- function(x) {
  y <- as.matrix(x[5:ncol(x)])
  y_rowSums <- rowSums(y, na.rm = TRUE)
  if(!all(x[["Total"]] == y_rowSums)) stop()
  y[is.na(y)] <- 0
  dimnames(y) <- NULL
  Matrix(y)
}
Sup <- as_matrix(sup)
Use <- as_matrix(use)
Cou_NA <- sparseMatrix(i = conc$FABIO_code, j = conc$EXIOBASE_code) * 1
Cou <- Cou_NA[, 1:49] # Remove 50th column of countries missing in EXIOBASE

# Function to create the hybrid part for a certain year
hybridise <- function(year, Sup, Use, Cou, Y_all) {
  
  require(Matrix) # Necessary for forked processes
  
  # Read EXIOBASE Z and FABIO Y
  Y <- Y_all[[as.character(year)]]
  if(year<1995){
    load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/1995_Z.RData"))
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
versions <- c("", "losses/", "wood/")
version <- versions[1]

for(version in versions){
  Y_all <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/v2/", version, "Y.rds"))
  
  # Setup to process in parallel
  n_cores <- parallel::detectCores() - 2
  cl <- parallel::makeCluster(n_cores)
  
  # Years to calculate hybridised FABIO for
  years <- 1986:2013
  
  output <- parallel::parLapply(cl, years, hybridise, Sup, Use, Cou, Y_all)
  
  for(i in seq_along(output)) {
    saveRDS(output[[i]], 
            paste0("/mnt/nfs_fineprint/tmp/fabio/v2/hybrid/", version, years[[i]], "_B.rds"))
  }
  
  parallel::stopCluster(cl)
  
  rm(cl, Y_all, n_cores, years, output); gc()
  
  # # Alternatively run a loop
  # for(year in years){
  #   saveRDS(hybridise(year, Sup, Use, Cou, Y_all), 
  #           paste0("/mnt/nfs_fineprint/tmp/fabio/v2/hybrid/", version, year, "_B.rds"))
  # }
}


