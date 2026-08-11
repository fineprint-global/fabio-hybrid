# ================================
# Creates the hybrid sub-matrix B for Gloria or Exiobase
# Execution time per year is expected to be ~ 1.5 min on the server 
# ================================
library(Matrix)
library(parallel)
library(qs2)

vers <- "v2" # "v1.1", v1.2
hybrid_model <- "gloria" #"exio"

# Input tables: supply-, use- and region concordance
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

# create matrices from input data
if (hybrid_model == "exio") {
  conc$FABIO_code <- 1:nrow(conc)
  conc$EXIOBASE_code[is.na(conc$EXIOBASE_code)] <- 50 # Move NAs to extra column to drop, allocate at some point
  
  Sup <- as_matrix(sup)
  Use <- as_matrix(use)
  Cou_NA <- sparseMatrix(i = conc$FABIO_code, j = conc$EXIOBASE_code) * 1
  Cou <- Cou_NA[, 1:49] # Remove 50th column of countries missing in EXIOBASE
  
  n_fabio  <- length(unique(conc$FABIO_area_code))   # 192 in v1.2;   # 181 for v2, though there concordance table would need to be provided with the correct region count
}
if (hybrid_model == "gloria") {
  # Sort by FABIO area code to make sure the ordering is appropriate
  conc <- conc[order(conc$FABIO_area_code), ]
  
  # Build sequential FABIO row index (repeats for multi-mapped countries)
  # Each unique FABIO country gets one row in Cou
  conc$FABIO_code <- match(conc$FABIO_area_code, 
                           sort(unique(conc$FABIO_area_code)))
  
  sup = sup[complete.cases(sup), ]
  use = use[complete.cases(use), ]
  
  Sup <- as_matrix(sup)
  Use <- as_matrix(use)
  
  n_fabio  <- length(unique(conc$FABIO_area_code))   # 181 for v2
  n_gloria <- length(unique(conc$GLORIA_region_code))  # 164
  
  # the sparseMatrix constructor inserts value x at position i/y. If there are multiple, it sums (in this case, all multi-mappings are 0 in conc per default, but it would support percentages)
  Cou <- sparseMatrix(i    = conc$FABIO_code,
                      j    = conc$GLORIA_Lfd_Nr,
                      x    = conc$allocation_share,
                      dims = c(n_fabio, n_gloria))
}

# Function to create the hybrid part for a certain year
hybridise <- function(year, Sup, Use, Cou, Y_all, hybrid_model) {
  
  require(Matrix) # Necessary for forked processes
  require(qs2)
  
  # Read EXIOBASE Z
  if (hybrid_model == "exio") {
    Z <- readRDS(paste0("/mnt/nfs_fineprint/tmp/exiobase/v3.10/pxp/IOT_", year, "_pxp/Z.rds"))

    # Exiobase dimensions
    n_regions <- 49
    n_sectors <- 200
    # Sanity check: 49 * 200 = 19,680
    stopifnot(nrow(Z) == n_regions * n_sectors)
  }
  
  # Read Gloria Z
  if (hybrid_model == "gloria") {
    Z <- qs_read(paste0("/mnt/nfs_fineprint/tmp/gloria/v060-compiled/IOTs_basic_prices/Z/Z_", year, ".qs2"))
    
    # Gloria dimensions
    n_regions <- 164
    n_sectors <- 120
    # Sanity check: 164 * 120 = 19,680
    stopifnot(nrow(Z) == n_regions * n_sectors)
  }
  
  # Calculate the per-region technology matrix by summing over regions, resulting in a sector x sector Tec matrix for each region
  print(paste0(Sys.time(), " : Calculate Tech matrices for ", hybrid_model))
  Tec <- vector("list", n_regions)
  for (i in 1:n_regions) {
    tmp <- Matrix(0, nrow = n_sectors, ncol = n_sectors)
    for (j in 1:n_regions)
      tmp <- tmp + Z[(1 + n_sectors * (j - 1)):(n_sectors * j),
                     (1 + n_sectors * (i - 1)):(n_sectors * i)]
    Tec[[i]] <- tmp
  }
  # Z for EXIOBASE has dims 9800 9800, processed to 200*200 (sector x sector) blocks
  # Z for Gloria   has dims 19680 19680, processed to 120*120 (sector x sector) blocks
  
  # read FABIO Y
  Y <- Y_all[[as.character(year)]]
  
  # Get the columns of Y containing the other use category to allocate
  Oth <- Y[, grep("other$", colnames(Y))]
  rm(Y, Z)
  
  print(paste0(Sys.time(), " : Match FABIO countries with EXIOBASE countries and restructure the Other use matrix"))
  # Match FABIO countries with EXIOBASE countries and restructure the Other use matrix
  
  Oth <- Oth %*% Cou
  
  nprod <- nrow(Oth) / n_fabio
  
  # Create matrix for sector matching
  print(paste0(Sys.time(), " : Create matrix for sector matching"))
  T <- vector("list", n_regions)
  for(i in 1:n_regions) {
    T[[i]] <- Sup[1:nprod,] %*% Tec[[i]] * Use[1:nprod,] # apply Supply/Use concordances to map Tec matrix to Fabio products
    T[[i]] <- T[[i]] / rowSums(T[[i]]) # row-normalize to get shares
    T[[i]][is.na(T[[i]])] <- 0 # replace NA results with 0
  }
  
  # Compute the hybrid part from Oth and T
  print(paste0(Sys.time(), " : Compute the hybrid part from Oth and T"))
  B <- Matrix(0, nrow = n_fabio * nprod, ncol = n_regions * n_sectors, sparse = TRUE)
  for(i in 1:n_regions) {
    B[, (1 + n_sectors * (i - 1)):(n_sectors * i)] <-
      do.call(rbind, replicate(n_fabio, T[[i]], simplify = FALSE)) * Oth[, i]
  }
  
  B
}

# Execute -----------------------------------------------------------------

# Select fabio version to run loop
versions <- c("", "losses/")
versions <- versions[2]

for(version in versions){
  Y_all <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",vers,"/", version, "Y.rds"))
  
  # Setup to process in parallel
  # n_cores <- parallel::detectCores() - 2
  # cl <- parallel::makeCluster(n_cores)
  
  # Years to calculate hybridised FABIO for
  # years <- 1986:2020
  years <- 2010:2023
  
  # output <- parallel::parLapply(cl, years, hybridise, Sup, Use, Cou, Y_all, hybrid_model)
  # 
  # for(i in seq_along(output)) {
  #   saveRDS(output[[i]], 
  #           paste0("/mnt/nfs_fineprint/tmp/fabio/",vers,"/hybrid/", version, years[[i]], "_B.rds"))
  # }
  
  # parallel::stopCluster(cl)
  
  # rm(cl, Y_all, n_cores, years, output); gc()
  
  # Alternatively run a loop
  for(year in years){
    print(paste0("processing ", year))
    result <- hybridise(year, Sup, Use, Cou, Y_all, hybrid_model)
    saveRDS(result, paste0("/mnt/nfs_fineprint/tmp/fabio/",vers,"/hybrid/", hybrid_model, "/", version, year, "_B.rds"))
    rm(result)
  }
}


