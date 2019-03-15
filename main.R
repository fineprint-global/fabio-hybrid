library(Matrix)

# Setup to process in parallel
library(parallel)
n_cores <- detectCores() - 2
cl <- makeCluster(n_cores)

# Years to calculate hybridised FABIO for
years <- 1995:2013

# Matrices necessary
sup <- readODS::read_ods("fabio-exiobase.ods", sheet = 1, skip = 1)
use <- readODS::read_ods("fabio-exiobase.ods", sheet = 2, skip = 1)
conc <- readODS::read_ods("fabio-exiobase.ods", sheet = 3)
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


hybridise <- function(year, Sup, Use, Cou) {
  
  require(Matrix) # necessary for forked processes
  
  load(paste0("/mnt/nfs_fineprint/tmp/fabio/", year, "_Y.RData"))
  load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/", year, "_Z.RData"))
  
  Tec <- vector("list", 49)
  for(i in 1:49) {
    tmp <- Matrix(0, nrow = 200, ncol = 200)
    for(j in 1:49)
      tmp <- tmp + Z[(1 + 200 * (j - 1)):(200 * j), 
                     (1 + 200 * (i - 1)):(200 * i)]
    Tec[[i]] <- tmp 
  }
  
  Oth <- Y[, grep("OtherUses$", colnames(Y))]
  rm(Y, Z)
  A <- Oth %*% Cou
  
  B <- vector("list", 49)
  for(i in 1:49) {
    B[[i]] <- Sup %*% Tec[[i]] * Use
    B[[i]] <- B[[i]] / rowSums(B[[i]])
    B[[i]][is.na(B[[i]])] <- 0
  }
  
  C <- Matrix(0, nrow = 192 * 130, ncol = 49 * 200, sparse = TRUE)
  
  for(i in 1:49) {
    C[, (1 + 200 * (i - 1)):(200 * i)] <-
      do.call(rbind, replicate(192, B[[i]], simplify = FALSE)) * A[, i]
  }

  C
}

output <- parLapply(cl, years, hybridise, Sup, Use, Cou)

for(i in seq_along(output)) {
  saveRDS(output[[i]], paste0(years[[i]], "_Z.rds"))
}
