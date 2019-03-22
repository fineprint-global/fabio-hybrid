################
# Hybrid footprint
################

agg <- function(x)
{
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)
}

library(Matrix)

items <- read.csv2("Items.csv")
regions <- read.csv2("Regions.csv")
regions_exio_fao <- read.csv2("Regions_FAO-EXIO.csv", stringsAsFactors = FALSE)
regions_exio <- unique(regions_exio_fao[,3:5])
regions_exio$EXIOcode <- as.numeric(regions_exio$EXIOcode)
regions_exio <- regions_exio[order(regions_exio$EXIOcode)[1:49],]

year=2013
# years <- 1995:2013

load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_Y.RData"))
E <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_E.rds"))
X <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_X.rds"))
L <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/hybrid/",year,"_L_120.rds"))

X[X<0] <- 0
e <- c(as.vector(E$Landuse) / X, rep(0,nrow(Y)))
e[!is.finite(e)] <- 0
MP <- e * L

# aggregate countries in Y
colnames(Y) <- rep(1:49, each = 7)
Y <- agg(Y)
Y <- rbind(matrix(0,nrow(E),49),Y)

# Total EU Footprint
FP <- MP %*% Y
FP <- t(FP)
colnames(FP) <- c(rep(1:192, each = 120), rep(192,9800))
FP <- agg(FP)
FP <- t(FP)

rownames(FP) <- regions$Country
colnames(FP) <- regions_exio$EXIOregion
write.csv2(FP, "footprints_2013.csv")
