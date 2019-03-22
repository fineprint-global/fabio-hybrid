################################
# aggregate FABIO
################################
require(Matrix) # Necessary for forked processes
agg <- function(x){
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)}

year=2010

load(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Z.RData"))
load(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_X.RData"))

products <- c("Rice (Milled Equivalent)","Wheat and products","Barley and products","Maize and products","Rye and products","Oats","Millet and products","Sorghum and products","Cereals, Other","Potatoes and products","Cassava and products","Sweet potatoes","Roots, Other","Yams","Sugar cane","Sugar beet","Beans","Peas","Pulses, Other and products","Nuts and products","Soyabeans","Groundnuts (Shelled Eq)","Sunflower seed","Rape and Mustardseed","Seed cotton","Coconuts - Incl Copra","Sesame seed","Oil, palm fruit","Olives (including preserved)","Oilcrops, Other","Tomatoes and products","Onions","Vegetables, Other","Oranges, Mandarines","Lemons, Limes and products","Grapefruit and products","Citrus, Other","Bananas","Plantains","Apples and products","Pineapples and products","Dates","Grapes and products (excl wine)","Fruits, Other","Coffee and products","Cocoa Beans and products","Tea (including mate)","Hops","Pepper","Pimento","Cloves","Spices, Other","Jute","Jute-Like Fibres","Soft-Fibres, Other","Sisal","Abaca","Hard Fibres, Other","Tobacco","Rubber","Fodder crops","Grazing","Cottonseed","Palm kernels","Sugar non-centrifugal","Molasses","Sugar, Refined Equiv","Sweeteners, Other","Soyabean Oil","Groundnut Oil","Sunflowerseed Oil","Rape and Mustard Oil","Cottonseed Oil","Palmkernel Oil","Palm Oil","Coconut Oil","Sesameseed Oil","Olive Oil","Ricebran Oil","Maize Germ Oil","Oilcrops Oil, Other","Soyabean Cake","Groundnut Cake","Sunflowerseed Cake","Rape and Mustard Cake","Cottonseed Cake","Palmkernel Cake","Copra Cake","Sesameseed Cake","Oilseed Cakes, Other","Wine","Beer","Beverages, Fermented","Beverages, Alcoholic","Alcohol, Non-Food","Cotton lint","Cattle, Buffaloes","Cattle, Buffaloes","Sheep, Goats","Sheep, Goats","Pigs","Poultry Birds","Live animals, other","Live animals, other","Live animals, other","Live animals, other","Live animals, other","Live animals, other","Live animals, other","Live animals, other","Milk","Milk","Eggs","Wool (Clean Eq.)","Bovine Meat","Mutton & Goat Meat","Pigmeat","Poultry Meat","Meat, Other","Offals, Edible","Fats, Animals, Raw","Hides and skins","Meat Meal","Pet food","Honey","Silk","Fish","Wood fuel","Industrial roundwood, coniferous","Industrial roundwood, non-coniferous")

colnames(Z) <- paste0(rep(1:192, each = 130), "_", rep(products,192))
Z <- t(agg(Z))
colnames(Z) <- paste0(rep(1:192, each = 130), "_", rep(products,192))
Z <- t(agg(Z))

X <- t(as.matrix(X))
colnames(X) <- paste0(rep(1:192, each = 130), "_", rep(products,192))
X <- as.vector(agg(X))

A <- t(t(Z)/X)
A[!is.finite(A)] <- 0
A[A<0] <- 0
diag(A)[diag(A)==1] <- 0.999999

L <- diag(nrow(A))-A
L <- solve(L, tol = 1.0e-20)

