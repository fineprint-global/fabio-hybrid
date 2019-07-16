
library(data.table)

# get_pos <- function(idx, cntry, item) {
#   which(idx$country == cntry & idx$item == item)
# }

get_path <- function(alpha, D, p, epsilon, lvl_cap = 3, lvl = 0) {
  
  tmp <- alpha * D[p, ]
  
  out <- tmp[tmp >= epsilon]
  rest <- sum(tmp[tmp < epsilon])
  
  if(lvl >= lvl_cap) {
    return(
      list(
        lvl = lvl,
        split = c(out, "rest" = rest)
      )
    )
  }
  lvl <- lvl + 1
  
  return(
    list(
      lvl = lvl - 1,
      split = c(out, "rest" = rest),
      down = lapply(structure(names(out), names = names(out)), 
                    function(x, out, D, epsilon, lvl_cap, lvl) {
                      get_path(out[[x]], 
                               p = as.integer(substr(x, 4, nchar(x))), 
                               D, epsilon, lvl_cap, lvl)
                      }, out, D, epsilon, lvl_cap, lvl)
    )
  )
  
}

cntry <- "BRA"
item <- "Cattle"

x <- c(0.5, 1)
D <- matrix(c(0.4, 0.8, 0.6, 0.2), nrow = 2, 
            dimnames = list(c("row1", "row2"), c("col1", "col2")))
# p <- get_pos(index, cntry, item)
p <- 1
alpha <- x[p]

epsilon <- 0.01
lvl_cap <- 3
lvl <- 0

tree <- get_path(alpha, D, p, epsilon, lvl_cap)

tree


df_ify <- function(x, prev = "") {
  
  if(is.null(x$down)) {
    data.table("lvl" = x[[1]], 
               id = paste0(prev, names(x[[2]])),
               val = x[[2]])
  } else {
    rbindlist(
      list(
        data.table("lvl" = x[[1]],
                   id = paste0(prev, names(x[[2]])),
                   val = x[[2]]),
        rbindlist(
          lapply(names(x[["down"]]), function(name, x, prev) {
            df_ify(x[["down"]][[name]], prev = paste0(name, " > ", prev))
            }, x, prev)
        )
      )
    )
  }
}

df <- df_ify(tree)

df


split <- strsplit(df$id, " > ")
for(i in 0:lvl_cap) {
  df[[paste0("l_", i)]] <- sapply(split, function(x, i) x[i], i = i + 1)
}

df
