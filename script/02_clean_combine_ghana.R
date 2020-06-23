# Read, combine and clean sweetpotato trial data
# ..........................................
# ..........................................

## Packages ####
library("readxl")
library("janitor")
library("PlackettLuce")
library("gosset")
library("ggplot2")
library("ggparty")
library("patchwork")

# ..........................................
# ..........................................
# Read the data ####
list.files("data/raw/")

# community tasting trial
l <- list()
for (i in 1:2){
dt <- read_excel("data/raw/ghana_community_tasting.xlsx",
                 sheet = i, 
                 na = c("No response", " "))

names(dt) <- make_clean_names(names(dt))

dt <- as.data.frame(dt)

codes <- dt[,c("codes", "geno")]
codes <- na.omit(codes)

dt <- dt[1:10]

# replace codes by the genotypes/varieties names
vars <- c("var1", "var2", "var3", "best_sp", "worst_sp")

for(j in seq_along(vars)){
  
  for(k in seq_along(codes$codes)) {
    
    dt[, vars[j]] <- ifelse(dt[, vars[j]] == codes$codes[k], 
                            codes$geno[k],
                            dt[, vars[j]])
    
  }
  
}

# set the middle genotype
middle <- apply(dt[vars], 1, function(x){
  item <- x[vars[1:3]]
  m <- !item %in% x[vars[4:5]]
  m <- as.vector(t(item[m]))
  if(length(m) > 1) m <- NA
  m
})

dt <- cbind(dt, middle = middle)

# remove rows with NA in middle
keep <- !is.na(dt$middle)
dt <- dt[keep, ]

# rename variables
names(dt) <- c("id","region","community","gender", "age",
               "item_a","item_b","item_c", "best","worst","middle")

# put in the list
l[[i]] <- dt

}


dt <- do.call(rbind, l)

dt

# fix gender labels
dt$gender
dt$gender <- as.factor(ifelse(dt$gender == "F", "Woman", 
                              ifelse(dt$gender == "M", "Man", dt$gender)))


dt$community <- as.factor(dt$community)
summary(dt$community)

itemnames <- sort(unique(unlist(dt[,paste0("item_", letters[1:3])])))

R <- as.rankings(dt[,c("best","middle","worst")],
                          input = "ordering", 
                          items = itemnames)

G <- group(R, index = 1:nrow(dt))


dat <- cbind(G, dt)

plt <- pltree(G ~ gender + age + community, data = dat, alpha = 0.1)

gosset:::plot_tree(plt)

