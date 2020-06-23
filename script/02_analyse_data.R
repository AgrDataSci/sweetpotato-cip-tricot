# Analyse data with Plackett-Luce model
# ..........................................
# ..........................................

## Packages ####
library("gosset")
library("PlackettLuce")
library("ggparty")
library("patchwork")
library("ggplot2")
library("multcompView")

# ..........................................
# ..........................................
# Read data ####
dt <- read.csv("data/sweetpotato_data.csv")

fct <- c("gender", "district", "trial")

dt[fct] <- lapply(dt[fct], function(x){
  as.factor(x)
})

# create a list with filtering conditions for all the data and the 
# rankings for each characteristic
# overall = rows with valid entries for the overall preference
# taste = rows with valid entries for taste
# color = rows with valid entries for color
# all = rows with valid entries for all characteristics
char <- c("overall", "taste", "color")
keep <- list()
for(i in seq_along(char)) {
  char_i <- char[[i]]
  char_i <- paste0(c("best_", "worst_"), char_i)
  k <- apply(dt[char_i], 1, is.na)
  k <- as.vector(colSums(k) == 0)
  keep[[i]] <- k
}

# now the filter for all
char_i <- paste0(c("best_", "worst_"), rep(char, each = 2))
k <- apply(dt[char_i], 1, is.na)
k <- as.vector(colSums(k) == 0)
keep[[4]] <- k

names(keep) <- c(char, "all")

# ..........................................
# ..........................................
# PlackettLuce rankings ####
G <- list()

for(i in seq_along(char)) {
  char_i <- char[[i]]
  char_i <- paste0(c("best_", "worst_"), char_i)
 
  g <- rank_tricot(dt[keep$all, ],
              items = paste0("item_", LETTERS[1:3]),
              input = char_i, 
              group = TRUE)
  
  G[[i]] <- g
  
}

names(G) <- char



# Check the agreement between rankings. Kendall should be read as kendall/100
summarise_agreement(G$overall, G[2:3], labels = char[2:3])




pld <- cbind(G = G$overall, dt[keep$all, c("gender", "district", "trial")])

plt <- pltree(G ~ ., data = pld, minsize = 50)

coef(plt, log = FALSE)

worst_regret(plt)

gosset:::plot_tree(plt, add.letters = TRUE)

