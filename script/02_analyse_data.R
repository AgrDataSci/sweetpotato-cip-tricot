# Analyse data with Plackett-Luce model
# ..........................................
# ..........................................

## Packages ####
library("gosset")
library("PlackettLuce")
library("ggparty")
library("patchwork")
library("ggplot2")

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
char <- c("best_overall", "worst_overall")
keep <- apply(dt[char], 1, is.na)
keep <- as.vector(colSums(keep) == 0)

G <- rank_tricot(dt[keep, ],
                 items = paste0("item_", LETTERS[1:3]),
                 input = c("best_overall","worst_overall"), 
                 group = TRUE)


pld <- cbind(G, dt[keep, c("gender", "district", "trial")])


plt <- pltree(G ~ ., data = pld, minsize = 50, alpha = 0.1)

gosset:::plot_tree(plt)

