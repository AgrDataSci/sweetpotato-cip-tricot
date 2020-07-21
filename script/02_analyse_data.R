# Analyse data with Plackett-Luce model
# ..........................................
# ..........................................

## Packages ####
library("gosset")
library("PlackettLuce")
library("igraph")
library("ggparty")
library("patchwork")
library("ggplot2")
library("multcompView")

# write session info
sessioninfo::session_info()
capture.output(sessioninfo::session_info(),
               file = "script/session_info/02_analyse_data.txt")

# ..........................................
# ..........................................
# Read data ####
dt <- read.csv("data/spotato_data.csv")

fct <- c("gender", "district", "trial")

dt[fct] <- lapply(dt[fct], function(x){
  as.factor(x)
})

head(dt)

# ..........................................
# ..........................................
# Uganda ####
# two trials (home, community)
# responses for overall appreciation (OA), taste and colour


# compare rankings from the three traits using OA as baseline
# take Kendall tau and the agreement of being best and worst among the traits

R <- rank_tricot(dt,
                 items = paste0("item_", LETTERS[1:3]),
                 input = c("best_overall","worst_overall"))


# Plackett-Luce (PL) tree using OA and gender, district and trial as covariates


# Post-hoc table from a PL model without covariates

# ..........................................
# ..........................................
# Ghana ####

# two trials (home and community)
# two genotype samples (advanced materials, released varieties)
# responses for OA and description for why ranking best and worst choice


# PLT with the OA using variables, 
# community, district, age, gender, trial and geno_type as covariates


# Post-hoc table from a PL model without covariates

# ..........................................
# ..........................................
# Both countries ####

# use data from both countries to visualise the cloud text with main reasons 
# given by participants for the best and worst samples


# put all together and plot the favourability score



# # check network for the entire dataset
# a <- adjacency(R)
# plot(graph_from_adjacency_matrix(a))
# 
# # only Uganda
# a <- rank_tricot(dt[dt$country == "Uganda", ],
#                  items = paste0("item_", LETTERS[1:3]),
#                  input = c("best_overall","worst_overall"))
# a <- adjacency(a)
# plot(graph_from_adjacency_matrix(a))
# 
# # only Ghana
# a <- rank_tricot(dt[dt$country == "Ghana", ],
#                  items = paste0("item_", LETTERS[1:3]),
#                  input = c("best_overall","worst_overall"))
# a <- adjacency(a)
# plot(graph_from_adjacency_matrix(a))
# 
# mod <- PlackettLuce(R)
# summary(mod)
# 
# plot(qvcalc(mod))
# 
# f <- summarise_favorite(R)
# plot(f) +
#   theme_bw() +
#   labs(x = "", y="") +
#   theme(legend.position = "none", 
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         axis.text = element_text(size = 12, color = "grey30"))
# 
# # Check the agreement between rankings. Kendall should be read as kendall/100
# summarise_agreement(G$overall, G[2:3], labels = char[2:3])
# 
# 
# pld <- cbind(G = G$overall, dt[keep$all, c("gender", "district", "trial")])
# 
# plt <- pltree(G ~ ., data = pld, minsize = 50)
# 
# coef(plt, log = FALSE)
# 
# worst_regret(plt)
# 
# gosset:::plot_tree(plt, add.letters = TRUE)
# 
