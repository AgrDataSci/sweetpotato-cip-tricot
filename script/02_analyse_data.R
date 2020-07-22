# Analyse data with Plackett-Luce model
# ..........................................
# ..........................................

## Packages ####
library("gosset")
library("ClimMobTools")
library("PlackettLuce")
library("gtools")
library("ggparty")
library("patchwork")
library("ggplot2")
library("egg")
library("multcompView")


# write session info
sessioninfo::session_info()
capture.output(sessioninfo::session_info(),
               file = "script/session_info/02_analyse_data.txt")

# ..........................................
# ..........................................
# Read data ####
dt <- read.csv("data/spotato_data.csv")

head(dt)

# create a table with frequencies where each item was evaluated
# this is to be used in the Methods in the Manuscript
n <- nrow(dt)

itemdata <- dt[, paste0("item_", LETTERS[1:3])]

it <- data.frame(table(unlist(itemdata)))

it$x <- with(it, round((Freq / n) * 100, 1))

it$x <- with(it, paste0(x, "%"))

names(it) <- c("Genotype", "Freq", "Relative freq")

# add gender info
gender <- dt[, "gender"]
idt <- unlist(itemdata)

nMan <- sum(gender == "Man", na.rm = TRUE)
nWom <- sum(gender == "Woman", na.rm = TRUE)
  
gender <- cbind(tapply(rep(gender, 3), idt, function(x) sum(x == "Man", na.rm = TRUE)), 
                tapply(rep(gender, 3), idt, function(x) sum(x == "Woman", na.rm = TRUE))) 
  
it <- cbind(it, gender)
  
names(it)[4:5] <- paste0(c("Man (n=","Woman (n="), c(nMan, nWom), ")")
  
# add type of trial
trial <- dt[,"trial"]

nC <- sum(trial == "community", na.rm = TRUE)
nH <- sum(trial == "home", na.rm = TRUE)

trial <- cbind(tapply(rep(trial, 3), idt, function(x) sum(x == "community", na.rm = TRUE)), 
               tapply(rep(trial, 3), idt, function(x) sum(x == "home", na.rm = TRUE))) 

it <- cbind(it, trial)

names(it)[6:7] <- paste0(c("Community (n=","Home (n="), c(nC, nH), ")")

# now add the information for the tested country
ug <- dt[dt$country == "Uganda", paste0("item_", LETTERS[1:3])]
ug <- unique(unlist(ug))
it$Country <- it$Genotype %in% ug

it$Country <- ifelse(it$Country == TRUE, "Uganda", "Ghana")

it <- it[order(it$Country), ]

it <- it[,union(c("Genotype", "Country"), names(it))]

head(it)

output <- "output/summary_tables"
dir.create(output, showWarnings = FALSE, recursive = TRUE)

write.csv(it, paste0(output, "/summary_tested_varieties_gender_trial.csv"), row.names = FALSE)

# ..........................................
# ..........................................
# Uganda ####
# two trials (home, community)
# responses for overall appreciation (OA), taste and colour
# description for why ranking best and worst choice

# ..........................................
# ..........................................
# compare rankings from the three traits using OA as baseline
# take Kendall tau and the agreement of being best and worst among the traits

u <- dt[dt$country == "Uganda", ]

names(u)

chars <- c("overall","taste","color")
sel <- paste0(rep(c("best_","worst_"), 3), chars)

# remove possible NAs in trait response
k <- apply(u[,sel], 1, function(x){
  sum(is.na(x)) == 0
})

u <- u[k, ]

# now create the rankings for each trait
# run over the traits
R <- list()
for(i in seq_along(chars)){
  R[[i]] <- rank_tricot(u, 
                        items = paste0("item_", LETTERS[1:3]),
                        input = paste0(c("best_","worst_"), chars[[i]]))
}

# compare rankings
a <- summarise_agreement(R[[1]],
                         compare.to = R[-1],
                         labels = c("Taste","Colour"))

p <- 
  plot(a, scales = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 11, face = "bold", color = "gray20"),
        strip.text.x = element_text(size = 12, color = "gray20", face = "bold"),
        strip.background = element_rect(fill = "#FFFFFF")) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) 

p <- tag_facet(p)

output <- "output/trait_correlation"
dir.create(output, recursive = TRUE, showWarnings = FALSE)

ggsave(paste0(output, "/correlation_uganda.png"), 
       plot = p, 
       width = 7,
       height = 5,
       dpi = 500)

# ..........................................
# ..........................................
# Plackett-Luce (PL) tree using OA and gender, district and trial as covariates
G <- R[[1]]
G <- group(G, index = seq_along(u$id))

vars <- c("trial","gender","district")

pld <- cbind(G, u[, vars])

# coerce variables to factor
fct <- c("gender", "district", "trial")
pld[fct] <- lapply(pld[fct], function(x){
  as.factor(x)
})

names(pld)[-1] <- ClimMobTools:::.title_case(names(pld)[-1])

head(pld)

plt <- pltree(G ~ ., data = pld, alpha = 0.1)

plot(plt)

p <- gosset:::plot_tree(plt, add.letters = TRUE, threshold = 0.1)

p

output <- "output/pltree"
dir.create(output, showWarnings = FALSE, recursive = TRUE)

ggsave(paste0(output, "/pltree_uganda.png"),
       plot = p, 
       width = 7,
       height = 6,
       dpi = 500)

# ..........................................
# ..........................................
# Post-hoc table from a PL model without covariates
mod <- PlackettLuce(R[[1]], alpha = 0.05, ref = 3)
summary(mod)
s <- summary(mod)$coefficients

s[,1:3] <- apply(s[,1:3], 2, function(x) {round(x, 4)})

s

s[,4] <- paste(format.pval(s[,4], digits = 4),
               stars.pval(s[, 4]))

mcomp <- gosset::multcompPL(mod, threshold = 0.05)
rownames(mcomp) <- mcomp$term
mcomp <- mcomp[rownames(s), ]

s <- cbind(s, . = mcomp$group) 

s

write.csv(s, paste0("output/summary_tables/PL_coefficients_uganda.csv"))

# ..........................................
# ..........................................
# Ghana ####

# two trials (home and community)
# two genotype samples (advanced materials, released varieties)
# responses for OA and description for why ranking best and worst choice

# ..........................................
# ..........................................
# PLT with the OA using variables, 
# community, district, age, gender, trial and geno_test as covariates
g <- dt[dt$country == "Ghana", ]

g$district <- abbreviate(g$district, 8)

R <- rank_tricot(g, 
                 items = paste0("item_", LETTERS[1:3]),
                 input = c("best_overall","worst_overall"))

G <- group(R, index = seq_along(g$id))

vars <- c("trial", "gender", "district","geno_test","age")

pld <- cbind(G, g[, vars])

# coerce variables to factor
fct <- c("gender", "district", "trial","geno_test")
pld[fct] <- lapply(pld[fct], function(x){
  as.factor(x)
})

head(pld)

pld$age <- as.integer(pld$age)
pld <- pld[!is.na(pld$age), ]

names(pld)[-1] <- ClimMobTools:::.title_case(names(pld)[-1])

head(pld)

plt <- pltree(G ~ District + Age, data = pld, alpha = 0.1, minsize = 50)
  
plot(plt)

p <- gosset:::plot_tree(plt, add.letters = TRUE, threshold = 0.1)

p

plt

output <- "output/pltree"
dir.create(output, showWarnings = FALSE, recursive = TRUE)

ggsave(paste0(output, "/pltree_ghana.png"),
       plot = p, 
       width = 14,
       height = 10,
       dpi = 900)

# ..........................................
# ..........................................
# Post-hoc table from a PL model without covariates
mod <- PlackettLuce(R, alpha = 0.05, ref = 3)
summary(mod)
s <- summary(mod)$coefficients

s[,1:3] <- apply(s[,1:3], 2, function(x) {round(x, 4)})

s

s[,4] <- paste(format.pval(s[,4], digits = 4),
               stars.pval(s[, 4]))

mcomp <- gosset::multcompPL(mod, threshold = 0.1)
rownames(mcomp) <- mcomp$term
mcomp <- mcomp[rownames(s), ]

s <- cbind(s, . = mcomp$group) 

s

write.csv(s, paste0("output/summary_tables/PL_coefficients_ghana.csv"))


# ..........................................
# ..........................................
# Both countries ####

# use data from both countries to visualise the cloud text with main reasons 
# given by participants for the best and worst samples


# put all together and plot the favourability score
R <- rank_tricot(dt, 
                 items = paste0("item_", LETTERS[1:3]),
                 input = c("best_overall","worst_overall"))

f <- summarise_favorite(R)

p <- 
  plot(f, abbreviate = FALSE) +
  theme_bw() +
  labs(x = "Genotype", y="Favourability score") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(size = 12, color = "grey30"),
        axis.title = element_text(size = 12, color = "grey30"))

output <- "output/favourability"
dir.create(output, showWarnings = FALSE, recursive = TRUE)

ggsave(paste0(output, "/favourability_score.png"),
       plot = p, 
       width = 7,
       height = 7,
       dpi = 800)
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
