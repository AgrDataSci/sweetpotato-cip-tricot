# Analyse data with Plackett-Luce model
# ..........................................
# ..........................................

## Packages ####
library("gosset")
library("tidyverse")
library("ClimMobTools")
library("PlackettLuce")
library("gtools")
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

head(dt)

# ..........................................
# ..........................................
# Summary tables ####
# make a table with number of participants per type of testing per country
n <- nrow(dt)

tb <- table(dt$country, dt$trial)

dimnames(tb)[[2]] <- c("Centralised", "Home")

# export the table
output <- "output/summary_tables"
dir.create(output, showWarnings = FALSE, recursive = TRUE)

write.csv(tb, paste0(output, "/summary_trials_per_country.csv"))

# ..........................................
# ..........................................
# create a table with frequencies where each item was evaluated
# this is to be used in the Methods in the Manuscript
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

names(it)[6:7] <- paste0(c("Centralised (n=","Home (n="), c(nC, nH), ")")

# now add the information for the tested country
ug <- dt[dt$country == "Uganda", paste0("item_", LETTERS[1:3])]
ug <- unique(unlist(ug))
it$Country <- it$Genotype %in% ug

it$Country <- ifelse(it$Country == TRUE, "Uganda", "Ghana")

# order the data by country
it <- it[order(it$Country), ]

# order the columns so genotype and country appears first
it <- it[,union(c("Genotype", "Country"), names(it))]

write.csv(it, paste0(output, "/summary_tested_varieties_gender_trial.csv"), 
          row.names = FALSE)

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
# subset the main dataset to retain only the Uganda data
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

output <- "output/trait_correlation"
dir.create(output, recursive = TRUE, showWarnings = FALSE)

ggsave(paste0(output, "/correlation_uganda.png"), 
       plot = p, 
       width = 7,
       height = 5,
       dpi = 500)


# put the scale into 0-1
a[2:4] <- lapply(a[2:4], function(x) x /100)

# export the table
write.csv(a, paste0(output, "/correlation_uganda.csv"), row.names = FALSE)

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

ref <- "Naspot 8"

summary(mod, ref = ref)

s <- summary(mod, ref = ref)$coefficients

s[,1:3] <- apply(s[,1:3], 2, function(x) {round(x, 4)})

s[,4] <- paste(format.pval(s[,4], digits = 4),
               stars.pval(s[, 4]))

mcomp <- gosset::multcompPL(mod, threshold = 0.05, ref = ref)
rownames(mcomp) <- mcomp$term
mcomp <- mcomp[rownames(s), ]

s <- cbind(s, . = mcomp$group) 

write.csv(s, paste0("output/summary_tables/PL_coefficients_uganda.csv"))

# ..........................................
# ..........................................
# subset the model per trial and use it later
PLm <- list()

# first only the data from the community testing
k <- u$trial == "community"

R <- R[[1]]

Rs <- R[k, ]

PLm[[1]] <- PlackettLuce(Rs, alpha = 0.05)

# now the data from the home testing
Rs <- R[!k, ]

PLm[[2]] <- PlackettLuce(Rs, alpha = 0.05)

names(PLm) <- c("uganda_community","uganda_home")

PLm

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

# abbreviate the name of districts
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

# fit the model
plt <- pltree(G ~ District + Age, data = pld, alpha = 0.1, minsize = 50)
  
plot(plt)

p <- gosset:::plot_tree(plt, add.letters = TRUE, threshold = 0.1)

p

plt

# export the plot
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

ref <- "Apomuden"

s <- summary(mod, ref = ref)$coefficients

s[,1:3] <- apply(s[,1:3], 2, function(x) {round(x, 4)})

s

s[,4] <- paste(format.pval(s[,4], digits = 4),
               stars.pval(s[, 4]))

mcomp <- gosset::multcompPL(mod, threshold = 0.1, ref = ref)
rownames(mcomp) <- mcomp$term
mcomp <- mcomp[rownames(s), ]

s <- cbind(s, . = mcomp$group) 

s

write.csv(s, paste0("output/summary_tables/PL_coefficients_ghana.csv"))

# ..........................................
# ..........................................
# split by trial
# first the community testing
k <- g$trial == "community"

Rs <- R[k, ]

PLm[[3]] <- PlackettLuce(Rs, alpha = 0.05)

# now the home testing
Rs <- R[!k, ]

PLm[[4]] <- PlackettLuce(Rs, alpha = 0.05)

names(PLm)[3:4] <- c("ghana_community","ghana_home")

# ..........................................
# ..........................................
# Compare methods ####

# Uganda
# get the probability of winning and than set back as log
comp <- data.frame(centralised = log(coefficients(PLm[[1]], ref = "Naspot 8", log = FALSE)),
                   home        = log(coefficients(PLm[[2]], ref = "Naspot 8", log = FALSE)))

comp
diff <- comp$centralised - comp$home
aver <- rowMeans(comp[,c("centralised", "home")])

r <- lm(centralised ~ home, data = comp)

coef <- coefficients(r)

# the mean of the difference
d <- mean(diff)

# the standard deviation of the difference
s <- sd(diff)

# limits of agreement
llim <- d - (2 * s)
ulim <- d + (2 * s)

plot(aver, diff, ylim = c(-0.5, 0.5), xlim = c(-1.5, 0))
abline(h = mean(diff), col = "red")
abline(h = llim, lty = 2, lwd=2)
abline(h = ulim, lty = 2, lwd=2)

# get the exponential of the limits so we know
# the proportion of difference in measuring 
# in centralised and in home trials
1 - (10 ^ llim)
(10 ^ ulim) - 1

# Ghana
# get the probability of winning and than set back as log
comp <- data.frame(centralised = log(coefficients(PLm[[3]], ref = "Naspot 8", log = FALSE)),
                   home        = log(coefficients(PLm[[4]], ref = "Naspot 8", log = FALSE)))

comp
diff <- comp$centralised - comp$home
aver <- rowMeans(comp[,c("centralised", "home")])

# the mean of the difference
d <- mean(diff)

# the standard deviation of the difference
s <- sd(diff)

# limits of agreement
llim <- d - (2 * s)
ulim <- d + (2 * s)

plot(aver, diff, xlim = c(-2, -.5), ylim = c(-0.6, 0.5))
abline(h = mean(diff), col = "red")
abline(h = llim, lty = 2, lwd=2)
abline(h = ulim, lty = 2, lwd=2)

# get the exponential of the limits so we know
# the proportion of difference in measuring 
# in centralised and in home trials
1 - (10 ^ llim)
(10 ^ ulim) - 1

# ..........................................
# ..........................................
# Plot coefficients by country and trial ####
multpl <- list()
multpl[[1]] <- multcompPL(PLm[[1]], ref = "Naspot 8")
multpl[[2]] <- multcompPL(PLm[[2]], ref = "Naspot 8")
multpl[[3]] <- multcompPL(PLm[[3]], ref = "Apomuden")
multpl[[4]] <- multcompPL(PLm[[4]], ref = "Apomuden")

# this is to get the lims for each plot by country
u <- rbind(multpl[[1]], multpl[[2]])
umax <- round(max(u$estimate + qnorm(1 - (1 - 0.95) / 2) * u$quasiSE) * 100, -1) / 100 + 0.1
umin <- round(min(u$estimate - qnorm(1 - (1 - 0.95) / 2) * u$quasiSE) * 100, -1) / 100 - 0.1

g <- rbind(multpl[[3]], multpl[[4]])
gmax <- round(max(g$estimate + qnorm(1 - (1 - 0.95) / 2) * g$quasiSE) * 100, -1) / 100 + 0.1
gmin <- round(min(g$estimate - qnorm(1 - (1 - 0.95) / 2) * g$quasiSE) * 100, -1) / 100 - 0.1

# now the define the factors so it is show equally in each plot
# the reference will be the community testing in each country
ufact <- rev(as.character(multpl[[1]]$term))
gfact <- rev(as.character(multpl[[3]]$term))

plots <- list()

for (i in seq_along(multpl)){
  
  # get the table
  o <- multpl[[i]]
  
  # set the lims for the x axis
  if(i < 3) {
    pmin <- umin
    pmax <- umax
  }
  
  if(i > 2) {
    pmin <- gmin
    pmax <- gmax
  }
  
  # and set the factors
  if(i < 3) {
    o$term <- factor(o$term, levels = ufact)
  }
  
  if(i > 2) {
    o$term <- factor(o$term, levels = gfact)
  }
  
  plots[[i]] <- 
  ggplot(data = o,
         aes(x = estimate, 
             y = term,
             label = group, 
             xmax = estimate + qnorm(1 - (1 - 0.95) / 2) * quasiSE,
             xmin = estimate - qnorm(1 - (1 - 0.95) / 2) * quasiSE)) +
    
    geom_vline(xintercept = 0, 
               colour = "#E5E7E9", size = 0.8) +
    geom_errorbar(width = 0.1, col = "grey30") +
    geom_point(col = "grey20") +
    labs(x = "", y = "") +
    geom_text(vjust = -0.5, col = "grey20") +
    scale_x_continuous(limits = c(pmin, pmax)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 11, color = "grey20", face = 2),
          axis.title = element_text(size = 12, color = "grey20", face = 2))
  
}

plots[[3]] <- 
  plots[[3]] +
  labs(x = "Estimate", y = "Genotype")

p <- 
  plots[[1]] + plots[[2]] +  plots[[3]] + plots[[4]] +
  plot_layout(heights = c(1,2)) +
  plot_annotation(tag_levels = "A")

p

output <- "output/model_estimates/"
dir.create(output, showWarnings = FALSE, recursive = TRUE)

ggsave(paste0(output, "model_estimates.png"),
       p, 
       width = 10,
       height = 10,
       dpi = 500)

# compute the correlation between estimates
# order the items so it will be the same 
multpl <- lapply(multpl, function(x){
  x$term <- as.character(x$term)
  x[order(x$term),]
})

cor(multpl[[1]]$estimate, multpl[[2]]$estimate)
cor(multpl[[3]]$estimate, multpl[[4]]$estimate)

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

p

output <- "output/favourability"
dir.create(output, showWarnings = FALSE, recursive = TRUE)

ggsave(paste0(output, "/favourability_score.png"),
       plot = p, 
       width = 7,
       height = 7,
       dpi = 800)

# ..........................................
# ..........................................
# Text analysis #####




