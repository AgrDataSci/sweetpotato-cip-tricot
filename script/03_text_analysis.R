# ..........................................
# ..........................................
# Text analysis #####
library("tidyverse")
library("tidytext")
library("magrittr")
library("readxl")
library("janitor")

# write session info
sessioninfo::session_info()
capture.output(sessioninfo::session_info(),
               file = "script/session_info/03_text_analysis.txt")

output <- "output/sentiment_analysis/"
dir.create(output, showWarnings = FALSE, recursive = TRUE)

# read data
dt <- read_xlsx("data/sentiment_spotato.xlsx")

names(dt) <- make_clean_names(names(dt))

sentiment <- names(dt)[2:ncol(dt)]
sentiment <- data.frame(word = sentiment,
                        sentiment = c(rep("Best", 8),
                                      rep("Worst", 12)))


dt %<>%
  pivot_longer(-genotype, names_to = "word") %>%
  inner_join(., sentiment, by = "word")

dt %<>%
  mutate(word = gsub("_", " ", word),
         word = str_to_sentence(word)) %>%
  filter(value == 1)


count_dt <-
  dt %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

count_dt %<>%
  group_by(sentiment) %>%
  top_n(8) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

count_dt$word <- gsub(" nice", "",count_dt$word)
count_dt$word <- gsub(" smell", "",count_dt$word)
count_dt$word <- gsub(" hard heavy", "",count_dt$word)

count_dt$word <- factor(count_dt$word, levels = rev(count_dt$word))

p1 <- 
count_dt %>% 
  ggplot(aes(x = n, y = word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Frequency of sentiment",
       y = NULL) +
  scale_fill_manual(values=c("#92c5de", "#d73027")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text.x = element_text(size = 11),
        strip.background = element_rect(fill="#FFFFFF", 
                                        colour = "#FFFFFF"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11))

p1

ggsave(paste0(output, "classification_drivers.png"),
       plot = p1,
       width = 20,
       height = 10,
       dpi = 500,
       units = "cm")

# # ..........................................
# # ..........................................
# # Read data ####
dt2 <- read_csv("data/spotato_data.csv")

head(dt2)

sel <- c("id","country","district","gender","trial",
         "item_A","item_B","item_C",
         "best_overall","worst_overall",
         "best_describe","worst_describe")


dt2 <- dt2[,sel]

# get the names for the best and worst overall
dt2$best_overall <- ifelse(dt2$best_overall == "A", dt2$item_A,
                           ifelse(dt2$best_overall == "B", dt2$item_B,
                                  ifelse(dt2$best_overall == "C", dt2$item_C, NA)))



dt2$worst_overall <- ifelse(dt2$worst_overall == "A", dt2$item_A,
                          ifelse(dt2$worst_overall == "B", dt2$item_B,
                                 ifelse(dt2$worst_overall == "C", dt2$item_C, NA)))



# now organise the words for the sentiments for best
best <-
  tibble(text = dt2$best_describe,
         geno = dt2$best_overall) %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing"))

sort(unique(best$word))

# organise some words
best$word[best$word == "smell"] <- "aroma"
best$word[best$word == "odor"] <- "aroma"
best$word[best$word == "smells"] <- "aroma"
best$word[best$word == "smelling"] <- "aroma"
best$word[best$word == "goof"] <- "good"
best$word[best$word == "hard"] <- "firm"
best$word[best$word == "sweetness"] <- "sweet"

# remove some words that are not related to positive sentiment
rmv <- c("pale","bad","enough","upset","best","worth","better","like")

best <- best[!best$word %in% rmv, ]

sort(unique(best$word))

best$sentiment <- "Best"


# now the same for negative sentiment
worst <-
  tibble(text = dt2$worst_describe,
         geno = dt2$worst_overall) %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing"))

sort(unique(worst$word))


worst$word[worst$word == "sweetness"] <- "sweet"
worst$word[worst$word == "smells"] <- "smelly"
worst$word[worst$word == "smell"] <- "smelly"
worst$word[worst$word == "odor"] <- "smelly"

rmv <- c("best", "delicious","enough","favorite",
         "fresh","good","like","nice","pleasing",
         "ready","rich","satisfying","smooth","well",
         "lacks","rotten","worst","abundance","infested",
         "pest","stimulating")

worst <- worst[!worst$word %in% rmv, ]

sort(unique(worst$word))

worst$sentiment <- "Worst"


# now combine both
dt2 <- rbind(best, worst)


# get the proportion of best and worst evaluation
dt2 %>%
  count(geno, sentiment, sort = TRUE) %>%
  group_by(geno) %>% 
  mutate(p = n / sum(n),
         id = paste0(geno, sentiment)) %>% 
  ungroup() ->
  bw

# now get the top three words from best and worst
dt2 %>%
  count(geno, sentiment, word, sort = TRUE) %>% 
  group_by(geno, sentiment) %>% 
  top_n(3) %>% 
  arrange(desc(n)) %>% 
  summarise(s = paste(word, collapse = ", ")) %>% 
  ungroup() %>% 
  mutate(id = paste0(geno, sentiment)) ->
  t3

bw %<>% 
  select(-geno, -sentiment) %>% 
  inner_join(., t3, by = "id")

bw %>% 
  filter(sentiment == "Best") %>% 
  arrange(desc(p)) %>% 
  select(geno) ->
  lev

bw$geno <- factor(bw$geno, levels = c("PGN16203-18", rev(lev$geno)))

bw$sentiment <- factor(bw$sentiment, levels = c("Worst","Best"))

p2 <- 
ggplot(bw) +
  geom_bar(aes(x = p, y = geno, fill = sentiment), 
           stat = "identity",  show.legend = TRUE) +
  geom_text(data = bw[bw$sentiment == "Best", ],
            aes(x = 0, y = geno, label = s),
            hjust = 1,
            col = "grey20", 
            size = 2) +
  geom_text(data = bw[bw$sentiment == "Worst", ],
            aes(x = 1, y = geno, label = s),
            hjust = 0, 
            col = "grey20",
            size = 2) +
  scale_fill_manual(values = c("#d73027", "#92c5de"), name = "") +
  geom_vline(aes(xintercept = 0.5), col = "grey50") +
  labs(x = "Share of sentiment", y = "Genotype") +
  scale_x_continuous(expand = expansion(mult = c(0.5, 0.5)),
                     labels = c("","0%","50%","100%", " ")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.text.x = element_text(size = 10, colour = "grey20"),
        strip.background = element_rect(fill="#FFFFFF", 
                                        colour = "#FFFFFF"),
        axis.text = element_text(size = 10, colour = "grey20"),
        axis.title = element_text(size = 10, colour = "grey20"),
        legend.position = "bottom",
        legend.text = element_text(size = 9, colour = "grey20"))


ggsave(paste0(output, "sentiment_geno.png"),
       plot = p2,
       width = 17,
       height = 15,
       dpi = 500,
       units = "cm")
