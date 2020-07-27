# ..........................................
# ..........................................
# Text analysis #####
library("tidyverse")

# write session info
sessioninfo::session_info()
capture.output(sessioninfo::session_info(),
               file = "script/session_info/03_text_analysis.txt")

# ..........................................
# ..........................................
# Read data ####
dt <- read.csv("data/spotato_data.csv")

head(dt)

sel <- c("id","country","district","gender","trial",
         "item_A","item_B","item_C",
         "best_overall","worst_overall",
         "best_describe","worst_describe")

dt <- dt[,sel]

length(unique(dt$id))

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text

library(dplyr)
text_df <- tibble(line = 1:4, text = text)

text_df

str(text_df)

library(tidytext)

text_df %>%
  unnest_tokens(word, text)
