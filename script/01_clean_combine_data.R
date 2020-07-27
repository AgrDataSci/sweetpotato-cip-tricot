# Read, combine and clean sweetpotato trial data
# ..........................................
# ..........................................

## Packages ####
library("tidyverse")
library("readxl")
library("janitor")
library("gosset")


sessioninfo::session_info()
# write session info
capture.output(sessioninfo::session_info(),
               file = "script/session_info/01_clean_combine_data.txt")

# ..........................................
# ..........................................
# Read the Uganda data ####
list.files("data/raw/")

# community tasting trial
dt1 <- read_xls("data/raw/uganda_community_tasting.xls",
               na = c("NA","Invalid response","99"))

names(dt1) <- make_clean_names(names(dt1))

dt1$trial <- "community"

# home tasting 
dt2 <- read_xls("data/raw/uganda_home_tasting.xls",
               na = c("NA","Invalid response","99"))

names(dt2) <- make_clean_names(names(dt2))

dt2 <- dt2[,-which(grepl("hh_id", names(dt2)))]

dt2$trial <- "home"

# put both data together
dt <- rowbind(dt1, dt2)

dt <- as.data.frame(dt)

names(dt)

# Organise colnames as required by ClimMob
names(dt)[names(dt)=="best_code"] <- "best_overall"
names(dt)[names(dt)=="worst_code"] <- "worst_overall"
names(dt)[names(dt)=="name_a"] <- "item_A"
names(dt)[names(dt)=="name_b"] <- "item_B"
names(dt)[names(dt)=="name_c"] <- "item_C"

# Gender
summary(as.factor(dt$gender))

dt$gender <- ifelse(dt$gender == "Female", "Woman", 
                    ifelse(dt$gender == "Male", "Man", dt$gender))

summary(as.factor(dt$gender))

# Check item names
unique(unlist(dt[,paste0("item_", LETTERS[1:3])]))

# remove equal best and worst
out <- dt$best_overall == dt$worst_overall
sum(out, na.rm = TRUE)
dt$best_overall[out] <- NA
dt$worst_overall[out] <- NA

# Now fix the answers as required by tricot 
# Here the answers are set as Yes/No based on the reference item selected as best/worst 
# for overall appreciation
# the answer should be A, B or C
charpattern <- c("_color","_taste")

for(i in seq_along(charpattern)) {
  index <- charpattern[i]
  b <- paste0("best", index)
  w <- paste0("worst", index)
  
  dt[,b] <- ifelse(dt[,b] == "Yes", dt$best_overall, dt[,b])
  dt[,w] <- ifelse(dt[,w] == "Yes", dt$worst_overall, dt[,w])
  
  overall <- dt[,c("best_overall", "worst_overall")]
  
  nr <- nrow(overall)
  
  for(j in seq_len(nr)){
    # the overall best and worst for that observer
    opt <- as.vector(t(overall[j,]))
    # the answer for the characteristic for that observer
    bw <- as.vector(t(dt[j,c(b,w)]))
    # if NA, then all is NA
    if (any(is.na(opt))) {
      dt[j, c(b,w)] <- c(NA, NA)
      dt[j,c("best_overall", "worst_overall")] <- c(NA, NA)
      next
    }
    # if both No then this require a reordering
    # the best will be the one not mentioned here
    # the middle will be the one placed as worst in overall
    # the worst will be the one placed as best in overall
    if (all(bw == "No")) {
      dt[j, b] <- LETTERS[1:3][!LETTERS[1:3] %in% opt]
      dt[j, w] <- opt[1]
    }
    
    # if the overall worst get No for that characteristic than it is placed and the middle one
    if (all((bw == "No") == c(FALSE, TRUE))) {
      dt[j, w] <- LETTERS[1:3][!LETTERS[1:3] %in% opt]
    }
    
    # if the overall best get No for that characteristic than it is placed and the middle one
    if (all((bw == "No") == c(TRUE, FALSE))) {
      dt[j, b] <- LETTERS[1:3][!LETTERS[1:3] %in% opt]
    }
    
  }
  
}

rm(w, out, opt, nr, index, i, j, charpattern, b, bw, overall)

# remove NAs in items names
items <- paste0("item_", LETTERS[1:3])
keep <- apply(dt[items], 1, is.na)
keep <- as.vector(colSums(keep) <= 1)

dt <- dt[keep,]

summary(as.factor(dt$district))

dt$country <- "Uganda"

# define columns to keep 
vars <- c("id", "country", "district", "gender", "trial",
          paste0("item_", LETTERS[1:3]),
          "best_overall","worst_overall","best_taste","worst_taste",
          "best_color","worst_color","best_describe","worst_describe")

# keep only selected columns
uganda <- dt[, vars]

rm(dt, dt1, dt2, items, vars, keep)

#...............................................
#...............................................
#...............................................
# Clean Ghana data #####

# ..........................................
# ..........................................
# Read the data ####
list.files("data/raw/")

# community tasting trial
l <- list()

# get sheet names
sheets <- excel_sheets("data/raw/ghana_community_tasting.xlsx")[1:2]

dt <- list()
# now read the files for the community tasting
for (i in seq_along(sheets)){
  
  # read the data
  x <- read_excel("data/raw/ghana_community_tasting.xlsx",
                   sheet = sheets[[i]], 
                   na = c("No response", " "))
  
  names(x) <- make_clean_names(names(x))
  
  x <- as.data.frame(x)
  
  codes <- x[,c("codes", "geno")]
  codes <- na.omit(codes)
  
  x <- x[, !names(x) %in% c("codes", "geno")]
  
  # replace codes by the genotypes/varieties names
  vars <- c("var1", "var2", "var3", "best_sp", "worst_sp")
  
  for(j in seq_along(vars)){
    
    for(k in seq_along(codes$codes)) {
      
      x[, vars[j]] <- ifelse(x[, vars[j]] == codes$codes[k], 
                              codes$geno[k],
                              x[, vars[j]])
      
    }
    
  }
  
  x$geno_test <- sheets[[i]] 
  
  dt[[i]] <- x
  
}

dt <- rowbind(dt)

names(dt)

names(dt) <- c("id","district", "community","gender","age",
               paste0("item_", LETTERS[1:3]), "best_overall","worst_overall","geno_test")

dt$trial <- "community"

names(dt)

dt1 <- dt

# now read the home trial
# get sheet names
sheets <- excel_sheets("data/raw/ghana_home_tasting.xlsx")[1:2]

dt2 <- list()
# now read the files for the community tasting
for (i in seq_along(sheets)){
  
  # read the data
  x <- read_excel("data/raw/ghana_home_tasting.xlsx",
                  sheet = sheets[[i]], 
                  na = c("No response", " "))
  
  names(x) <- make_clean_names(names(x))
  
  x <- as.data.frame(x)
  
  codes <- x[,c("codes", "geno")]
  codes <- na.omit(codes)
  
  x <- x[, !names(x) %in% c("codes", "geno")]
  
  # replace codes by the genotypes/varieties names
  vars <- c("option_1", "option_2", "option_3", "best_variety", "worst_variety")
  
  for(j in seq_along(vars)){
    
    for(k in seq_along(codes$codes)) {
      
      x[, vars[j]] <- ifelse(x[, vars[j]] == codes$codes[k], 
                             codes$geno[k],
                             x[, vars[j]])
      
    }
    
  }
  
  x$geno_test <- sheets[[i]] 
  
  dt2[[i]] <- x
  
}

names(dt2[[1]])
names(dt2[[2]])

dt2 <- rowbind(dt2)

names(dt2)

names(uganda)

dt2 <- dt2[, !names(dt2) %in% c("no_of_household", "name", "attributes", "atributes")]

names(dt2)

names(dt2) <- c("id","age","gender","region","district", 
                paste0("item_", LETTERS[1:3]), "best_overall","best_describe", 
                "worst_overall","worst_describe", "geno_test")


dt2$trial <- "home"

# combine the two datasets
dt <- rowbind(dt1, dt2)


# now put back the answers as LETTERS (ABC) so it can match with the Uganda data
n <- dim(dt)[[1]]

for(i in seq_len(n)) {
  # find the letter for which best
  wb <- which(dt[i, paste0("item_", LETTERS[1:3])] %in% dt[i,"best_overall"])
  ww <- which(dt[i, paste0("item_", LETTERS[1:3])] %in% dt[i,"worst_overall"])
  
  if(any(length(wb) == 0, length(ww) == 0)){
    dt[i,"best_overall"] <- NA
    dt[i,"worst_overall"] <- NA
    next
  } 
  
  dt[i,"best_overall"] <- LETTERS[wb]
  dt[i,"worst_overall"] <- LETTERS[ww]
}

head(dt)


# fix geno_test
unique(dt$geno_test)

dt$geno_test <- ifelse(grepl("AT", dt$geno_test),
                       "advanced_trial","variety_trial")

dt$country <- "Ghana"

names(dt)
names(uganda)


# put Uganda and Ghana data together
dt <- rowbind(uganda, dt)

# remove NAs in items names
items <- paste0("item_", LETTERS[1:3])
keep <- apply(dt[items], 1, is.na)
keep <- as.vector(colSums(keep) <= 1)

sum(keep)

dt <- dt[keep, ]

# remove NAs in evaluator response
dt <- dt[!is.na(dt$best_overall), ]
dt <- dt[!is.na(dt$worst_overall), ]

summary(as.factor(dt$best_overall))
summary(as.factor(dt$worst_overall))

dt$gender <- ifelse(dt$gender == "F", "Woman", 
                    ifelse(dt$gender == "M", "Man", 
                           ifelse(dt$gender == "Female", "Woman", 
                                  ifelse(dt$gender == "Male", "Man", dt$gender))))

summary(as.factor(dt$gender))

head(dt)

summary(as.factor(dt$country))
summary(as.factor(dt$community))
summary(as.factor(dt$district))

# Standardize the names of varieties

vars <- sort(unique(unlist(dt[, paste0("item_", LETTERS[1:3])])))

vars 

dt[, paste0("item_", LETTERS[1:3])] <- 
  lapply(dt[, paste0("item_", LETTERS[1:3])], function(x){
  x[x == "APOMUDEN"] <- "Apomuden"
  x[x == "LIGRI"] <- "Ligri"
  x[x == "NAN"] <- "Nan"
  x[x == "OBARE"] <- "Obare"
  x[x == "PURPLE"] <- "Tu-Purple (Diedi)"
  x[x == "Naspot 10"] <- "Naspot 10 (Kabode)"
  x
})

vars <- sort(unique(unlist(dt[, paste0("item_", LETTERS[1:3])])))

vars


dt$id <- paste0(dt$id, dt$item_A, dt$item_B, dt$item_C, dt$country, 
                dt$district, dt$trial, dt$geno_test)
dt$id <- as.integer(as.factor(dt$id))

# remove duplicates
dt <- dt[!(duplicated(dt$id) | duplicated(dt$id, fromLast = TRUE)), ]

write.csv(dt, "data/spotato_data.csv", row.names = FALSE)
