# Read, combine and clean sweetpotato trial data
# ..........................................
# ..........................................

## Packages ####
library("readxl")
library("janitor")

# ..........................................
# ..........................................
# Read the data ####
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
l <- list(dt1, dt2)
nm <- union(names(dt2), names(dt1))

dt <- data.frame(matrix(NA, 
                        ncol = length(nm),
                        nrow = 0,
                        dimnames = list(NULL, nm)))

for (i in seq_along(l)) {

  # take the data
  x <- l[[i]]
  
  # select the variable available in the data
  in_x <- nm %in% names(x)
  
  # if any missing variable, then add it as NAs
  if (any(!in_x)) {
    
    miss <- nm[!in_x]
    
    miss <- data.frame(matrix(NA, 
                              ncol = length(miss),
                              nrow = nrow(x),
                              dimnames = list(1:nrow(x), miss)))
    
    x <- cbind(x, miss)
    
  }
  
  x <- x[, nm]
  
  # bind with the main data
  dt <- rbind(dt, x)
}

rm(dt1, dt2, l, x, miss, i, in_x, nm)

dt <- as.data.frame(as.matrix(dt))

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

dt <- dt[,union(c("id","country","district","gender"), names(dt))]

#...............................................
#...............................................
#...............................................
# Clean Ghana data #####

write.csv(dt, "data/spotato_data.csv", row.names = FALSE)
