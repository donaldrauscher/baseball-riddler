library(dplyr)
library(tidyr)
library(ggplot2)

# build outcomes for a single round robin (10 games, 4 games for each team)
games <- combn(5, 2)
outcomes <- do.call(expand.grid, lapply(1:10, function(x) 0:1))
outcomes <- as.data.frame(do.call(cbind, lapply(1:10, function(x) games[outcomes[,x]+1,x])))
names(outcomes) <- paste("Game", 1:10, sep = ".")

for (i in 1:5){
  outcomes[[paste("T", i, ".Count", sep = "")]] <- apply(outcomes[,1:10], 1, function(x) sum(ifelse(x == i, 1, 0)))  
}

outcomes <- outcomes %>%
  group_by(T1.Count, T2.Count, T3.Count, T4.Count, T5.Count) %>%
  summarise(n = n())

outcomes$p <- outcomes$n / sum(outcomes$n)
outcomes$Outcome.Id <- 1:nrow(outcomes)

# for a given state, determine possible future states after another round robin
get_state_changes <- function(key, base, t1, t2, t3, t4, t5, p){
  
  # start with outcomes and increment
  x <- outcomes
  x$T1.Count <- x$T1.Count + t1
  x$T2.Count <- x$T2.Count + t2
  x$T3.Count <- x$T3.Count + t3
  x$T4.Count <- x$T4.Count + t4
  x$T5.Count <- x$T5.Count + t5
  
  # order wins from greatest to least (reduces dimensionality)
  y <- x %>%
    select(-n, -p) %>%
    gather(Team, Wins, -Outcome.Id) %>%
    mutate(Rank = 1) %>%
    group_by(Outcome.Id) %>%
    arrange(-Wins) %>%
    mutate(Rank = cumsum(Rank)) %>%
    mutate(Rank = paste("t", Rank, sep = "")) %>%
    select(Outcome.Id, Rank, Wins) %>%
    spread(Rank, Wins) %>%
    ungroup() %>%
    arrange(Outcome.Id) %>%
    select(-Outcome.Id)
  
  x <- cbind(x, y)

  x <- x %>%
    group_by(t1, t2, t3, t4, t5) %>%
    summarise(p = sum(p)) %>%
    ungroup()
  
  # add key, base, and probabilities
  x$key <- key
  x$base <- base
  x$p1 <- p
  x$p2 <- x$p
  x$p <- x$p1 * x$p2
  
  return(x) 
}

# do a bunch of round robins
state_changes <- NULL
rounds <- 19

for (i in 1:rounds){
  
  cat("Beginning round robin ", i, "...\n")
  
  if (i == 1){
    states <- data.frame(t1 = 0, t2 = 0, t3 = 0, t4 = 0, t5 = 0, p = 1)
  }
  
  # create base and keys
  states$base <- apply(states[,1:5], 1, min)
  states$key <- apply(states[,1:5] - states$base, 1, function(x) paste(x, collapse = "_"))
  
  # seperate into two pieces: one with known state changes, one for which we need to generate state changes
  states1 <- states %>% filter(!(key %in% state_changes$key))
  states2 <- states %>% filter(key %in% state_changes$key)
  
  cat("Started with", nrow(states), "states; need to generate state changes for", nrow(states1), "of the states; known state changes for", nrow(states2), "of the states.\n")
  cat("Generating new state changes...")
  
  # get state changes for new states 
  states3 <- states1 %>% 
    rowwise() %>% 
    do(get_state_changes(.$key, .$base, .$t1, .$t2, .$t3, .$t4, .$t5, .$p)) %>%
    ungroup() %>%
    group_by(key, base, p1, t1, t2, t3, t4, t5) %>% 
    summarise(p = sum(p), p2 = sum(p2)) %>%
    ungroup()
  
  # add new state changes
  state_changes_new <- states3
  state_changes_new[,c("t1", "t2", "t3", "t4", "t5")] <-   state_changes_new[,c("t1", "t2", "t3", "t4", "t5")] - state_changes_new$base
  state_changes_new <- state_changes_new %>%
    select(key, t1, t2, t3, t4, t5, p2) %>%
    rename(p = p2)
  
  if (is.null(state_changes)){
    state_changes <- state_changes_new
  } else {
    state_changes <- rbind(state_changes, state_changes_new)
  }

  states <- states3 %>% select(t1, t2, t3, t4, t5, p)
  
  cat("done.\n")
  cat("Pulling in known state changes...")  
  
  # get state changes for states we've already seen
  if (nrow(states2) > 0){
    states4 <- states2 %>%
      select(key, base, p) %>% 
      rename(p1 = p) %>%
      inner_join(state_changes %>% rename(p2 = p), by = "key") %>%
      mutate(
        t1 = t1 + base,
        t2 = t2 + base,
        t3 = t3 + base,
        t4 = t4 + base,
        t5 = t5 + base,
        p = p1 * p2
      ) %>%
      select(t1, t2, t3, t4, t5, p)
    
    states <- rbind(states, states4)
  }

  cat("done.\n")
  cat("Reducing dimensionality if we can...")  
    
  # reduce if we can
  states <- states %>%
    group_by(t1, t2, t3, t4, t5) %>% 
    summarise(p = sum(p)) %>%
    ungroup()

  cat("done.\n")
  
}

# generating cdf for 1st place record based on remaining out-of-division games (which are independent)
first.dist <- data.frame(wins = min(states$t1):162)
first.dist$cdf <- NA

for (i in first.dist$wins){
  
  states <- states %>% 
    mutate(cdf = p*pbinom(i - t1, 86, 0.5)*pbinom(i - t2, 86, 0.5)*pbinom(i - t3, 86, 0.5)*pbinom(i - t4, 86, 0.5)*pbinom(i - t5, 86, 0.5))

  first.dist$cdf[match(i, first.dist$wins)] <- sum(states$cdf) 
  
}

# convert into pmf and generate mean
first.dist$pmf <- first.dist$cdf - c(0, head(first.dist$cdf, -1))

cat("Average number of wins for first place is ", with(first.dist, round(sum(pmf * wins), 1)), ".\n", sep = "")

ggplot(first.dist) + 
  geom_bar(aes(x = wins, y = pmf), stat = "identity", width = 1) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "# of Wins", y = "Probability") + 
  ggtitle("Division 1st Place Wins Distribution") + 
  scale_x_continuous(limits = c(60, 120))
