library(StatsBombR)
library(dplyr)
library(SimilarityMeasures)
library(dtw)
library(apcluster)
library(ggsoccer)
library(ggtext)
library(ggplot2)

comps <- FreeCompetitions() %>% filter(competition_id == 11 & season_id != 278)

matches <- FreeMatches(comps)

events <- free_allevents(matches) %>%
  arrange(period, timestamp) %>%
  mutate(pos_id = paste0(match_id, "-", possession))

clean_events <- events %>%
  filter(type.name %in% c("Pass", "Carry", "Shot")) %>%
  mutate(end_location = case_when(type.name == "Pass" ~ pass.end_location, type.name == "Carry" ~ carry.end_location, type.name == "Shot" ~ shot.end_location)) %>%
  mutate(location = gsub("c\\(|\\)", "", location), end_location = gsub("c\\(|\\)", "", end_location)) %>%
  separate(location, into = c("x", "y"), sep = ",", convert = TRUE) %>%
  separate(end_location, into = c("x_end", "y_end"), sep = ",", convert = TRUE) %>%
  mutate(y_rev = ifelse(y < 40, 80 - y, y), y_end_rev = ifelse(y_end < 40, 80 - y_end, y_end)) %>%
  filter((x >= 45 & x <= 75) | (x_end >= 45 & x_end <= 75), possession_team.name == "Barcelona") %>%
  arrange(pos_id, timestamp) %>%
  select(pos_id, timestamp, season_id, duration, team.name, player.name, type.name, x, y_rev, x_end, y_end_rev) 

dist_matrix_func <- function(szn_data) {
  clean_locs <- szn_data %>%
    select(pos_id, x, y_rev)
  
  list_data <- split(clean_locs, clean_locs$pos_id)
  list_data <- lapply(list_data, function(df) df[, -which(names(df) == "pos_id")])
  
  len <- length(list_data)
  dist_matrix <- matrix(0, nrow = len, ncol = len)
  
  for (i in 1:(len-1)) {
    s1 <- list_data[[i]]
    for (j in (i+1):len) {
      s2 <- list_data[[j]]
      output <- dtw(s1, s2)
      distance <- output$distance
      dist_matrix[i, j] <- distance
      dist_matrix[j, i] <- distance  
    }
  }
  
  return(dist_matrix)
}

dist_matrices <- list()

for (id in unique(clean_events$season_id)) {
  szn_data <- clean_events %>% filter(season_id == id)
  dist_matrices[[as.character(id)]] <- dist_matrix_func(szn_data)
}

pos_nums <- clean_events %>%
  group_by(season_id) %>%
  summarise(unique_pos_count = n_distinct(pos_id))

set.seed(123)

clust_labels_list <- list() 

for (szn in names(dist_matrices)) {
  dist_matrix <- dist_matrices[[szn]]
  
  clust_model <- apcluster(negDistMat(dist_matrix, r=2))
  
  clust_labels <- clust_model@clusters
  
  clust_labels_list[[szn]] <- clust_labels
}

longest_clust_label_list <- list()

for (szn in names(clust_labels_list)) {
  clust_labels <- clust_labels_list[[szn]]
  longest_clust_label_list[[szn]] <- clust_labels[which.max(sapply(clust_labels, length))]
}

pos_nums <- clean_events %>%
  distinct(pos_id, .keep_all = TRUE) %>%
  group_by(season_id) %>%
  mutate(row_number = row_number()) %>%
  ungroup() %>%
  select(season_id, pos_id, row_number)

use_nums <- data.frame(season_id = character(), pos_num = integer(), stringsAsFactors = FALSE)

for (i in 1:length(longest_clust_label_list)) {
  for (j in 1:length(longest_clust_label_list[i][[1]][[1]])) {
    pos_num <- longest_clust_label_list[i][[1]][[1]][j]
    season_id <- names(longest_clust_label_list)[i]
    use_nums <- rbind(use_nums, data.frame(season_id = season_id, pos_num = pos_num))
  }
}

use_nums$season_id <- as.numeric(use_nums$season_id)

pos_nums <- inner_join(pos_nums, use_nums, by = c("season_id", "row_number"="pos_num"))

ex_nums <- pos_nums %>% group_by(season_id) %>% filter(row_number() == 1)

viz_events_full <- clean_events %>% filter(pos_id %in% pos_nums$pos_id)
viz_events <- clean_events %>% filter(pos_id %in% ex_nums$pos_id)

most_common_players <- viz_events_full %>%
  group_by(season_id, player.name) %>%
  summarize(player_count = n()) %>%
  top_n(3, player_count) %>%
  arrange(season_id, desc(player_count)) %>%
  select(-player_count)

for(id in unique(viz_events$season_id)) {
  each_szn <- viz_events %>%
    filter(season_id == id)
  
  most_common <- most_common_players %>%
    filter(season_id == id)
  
  szn_name <- comps$season_name[which(comps$season_id == id)]
  last_year <- substr(szn_name, nchar(szn_name) - 3, nchar(szn_name))
  filename <- paste0(last_year, ".png")
  
  plot <- ggplot(each_szn)+
    annotate_pitch(dimensions = pitch_statsbomb, colour = "white",
                   fill = "#3ab54a") + 
    theme_pitch() +
    geom_segment(data = subset(each_szn, type.name == "Pass"),
                 aes(x = x, y = y_rev, xend = x_end, yend = y_end_rev),
                 arrow = arrow(length = unit(0.15, "cm"), type = "closed"), colour = "blue", size = 1) +
    geom_segment(data = subset(each_szn, type.name == "Carry"),
                 aes(x = x, y = y_rev, xend = x_end, yend = y_end_rev),
                 arrow = arrow(length = unit(0.15, "cm"), type = "closed"), colour = "black", size = 1) +
    geom_segment(data = subset(each_szn, type.name == "Shot"),
                 aes(x = x, y = y_rev, xend = x_end, yend = y_end_rev),
                 arrow = arrow(length = unit(0.15, "cm"), type = "closed"), colour = "red", size = 1) +
    geom_segment(data = subset(each_szn, type.name == "Shot"),
                 aes(x = 50, y = 20, xend = 50, yend = 15),
                 arrow = arrow(length = unit(0.15, "cm"), type = "closed"), colour = "blue", size = 1) +
    geom_segment(data = subset(each_szn, type.name == "Shot"),
                 aes(x = 45, y = 20, xend = 45, yend = 15),
                 arrow = arrow(length = unit(0.15, "cm"), type = "closed"), colour = "black", size = 1) +
    theme_pitch() +
    scale_color_manual(values = c("Pass" = "blue", "Carry" = "green", "Shot" = "red")) +
    ggtitle(paste("Common FCB Midfield Playstyle: ", szn_name)) + 
    coord_flip(xlim = c(45, 75)) +
    scale_y_reverse() +
    labs(caption = "Amrit Vignesh | **@avsportsanalyst** | Anirudh Srinivasan | Data from **StatsBomb**") + 
    theme(plot.title = element_text(hjust = 0.5), plot.caption = element_markdown(hjust = 0.5)) +
    annotate("text", x = 73, y = 20, label = "Most Common Players Involved in Sequence:", size = 3, color = "blue") +
    annotate("text", x = 72, y = 20, label = paste("1. ", most_common$player.name[1]), size = 3, color = "blue") +
    annotate("text", x = 71, y = 20, label = paste("2. ", most_common$player.name[2]), size = 3, color = "blue") +
    annotate("text", x = 70, y = 20, label = paste("3. ", most_common$player.name[3]), size = 3, color = "blue") +
    annotate("text", x = 50, y = 10, label = "Pass", size = 3, color = "red") + 
    annotate("text", x = 45, y = 10, label = "Carry", size = 3, color = "red")
  ggsave(filename, plot)
}

