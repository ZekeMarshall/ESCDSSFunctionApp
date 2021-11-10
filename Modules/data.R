# Read default suitability scores

tidy_scores_v1 <- read.csv(scores_path)

species <- unique(tidy_scores_v1$species) |> sort()
