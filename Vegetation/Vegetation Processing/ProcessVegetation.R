library(dplyr)
library(tidyr)


raw_vegetation <- read.csv("/Users/maxgotts/Desktop/MPALA/Vegetation/Vegetation\ Processing/RawVegetation.csv")
raw_vegetation$X <- NULL
raw_vegetation$Forb.height <- as.numeric(raw_vegetation$Forb.height)

stats_df_cols <- c("Date",	"Loop",	"Metadata file",	"GPS.x",	"GPS.y",	"Latitude",	"Longitude",	"Animal species",	"% Cover",	"% Leaf",	"% Green",	"% Seeds",	"% Forbs",	"Average Height",	"SD Height",	"CV Height",	"Average Hits/Pin",	"Average Leaves/Pin",	"Sp. Diversity")
stats_df <- as.data.frame(matrix(NA,ncol=length(stats_df_cols),nrow=0))
colnames(stats_df) <- stats_df_cols


raw_row <- 1
stats_row <-1

while(raw_row < nrow(raw_vegetation)) {
  file_name <- raw_vegetation$Filename[raw_row]
  if (is.na(file_name)) break
  
  df <- raw_vegetation %>% filter(`Filename` == toString(file_name)) 
  df_sub <- df %>% dplyr::select(12:17, 19:21)
  df_sub[is.na(df_sub)] <- 0
  
  stats_df[stats_row, 8] <- df[1, 8]
  
  brnst <- df_sub %>% dplyr::select("Brown.stem") %>% filter(`Brown.stem` != 0)
  sum_brnst <- 0
  count_brnst <- nrow(brnst)
  if (count_brnst != 0) {
    sum_brnst <- sum(brnst)
  }
  
  grnst <- df_sub %>% dplyr::select("Green.stem") %>% filter(`Green.stem` != 0)
  sum_grnst <- 0
  count_grnst <- nrow(grnst)
  if (count_grnst != 0) {
    sum_grnst <- sum(grnst)
  }
  
  brnlf <- df_sub %>% dplyr::select("Brown.leaf") %>% filter(`Brown.leaf` != 0)
  sum_brnlf <- 0
  count_brnlf <- nrow(brnlf)
  if (count_brnlf != 0) {
    sum_brnlf <- sum(brnlf)
  }
  
  grnlf <- df_sub %>% dplyr::select("Green.leaf") %>% filter(`Green.leaf` != 0)
  sum_grnlf <- 0
  count_grnlf <- nrow(grnlf)
  if (count_grnlf != 0) {
    sum_grnlf <- sum(grnlf)
  }
  
  seed <- df_sub %>% dplyr::select("Seed") %>% filter(`Seed` != 0)
  count_seed <- nrow(seed)
  sum_seed <- 0
  if (count_seed != 0) {
    sum_seed <- sum(seed)
  } 
  
  brnfrb <- df_sub %>% dplyr::select("Brown.forb") %>% filter(`Brown.forb` != 0)
  count_brnfrb <- nrow(brnfrb)
  sum_brnfrb <- 0
  if (count_brnfrb != 0) {
    sum_brnfrb <- sum(brnfrb)
  }
  
  grnfrb <- df_sub %>% dplyr::select("Green.forb") %>% filter(`Green.forb` != 0)
  sum_grnfrb <- 0
  count_grnfrb <- nrow(grnfrb)
  if (count_grnfrb != 0) {
    sum_grnfrb <- sum(grnfrb)
  }
  
  height <- df_sub %>% dplyr::select("Max.height") %>% filter(`Max.height` != 0)
  sum_height <- 0
  count_height <- nrow(height)
  if (count_height != 0) {
    sum_height <- sum(height)
  }
  
  hits_pin <- as.data.frame(rowSums(df_sub)) %>%
    rename('sums'= "rowSums(df_sub)")
  num_zeros <- hits_pin %>% 
    filter(`sums` == 0) %>% 
    nrow()
  sum_hits_pin <- hits_pin %>% 
    sum(hits_pin)
  count_hits_pin <- hits_pin %>%
    nrow()
  
  sum_parts <-  
    sum(sum_brnfrb, sum_brnlf, sum_brnst, sum_grnfrb, sum_grnlf, sum_grnst, sum_seed)
  count_parts <- 
    sum(count_brnfrb, count_brnlf, count_brnst, count_grnfrb, count_grnlf, count_grnst, count_seed)
  
  sum_grass <- 
    sum(sum_brnlf, sum_grnlf, sum_brnst, sum_grnst, sum_seed)
  count_grass <- hits_pin %>% 
    sum(count_brnlf, count_grnlf, count_brnst, count_grnst, count_seed)
  
  sum_green <- 
    sum(sum_grnlf, sum_grnst, sum_grnfrb)
  sum_leaf <- 
    sum(sum_grnlf, sum_brnlf)
  
  # STATS
  stats_df$`% Cover`[stats_row] <- (1 - (num_zeros/count_hits_pin))
  stats_df$`% Leaf`[stats_row] <- sum_leaf/sum_grass
  stats_df$`% Green`[stats_row] <- sum_green/sum_parts
  stats_df$`% Seeds`[stats_row] <- sum_seed/sum_grass
  stats_df$`% Forbs`[stats_row] <- (sum_brnfrb + sum_grnfrb)/sum_parts
  stats_df$`Average Height`[stats_row] <- mean(df_sub$`Max.height`)
  stats_df$`SD Height`[stats_row] <- sd(df_sub$`Max.height`)
  stats_df$`CV Height`[stats_row] <- sd(df_sub$`Max.height`)/mean(df_sub$`Max.height`)
  stats_df$`Average Hits/Pin`[stats_row] <- sum_hits_pin/(count_hits_pin - num_zeros)
  stats_df$`Average Leaves/Pin`[stats_row] <- sum_leaf/(count_brnlf + count_grnlf)
  
  stats_df[stats_row, 1:2] <- df[1, 1:2]
  stats_df[stats_row, 3] <- df[1, 9]
  stats_df[stats_row, 4:7] <- df[1, 3:6]
  
  raw_row <- raw_row + nrow(df)
  stats_row <- stats_row + 1
}

write.csv(stats_df, "/Users/maxgotts/Desktop/MPALA/Vegetation/ConvertedVegetation.csv", row.names = FALSE)

if (FALSE) {
  source('~/Desktop/MPALA/Vegetation/Vegetation Processing/ProcessVegetation.R')
}
