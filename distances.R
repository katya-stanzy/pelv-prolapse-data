# dataframe, lm_names, nums

fem_heads_dist <-nums$ax[c(1,2)]
interac_dist <- nums$ax[c(3,4)]
isch_sp_dist <- nums$ax[c(5,6)]
inferSymph_coccx_dist <- nums$sag[c(2,4)]

dist_df <- data.frame(
  id = ind_names,
  groups = groups,
  fem_heads_dist = rep(0, 15),
  interac_dist = rep(0, 15),
  isch_sp_dist = rep(0, 15),
  inferSymph_coccx_dist = rep(0, 15)
)

for (i in 1:15) {
  dist_df$fem_heads_dist[i] = norm(dataframe[fem_heads_dist[1],,i] - dataframe[fem_heads_dist[2],,i], type = '2')
}

for (i in 1:15) {
  dist_df$interac_dist[i] = norm(dataframe[interac_dist[1],,i] - dataframe[interac_dist[2],,i], type = '2')
}

for (i in 1:15) {
  dist_df$isch_sp_dist[i] = norm(dataframe[isch_sp_dist[1],,i] - dataframe[isch_sp_dist[2],,i], type = '2')
}

for (i in 1:15) {
  dist_df$inferSymph_coccx_dist[i] = norm(dataframe[inferSymph_coccx_dist[1],,i] - dataframe[inferSymph_coccx_dist[2],,i], type = '2')
}

opal_meta_df <- read_csv("clean_data/sample_opal2.csv", show_col_types = FALSE, name_repair = "unique_quiet")
opal_meta_df <- opal_meta_df[-1]
opal_sample <- opal_meta_df[opal_meta_df$id %in% c('016', '044', '046', '060', '075', '089', '099', '104', '111', '191'),]

ruby_meta_df <- read_csv("clean_data/sample_rubi.csv", show_col_types = FALSE, name_repair = "unique_quiet")
ruby_meta_df <- ruby_meta_df[-1]
ruby_meta_df <- ruby_meta_df[-1,]
ruby_sample <- (ruby_meta_df[ruby_meta_df$id %in% c(7,8,21,23,93),])[1:4]

dist_df$age <- c(opal_sample$q_age.x, ruby_sample$age_yr)
dist_df$height <- c(opal_sample$height_cm, ruby_sample$height_cm)
dist_df$weight <- c(opal_sample$weight_kg, ruby_sample$weight_kg)
dist_df$births <- c(opal_sample$births.x, rep(0,5))
dist_df$la_defect_score <- c(opal_sample$final_la_defect_score, rep(0,5))

dist_df[is.na(dist_df)] <- 0



## plot bar chart
extract <- dist_df %>% gather(key = dimensions, value = Value, c(fem_heads_dist,interac_dist,isch_sp_dist,inferSymph_coccx_dist, age, height, weight))
ggplot(extract, aes(dimensions, Value, fill = groups)) + geom_col(position = "dodge")

ggplot_colors <- c("#F8766D", "#00BA38", "#619CFF")
palette(ggplot_colors)


## scatter plot with correlations
### Customize upper panel
palette(ggplot_colors)

upper.panel<-function(x, y){
    points(x,y, pch=19, cex = 1, col=as.factor(dist_df$groups))
    r <- round(cor(x, y), digits=2)
    txt <- paste0("R = ", r)
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    text(0.5, 0.9, txt)
  }

suppressWarnings({
  pairs(dist_df[,c(3:6,8,9,11)], lower.panel = NULL, 
      upper.panel = upper.panel)
  })
