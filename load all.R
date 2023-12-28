# Load packages
library(geomorph)
library(Morpho)
library(tidyverse)
library(readr)
library(rgl)


# -- a function to import one individual -- 
# -- it uses a path, file name and names for landmark sets to load ind. data, resample simphysis lms,
# -- reduce the number of sacral lms to 3 and collect data in a list of 7 elements

import_ind <- function(path_to_folder, csv_names, lm_set_names) {
  for (i in 1:length(lm_set_names)) {
    landmarks_df <- read_csv(paste(path_to_folder, csv_names[i], sep = "/"),
                             # id = 'label', 
                             show_col_types = FALSE, 
                             name_repair = "unique_quiet")
    if (colnames(landmarks_df)[1]=='...1') {
        landmarks_df <- rename(landmarks_df, 'label' = '...1')
    }
    assign(paste(lm_set_names[i], '_m', sep = ""), landmarks_df) #, as.matrix(landmarks_df[,2:4])
  }
  data <- list(axial=axial_m, sagittal=sagittal_m, ischiopubic=ischiopubic_m, right=interpolated_right_m, left=interpolated_left_m, central=interpolated_central_m, surface=projected_surface_semilandmarks_m)
}

# ---------------------------------------------------

# create a list of files inside each individual
csv_names = c("axial.csv", "sagittal.csv", "ischiopubic.csv", "interpolated_right.csv", "interpolated_left.csv", "interpolated_central.csv", "projected_surface_semilandmarks.csv")

# create a list of names without .csv
lm_set_names <- c()
for (i in 1:length(csv_names)) {
  lm_set_names <-append(lm_set_names, substr(csv_names[i], 1, nchar(csv_names[i])-4))
}

# create lists of individuals
cases_files = list.files("clean_data/cases")
control_files = list.files("clean_data/control")
ruby_files = list.files("clean_data/ruby")

# create case names
cases_names <- c()
for (i in 1:length(cases_files)) {
  cases_names <-append(cases_names, substr(cases_files[i], 1, 8))
}

# create control names
control_names <- c()
for (i in 1:length(control_files)) {
  control_names <-append(control_names, substr(control_files[i], 1, 8))
}

# create ruby names
ruby_names <- c()
for (i in 1:length(ruby_files)) {
  ruby_names <-append(ruby_names, substr(ruby_files[i], 1, nchar(ruby_files[i])))
}

# import cases
for (i in 1:length(cases_files)) {
  path <- paste("clean_data/cases", cases_files[i], sep = "/")
  assign(cases_names[i], import_ind(path, csv_names, lm_set_names))
}

# import controls
for (i in 1:length(control_files)) {
  path <- paste("clean_data/control", control_files[i], sep = "/")
  assign(control_names[i], import_ind(path, csv_names, lm_set_names))
}

# import ruby
for (i in 1:length(ruby_files)) {
  path <- paste("clean_data/ruby", ruby_files[i], sep = "/")
  assign(ruby_names[i], import_ind(path, csv_names, lm_set_names))
}
# landmark names for one individual
lm_tags = c('ax', 'sag', 'isch', 'r', 'l', 'c', 'surf')
ind <- OPII_191
p = 0
for (df in ind) {
  #print(df)
  p = p+1
  #print(lm_tags[p])
  lbls = df%>%pull('label')
  #print(lbls)
  assign(lm_tags[p], lbls)
}
lm_names <- list(ax=ax, sag=sag, isch=isch, r=r, l=l, c=c, surf=surf)

# landmark indices in one individual
lm_tags = c('ax', 'sag', 'isch', 'r', 'l', 'c', 'surf')
list_name <- OPII_191
tot = 0
for (i in 1:length(list_name )) {
    assign(lm_tags[i], seq(from = tot + 1, to = tot + dim(list_name[[i]])[1]))
    tot = tot + dim(list_name[[i]])[1]
  }
nums <- list(ax=ax, sag=sag, isch=isch, r=r, l=l, c=c, surf=surf)