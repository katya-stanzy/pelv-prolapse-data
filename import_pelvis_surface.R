library(rjson)

import_json <- function(path_to_file) {
  JsonData <- fromJSON(file = path_to_file)
  json_points<-JsonData$markups[[1]]$controlPoints
  a_matrix <- matrix(ncol = 3, nrow = length(json_points))  
  names = c()
  for (i in 1:length(json_points)) {
    point <- json_points[[i]]
    point_name<-point$label
    point_position<-point$position
    a_matrix[i,] <- point_position
    names <- append(names, point_name)}
  return(list(names, a_matrix))
  }

pelvis_axial <- import_json('pelvis/axial.mrk.json')
pelvis_sagittal <- import_json('pelvis/sagittal.mrk.json')
pelvis_ischiopubic <- import_json('pelvis/ischiopubic.mrk.json')

mat1<-rbind(pelvis_axial[[2]], pelvis_sagittal[[2]], pelvis_ischiopubic[[2]])
pelvis_bone <- mat1[bone_nums,]

pelvis_surf_template <- pelvis_bone
pelvis_mesh_template <- read.ply("pelvis/pelvis_small.ply", ShowSpecimen = F) 

"open3d()
plot3d(pelvis_surf_template, col = 'blue', size = 8)
shade3d(pelvis_mesh_template, alpha = 0.2)
aspect3d('iso')"

