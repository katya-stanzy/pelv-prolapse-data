# ALL DATA, SUPERIMPOSITION ON BONE

v <- c()
nu <- 0
for (i in 1:length(lm_names$sag)) {
  if ((lm_names$sag[[i]] == 'anus') | (lm_names$sag[[i]] =='urethra')) {
    nu <- nu + 1
    v[nu] <- nums$sag[[i]]
  }
}
bone_nums <- c(nums$ax, setdiff(nums$sag, v), nums$isch)

transforms <- array(numeric(15*4*4), dim=c(4,4,15))
for (i in 1:dim(gpa_results$coords)[3]) {
  ind <- gpa_results$coords[bone_nums,,i]
  transforms[,,i] <- computeTransform(gpa_results$consensus[bone_nums,], ind, type = 'rigid')
}

superimposed_to_bone <- gpa_results$coords
for (i in 1:15) {
  ind <- gpa_results$coords[,,i]
  superimposed_to_bone[,,i] <- applyTransform(ind, transforms[,,i])
}

# BONE ONLY
## PCA
pca_bone <- gm.prcomp(superimposed_to_bone[bone_nums,,])
pc1<-pca_bone$d[1]/(sum(pca_bone$d))
pc2<-pca_bone$d[2]/(sum(pca_bone$d))

### plot colour
data_df<-as_tibble(pca_bone$x, validate = NULL, .name_repair = NULL)

pc1<-format(round(pca_bone$d[1]/(sum(pca_bone$d))*100, 2), nsmall=2)
pc2<-format(round(pca_bone$d[2]/(sum(pca_bone$d))*100, 2), nsmall=2)
print(pc1)
print(pc2)

plot <- ggplot(data_df, aes(x=Comp1, y=Comp2, color = groups, label = ind_names))
plot +  geom_point(size = 3, aes(colour = groups)) + geom_text(hjust=0.5, vjust=1) + 
  labs(title = 'Principal Components - bone only', 
       x = paste0("PC1 (\"", pc1,"\"%)"), 
       y = paste0("PC2 (\"", pc2, "\"%)")
       ) 

### pc1
f1_min = pca_bone$shapes$shapes.comp1$min
f1_max = pca_bone$shapes$shapes.comp1$max

### pc2
f2_min = pca_bone$shapes$shapes.comp2$min
f2_max = pca_bone$shapes$shapes.comp2$max

### To Execte the code below, one needs to run 'import_pelvis_surface.R' first
### Plot PC1 with the bone surface

min_mesh <- tps3d(pelvis_mesh_template, pelvis_surf_template, f1_min, lambda = 1e-08, threads = 0)
min_mesh$material <- list(color = "blue", alpha = 0.2)
max_mesh <- tps3d(pelvis_mesh_template, pelvis_surf_template, f1_max, lambda = 1e-08, threads = 0)
max_mesh$material <- list(color = "magenta", alpha = 0.2)

#### plot pc1 - blue
open3d()
mfrow3d(1, 2, sharedMouse=FALSE)
view3d(theta = 0, phi = -70, fov = 20, zoom = .6)
plot3d(f1_min, col = 'blue', size = 8, axes = FALSE)
lines3d(f1_min[c((length(bone_nums) - length(nums$isch)+1):length(bone_nums)),], col="blue", lwd=5, add = TRUE)
spheres3d(f1_min[nums$ax[1:2],], col = 'blue', radius = 0.01, add = TRUE)
lines3d(f1_min[nums$ax[9:11],], col = 'blue', lwd=5, add = TRUE)
lines3d(f1_min[c(16,17,18),], col = 'blue', lwd=5, add = TRUE)
shade3d(min_mesh)

plot3d(f1_max, col = 'magenta', size = 8, add = TRUE)
lines3d(f1_max[c((length(bone_nums) - length(nums$isch)+1):length(bone_nums)),], col="magenta", lwd=5, add = TRUE)
spheres3d(f1_max[nums$ax[1:2],], col = 'magenta', radius = 0.01, add = TRUE)
lines3d(f1_max[nums$ax[9:11],], col = 'magenta', lwd=5, add = TRUE)
lines3d(f1_max[c(16,17,18),], col = 'magenta', lwd=5, add = TRUE)
aspect3d("iso")
title3d('pelvis shape for min PC1')
#shade3d(max_mesh)

#### plot pc1 - magenta
next3d()
view3d(theta = 0, phi = -70, fov = 20, zoom = .6)
plot3d(f1_min, col = 'blue', size = 8, axes = FALSE)
lines3d(f1_min[c((length(bone_nums) - length(nums$isch)+1):length(bone_nums)),], col="blue", lwd=5, add = TRUE)
spheres3d(f1_min[nums$ax[1:2],], col = 'blue', radius = 0.01, add = TRUE)
lines3d(f1_min[nums$ax[9:11],], col = 'blue', lwd=5, add = TRUE)
lines3d(f1_min[c(16,17,18),], col = 'blue', lwd=5, add = TRUE)
#shade3d(min_mesh)

plot3d(f1_max, col = 'magenta', size = 8, add = TRUE)
lines3d(f1_max[c((length(bone_nums) - length(nums$isch)+1):length(bone_nums)),], col="magenta", lwd=5, add = TRUE)
spheres3d(f1_max[nums$ax[1:2],], col = 'magenta', radius = 0.01, add = TRUE)
lines3d(f1_max[nums$ax[9:11],], col = 'magenta', lwd=5, add = TRUE)
lines3d(f1_max[c(16,17,18),], col = 'magenta', lwd=5, add = TRUE)
aspect3d("iso")
title3d('pelvis shape for max PC1')
shade3d(max_mesh)
