
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

### PCA for All Data Superimposed on Bone
pca_superimposed_to_bone <- gm.prcomp(superimposed_to_bone)
pc1<-format(round(pca_results$d[1]/(sum(pca_results$d))*100, 2), nsmall=2)
pc2<-format(round(pca_results$d[2]/(sum(pca_results$d))*100, 2), nsmall=2)

data_df<-as_tibble(pca_superimposed_to_bone$x, validate = NULL, .name_repair = NULL)

plot <- ggplot(data=data_df, aes(x=Comp1, y=Comp2, color = groups, label = ind_names)) +  geom_point(size = 3, aes(colour = groups))
plot  + geom_text(hjust=0.5, vjust=1) + labs(title = 'Principal Components',
                                             x = paste0("PC1 (\"", pc1,"\"%)"), 
                                             y = paste0("PC2 (\"", pc2, "\"%)"))

### import surface template for plotting:
surf_template <- read_csv('surf_template/smlm_44.csv')
surf_template <- as.matrix(surf_template[c('x', 'y', 'z')])
surf_mesh_template <- read.ply("surf_template/surf_44.ply", ShowSpecimen = F) 

## pc1
f1_min = pca_superimposed_to_bone$shapes$shapes.comp1$min
f1_max = pca_superimposed_to_bone$shapes$shapes.comp1$max

surf_min_mesh <- tps3d(surf_mesh_template, surf_template, f1_min[nums$surf,], lambda = 1e-08, threads = 0)
surf_min_mesh$material <- list(color = "blue", alpha = 0.4)
surf_max_mesh <- tps3d(surf_mesh_template, surf_template, f1_max[nums$surf,], lambda = 1e-08, threads = 0)
surf_max_mesh$material <- list(color = "magenta", alpha = 0.4)

pelvis_min_mesh <- tps3d(pelvis_mesh_template, pelvis_surf_template, f1_min[bone_nums,], lambda = 1e-08, threads = 0)
pelvis_min_mesh$material <- list(color = "blue", alpha = 0.1)
pelvis_max_mesh <- tps3d(pelvis_mesh_template, pelvis_surf_template, f1_max[bone_nums,], lambda = 1e-08, threads = 0)
pelvis_max_mesh$material <- list(color = "magenta", alpha = 0.1)

## pc1
open3d()
mfrow3d(1, 2, sharedMouse=FALSE)
view3d(theta = 0, phi = -70, fov = 20, zoom = .6)
plot3d(f1_min[c(nums$ax, nums$sag, nums$isch),], col = 'blue', size = 8, axes=FALSE)
plot3d(f1_min[c(nums$r, nums$l, nums$c, nums$surf),], col = 'blue', size = 3, add = TRUE)

lines3d(f1_min[nums$isch,], col='blue', lwd=5, add = TRUE)
spheres3d(f1_min[nums$ax[1:2],], col = 'blue', radius = 0.01, add = TRUE)
lines3d(f1_min[nums$ax[9:11],], col = 'blue', lwd=5, add = TRUE)
lines3d(f1_min[nums$sag[c(2,7,8,9)],], col = 'blue', lwd=5, add = TRUE)
shade3d(surf_min_mesh)
shade3d(pelvis_min_mesh)

plot3d(f1_max[c(nums$ax, nums$sag, nums$isch),], col = 'magenta', size = 8, add = TRUE)
plot3d(f1_max[c(nums$r, nums$l, nums$c, nums$surf),], col = 'magenta', size = 3, add = TRUE)

lines3d(f1_max[nums$isch,], col='magenta', lwd=5, add = TRUE)
spheres3d(f1_max[nums$ax[1:2],], col = 'magenta', radius = 0.01, add = TRUE)
lines3d(f1_max[nums$ax[9:11],], col = 'magenta', lwd=5, add = TRUE)
lines3d(f1_max[nums$sag[c(2,7,8,9)],], col = 'magenta', lwd=5, add = TRUE)
shade3d(surf_max_mesh)

bone_f1_min = f1_min[c(nums$ax, nums$sag, nums$isch),]
bone_f1_max = f1_max[c(nums$ax, nums$sag, nums$isch),]
for (i in 1: dim(bone_f1_min)[1]) {
  lines3d(c(bone_f1_min[i,], bone_f1_max[i,]), col='grey', lwd=3, add = TRUE)
}
aspect3d('iso')
title3d('min and max along PC1')

## pc2
f2_min = pca_superimposed_to_bone$shapes$shapes.comp2$min
f2_max = pca_superimposed_to_bone$shapes$shapes.comp2$max

surf_min_mesh <- tps3d(surf_mesh_template, surf_template, f2_min[nums$surf,], lambda = 1e-08, threads = 0)
surf_min_mesh$material <- list(color = "black", alpha = 0.4)
surf_max_mesh <- tps3d(surf_mesh_template, surf_template, f2_max[nums$surf,], lambda = 1e-08, threads = 0)
surf_max_mesh$material <- list(color = "orange", alpha = 0.4)

pelvis_min_mesh <- tps3d(pelvis_mesh_template, pelvis_surf_template, f2_min[bone_nums,], lambda = 1e-08, threads = 0)
pelvis_min_mesh$material <- list(color = "black", alpha = 0.1)
pelvis_max_mesh <- tps3d(pelvis_mesh_template, pelvis_surf_template, f2_max[bone_nums,], lambda = 1e-08, threads = 0)
pelvis_max_mesh$material <- list(color = "orange", alpha = 0.1)

next3d()
view3d(theta = 0, phi = -70, fov = 20, zoom = .6)
plot3d(f2_min[c(nums$ax, nums$sag, nums$isch),], col = 'black', size = 8, axes=FALSE)
plot3d(f2_min[c(nums$r, nums$l, nums$c, nums$surf),], col = 'black', size = 3, add = TRUE)

lines3d(f2_min[nums$isch,], col='black', lwd=5, add = TRUE)
spheres3d(f2_min[nums$ax[1:2],], col = 'black', radius = 0.01, add = TRUE)
lines3d(f2_min[nums$ax[9:11],], col = 'black', lwd=5, add = TRUE)
lines3d(f2_min[nums$sag[c(2,7,8,9)],], col = 'black', lwd=5, add = TRUE)
shade3d(surf_min_mesh)
shade3d(pelvis_min_mesh)

plot3d(f2_max[c(nums$ax, nums$sag, nums$isch),], col = 'orange', size = 8, add = TRUE)
plot3d(f2_max[c(nums$r, nums$l, nums$c, nums$surf),], col = 'orange', size = 3, add = TRUE)

lines3d(f2_max[nums$isch,], col='orange', lwd=5, add = TRUE)
spheres3d(f2_max[nums$ax[1:2],], col = 'orange', radius = 0.01, add = TRUE)
lines3d(f2_max[nums$ax[9:11],], col = 'orange', lwd=5, add = TRUE)
lines3d(f2_max[nums$sag[c(2,7,8,9)],], col = 'orange', lwd=5, add = TRUE)
shade3d(surf_max_mesh)

bone_f2_min = f2_min[c(nums$ax, nums$sag, nums$isch),]
bone_f2_max = f2_max[c(nums$ax, nums$sag, nums$isch),]
for (i in 1: dim(bone_f2_min)[1]) {
  lines3d(c(bone_f2_min[i,], bone_f2_max[i,]), col='grey', lwd=3, add = TRUE)
}
aspect3d('iso')
title3d('min and max along PC2')

## plot outliers
plotOutliers(superimposed_to_bone)