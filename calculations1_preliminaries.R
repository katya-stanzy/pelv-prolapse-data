# this a script is No 2, to be executed after loading all

'____________________________________________________________'

# PRELIMINARIES

## Vars and Names
vars = list(OPII_016, OPII_044, OPII_046, OPII_060, OPII_075, OPII_089, OPII_099, OPII_104, OPII_111, OPII_191, RU_007, RU_008, RU_021, RU_023, RU_093) 
ind_names = c('OPII_016','OPII_044', 'OPII_046', 'OPII_060', 'OPII_075', 'OPII_089', 'OPII_099', 'OPII_104', 'OPII_111', 'OPII_191', 'RU_007', 'RU_008', 'RU_021', 'RU_023', 'RU_093')

## prepare a matrix of the data for further development

matrix_all = c()

for (i in 1:length(vars)) {

  ind_list = vars[[i]]
  ind <- c()
  for (df in ind_list) {
    m <- as.matrix(df[,2:4])
    ind <- c(ind, c(t(m)))
  }

  if (i==1) {
    matrix_all<-ind
  }
  else {matrix_all<-rbind(matrix_all, ind)}
}

rownames(matrix_all)<-ind_names

## convert the matrix to the Geomorph data frame
p = dim(matrix_all)[2]/3
k=3
dataframe = arrayspecs(matrix_all, p, k, sep = NULL)
dimnames(dataframe)[[3]]<-ind_names

## define sliders for for curves and for the surface
sliders = rbind(define.sliders(nums$isch[1:6]), define.sliders(nums$isch[6:11]), define.sliders(nums$r), define.sliders(nums$l),define.sliders(nums$c))
surface_sliders = define.sliders(nums$c)

## run superimposition and sliding
gpa_results <- gpagen(dataframe, curves = sliders, surfaces = surface_sliders)

## plot superimposition results
# plot(gpa_results)

## colour
groups <- ind_names
for (i in 1:length(groups)) {
  if (groups[i] %in% cases_names) {
    groups[i] = 'cases'
  }  
  if (groups[i] %in% control_names) {
    groups[i] = 'controls'
  }
  if (groups[i] %in% ruby_names) {
    groups[i] = 'nullipara'
  }
}
## bone nums


v <- c()
nu <- 0
for (i in 1:length(lm_names$sag)) {
  if ((lm_names$sag[[i]] == 'anus') | (lm_names$sag[[i]] =='urethra')) {
    nu <- nu + 1
    v[nu] <- nums$sag[[i]]
  }
}

bone_nums <- c(nums$ax, setdiff(nums$sag, v), nums$isch)

'_________________________________________________________________________________________'
