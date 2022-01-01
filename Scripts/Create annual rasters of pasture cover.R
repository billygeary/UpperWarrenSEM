## Script for creating layer of pasture per time step
library(sf)
library(raster)
library(fasterize)
library(dplyr)
std.crs = st_crs(32750)

# Create mask for study area
project.area = read_sf("~/Dropbox/_Research/_PhD/07_UpperWarren_WA/data/from Adrian Feb19/Spatial data/Covariates/ForestBlocksStudyArea.shp")
woody.veg = raster("~/Dropbox/_research/_PhD/07_UpperWarren_WA/data/si50_woody_geo/l8att_2020_NCAS_si50_woody_geo.ers")
project.area = st_transform(project.area, st_crs(woody.veg))

lps = read_sf("~/Dropbox/_Research/_PhD/07_UpperWarren_WA/data/from Adrian Feb19/Spatial data/Covariates/LPS_Zoning_clip.shp")
lps_rural = lps %>% dplyr::filter(ZONE_CAT == "Rural") %>% st_transform(st_crs(project.area)) %>% st_union() %>% st_as_sf()


r = raster(extent(project.area), res = 50)
mask = fasterize(project.area, r,fun = 'count', background = NA)

# We know from looking at the satellite data that we need to mask out private plantations as they are established over time
pp = read_sf("~/Dropbox/_Research/_PhD/07_UpperWarren_WA/data/from Adrian Feb19/Spatial data/Covariates/PrivatePlantations.shp")
pp= st_transform(pp, st_crs(woody.veg))

change.rasters = list.files("~/Dropbox/_research/_PhD/07_UpperWarren_WA/data/si50_change_geo/", pattern = "NCAS_si50_chnge_geo.ers", full.names = TRUE)

plantations.out = pp

for (l in 1:length(change.rasters)){
  path = change.rasters[l]
  change.veg = raster(path)
  name = names(change.veg)
  
  # Extract cells that changed from forest or sparse to non-woody
  # This creates a raster of just where there is cleared forest
  change.to.forest = crop(change.veg, extent(project.area))
  change.to.forest[change.to.forest<5] <- 0
  
  # Extract which plantation polygons had changes occur within them and append to dataframe
  # We assume that these are plantations that have been harvested, rather than 'new pasture' and so need to mask out from pasture layer a next step
  new.plantations = data.frame(exactextractr::exact_extract(change.to.forest, pp, fun = 'max'))
  colnames(new.plantations) = name
  plantations.out = cbind(plantations.out, new.plantations)
}

# Create the pasture layers

layers = list.files("~/Dropbox/_research/_PhD/07_UpperWarren_WA/data/si50_woody_geo/", pattern = "NCAS_si50_woody_geo.ers", full.names = TRUE)
pasture.list = list()

for (l in 1:length(layers)){
  path = layers[l]
  woody.veg = raster(path)
  name = names(woody.veg)
  
  # Create non-woody veg layer within project extent
  woody.veg = crop(woody.veg, extent(project.area))
  woody.veg[is.na(woody.veg)] <- 0
  nonwoody = woody.veg
  nonwoody[nonwoody > 0] <- NA
  nonwoody[nonwoody == 0] <- 1
  
  # Remove managed tenure from non-woody layer
  pasture = mask(nonwoody, project.area, inverse=TRUE)
  
  # Select for just 'rural' land use in the lps layer
  lps_rural_ras = fasterize(lps_rural,pasture,background=NA)
  rural_pasture = lps_rural_ras + pasture
  rural_pasture[rural_pasture == 2] <- 1
  rural_pasture[is.na(rural_pasture)] <- 0 
  
  # Select plantations recently harvested and remove from pasture layer
  col.no  = names(plantations.out)[7+l-1]
  plantations.out.subset = plantations.out[plantations.out[[col.no]]%in%c(5,6),]
  pasture = mask(pasture, plantations.out.subset, inverse = TRUE)

  # Add to list of rasters
  pasture.list[[name]] <- rural_pasture
  print(paste(l, "out of", length(layers)))
}

pasture.stack = stack(pasture.list)

save(pasture.stack, file="~/Dropbox/_research/_PhD/07_UpperWarren_WA/data/si50_woody_geo/pasture.stack.30052021.RData")

load("~/Dropbox/_research/_PhD/07_UpperWarren_WA/data/si50_woody_geo/pasture.stack.30052021.RData")

writeRaster(pasture.stack[[19]], "~/Dropbox/_research/_PhD/07_UpperWarren_WA/data/si50_woody_geo/pasture2000.tif",overwrite=TRUE)
writeRaster(pasture.stack[[21]], "~/Dropbox/_research/_PhD/07_UpperWarren_WA/data/si50_woody_geo/pasture2012.tif",overwrite=TRUE)
writeRaster(pasture.stack[[28]], "~/Dropbox/_research/_PhD/07_UpperWarren_WA/data/si50_woody_geo/pasture2019.tif",overwrite=TRUE)

# Quick check to look at plot
plot(pasture.stack[[26:29]])

# Quick check to see variation in pasture (number of pixels) over time
dat.out = data.frame()
for (i in 1:length(pasture.list)){
  ras.name = sort(names(pasture.stack))[i]
  cellsum = sum(values(pasture.stack[[ras.name]]))
  dat = data.frame(Name = ras.name, Value = cellsum)
  dat.out = rbind(dat.out, dat)
}

plot(dat.out$Value[11:28]/max(dat.out$Value[11:28]))

# Make pdf file of annual baiting intensity maps for visual check
names(pasture.stack) = gsub("_NCAS_si50_woody_geo", "", names(pasture.stack))
names(pasture.stack) = as.numeric(gsub(".*_","",names(pasture.stack)))

pdf("~/Dropbox/_research/_PhD/07_UpperWarren_WA/data/si50_woody_geo/pasturemaps.31052021.pdf")
for (i in 1:29){
  ras.name = sort(names(pasture.stack))[i]
  ras = pasture.stack[[ras.name]]
  plot(ras, main = paste(names(ras)))
}
dev.off()


