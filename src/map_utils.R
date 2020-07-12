#' take map arguments and return a projected sp object
#' 
#' @param \dots arguments passed to \code{\link[maps]{map}} excluding \code{fill} and \code{plot}
#' 

proj.string <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
to_sp <- function(...){
  library(maptools)
  library(maps)
  map <- maps::map(..., fill=TRUE, plot = FALSE)
  IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
  map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  map.sp.t <- spTransform(map.sp, CRS(proj.string))
  return(map.sp.t)
}

#' @param locations a data.frame with dec_long_va and dec_lat_va
points_sp <- function(locations){
  library(dplyr)
  
  points <- cbind(locations$dec_long_va, locations$dec_lat_va) %>% 
    sp::SpatialPoints(proj4string = CRS("+proj=longlat +datum=WGS84")) %>% 
    sp::spTransform(CRS(proj.string)) %>% 
    sp::SpatialPointsDataFrame(data = locations[c('site_no')])
}
# 
# shifts <- list(AK = list(scale = 0.37, shift = c(90,-460), rotate = -50),
#                HI = list(scale = 1, shift = c(520, -110), rotate = -35),
#                PR = list(scale = 2.5, shift = c(-140, 90), rotate=20))
# 
# stuff_to_move <- list(
#   AK = to_sp("world", "USA:alaska"),
#   HI = to_sp("world", "USA:hawaii"),
#   PR = to_sp("world", "Puerto Rico")
# )
# 

#' create the sp object 
#'
fetch_state_map <- function(...){
  shift_details <- list(...)
  
  state_map <- to_sp('state')
  for(i in 1:length(shift_details)){
    
    this_sp <- to_sp("world", shift_details[[i]]$regions)
    these_shifts <- shift_details[[i]][c('scale','shift','rotate')]
    shifted <- do.call(shift_sp, c(sp = this_sp,
                                   these_shifts,  
                                   proj.string = proj4string(state_map),
                                   row.names = shift_details[[i]]$regions))
    state_map <- rbind(shifted, state_map, makeUniqueIDs = TRUE)
  }
  
  return(state_map)
}

process.site_map <- function(...){
  library(dplyr)
  sites <- readRDS('cache/disch-sites.rds') %>% filter(!is.na(dec_lat_va))
  huc.map <- c(AK = "19", HI = "20", PR = "21")
  
  #parse huc_cd to 2 digits, and rename to huc to stay consistent
  sites <- sites %>% mutate(huc = substr(huc, 1,2)) 
  
  sites.out <- sites %>% filter(!huc %in% huc.map) %>% 
    points_sp()
  
  for (region in names(huc.map)){
    sites.tmp <- sites %>% filter(huc %in% huc.map[[region]]) %>% 
      points_sp()
    sites.tmp <- do.call(shift_sp, c(sp = sites.tmp, ref = stuff_to_move[[region]], 
                                     shifts[[region]]))
    sites.out <- rbind(sites.out, sites.tmp)
  }
  saveRDS(sites.out, file = 'cache/site-map.rds')
}


shift_sp <- function(sp, scale = NULL, shift = NULL, rotate = 0, ref=sp, proj.string=NULL, row.names=NULL){
  if (is.null(scale) & is.null(shift) & rotate == 0){
    return(obj)
  }
  orig.cent <- rgeos::gCentroid(ref, byid=TRUE)@coords
  scale <- max(apply(bbox(ref), 1, diff)) * scale
  obj <- elide(sp, rotate=rotate, center=orig.cent, bb = bbox(ref))
  ref <- elide(ref, rotate=rotate, center=orig.cent, bb = bbox(ref))
  obj <- elide(obj, scale=scale, center=orig.cent, bb = bbox(ref))
  ref <- elide(ref, scale=scale, center=orig.cent, bb = bbox(ref))
  new.cent <- rgeos::gCentroid(ref, byid=TRUE)@coords
  obj <- elide(obj, shift=shift*10000+c(orig.cent-new.cent))
  if (is.null(proj.string)){
    proj4string(obj) <- proj4string(sp)
  } else {
    proj4string(obj) <- proj.string
  }
  
  if (!is.null(row.names)){
    row.names(obj) <- row.names
  }
  return(obj)
}
