
sfc_as_cols <- function(x, geometry, names = c("x","y")) {
  #A function for converting geomestry (from sf) to coordinates
  if (missing(geometry)) {
    geometry <- sf::st_geometry(x)
  } else {
    geometry <- rlang::eval_tidy(enquo(geometry), x)
  }
  stopifnot(inherits(x,"sf") && inherits(geometry,"sfc_POINT"))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

#####################
grp_num_assign <- function(vec,set_length){
  rws=length(vec)
  grp_id = rep(1:ceiling(rws/set_length), each=set_length, length.out=rws)
  
  return(grp_id)
}

generate_random_string <- function(length = 5) {
  paste0(sample(c(0:9, LETTERS), length, replace = TRUE), collapse = "")
}
