get_line_projection_results <- function(ind.coord, data, line.direction, line.point, center = TRUE, scale = TRUE) {
  
  if (!center) line.point <- rep(0, ncol(data))
  if (!scale) line.direction <- rep(1, ncol(data))
  
  # Normalize the line direction vector
  line.direction <- line.direction / sqrt(sum(line.direction^2))
  
  # Compute the projection of each point onto the line
  project_onto_line <- function(ind_row, line.direction, line.point) {
    projection_length <- sum((ind_row - line.point) * line.direction)
    projection_point <- line.point + projection_length * line.direction
    return(projection_point)
  }
  
  projections <- t(apply(data, 1, project_onto_line, line.direction, line.point))
  
  # Compute the square of the distance between an individual and the projection on the line
  getdistance <- function(ind_row, projection_row) {
    return(sum((ind_row - projection_row)^2))
  }
  d2 <- apply(data, 1, function(row) getdistance(row, project_onto_line(row, line.direction, line.point)))
  
  # Compute the cos2
  cos2 <- function(ind.coord, d2) {
    return(ind.coord^2 / d2)
  }
  ind.cos2 <- apply(ind.coord, 2, cos2, d2)
  
  # Individual contributions
  contrib <- function(ind.coord, line.direction, n.ind) {
    100 * (1 / n.ind) * (ind.coord^2 / sum(line.direction^2))
  }
  ind.contrib <- t(apply(ind.coord, 1, contrib, line.direction, nrow(ind.coord)))
  
  colnames(ind.coord) <- colnames(ind.cos2) <- colnames(ind.contrib) <- paste0("Dim.", 1:ncol(ind.coord))
  
  rnames <- rownames(ind.coord)
  if (is.null(rnames)) rnames <- as.character(1:nrow(ind.coord))
  rownames(ind.coord) <- rownames(ind.cos2) <- rownames(ind.contrib) <- rnames
  
  # Individuals coord, cos2 and contrib
  ind = list(coord = projections, cos2 = ind.cos2, contrib = ind.contrib)
  ind
}