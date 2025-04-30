process_subcloud_people <- function(
  scores_matrix,
  pc = 1,
  direction = "positive"
) {
  if (direction == "positive") {
    direction_index <- scores_matrix[, pc] > 0
  } else {
    direction_index <- scores_matrix[, pc] < 0
  }

  # Subset scores based on the direction
  scores <- scores_matrix[direction_index, pc]

  # Calculate squared scores and average
  scores_squared <- scores^2
  avg_scores_squared <- mean(scores_squared)

  # Find indices where scores are above average
  indices_above_avg <- which(scores_squared > avg_scores_squared)

  # Return the row names of the scores above average
  return(rownames(scores_matrix)[direction_index][indices_above_avg])
}
