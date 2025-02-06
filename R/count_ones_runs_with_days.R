count_ones_runs_with_days <- function(series) {
  runs <- rle(series)
  ones_runs <- runs$lengths[runs$values == 1]
  cumulative_lengths <- cumsum(runs$lengths)
  start_days <- c(1, cumulative_lengths[-length(cumulative_lengths)] + 1)
  end_days <- cumulative_lengths
  ones_start_days <- start_days[runs$values == 1]
  ones_end_days <- end_days[runs$values == 1]
  return(list(lengths = ones_runs, start_days = ones_start_days, end_days = ones_end_days))
}