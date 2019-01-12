get_master_schedule <- function(year, month, day) {
  date <- paste0(year, ifelse(nchar(month) == 1, paste0("0", month), month),
                 ifelse(nchar(day) == 1, paste0("0", day), day))
  url <- paste0("http://www.espn.com/mens-college-basketball/schedule/_/date/", date, "/group/50")
  z <- XML::readHTMLTable(url)
  if(length(z) > 1) {
    schedule <- as.data.frame(z[[1]])[,c(1,2)]
    completed <- as.data.frame(z[[2]][-1,1:3])
    names(completed) <- c("away", "home", "result")
    names(schedule) <- c("away", "home")
  }else{
    completed <- as.data.frame(z[[1]][,1:3])
    names(completed) <- c("away", "home", "result")
    schedule <- NA
  }
  
  n_canceled <- sum(grepl("Canceled", completed$result))
  completed <- dplyr::filter(completed, result != "Canceled")
  
  ### Extract Ranking
  ranking <- function(team) {
    rank <- gsub("[^0-9]", "", team)
    return(ifelse(rank == "", NA, rank))
  }
  
  ### Clean Team Name
  clean <- function(team) {
    team <- gsub("[#0-9]", "", team)
    team <- gsub("\\s[A-Z]*-*[A-Z]*$", "", team)
    team <- gsub("TA&M", "", team)
    team <- gsub("\\s*$", "", gsub("^\\s*", "", team))
    return(team)
  }
  
  if(any(!is.na(schedule[1]))) {
    schedule <- dplyr::mutate(schedule,
                              "away" = as.character(sapply(schedule$away, clean)),
                              "home" = as.character(sapply(schedule$home, clean)),
                              "away_rank" = as.numeric(sapply(schedule$away, ranking)),
                              "home_rank" = as.numeric(sapply(schedule$home, ranking)),
                              "away_score" = NA,
                              "home_score" = NA)
  }
  
  x <- scan(url, sep = "\n", what = "")
  x <- x[grep("gameId", x)[1]]
  x <- gsub("[A-z]", "", x)
  x <- strsplit(x, "\\?=")[[1]]
  x <- suppressWarnings(as.numeric(unname(sapply(x, function(y){ substring(y, 1, 9) }))))
  x <- x[!is.na(x) & !duplicated(x)]
  x <- x[1:(length(x) - n_canceled)]
  
  ### Add in Completed Games
  find_anchor <- function(team) {
    cleaned <- clean(team)
    team <- gsub("[#0-9]", "", team)
    team <- gsub("\\s*$", "", gsub("^\\s*", "", team))
    anchor <- unlist(strsplit(team, ""))[-c(1:(nchar(cleaned) + 1))]
    return(paste0(anchor, collapse = ""))
  }
  
  completed <- dplyr::mutate(completed,
                             "away" = as.character(sapply(away, clean)),
                             "home" = as.character(sapply(home, clean)),
                             "result" = as.character(result),
                             "away_rank" = as.numeric(sapply(completed$away, ranking)),
                             "home_rank" = as.numeric(sapply(completed$home, ranking)),
                             "away_anchor" = sapply(completed$away, find_anchor),
                             "away_score" = NA,
                             "home_score" = NA)
  
  winners <- unname(sapply(completed$result, function(y) { gsub("\\s[0-9]*.*", "", y) }))
  scores <- as.numeric(gsub("[^0-9]", "", gsub("\\(.*\\)", "", unlist(strsplit(completed$result, ",")))))
  
  # if(length(scores) > 0) {
  #   winning_scores <- scores[seq(1, length(scores) - 1, 2)]
  #   losing_scores <- scores[seq(2, length(scores), 2)]
  #   
  #   index <- sapply(completed$away_anchor, function(y) { y %in% winners })
  #   completed$home_score[index] <- losing_scores[index]
  #   completed$home_score[!index] <- winning_scores[!index]
  #   completed$away_score[!index] <- losing_scores[!index]
  #   completed$away_score[index] <- winning_scores[index]
  # }
  
  if(any(!is.na(schedule[1]))) {
    schedule <- rbind(schedule, dplyr::select(completed, -away_anchor, -result))
  }else{
    schedule <- completed
  }
  
  schedule <- dplyr::mutate(schedule, "game_id" = x)
  schedule <- dplyr::select(schedule, game_id, away, home, away_rank, home_rank, away_score, home_score)
  
  return(schedule)
}