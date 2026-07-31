# Load the NCAA volleyball library
suppressPackageStartupMessages(library(ncaavolleyballr))

# Pull one team's overall season statistics
get_main_team_stats <- function(team_name, season, sport) {
  team_stats <- team_season_stats(
    team = team_name,
    opponent = FALSE,
    sport = sport
  )

  if (is.null(team_stats) || !is.data.frame(team_stats) || nrow(team_stats) == 0) {
    stop("team_season_stats() did not return team statistics.")
  }

  season_pattern <- paste0("^", season, "-")

  if (!("Season" %in% colnames(team_stats))) {
    stop("The main team statistics are missing the Season column.")
  }

  team_stats <- team_stats[
    grepl(season_pattern, as.character(team_stats$Season)),
    ,
    drop = FALSE
  ]

  if (nrow(team_stats) == 0) {
    stop(
      paste(
        "No overall team statistics were found for",
        team_name,
        "in season",
        season
      )
    )
  }

  # Keep only one team-total row for the requested season.
  team_stats <- team_stats[1, , drop = FALSE]
  rownames(team_stats) <- NULL

  return(team_stats)
}

# Pull opponent season totals for the other teams in a conference.
# opponent = TRUE returns what opposing offenses produced against each team,
# which is used here as that team's overall defensive profile.
get_conference_defensive_stats <- function(
  conference_name,
  main_team_name,
  season,
  sport
) {
  conference_data <- conference_stats(
    year = season,
    conf = conference_name,
    level = "teamseason",
    sport = sport
  )

  if (
    is.null(conference_data) ||
    !is.list(conference_data) ||
    is.null(conference_data$teamdata)
  ) {
    stop("conference_stats() did not return conference team data.")
  }

  team_data <- conference_data$teamdata

  if (!is.data.frame(team_data) || nrow(team_data) == 0) {
    stop("The conference team data is empty.")
  }

  required_team_columns <- c("TeamID", "Team")

  if (!all(required_team_columns %in% colnames(team_data))) {
    stop(
      paste(
        "Conference data is missing TeamID or Team. Available columns:",
        paste(colnames(team_data), collapse = ", ")
      )
    )
  }

  conference_teams <- unique(
    team_data[, required_team_columns, drop = FALSE]
  )

  conference_teams$TeamID <- as.character(conference_teams$TeamID)
  conference_teams$Team <- trimws(as.character(conference_teams$Team))

  conference_teams <- conference_teams[
    !is.na(conference_teams$TeamID) &
      conference_teams$TeamID != "" &
      !is.na(conference_teams$Team) &
      conference_teams$Team != "" &
      tolower(conference_teams$Team) != tolower(trimws(main_team_name)),
    ,
    drop = FALSE
  ]

  if (nrow(conference_teams) == 0) {
    stop("No other conference teams were found.")
  }

  # A team is kept only when all of these core defensive fields exist and
  # contain values. Additional available columns are retained as well.
  required_defensive_columns <- c(
    "Season",
    "Kills",
    "Errors",
    "Total Attacks",
    "Hit Pct",
    "Digs",
    "Block Solos",
    "Block Assists"
  )

  defensive_rows <- list()
  skipped_teams <- character(0)

  for (index in seq_len(nrow(conference_teams))) {
    current_team_id <- conference_teams$TeamID[index]
    current_team_name <- conference_teams$Team[index]

    cat(
      "Retrieving defense for:",
      current_team_name,
      "(Team ID", current_team_id, ")...\n"
    )

    opponent_stats <- tryCatch(
      suppressWarnings(
        team_season_stats(
          team = current_team_name,
          opponent = TRUE,
          sport = sport
        )
      ),
      error = function(error) {
        cat(
          "Skipping",
          current_team_name,
          "-",
          conditionMessage(error),
          "\n"
        )
        return(NULL)
      }
    )

    if (
      is.null(opponent_stats) ||
      !is.data.frame(opponent_stats) ||
      nrow(opponent_stats) == 0
    ) {
      skipped_teams <- c(skipped_teams, current_team_name)
      next
    }

    if (!("Season" %in% colnames(opponent_stats))) {
      cat("Skipping", current_team_name, "- missing Season column.\n")
      skipped_teams <- c(skipped_teams, current_team_name)
      next
    }

    season_pattern <- paste0("^", season, "-")
    opponent_stats <- opponent_stats[
      grepl(season_pattern, as.character(opponent_stats$Season)),
      ,
      drop = FALSE
    ]

    if (nrow(opponent_stats) == 0) {
      cat("Skipping", current_team_name, "- no requested-season row.\n")
      skipped_teams <- c(skipped_teams, current_team_name)
      next
    }

    missing_columns <- setdiff(
      required_defensive_columns,
      colnames(opponent_stats)
    )

    if (length(missing_columns) > 0) {
      cat(
        "Skipping",
        current_team_name,
        "- missing columns:",
        paste(missing_columns, collapse = ", "),
        "\n"
      )
      skipped_teams <- c(skipped_teams, current_team_name)
      next
    }

    opponent_row <- opponent_stats[1, , drop = FALSE]

    required_values <- opponent_row[
      ,
      required_defensive_columns,
      drop = FALSE
    ]

    incomplete_value <- any(
      is.na(required_values) |
        trimws(as.character(unlist(required_values))) == ""
    )

    if (incomplete_value) {
      cat(
        "Skipping",
        current_team_name,
        "- one or more required defensive values are blank.\n"
      )
      skipped_teams <- c(skipped_teams, current_team_name)
      next
    }

    # Store the defended team's identity, not the opponent-summary label.
    opponent_row$TeamID <- current_team_id
    opponent_row$Team <- current_team_name
    opponent_row$Conference <- conference_name

    defensive_rows[[length(defensive_rows) + 1]] <- opponent_row
  }

  if (length(defensive_rows) == 0) {
    stop(
      paste(
        "No complete defensive team rows were found.",
        "Teams checked:",
        paste(conference_teams$Team, collapse = ", ")
      )
    )
  }

  # Keep only columns shared by every successful team before combining rows.
  shared_columns <- Reduce(
    intersect,
    lapply(defensive_rows, colnames)
  )

  defensive_rows <- lapply(
    defensive_rows,
    function(row) row[, shared_columns, drop = FALSE]
  )

  defensive_stats <- do.call(rbind, defensive_rows)
  rownames(defensive_stats) <- NULL

  # Put identifying columns first.
  first_columns <- c("Season", "TeamID", "Team", "Conference")
  first_columns <- first_columns[first_columns %in% colnames(defensive_stats)]
  other_columns <- setdiff(colnames(defensive_stats), first_columns)
  defensive_stats <- defensive_stats[
    ,
    c(first_columns, other_columns),
    drop = FALSE
  ]

  if (length(skipped_teams) > 0) {
    cat(
      "Skipped incomplete or unavailable teams:",
      paste(unique(skipped_teams), collapse = ", "),
      "\n"
    )
  }

  return(defensive_stats)
}

# Read arguments supplied by Python
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 4) {
  stop(
    paste(
      "Expected 4 arguments but received",
      length(args),
      "\nArguments:",
      paste(args, collapse = " | ")
    )
  )
}

team_name <- trimws(args[1])
season <- as.integer(args[2])
sport <- toupper(trimws(args[3]))
output_directory <- args[4]

if (!(sport %in% c("MVB", "WVB"))) {
  stop("Sport must be either MVB or WVB.")
}

if (is.na(season) || season < 2020 || season > 2025) {
  stop("The season must currently be between 2020 and 2025.")
}

if (!dir.exists(output_directory)) {
  dir.create(
    output_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

if (!dir.exists(output_directory)) {
  stop(
    paste(
      "Could not create the output directory:",
      output_directory
    )
  )
}

main_team_output <- file.path(
  output_directory,
  "main_team_stats.csv"
)

conference_defense_output <- file.path(
  output_directory,
  "conference_defensive_stats.csv"
)

cat("Team:", team_name, "\n")
cat("Season:", season, "\n")
cat("Sport:", sport, "\n")
cat("Output directory:", output_directory, "\n")

cat("Retrieving main team overall statistics...\n")
main_team_stats <- get_main_team_stats(
  team_name,
  season,
  sport
)

conference_name <- as.character(main_team_stats$Conference[1])

if (is.na(conference_name) || trimws(conference_name) == "") {
  stop("The main team's conference could not be identified.")
}

cat("Conference:", conference_name, "\n")
cat("Retrieving defensive statistics for other conference teams...\n")

conference_defensive_stats <- get_conference_defensive_stats(
  conference_name,
  team_name,
  season,
  sport
)

write.csv(
  main_team_stats,
  file = main_team_output,
  row.names = FALSE,
  na = ""
)

write.csv(
  conference_defensive_stats,
  file = conference_defense_output,
  row.names = FALSE,
  na = ""
)

if (!file.exists(main_team_output)) {
  stop("The main team statistics CSV was not created.")
}

if (!file.exists(conference_defense_output)) {
  stop("The conference defensive statistics CSV was not created.")
}

cat("Main team rows:", nrow(main_team_stats), "\n")
cat("Conference defensive rows:", nrow(conference_defensive_stats), "\n")
cat("Main team CSV:", main_team_output, "\n")
cat("Conference defense CSV:", conference_defense_output, "\n")