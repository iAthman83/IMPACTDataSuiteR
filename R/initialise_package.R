#' Initialize the Impact Data SuiteR workflow
#'
#' This function prepares the environment for the full data‑cleaning and analysis workflow. It installs/loads required packages, sources custom utility scripts, loads the KOBO survey and choices sheets (using environment variable paths), checks for relevant constraints, and sets up other initial variables such as numeric column names, special value lists, and audits.
#'
#' @param None This function takes no user‑supplied parameters: it relies on environment variables being set for paths (e.g., `Sys.getenv("kobo")`, `Sys.getenv("raw_dataset")`, `Sys.getenv("audit_dir")`, `Sys.getenv("user_username")`, `Sys.getenv("user_password")`).
#'
#' @return Invisibly returns `NULL`. The primary effect is side‑effects: packages loaded, scripts sourced, environment variables prepared, relevant global objects created (e.g., `kobo_survey`, `kobo_choices`, `cols_numeric`, `values_to_check`, `other_labels`, `other_db`), and audit‑zip produced.
#'
#' @details
#' This function performs the following steps to initialize the workflow:
#' \itemize{
#'   \item Installs the \code{pacman} package if missing, then loads a predefined suite of packages including \code{devtools}, \code{tidyverse}, \code{readxl}, \code{writexl}, \code{openxlsx}, \code{randomcoloR}, \code{anytime}, \code{qdapRegex}, \code{sf}, \code{leaflet}, \code{leaflet.extras}, \code{crayon}, \code{listr}, \code{zip}, \code{httr}, \code{glue}, \code{googlesheets4}, \code{robotoolbox}, \code{shiny}, \code{RDS}, \code{ggplot2}, \code{lubridate}.
#'   \item Loads the \code{cleaningtools} package. To install, use \code{devtools::install_github("impact-initiatives/cleaningtools")}
#'   \item Sets global options: \code{options(scipen = 999)} and \code{options(dplyr.summarise.inform = FALSE)}.
#'   \item Sources two scripts from \code{src/}: \code{check_kobo.R} and \code{utils_cleaning.R}.
#'   \item Reads the KOBO survey tool (sheet “survey”) and choices tool (sheet “choices”) from the Excel file specified by the \code{KOBO} environment variable. The data is then cleaned: column names set to lowercase, \code{q_type} and \code{list_name} computed, and special types (e.g., \code{resp_name}, \code{resp_phone_number}) forced to type \code{text}.
#'   \item Calls \code{check_constraints(questions = kobo_survey, choices = kobo_choices)} to identify any issues with \code{selected(question_name, question_answer)} constraints. If any are found, a warning is issued listing the problematic constraints.
#'   \item Derives \code{cols_numeric} as a vector of column names from \code{kobo_survey} whose \code{type} is “integer” or “decimal”.
#'   \item Defines \code{values_to_check} as a hard‑coded vector of numeric codes (e.g., 96, 97, 98, 99, 999, 88, 888, ‑96, ‑97, ‑98, ‑99, ‑999) for later cleaning.
#'   \item Generates \code{other_labels} and \code{other_db} by calling internal helper functions \code{get_other_labels()} and \code{get_other_db()}.
#'   \item Downloads audit files using \code{download_audit_files()}, then creates a zip archive of the audit directory (if it exists), using environment variables for the raw dataset path, audit directory, user credentials, and audits file path.
#'   \item Prints progress messages to the console to indicate loading and initialization status.
#' }
#'
#' @examples
#' \dontrun{
#'   # Ensure environment variables are correctly set:
#'   Sys.setenv(kobo = "path/to/kobo_tool.xlsx",
#'              raw_dataset = "path/to/raw_dataset.xlsx",
#'              audit_dir = "path/to/audit_folder",
#'              user_username = "myusername",
#'              user_password = "mypassword",
#'              audits = "path/to/audits.zip")
#'   # Run the initialization
#'   init_impact_data_suiteR()
#' }
#'
#' @export

init_impact_data_suiteR <- function() {
  #-------------------------------------------------------------------------------
  # load packages
  #-------------------------------------------------------------------------------
  if (!require("pacman")) {
    install.packages("pacman")
  }
  pacman::p_load(
    devtools,
    tidyverse,
    readxl,
    writexl,
    openxlsx,
    randomcoloR,
    anytime,
    qdapRegex,
    sf,
    leaflet,
    leaflet.extras,
    crayon,
    listr,
    zip,
    httr,
    glue,
    googlesheets4,
    robotoolbox,
    shiny,
    RDS,
    ggplot2,
    lubridate
  )

  # devtools::install_github("impact-initiatives/cleaningtools")
  library(cleaningtools)

  options(scipen = 999) # to prevent scientific notation
  options(dplyr.summarise.inform = FALSE)

  #-------------------------------------------------------------------------------
  # load functions
  #-------------------------------------------------------------------------------
  source("src/check_kobo.R")
  source("src/utils_cleaning.R")

  #-------------------------------------------------------------------------------
  # load KOBO tool
  #-------------------------------------------------------------------------------
  cat("\n- LOADING tool ...\n")
  kobo_survey <- read_excel(
    Sys.getenv("kobo"),
    sheet = "survey",
    col_types = "text"
  ) %>%
    mutate(
      type = ifelse(name %in% c("resp_name", "resp_phone_number"), "text", type)
    ) %>%
    rename_with(., tolower) %>%
    rename(label = `label::english (en)`) %>%
    filter(!is.na(name)) %>%
    mutate(
      q_type = as.character(lapply(type, function(x) {
        str_split(x, " ")[[1]][1]
      })),
      list_name = as.character(lapply(type, function(x) {
        str_split(x, " ")[[1]][2]
      }))
    )

  kobo_choices <- read_excel(
    Sys.getenv("kobo"),
    sheet = "choices",
    col_types = "text"
  ) %>%
    rename_with(., tolower) %>%
    rename(label = `label::english (en)`) %>%
    filter(!is.na(list_name)) %>%
    select(list_name, name, label) %>%
    distinct()

  # check tool relevant column
  t <- check_constraints(questions = kobo_survey, choices = kobo_choices)
  if (length(t) > 0) {
    warning(paste0("\n", "Issues with relevancies: ", t))
  }

  cat("..OK\n")

  #-------------------------------------------------------------------------------
  # get all integer column names from kobo tool
  cols_numeric <- filter(kobo_survey, type %in% c("integer", "decimal")) %>%
    pull(name)

  #-------------------------------------------------------------------------------
  # values to check
  values_to_check <- c(
    96,
    97,
    98,
    99,
    999,
    999,
    88,
    888,
    888,
    -96,
    -97,
    -98,
    -99,
    -999
  )

  #-------------------------------------------------------------------------------
  # generate database of other_labels
  other_labels <- get_other_labels()

  # generate database of question_other
  other_db <- get_other_db()

  # ------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------
  # download audit files and add to zip
  #-------------------------------------------------------------------------------
  download_audit_files(
    df = read_raw_data(Sys.getenv("raw_dataset")),
    uuid_column = "uuid",
    audit_dir = Sys.getenv("audit_dir"),
    usr = Sys.getenv("user_username"),
    pass = Sys.getenv("user_password")
  )

  if (dir.exists(Sys.getenv("audit_dir"))) {
    zip::zip(
      zipfile = Sys.getenv("audits"),
      files = list.dirs(
        path = Sys.getenv("audit_dir"),
        full.names = TRUE,
        recursive = FALSE
      ),
      mode = "cherry-pick"
    )
  }
}
