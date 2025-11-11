#' Checking the XLS Kobo tools for constraints errors
#'
#' These functions check the relevant column in the questionnaire and flag issues with constraints of the form `selected(question_name, question_answer)`.
#'
#' @param questions A data frame (from the XLS Kobo “survey” sheet) containing at least the columns `name` and `type`.
#' @param choices A data frame (from the XLS Kobo “choices” sheet) containing at least the columns `list_name` and `name`.
#' @param constraint A single constraint string of the form `selected(question_name, question_answer)` (or multiple if comma-separated).
#'
#' @return A logical value: `TRUE` if the constraint is valid (i.e., the answer exists in the choices list for the question) or `FALSE` if it is invalid.
#' @details
#' This function is intended to be used internally by your package to verify that for each question whose `relevant` (or `constraint`) field uses the `selected()` operator, the given `question_answer` is indeed present in the relevant choices list (which is derived from `question_type` by removing the prefix up to the last space).
#' If the question type is `calculate`, the function returns `TRUE` automatically (i.e., the constraint is not applicable).
#'
#' @examples
#' \dontrun{
#' questions_df <- data.frame(name = c("q1","q2"), type = c("select_one list1","calculate"))
#' choices_df   <- data.frame(list_name = "list1", name = c("yes","no"))
#' check_answer_in_list(questions_df, choices_df, "selected(q1,'yes')")
#' check_answer_in_list(questions_df, choices_df, "selected(q1,'maybe')") # will give FALSE
#' }
#' @export

check_answer_in_list <- function(questions, choices, constraint) {
  if (!str_detect(constraint, ",")) {
    return(T)
  }

  question_regex <- "\\{([^()]+)\\}"
  answer_regex <- "\\'([^()]+)\\'"

  question <- gsub(
    question_regex,
    "\\1",
    str_extract_all(constraint, question_regex)[[1]]
  )
  answer <- gsub(
    answer_regex,
    "\\1",
    str_extract_all(constraint, answer_regex)[[1]]
  )

  question_type <- questions %>%
    filter(name == question) %>%
    filter(!grepl("^(begin|end)\\s+group$", type)) %>%
    pull(type)

  if (question_type == "calculate") {
    return(T)
  } else {
    listname <- gsub("^.*\\s", "", question_type)
    choices_list <- choices %>% filter(list_name == listname) %>% pull(name)
    return(answer %in% choices_list)
  }
}

# ------------------------------------------------------------------------------

check_constraints <- function(questions, choices) {
  # verify survey and choices using cleaning tools package
  if (!verify_valid_survey(kobo_survey)) {
    stop("Survey sheet does not meets all the criteria. Please check!")
  }
  if (!verify_valid_choices(kobo_choices)) {
    stop("Choices sheet does not meets all the criteria. Please check!")
  }

  questions <- mutate_at(questions, c("name", "type"), ~ str_trim(.))
  choices <- mutate_at(choices, c("list_name", "name"), ~ str_trim(.))

  all_contraints <- questions %>%
    filter(grepl("selected", relevant)) %>%
    pull(relevant)
  all_contraints <- gsub('"', "'", all_contraints)
  rs_list <- map(
    all_contraints,
    ~ map_lgl(
      unlist(ex_default(.x, pattern = "selected\\s*\\([^\\)]*\\)")),
      ~ check_answer_in_list(questions, choices, .)
    )
  )

  map2(
    rs_list,
    seq_along(rs_list),
    ~ if (length(which(!.x)) != 0) {
      return(unlist(ex_default(
        all_contraints[.y],
        pattern = "selected\\s*\\([^\\)]*\\)"
      ))[which(!.x)])
    }
  ) %>%
    unlist() %>%
    unique()
}
