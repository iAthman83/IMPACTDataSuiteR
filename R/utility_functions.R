################################################################################
# DOWNLOAD DATA FROM KOBO
################################################################################
download_data_kobo <- function() {
  kobo_setup(
    url = Sys.getenv("KOBOTOOLBOX_URL"),
    token = Sys.getenv("KOBOTOOLBOX_TOKEN")
  )

  # Retrieve the asset using the provided sheet name
  asset <- kobo_asset(Sys.getenv("ASSET_ID"))

  # Fetch the data from the specified sheet
  df <- kobo_data(asset, lang = "_xml", progress = TRUE)

  return(df)

  # Check if the sheet_name exists in the data frame
  # if (sheet_name %in% colnames(df)) {
  #   return(df[[sheet_name]])
  # } else {
  #   stop("Sheet name '", sheet_name, "' not found in the data.")
  # }
}
################################################################################
# READ DATASET
################################################################################

read_raw_data <- function(filename) {
  # fix dates
  df <- read_excel(filename, sheet = 1, col_types = "text")
  cat(green(paste0(
    "\n--> RAW --> ",
    filename,
    " --> ",
    nrow(df),
    " entries, ",
    ncol(df),
    " columns \n"
  )))
  cat(green("--> convert dates \n"))
  cols_date <- c(
    kobo_survey$name[kobo_survey$type %in% c("today")],
    "submission_date"
  )
  df <- df %>%
    mutate(submission_date = `_submission_time`) %>%
    mutate_at(
      cols_date,
      ~ ifelse(
        is.na(.),
        NA,
        as.character(as.Date(convertToDate(as.numeric(.))))
      )
    ) %>%
    mutate_at(
      c("start", "end", "_submission_time"),
      ~ ifelse(is.na(.), NA, as.character(convertToDateTime(as.numeric(.))))
    ) %>%
    mutate(across(
      everything(),
      ~ str_trim(str_remove_all(as.character(.), "xml:space=\\\"preserve\\\">"))
    ))

  #---------------------------------------------------------------------------
  cat(green("--> rename id/uuid and add submission time \n"))
  df <- df %>%
    rename(
      id = "_id",
      uuid = "_uuid",
      submission_time = "_submission_time",
      # enum_id = "Enumerator/Organization id:"
    ) %>%
    mutate(df_name = "main")
  #-----------------------------------------------------------------------------
  # convert numeric columns
  cat(green("--> convert integer columns to numeric \n"))
  df <- df %>% mutate_at(intersect(cols_numeric, colnames(df)), as.numeric)
  names(df) <- gsub("/", ".", names(df))
  return(df)
}

read_loop_data <- function(
  filename,
  sheet,
  loop_name,
  ind_count_col = "ind_pos"
) {
  # fix dates
  df <- read_excel(filename, sheet, col_types = "text")

  cat(green(paste0(
    "--> RAW LOOP --> ",
    filename,
    " --> ",
    nrow(df),
    " entries, ",
    ncol(df),
    " columns \n"
  )))
  cat(green("--> convert dates \n"))
  df <- df %>%
    mutate(submission_date = `_submission__submission_time`) %>%
    mutate_at(
      c("submission_date"),
      ~ ifelse(
        is.na(.),
        NA,
        as.character(as.Date(convertToDate(as.numeric(.))))
      )
    ) %>%
    mutate_at(
      c("_submission__submission_time"),
      ~ ifelse(is.na(.), NA, as.character(convertToDateTime(as.numeric(.))))
    ) %>%
    mutate(across(
      everything(),
      ~ str_trim(str_remove_all(as.character(.), "xml:space=\\\"preserve\\\">"))
    ))

  #---------------------------------------------------------------------------
  cat(green("--> rename uuid, concate loop count and uuid \n"))
  df <- df %>%
    mutate(uuid = paste0(!!sym(ind_count_col), "_", `_submission__uuid`)) %>%
    mutate(df_name = loop_name)
  #-----------------------------------------------------------------------------
  # convert numeric columns
  cat(green("--> convert integer columns to numeric \n"))
  df <- df %>% mutate_at(intersect(cols_numeric, colnames(df)), as.numeric)
  names(df) <- gsub("/", ".", names(df))
  return(df)
}

#############################################################################################################
# Check Soft Duplicate - Troubleshooting
#############################################################################################################

view_soft_duplicates <- function(df, uuids, only_differences = F) {
  cols_to_remove <- kobo_survey %>%
    filter(type %in% c("note", "calculate")) %>%
    pull(name) %>%
    intersect(colnames(df))

  check <- df %>%
    select(-all_of(cols_to_remove)) %>%
    filter(uuid %in% uuids) %>%
    t() %>%
    as.data.frame()
  check$num_unique <- unlist(lapply(1:nrow(check), function(r) {
    length(unique(as.character(check[r, colnames(check)])))
  }))
  check <- check[!(rownames(check) %in% "_index"), ]
  if (only_differences) {
    check <- check %>% filter(num_unique != 1) %>% select(-num_unique)
  } else {
    check <- check %>% arrange(-num_unique)
  }
  return(check)
}

#############################################################################################################
# OTHER RESPONSES
#############################################################################################################

get_other_labels <- function() {
  other_labels <- kobo_survey %>%
    filter((type == "text" & str_ends(name, "_other"))) %>%
    mutate(ref_question = as.character(lapply(relevant, get_ref_question))) %>%
    mutate(ref_question = ifelse(is.na(ref_question), name, ref_question)) %>%
    select(name, ref_question) %>%
    left_join(
      select(kobo_survey, ref_question = name, full_label = label),
      by = "ref_question"
    )

  cat(green(" - SAVING (./resources/labels_questions_others.xlsx) ... \n"))

  write.xlsx(
    other_labels,
    "resources/labels_questions_others.xlsx",
    overwrite = T
  )

  return(other_labels)
}
################################################################################

get_other_db <- function() {
  # generate other_db
  other_db <- other_labels %>%
    left_join(
      select(kobo_survey, name, q_type, list_name),
      by = c("ref_question" = "name")
    ) %>%
    left_join(select(kobo_survey, name, relevant), by = "name") %>%
    mutate(
      option_other = str_replace_all(str_extract(relevant, "\'.*\'"), "'", "")
    ) %>%
    select(-relevant)

  # remove all option_other from choices
  kobo_choices_sub <- filter(kobo_choices, list_name %in% other_db$list_name)

  for (r in 1:nrow(other_db)) {
    if (!is.na(other_db$option_other[r])) {
      kobo_choices_sub <- kobo_choices_sub %>%
        filter(
          !(list_name == other_db$list_name[r] &
            name == other_db$option_other[r])
        )
    }
  }
  # add list of available choices
  other_db <- other_db %>%
    left_join(
      select(kobo_choices_sub, list_name, label = "label") %>%
        group_by(list_name) %>%
        summarise(num_choices = n(), choices = paste0(label, collapse = ";;")),
      by = "list_name"
    )

  return(other_db)
}
################################################################################
add_to_cleaning_log_other_remove <- function(x) {
  change_type <- "Removing other response"
  temp_df <- data.frame() # Create a temporary data frame to store changes

  # load option other
  option_other <- other_db$option_other[other_db$name == x$name]
  var_option_other <- paste0(x$ref_name, ".", option_other)

  # remove text of the response
  df <- data.frame(
    uuid = x$uuid,
    question = x$name,
    change_type = change_type,
    old_value = x$response_en,
    new_value = NA
  )
  temp_df <- rbind(temp_df, df)

  # remove relative entries
  if (x$ref_type == "select_one") {
    df <- data.frame(
      uuid = x$uuid,
      question = x$ref_name,
      change_type = change_type,
      old_value = option_other,
      new_value = NA
    )
    temp_df <- rbind(temp_df, df)
  } else if (x$ref_type == "select_multiple") {
    old_concat_value <- get_value_from_uuid(x$uuid, x$ref_name)
    new_concat_value <- remove_choice(old_concat_value, option_other)
    new_concat_value <- ifelse(new_concat_value == "", NA, new_concat_value)
    df <- data.frame(
      uuid = x$uuid,
      question = x$ref_name,
      change_type = change_type,
      old_value = old_concat_value,
      new_value = new_concat_value
    )
    temp_df <- rbind(temp_df, df)

    if (is.na(new_concat_value)) {
      # set all options to NA
      if (str_detect(x$uuid, "_")) {
        cols <- colnames(clean_data_loop)[str_starts(
          colnames(clean_data_loop),
          paste0(x$ref_name, ".")
        )]
        old_values <- clean_data_loop[
          clean_data_loop$uuid == x$uuid,
          all_of(cols)
        ]
      } else {
        cols <- colnames(clean_data_main)[str_starts(
          colnames(clean_data_main),
          paste0(x$ref_name, ".")
        )]
        old_values <- clean_data_main[
          clean_data_main$uuid == x$uuid,
          all_of(cols)
        ]
      }

      old_values <- as.character(old_values)
      if (length(cols) != length(old_values)) {
        stop("cols and old_values have different lengths")
      }
      for (i in 1:length(cols)) {
        df <- data.frame(
          uuid = x$uuid,
          question = cols[i],
          change_type = change_type,
          old_value = old_values[i],
          new_value = NA
        )
        temp_df <- rbind(temp_df, df)
      }
    } else {
      df <- data.frame(
        uuid = x$uuid,
        question = var_option_other,
        change_type = change_type,
        old_value = "1",
        new_value = "0"
      )
      temp_df <- rbind(temp_df, df)
    }
  } else if (x$ref_type == "text") {
    df <- data.frame(
      uuid = x$uuid,
      question = x$ref_name,
      change_type = change_type,
      old_value = x$response_en,
      new_value = NA
    )
    temp_df <- rbind(temp_df, df)
  }

  # Append only unique combinations
  cleaning_log_other <<- rbind(
    cleaning_log_other,
    temp_df[!duplicated(temp_df[c("uuid", "question")]), ]
  )
}
################################################################################
add_to_cleaning_log_other_recode <- function(x) {
  if (x$ref_type[1] == "select_one") {
    add_to_cleaning_log_other_recode_one(x)
  }
  if (x$ref_type[1] == "select_multiple") {
    add_to_cleaning_log_other_recode_multiple(x)
  }
}
################################################################################
add_to_cleaning_log_other_recode_one <- function(x) {
  change_type <- "Recoding other response"

  # remove text of the response
  df <- data.frame(
    uuid = x$uuid,
    question = x$name,
    change_type = change_type,
    old_value = x$response_en,
    new_value = NA
  )
  cleaning_log_other <<- rbind(cleaning_log_other, df)

  # recode choice
  new_value <- get_name_from_label(x$list_name, x$existing_other)
  if (length(new_value) != 1) {
    stop(paste0(
      "Choice is not in the list: ",
      x$uuid,
      "; ",
      x$list_name,
      "; ",
      x$existing_other
    ))
  } else {
    df <- data.frame(
      uuid = x$uuid,
      question = x$ref_name,
      change_type = change_type,
      old_value = get_value_from_uuid(x$uuid, x$ref_name),
      new_value = new_value
    )
    cleaning_log_other <<- rbind(cleaning_log_other, df)
  }
}
################################################################################
add_to_cleaning_log_other_recode_multiple <- function(x) {
  change_type <- "Recoding other response"

  # get option other
  option_other <- other_db$option_other[other_db$name == x$name]
  var_option_other <- paste0(x$ref_name, ".", option_other)

  # remove text of the response
  df <- data.frame(
    uuid = x$uuid,
    question = x$name,
    change_type = change_type,
    old_value = x$response_en,
    new_value = NA
  )
  cleaning_log_other <<- rbind(cleaning_log_other, df)

  # set option other to "0" and selected choices to "1" (if not already "1")
  choices <- unlist(lapply(
    str_split(x$existing_other, ";")[[1]],
    function(c) get_name_from_label(x$list_name, c)
  ))
  if (option_other %in% choices) {
    warning(paste0(x$name, ": adding again the other option"))
  }
  el <- list(
    uuid = x$uuid,
    ref_name = other_db$ref_question[other_db$name == x$name],
    change_type = change_type
  )
  cleaning_log_other <<- rbind(
    cleaning_log_other,
    select_multiple_add_remove(
      el,
      to_remove = c(option_other),
      to_add = choices
    )
  )
}
################################################################################
select_multiple_add_remove <- function(el, to_remove, to_add = c()) {
  # load exclusive options
  exclusive_options <- c()
  # get column names
  if (str_detect(el$uuid, "_")) {
    cols <- colnames(clean_data_loop)[str_starts(
      colnames(clean_data_loop),
      paste0(el$ref_name, ".")
    )]
  } else {
    cols <- colnames(clean_data_main)[str_starts(
      colnames(clean_data_main),
      paste0(el$ref_name, ".")
    )]
  }

  # generate cleaning log
  cl <- data.frame()
  if (
    is.na(get_value_from_uuid(el$uuid, el$ref_name)) & length(to_remove) > 0
  ) {
    stop()
  }
  if (is.na(get_value_from_uuid(el$uuid, el$ref_name))) {
    #---------------------------------------------------------------------------
    # CASE 1) old value is NA
    if (length(exclusive_options) > 0 & any(exclusive_options %in% to_add)) {
      stop("To be implemented if needed")
    }
    concat <- ""
    for (col in cols) {
      choice <- str_split(col, ".")[[1]][2]
      if (choice %in% to_add) {
        new_value <- "1"
      } else {
        new_value <- "0"
      }
      cl <- rbind(
        cl,
        data.frame(
          uuid = el$uuid,
          question = col,
          old_value = NA,
          new_value = new_value
        )
      )
      if (choice %in% to_add) concat <- add_choice(concat, choice)
    }
    if (concat == "") {
      stop()
    }
    cl <- rbind(
      cl,
      data.frame(
        uuid = el$uuid,
        question = el$ref_name,
        old_value = NA,
        new_value = trimws(concat)
      )
    )
  } else {
    #---------------------------------------------------------------------------
    # CASE 2) old value is not NA
    old_concat <- get_value_from_uuid(el$uuid, el$ref_name)
    if (is.na(old_concat) | old_concat == "") {
      stop()
    }
    new_concat <- old_concat
    # remove options
    if (length(to_remove) > 0) {
      for (choice in to_remove) {
        cl <- rbind(
          cl,
          data.frame(
            uuid = el$uuid,
            question = paste0(el$ref_name, ".", choice),
            old_value = "1",
            new_value = "0"
          )
        )
        new_concat <- remove_choice(new_concat, choice)
      }
    }
    # add options
    if (length(to_add) > 0) {
      if (any(exclusive_options %in% to_add)) {
        print(paste0(
          "Recoding exclusive option: ",
          el$uuid,
          " --> ",
          el$ref_name,
          " --> ",
          to_add[1]
        ))
        if (length(to_add) > 1) {
          stop("Cannot select an exclusive option with other options")
        }
        cl <- data.frame()
        for (col in cols) {
          option <- str_split(col, ".")[[1]][2]
          old_value <- get_value_from_uuid(el$uuid, col)
          if (option == to_add[1]) {
            cl <- rbind(
              cl,
              data.frame(
                uuid = el$uuid,
                question = col,
                old_value = "0",
                new_value = "1"
              )
            )
            new_concat <- to_add[1]
          } else if (old_value == "1") {
            cl <- rbind(
              cl,
              data.frame(
                uuid = el$uuid,
                question = col,
                old_value = "1",
                new_value = "0"
              )
            )
          }
        }
      } else {
        for (choice in to_add) {
          old_value <- get_value_from_uuid(
            el$uuid,
            paste0(el$ref_name, ".", choice)
          )
          if (old_value == "0") {
            cl <- rbind(
              cl,
              data.frame(
                uuid = el$uuid,
                question = paste0(el$ref_name, ".", choice),
                old_value = "0",
                new_value = "1"
              )
            )
            new_concat <- add_choice(new_concat, choice)
          }
        }
      }
    }
    #---------------------------------------------------------------------------
    # either update the concatenate column or set all to NA if new_concat is empty
    if (new_concat != "" & new_concat != old_concat) {
      cl <- rbind(
        cl,
        data.frame(
          uuid = el$uuid,
          question = el$ref_name,
          old_value = old_concat,
          new_value = trimws(new_concat)
        )
      )
    } else if (new_concat == "") {
      cl <- data.frame()
      for (col in cols) {
        cl <- rbind(
          cl,
          data.frame(
            uuid = el$uuid,
            question = col,
            old_value = get_value_from_uuid(el$uuid, col),
            new_value = NA
          )
        )
      }
      cl <- rbind(
        cl,
        data.frame(
          uuid = el$uuid,
          question = el$ref_name,
          old_value = old_concat,
          new_value = NA
        )
      )
    }
  }
  return(cl %>% mutate(change_type = el$change_type))
}
################################################################################
save_other_responses <- function(df, ref_date = "") {
  get_column_letter <- function(r) {
    return(ifelse(
      r <= 26,
      LETTERS[r],
      ifelse(
        r <= 52,
        paste0(LETTERS[1], LETTERS[r - 26]),
        paste0(LETTERS[2], LETTERS[r - 52])
      )
    ))
  }

  # save other responses
  wb <- createWorkbook()
  style.col.color <- createStyle(
    fgFill = "#E5FFCC",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    valign = "top",
    wrapText = T
  )
  style.col.color1 <- createStyle(
    fgFill = "#E5FFEC",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    valign = "top",
    wrapText = T
  )
  style.col.color2 <- createStyle(
    fgFill = "#CCE5FF",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    valign = "top",
    wrapText = T
  )
  style.col.color.first <- createStyle(
    textDecoration = "bold",
    fgFill = "#E5FFCC",
    valign = "top",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    wrapText = T
  )
  style.col.color.first1 <- createStyle(
    textDecoration = "bold",
    fgFill = "#E5FFEC",
    valign = "top",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    wrapText = T
  )
  style.col.color.first2 <- createStyle(
    textDecoration = "bold",
    fgFill = "#CCE5FF",
    valign = "top",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    wrapText = T
  )

  addWorksheet(wb, "Sheet1")
  writeData(wb = wb, x = df, sheet = "Sheet1", startRow = 1)

  addWorksheet(wb, "Dropdown_values")
  for (r in 1:nrow(other_db)) {
    if (other_db$q_type[r] != "text") {
      choices <- str_split(other_db$choices[r], ";;")[[1]]
      writeData(wb, sheet = "Dropdown_values", x = choices, startCol = r)
      uuids <- which(df$question_name == other_db$name[r])
      if (length(uuids) > 0) {
        column_letter <- get_column_letter(r)
        values <- paste0(
          "'Dropdown_values'!$",
          column_letter,
          "$1:$",
          column_letter,
          "$",
          other_db$num_choices[r]
        )
        dataValidation(
          wb,
          "Sheet1",
          col = 10,
          rows = uuids + 1,
          type = "list",
          value = values
        )
        dataValidation(
          wb,
          "Sheet1",
          col = 11,
          rows = uuids + 1,
          type = "list",
          value = values
        )
        dataValidation(
          wb,
          "Sheet1",
          col = 12,
          rows = uuids + 1,
          type = "list",
          value = values
        )
      }
    }
  }
  writeData(wb, sheet = "Dropdown_values", x = c("Yes"), startCol = r + 1)
  column_letter <- get_column_letter(r + 1)
  values <- paste0(
    "'Dropdown_values'!$",
    column_letter,
    "$1:$",
    column_letter,
    "$1"
  )
  dataValidation(
    wb,
    "Sheet1",
    col = 13,
    rows = 2:(nrow(df) + 1),
    type = "list",
    value = values
  )

  setColWidths(wb, "Sheet1", cols = 1:5, widths = 10)
  setColWidths(wb, "Sheet1", cols = 6:9, widths = 30)
  setColWidths(wb, "Sheet1", cols = 10:14, widths = 25)
  setColWidths(wb, "Sheet1", cols = 15:16, widths = 30)

  addStyle(
    wb,
    "Sheet1",
    style = createStyle(valign = "top"),
    rows = 1:(nrow(df) + 1),
    cols = 1
  )
  addStyle(
    wb,
    "Sheet1",
    style = createStyle(valign = "top"),
    rows = 1:(nrow(df) + 1),
    cols = 2
  )
  addStyle(
    wb,
    "Sheet1",
    style = createStyle(valign = "top"),
    rows = 1:(nrow(df) + 1),
    cols = 3
  )
  addStyle(
    wb,
    "Sheet1",
    style = createStyle(valign = "top"),
    rows = 1:(nrow(df) + 1),
    cols = 4
  )
  addStyle(
    wb,
    "Sheet1",
    style = createStyle(valign = "top"),
    rows = 1:(nrow(df) + 1),
    cols = 5
  )
  addStyle(
    wb,
    "Sheet1",
    style = createStyle(wrapText = T, valign = "top"),
    rows = 1:(nrow(df) + 1),
    cols = 6
  )
  addStyle(
    wb,
    "Sheet1",
    style = createStyle(wrapText = T, valign = "top"),
    rows = 1:(nrow(df) + 1),
    cols = 7
  )
  addStyle(
    wb,
    "Sheet1",
    style = createStyle(wrapText = T, valign = "top"),
    rows = 1:(nrow(df) + 1),
    cols = 8
  )
  addStyle(
    wb,
    "Sheet1",
    style = createStyle(wrapText = T, valign = "top"),
    rows = 1:(nrow(df) + 1),
    cols = 9
  )
  addStyle(
    wb,
    "Sheet1",
    style = createStyle(textDecoration = "bold"),
    rows = 1,
    cols = 1:ncol(df)
  )
  addStyle(wb, "Sheet1", style = style.col.color.first, rows = 1, cols = 9)
  addStyle(wb, "Sheet1", style = style.col.color.first1, rows = 1, cols = 10:12)
  addStyle(wb, "Sheet1", style = style.col.color.first, rows = 1, cols = 13)
  addStyle(wb, "Sheet1", style = style.col.color.first2, rows = 1, cols = 14:15)

  addStyle(
    wb,
    "Sheet1",
    style = style.col.color,
    rows = 1:(nrow(df) + 1),
    cols = 9
  )
  addStyle(
    wb,
    "Sheet1",
    style = style.col.color1,
    rows = 1:(nrow(df) + 1),
    cols = 10
  )
  addStyle(
    wb,
    "Sheet1",
    style = style.col.color1,
    rows = 1:(nrow(df) + 1),
    cols = 11
  )
  addStyle(
    wb,
    "Sheet1",
    style = style.col.color1,
    rows = 1:(nrow(df) + 1),
    cols = 12
  )
  addStyle(
    wb,
    "Sheet1",
    style = style.col.color,
    rows = 1:(nrow(df) + 1),
    cols = 13
  )
  addStyle(
    wb,
    "Sheet1",
    style = style.col.color2,
    rows = 1:(nrow(df) + 1),
    cols = 14
  )
  addStyle(
    wb,
    "Sheet1",
    style = style.col.color2,
    rows = 1:(nrow(df) + 1),
    cols = 15
  )

  modifyBaseFont(wb, fontSize = 12, fontColour = "black", fontName = "Calibri")

  # sub.filename=paste0("2024-08-11", "_other_responses.xlsx")
  sub.filename = paste0(Sys.Date(), "_other_responses.xlsx")

  saveWorkbook(
    wb,
    paste0("output/checking/other_responses/", sub.filename),
    overwrite = T
  )
}

#############################################################################################################
# UTILS
#############################################################################################################
get_name_from_label <- function(list_name, label) {
  return(kobo_choices$name[
    kobo_choices$list_name == list_name & kobo_choices$label == label
  ])
}
################################################################################
get_label_from_name <- function(list.name, name) {
  return(kobo_choices$label[
    kobo_choices$list_name == list.name & kobo_choices$name == name
  ])
}
################################################################################
get_value_from_uuid <- function(uuid, column) {
  # print(uuid)
  if (missing(uuid) || missing(column)) {
    warning("Missing parameters in get_value_from_uuid")
    return(NA)
  }
  if (str_detect(uuid, "_")) {
    # Sequentially check the relevant data frames for a match
    if (uuid %in% raw_roster$uuid) {
      value <- raw_roster[[column]][raw_roster$uuid == uuid]
    } else if (uuid %in% raw_non_agric_business$uuid) {
      value <- raw_non_agric_business[[column]][
        raw_non_agric_business$uuid == uuid
      ]
    } else if (uuid %in% raw_other_major_expenses$uuid) {
      value <- raw_other_major_expenses[[column]][
        raw_other_major_expenses$uuid == uuid
      ]
    } else {
      warning("UUID not found in any sub-data frame: ", uuid)
      value <- NA
    }
  } else {
    # If no underscore, pull from main dataset
    if (uuid %in% raw_data$uuid) {
      value <- raw_data[[column]][raw_data$uuid == uuid]
    } else {
      warning("UUID not found in raw_data: ", uuid)
      value <- NA
    }
    # value <- raw_data[[column]][raw_data$uuid == uuid]
  }
  # Ensure we return a single value (not a vector)
  if (length(value) > 1) {
    warning(paste(
      "Multiple matches found for UUID:",
      uuid,
      "- returning first value"
    ))
    value <- value[1]
  } else if (length(value) == 0) {
    warning(paste("No matches found for UUID:", uuid))
    value <- NA
  }

  return(value)
}
################################################################################
add_choice <- function(concat_value, choice) {
  l <- str_split(concat_value, " ")[[1]]
  l <- sort(unique(c(l, choice)))
  l <- l[l != ""]
  return(paste(l, collapse = " "))
}
################################################################################
remove_choice <- function(concat_value, choice) {
  l <- str_split(concat_value, " ")[[1]]
  l <- l[l != choice]
  return(paste(l, collapse = " "))
}
################################################################################
get_ref_question <- function(x) {
  x.1 <- str_split(x, "\\{")[[1]][2]
  return(str_split(x.1, "\\}")[[1]][1])
}
################################################################################

#############################################################################################################
# DOWNLOAD audit files
#############################################################################################################
download_audit_files <- function(
  df,
  uuid_column = "_uuid",
  audit_dir,
  usr,
  pass
) {
  df <- df %>% filter(!is.na(audit_URL))
  if (!"httr" %in% installed.packages()) {
    stop("The package is httr is required!")
  }
  if (is.na(audit_dir) || audit_dir == "") {
    stop("The path for storing audit files can't be empty!")
  }
  if (is.na(usr) || usr == "") {
    stop("Username can't be empty!")
  }
  if (is.na(pass) || pass == "") {
    stop("Password can't be empty!")
  }

  # checking if the output directory is already available
  if (!dir.exists(audit_dir)) {
    dir.create(audit_dir)
    if (dir.exists(audit_dir)) {
      cat("Attention: The audit file directory was created in", audit_dir, "\n")
    }
  }

  # checking if creating output directory was successful
  if (!dir.exists(audit_dir)) {
    stop("download_audit_fils was not able to create the output directory!")
  }
  # checking if uuid column exists in data set
  if (!uuid_column %in% names(df)) {
    stop("The column ", uuid_column, " is not available in data set.")
  }
  # checking if column audit_URL exists in data set
  if (!"audit_URL" %in% names(df)) {
    stop("Error: the column audit_URL is not available in data set.")
  }

  # getting the list of uuids that are already downloaded
  available_audits <- dir(audit_dir)

  # excluding uuids that their audit files are already downloaded
  df <- df[!df[[uuid_column]] %in% available_audits, ]

  audits_endpoint_link <- df[["audit_URL"]]
  names(audits_endpoint_link) <- df[[uuid_column]]
  audits_endpoint_link <- na.omit(audits_endpoint_link)

  if (length(audits_endpoint_link) > 0) {
    # iterating over each audit endpoint from data
    for (i in 1:length(audits_endpoint_link)) {
      uuid = names(audits_endpoint_link[i])
      endpoint_link_i <- audits_endpoint_link[i]
      cat("Downloading audit file for", uuid, "\n")

      # requesting data
      audit_file <- content(
        GET(
          endpoint_link_i,
          authenticate(usr, pass),
          timeout(1000),
          progress()
        ),
        "text",
        encoding = "UTF-8"
      )

      if (!is.na(audit_file)) {
        if (length(audit_file) > 2) {
          dir.create(paste0(audit_dir, "/", uuid), showWarnings = F)
          write.csv(
            audit_file,
            paste0(audit_dir, "/", uuid, "/audit.csv"),
            row.names = F
          )
        } else if (!audit_file == "Attachment not found") {
          if (grepl("[eventnodestartend]", audit_file)) {
            dir.create(paste0(audit_dir, "/", uuid), showWarnings = F)
            write.table(
              audit_file,
              paste0(audit_dir, "/", uuid, "/audit.csv"),
              row.names = F,
              col.names = FALSE,
              quote = F
            )
          } else {
            cat("Error: Downloading audit was unsucessful!\n")
          }
        }
      } else {
        cat("Error: No audit file audit was unsucessful!\n")
      }
    }
  } else {
    cat("Attention: All audit files are already downloaded!")
  }
}
