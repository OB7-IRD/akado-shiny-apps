
# Function that checks the consistency of the various lists before filtering by the user
check_consistency_list <- function(sql_info, check_info, column_user_info, type_check_info, tab_info) {
  # 1 - Arguments verification ----
  name_file_sql <- sapply(sql_info, `[[`, "file")
  name_column_sql <- sapply(sql_info, `[[`, "column_user_id")
  name_rename_column_user_check <- sapply(check_info, `[[`, "rename_column_user")
  # Check tab_info arguments and retrieve all tab identifiers
  all_id_tab <- lapply(tab_info, function(tab) {
    # Check that the sublist contains an 'id' element
    if (!("id" %in% names(tab))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Impossible to identify the tab because there is no element in the sub-list named 'id'.
   Present element : ",
        paste0(paste(names(tab), tab, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    if (!codama::r_type_checking(
      r_object = tab[["id"]],
      type = "character",
      length = 1L,
      output = "logical"
    )) {
      return(codama::r_type_checking(
        r_object = tab[["id"]],
        type = "character",
        length = 1L,
        output = "error"
      ))
    }
    # Check that element 'title' in the sub-list is character
    if (!("title" %in% names(tab))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Tabs must have a title, there is no element in the sub-list named 'title'.
   Present element : ",
        paste0(paste(names(tab), tab, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    if (!codama::r_type_checking(
      r_object = tab[["title"]],
      type = "character",
      output = "logical"
    )) {
      return(codama::r_type_checking(
        r_object = tab[["title"]],
        type = "character",
        output = "error"
      ))
    }
    # Check that element 'text' in the sub-list is character
    if ("text" %in% names(tab)) {
      if (!codama::r_type_checking(
        r_object = tab[["text"]],
        type = "character",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = tab[["text"]],
          type = "character",
          output = "error"
        ))
      }
    }
    # Check that element 'display_dividing_lines' in the sub-list is logical
    if ("display_dividing_lines" %in% names(tab)) {
      if (!codama::r_type_checking(
        r_object = tab[["display_dividing_lines"]],
        type = "logical",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = tab[["display_dividing_lines"]],
          type = "logical",
          output = "error"
        ))
      }
    }
    return(tab[["id"]])
  })
  if (length(unlist(all_id_tab)) != length(unique(unlist(all_id_tab)))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - Tab id are not unique.",
      "\n Tab id : ",
      paste0(unlist(all_id_tab), collapse = ", "),
      sep = ""
    )
  }
  # Check type_check_info arguments and retrieve all choice identifiers
  # Check that the sublist contains an 'title' element
  if (!("title" %in% names(type_check_info))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - Information is missing for the button used to select the check to be displayed. There is no element in the sub-list named 'title'.",
      "\n Present element : ",
      paste0(paste(names(type_check_info), type_check_info, sep = " : "), collapse = ", "),
      sep = ""
    )
  }
  if (!codama::r_type_checking(
    r_object = type_check_info[["title"]],
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = type_check_info[["title"]],
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  all_id_choice_type_check <- lapply(type_check_info, function(choice) {
    if (is.list(choice)) {
      # Check that the sublist contains an 'id' element
      if (!("id" %in% names(choice))) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - Impossible to identify the choice because there is no element in the sub-list named 'id'.
   Present element : ",
          paste0(paste(names(choice), choice, sep = " : "), collapse = ", "),
          sep = ""
        )
      }
      if (!codama::r_type_checking(
        r_object = choice[["id"]],
        type = "character",
        length = 1L,
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = choice[["id"]],
          type = "character",
          length = 1L,
          output = "error"
        ))
      }
      # Check that element 'text' in the sub-list is character
      if (!("text" %in% names(choice))) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - Tabs must have a text, there is no element in the sub-list named 'text'.
     Present element : ",
          paste0(paste(names(choice), choice, sep = " : "), collapse = ", "),
          sep = ""
        )
      }
      if (!codama::r_type_checking(
        r_object = choice[["text"]],
        type = "character",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = choice[["text"]],
          type = "character",
          output = "error"
        ))
      }
      # Check that element 'specific_check' in the sub-list is logical
      if ("specific_check" %in% names(choice)) {
        if (!codama::r_type_checking(
          r_object = choice[["specific_check"]],
          type = "logical",
          output = "logical"
        )) {
          return(codama::r_type_checking(
            r_object = choice[["specific_check"]],
            type = "logical",
            output = "error"
          ))
        }
      }
      return(choice[["id"]])
    }
  })
  if (length(unlist(all_id_choice_type_check)) != length(unique(unlist(all_id_choice_type_check)))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - Choice id of check types are not unique.",
      "\n Choice id : ",
      paste0(unlist(all_id_choice_type_check), collapse = ", "),
      sep = ""
    )
  }
  # Check check_info arguments and retrieve all tab identifiers
  all_id_check <- lapply(check_info, function(check) {
    # Check that the sublist contains an 'id' element
    if (!("id" %in% names(check))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Impossible to identify the check because there is no element in the sub-list named 'id'.",
        "\n Present element : ",
        paste0(paste(names(check), check, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    if (!codama::r_type_checking(
      r_object = check[["id"]],
      type = "character",
      length = 1L,
      output = "logical"
    )) {
      return(codama::r_type_checking(
        r_object = check[["id"]],
        type = "character",
        length = 1L,
        output = "error"
      ))
    }
    # Check that the sublist contains an 'function_check' element
    if (!("function_check" %in% names(check))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Information is missing for check. There is no element in the sub-list named 'function_check', mandatory.",
        "\n Present element : ",
        paste0(paste(names(check), check, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    # Check that element 'function_check' in the sub-list is function
    if (!inherits(x = check[["function_check"]], what = "function")) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - The sub-list named 'function_check' must be a function.",
        "\n check id : ",
        check[["id"]],
        "\n function_check : ",
        check[["function_check"]]
        ,
        sep = ""
      )
    }
    # Check that the sublist contains an 'argument_function_check' element
    if (!("argument_function_check" %in% names(check))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Information is missing for check. There is no element in the sub-list named 'argument_function_check', mandatory.",
        "\n Present element : ",
        paste0(paste(names(check), check, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    if (!codama::r_type_checking(
      r_object = check[["argument_function_check"]],
      type = "list",
      output = "logical"
    )) {
      return(codama::r_type_checking(
        r_object = check[["argument_function_check"]],
        type = "list",
        output = "error"
      ))
    }
    # Check that the sublist contains an 'table_user_id' element
    if (!("table_user_id" %in% names(check))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Information is missing for check. There is no element in the sub-list named 'table_user_id', mandatory.",
        "\n Present element : ",
        paste0(paste(names(check), check, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    if (!codama::r_type_checking(
      r_object = check[["table_user_id"]],
      type = "character",
      output = "logical"
    )) {
      return(codama::r_type_checking(
        r_object = check[["table_user_id"]],
        type = "character",
        output = "error"
      ))
    }
    # Check that the sublist contains an 'user_type' element
    if (!("user_type" %in% names(check))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Information is missing for check. There is no element in the sub-list named 'user_type', mandatory.",
        "\n Present element : ",
        paste0(paste(names(check), check, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    if (!codama::r_type_checking(
      r_object = check[["user_type"]],
      type = "character",
      output = "logical"
    )) {
      return(codama::r_type_checking(
        r_object = check[["user_type"]],
        type = "character",
        output = "error"
      ))
    }
    # Check that element 'rename_column_user' in the sub-list is list
    if ("rename_column_user" %in% names(check)) {
      if (!codama::r_type_checking(
        r_object = check[["rename_column_user"]],
        type = "list",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["rename_column_user"]],
          type = "list",
          output = "error"
        ))
      }
    }
    # Check that the sublist contains an 'function_data_plot' element
    if ("function_data_plot" %in% names(check)) {
      # Check that element 'function_data_plot' in the sub-list is function
      if (!inherits(x = check[["function_data_plot"]], what = "function")) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - The sub-list named 'function_data_plot' must be a function.",
          "\n check id : ",
          check[["id"]],
          "\n function_data_plot : ",
          check[["function_data_plot"]]
          ,
          sep = ""
        )
      }
      # Check that the sublist contains an 'argument_function_data_plot' element
      if (!("argument_function_data_plot" %in% names(check))) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - Information is missing for check. There is no element in the sub-list named 'argument_function_data_plot', mandatory if function_data_plot exist.",
          "\n Present element : ",
          paste0(paste(names(check), check, sep = " : "), collapse = ", "),
          sep = ""
        )
      }
      # Check that element 'argument_function_data_plot' in the sub-list is list
      if (!codama::r_type_checking(
        r_object = check[["argument_function_data_plot"]],
        type = "list",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["argument_function_data_plot"]],
          type = "list",
          output = "error"
        ))
      }
    }
    if ("additional_column_user" %in% names(check)) {
      # Check that element 'additional_column_user' in the sub-list is character
      if (!codama::r_type_checking(
        r_object = check[["additional_column_user"]],
        type = "character",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["additional_column_user"]],
          type = "character",
          output = "error"
        ))
      }
    }
    if ("function_display" %in% names(check)) {
      # Check that element 'function_display' in the sub-list is function
      if (!inherits(x = check[["function_display"]], what = "function")) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - The sub-list named 'function_display' must be a function.",
          "\n check id : ",
          check[["id"]],
          "\n function_display : ",
          check[["function_display"]]
          ,
          sep = ""
        )
      }
    }
    if ("argument_function_display" %in% names(check)) {
      # Check that element 'argument_function_display' in the sub-list is list
      if (!codama::r_type_checking(
        r_object = check[["argument_function_display"]],
        type = "list",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["argument_function_display"]],
          type = "list",
          output = "error"
        ))
      }
    }
    if ("need_vms" %in% names(check)) {
      # Check that element 'need_vms' in the sub-list is logical
      if (!codama::r_type_checking(
        r_object = check[["need_vms"]],
        type = "logical",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["need_vms"]],
          type = "logical",
          output = "error"
        ))
      }
    }
    # Check that element 'title' in the sub-list is character
    if ("title" %in% names(check)) {
      if (!codama::r_type_checking(
        r_object = check[["title"]],
        type = "character",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["title"]],
          type = "character",
          output = "error"
        ))
      }
    }
    # Check that element 'text' in the sub-list is character
    if ("text" %in% names(check)) {
      if (!codama::r_type_checking(
        r_object = check[["text"]],
        type = "character",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["text"]],
          type = "character",
          output = "error"
        ))
      }
    }
    # Check that element 'type' in the sub-list is character
    if (!("type" %in% names(check))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Check must have a type, there is no element in the sub-list named 'type'.",
        "\n Present element : ",
        paste0(paste(names(check), check, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    if (!codama::r_type_checking(
      r_object = check[["type"]],
      type = "character",
      allowed_value = unlist(all_id_choice_type_check),
      output = "logical"
    )) {
      return(codama::r_type_checking(
        r_object = check[["type"]],
        type = "character",
        allowed_value = unlist(all_id_choice_type_check),
        output = "error"
      ))
    }
    # Check that element 'size_box' in the sub-list is character
    if ("size_box" %in% names(check)) {
      if (!codama::r_type_checking(
        r_object = check[["size_box"]],
        type = "character",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["size_box"]],
          type = "character",
          output = "error"
        ))
      }
    }
    # Check that element 'column_no_wrap' in the sub-list is numeric
    if ("column_no_wrap" %in% names(check)) {
      if (!codama::r_type_checking(
        r_object = check[["column_no_wrap"]],
        type = "numeric",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["column_no_wrap"]],
          type = "numeric",
          output = "error"
        ))
      }
    }
    # Check that element 'function_plot' in the sub-list is function
    if ("function_plot" %in% names(check)) {
      if (!inherits(x = check[["function_plot"]], what = "function")) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - The sub-list named 'function_plot' must be a function.",
          "\n check id : ",
          check[["id"]],
          "\n function_plot : ",
          check[["function_plot"]]
          ,
          sep = ""
        )
      }
    }
    # Check that element 'function_text_plot' in the sub-list is character
    if ("function_text_plot" %in% names(check)) {
      if (!inherits(x = check[["function_text_plot"]], what = "function")) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - The sub-list named 'function_text_plot' must be a function.",
          "\n check id : ",
          check[["id"]],
          "\n function_text_plot : ",
          check[["function_text_plot"]]
          ,
          sep = ""
        )
      }
    }
    # Check that element 'title_window' in the sub-list is character
    if ("title_window" %in% names(check)) {
      if (!codama::r_type_checking(
        r_object = check[["title_window"]],
        type = "character",
        length = 1L,
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["title_window"]],
          type = "character",
          length = 1L,
          output = "error"
        ))
      }
    }
    # Check that the sublist contains an 'tab' element
    if (!("tab" %in% names(check))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Impossible to identify the tab for check because there is no element in the sub-list named 'tab'.",
        "\n Control element available : ",
        paste0(paste(names(check), check, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    # Check that element 'tab' in the sub-list is character
    if (!codama::r_type_checking(
      r_object = check[["tab"]],
      type = "character",
      length = 1L,
      output = "logical"
    )) {
      return(codama::r_type_checking(
        r_object = check[["tab"]],
        type = "character",
        length = 1L,
        output = "error"
      ))
    }
    if (!(check[["tab"]] %in% unlist(all_id_tab))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Invalid tab reference for check display.",
        "\n Check tab name : ",
        check[["tab"]],
        " for check id ", check[["id"]],
        "\n Tab name available : ",
        paste0(unlist(all_id_tab), collapse = ", "),
        sep = ""
      )
    }
    return(check[["id"]])
  })
  if (length(unlist(all_id_check)) != length(unique(unlist(all_id_check)))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - Check id are not unique.",
      "\n Check id : ",
      paste0(unlist(all_id_check), collapse = ", "),
      sep = ""
    )
  }
  # Check arguments in sql_info
  lapply(sql_info, function(sql) {
    # Check that the sublist contains an 'file' element
    if (!("file" %in% names(sql))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Impossible to identify the sql because there is no element in the sub-list named 'file'.",
        "\n Present element : ",
        paste0(paste(names(sql), sql, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    if (!codama::r_type_checking(
      r_object = sql[["file"]],
      type = "character",
      length = 1L,
      output = "logical"
    )) {
      return(codama::r_type_checking(
        r_object = sql[["file"]],
        type = "character",
        length = 1L,
        output = "error"
      ))
    }
    # Check that element 'anchor' in the sub-list is list
    if ("anchor" %in% names(sql)) {
      if (!codama::r_type_checking(
        r_object = sql[["anchor"]],
        type = "list",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = sql[["anchor"]],
          type = "list",
          output = "error"
        ))
      }
    }
    # Check that element 'use_selection_other_sql' in the sub-list is list
    if ("use_selection_other_sql" %in% names(sql)) {
      if (!codama::r_type_checking(
        r_object = sql[["use_selection_other_sql"]],
        type = "logical",
        length = 1L,
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = sql[["use_selection_other_sql"]],
          type = "logical",
          length = 1L,
          output = "error"
        ))
      }
      # Check that the sublist contains an 'column_anchor' element
      if (sql[["use_selection_other_sql"]] && (!("vector" %in% names(sql)) || !sql[["vector"]])) {
        if (!("column_anchor" %in% names(sql))) {
          stop(
            format(
              x = Sys.time(),
              format = "%Y-%m-%d %H:%M:%S"
            ),
            " - Information is missing for SQL. There is no element in the sub-list named 'column_anchor', mandatory if use_selection_other_sql are TRUE and vector is FALSE.",
            "\n Present element : ",
            paste0(paste(names(sql), sql, sep = " : "), collapse = ", "),
            sep = ""
          )
        }
        if (!codama::r_type_checking(
          r_object = sql[["column_anchor"]],
          type = "character",
          length = 1L,
          output = "logical"
        )) {
          return(codama::r_type_checking(
            r_object = sql[["column_anchor"]],
            type = "character",
            length = 1L,
            output = "error"
          ))
        }
      }
    }
    # Check that element 'column_user_id' in the sub-list is list
    if ("column_user_id" %in% names(sql)) {
      if (!codama::r_type_checking(
        r_object = sql[["column_user_id"]],
        type = "character",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = sql[["column_user_id"]],
          type = "character",
          output = "error"
        ))
      }
    }
    # Check that element 'vector' in the sub-list is list
    if ("vector" %in% names(sql)) {
      # Check that element 'vector' in the sub-list is logical
      if (!codama::r_type_checking(
        r_object = sql[["vector"]],
        type = "logical",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = sql[["vector"]],
          type = "logical",
          output = "error"
        ))
      }
    }
  })
  if (length(unlist(name_file_sql)) != length(unique(unlist(name_file_sql)))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - File names must be unique.",
      "\n File names : ",
      paste0(unlist(name_file_sql), collapse = ", "),
      sep = ""
    )
  }
  # Check that the sublist contains an 'rename_id_column_user' element
  if (!("rename_id_column_user" %in% names(column_user_info))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - There is no element in the sub-list named 'rename_id_column_user' in list column_user_info.",
      "\n Present element : ",
      paste0(paste(names(column_user_info), column_user_info, sep = " : "), collapse = ", "),
      sep = ""
    )
  }
  if (!codama::r_type_checking(
    r_object = column_user_info[["rename_id_column_user"]],
    type = "list",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = column_user_info[["rename_id_column_user"]],
      type = "list",
      output = "error"
    ))
  }
  if (!all(names(column_user_info[["rename_id_column_user"]]) %in% unlist(name_column_sql))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - The column names of sub-list named 'rename_id_column_user' must also exist in at least one sub-list 'column_user_id', if the rename only concerns a specific check, then use the 'rename_column_user' sub-list.",
      "\n Problematic column names of sub-list named 'rename_id_column_user' : ",
      paste0(names(column_user_info[["rename_id_column_user"]])[!(names(column_user_info[["rename_id_column_user"]]) %in% unlist(name_column_sql))], collapse = ", "),
      sep = ""
    )
  }
  if (any(names(column_user_info[["rename_id_column_user"]]) %in% names(unlist(name_rename_column_user_check)))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - The column names of sub-list named 'rename_id_column_user' must not also be indicated in a sub-list 'rename_column_user', either the column allows the user to identify the row, in which case use sub-list 'rename_id_column_user', or it is specific to a check, in which case use sub-list 'rename_column_user'.",
      "\n Problematic column names of sub-list named 'rename_id_column_user' : ",
      paste0(names(column_user_info[["rename_id_column_user"]])[names(column_user_info[["rename_id_column_user"]]) %in% names(unlist(name_rename_column_user_check))], collapse = ", "),
      sep = ""
    )
  }
  # Check that the sublist contains an 'order_id_column_user' element
  if (!("order_id_column_user" %in% names(column_user_info))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - There is no element in the sub-list named 'order_id_column_user' in list column_user_info.",
      "\n Present element : ",
      paste0(paste(names(column_user_info), column_user_info, sep = " : "), collapse = ", "),
      sep = ""
    )
  }
  if (!codama::r_type_checking(
    r_object = column_user_info[["order_id_column_user"]],
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = column_user_info[["order_id_column_user"]],
      type = "character",
      output = "error"
    ))
  }
  if (!all(names(column_user_info[["order_id_column_user"]]) %in% unlist(name_column_sql))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - The column names of sub-list named 'order_id_column_user' must also exist in at least one sub-list 'column_user_id'.",
      "\n Problematic column names of sub-list named 'order_id_column_user' : ",
      paste0(names(column_user_info[["order_id_column_user"]])[!(names(column_user_info[["order_id_column_user"]]) %in% unlist(name_column_sql))], collapse = ", "),
      sep = ""
    )
  }
}

# Shiny function : Error message if the trip selection elements are not correctly filled in
text_error_trip_select_server <- function(id, parent_in, parameters_trip_select) {
  moduleServer(id, function(input, output, session) {
    eventReactive(input$start_button, {
      # If the database selection is missing
      if (is.null(parent_in$`tab-data_base_observe`)) {
        return("Error: please select a database Observe")
      }
      # If trip selection elements are not loaded
      if (parent_in[["start_data_base-start_data_base"]] == 0 || !is.data.frame(parameters_trip_select())) {
        return("Error: please load trip selection elements")
      }
      # If the check selection is missing
      if (is.null(parent_in[["tab-select_check"]])) {
        return("Error: please select at least 1 check")
      }
      return(TRUE)
    })
  })
}

# Shiny function : Read the .yml file of configuration for the connection
config_data_server <- function(id, parent_in) {
  moduleServer(id, function(input, output, session) {
    # Triggers reading of the configuration file when the user loads one (parent_in$setting_file_path) (also enables reading of the file when the application is launched via initialization of parent_in$setting_file_path) or when a calculation is restarted (input$start_button or input$start_data_base) (enables the user to take advantage of the new version available in .appconfig)
    eventReactive(c(input$start_data_base, input$start_button, parent_in$setting_file_path), {
      # If the user has not specified a file and the file exists in the default path, indicates the default path
      if (is.null(parent_in$setting_file_path$datapath) && file.exists(file.path(path.expand("~"), ".appconfig", "akador", "configuration_file.yml"))) {
        path_setting_file <- file.path(path.expand("~"), ".appconfig", "akador", "configuration_file.yml")
        # If the user has specified a file, indicates the path to specify
      } else if (!is.null(parent_in$setting_file_path$datapath)) {
        path_setting_file <- parent_in$setting_file_path$datapath
      }
      # Read the file if the path is available
      if (exists("path_setting_file")) {
        furdeb::configuration_file(
          path_file = path_setting_file,
          silent = TRUE
        )
      }
    })
  })
}

# Shiny function : Retrieves the list of trips and VMS selected by the user
trip_select_server <- function(id, parent_in, text_error_trip_select, config_data, sql_info_selected, parameters_trip_select) {
  # 0 - Global variables assignement ----
  vessel_status <- NULL
  vessel_id <- NULL
  ocean_id <- NULL
  flagcountry_id <- NULL
  fleetcountry_id <- NULL
  logbookprogram_id <- NULL
  trip_enddate <- NULL
  trip_id <- NULL
  vms_id <- NULL
  vessel_code <- NULL
  vms_date <- NULL
  vms_codevessel <- NULL
  vessel_type <- NULL
  vessel_statut <- NULL
  # 1 - Data design ----
  moduleServer(id, function(input, output, session) {
    eventReactive(input$start_button, {
      # Recovers user trip selection parameters
      if (text_error_trip_select() == TRUE) {
        trip_select <- parameters_trip_select()
        # If the user wants active trips
        if (parent_in[["tab-vessel_active"]]) {
          trip_select <- trip_select %>% dplyr::filter(vessel_status == 1)
        }
        # If the user wants several trips
        if (parent_in[["tab-select_several_trip"]]) {
          # If the user has chosen a vessel
          if (!is.null(parent_in[["tab-vessel"]])) {
            trip_select <- trip_select %>% dplyr::filter(vessel_id == parent_in[["tab-vessel"]])
          }
          # If the user has chosen a ocean
          if (!is.null(parent_in[["tab-ocean"]])) {
            trip_select <- trip_select %>% dplyr::filter(ocean_id == parent_in[["tab-ocean"]])
          }
          # If the user has chosen a flag country
          if (!is.null(parent_in[["tab-flag"]])) {
            trip_select <- trip_select %>% dplyr::filter(flagcountry_id == parent_in[["tab-flag"]])
          }
          # If the user has chosen a fleet country
          if (!is.null(parent_in[["tab-fleet"]])) {
            trip_select <- trip_select %>% dplyr::filter(fleetcountry_id == parent_in[["tab-fleet"]])
          }
          # If the user has chosen a logbook program
          if (!is.null(parent_in[["tab-program"]])) {
            trip_select <- trip_select %>% dplyr::filter(logbookprogram_id == parent_in[["tab-program"]])
          }
          # User has chosen a range end date
          trip_select <- trip_select %>% dplyr::filter(trip_enddate >= parent_in[["tab-range_enddate"]][1] & trip_enddate <= parent_in[["tab-range_enddate"]][2])
        } else {
          # If the user wants only one trip
          trip_select <- trip_select %>% dplyr::filter(trip_id == parent_in[["tab-one_trip"]])
        }
        list_return <- list(trip_selected = trip_select)
        if (dim(trip_select)[1] > 0) {
          # If selected several trips
          if (parent_in[["tab-select_several_trip"]]) {
            # VMS selection parameter date range by user-selected period explicit
            start_date_range <- parent_in[["tab-range_enddate"]][1]
            end_date_range <- parent_in[["tab-range_enddate"]][2]
            # VMS selection if parameter vessel by user-selected explicit
            if (!is.null(parent_in[["tab-vessel"]])) {
              vessel_number <- parent_in[["tab-vessel"]]
            } else {
              vessel_number <- ""
            }
          } else {
            # If selected only one trip
            # VMS selection parameter date range by user-selected trip duration
            start_date_range <- min(trip_select$trip_startdate)
            end_date_range <- max(trip_select$trip_enddate)
            # VMS selection parameter vessel number by user-selected explicit
            vessel_number <- as.character(trip_select$vessel_code)
          }
        }
        # Recovery of reactive values
        sql_info <- sql_info_selected()[["sql_info_input_user"]]
        # If the connection data exists and there was no error in the trip selection, makes the connection
        if (dim(trip_select)[1] > 0 && "activity_vms" %in% sapply(sql_info, `[[`, "file")) {
          cat(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start sql activity_vms \n", sep = "")
          config_observe_database <- config_data()[["databases_configuration"]]
          data_connection <- list()
          for (observe_database in config_observe_database[c(parent_in$`tab-data_base_observe`)]){
            data_connection <- append(data_connection, list(furdeb::postgresql_dbconnection(
              db_user = observe_database[["login"]],
              db_password = observe_database[["password"]],
              db_dbname = observe_database[["dbname"]],
              db_host = observe_database[["host"]],
              db_port = observe_database[["port"]]
            )))
          }
          # Uses a function to extract data from VMS
          activity_vms <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "activity_vms.sql",
                                    package = "AkadoR"),
            database_connection = data_connection,
            anchor = list(select_item_1 = start_date_range, select_item_2 = end_date_range, select_item_3 = vessel_number)
          )
          list_return <- append(list_return, list(activity_vms = activity_vms))
          # Disconnection to the base
          for (i in seq(from = 1, to = length(data_connection))) {
            DBI::dbDisconnect(data_connection[[i]][[2]])
          }
        }
        if (dim(trip_select)[1] > 0 && "vms" %in% sapply(sql_info, `[[`, "file")) {
          # If the database is "vms", read, transform and execute the SQL query that selects the trips according to the user parameters
          if (!is.null(config_data()[["vms_databases_configuration"]][["vms"]])) {
            # Connection to the base VMS
            data_connection_vms <- furdeb::postgresql_dbconnection(
              db_user = config_data()[["vms_databases_configuration"]][["vms"]][["login"]],
              db_password = config_data()[["vms_databases_configuration"]][["vms"]][["password"]],
              db_dbname = config_data()[["vms_databases_configuration"]][["vms"]][["dbname"]],
              db_host = config_data()[["vms_databases_configuration"]][["vms"]][["host"]],
              db_port = config_data()[["vms_databases_configuration"]][["vms"]][["port"]]
            )
            # Uses a function to extract data from VMS
            cat(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start sql vms \n", sep = "")
            vms <- furdeb::data_extraction(
              type = "database",
              file_path = system.file("sql",
                                      "vms.sql",
                                      package = "AkadoR"),
              database_connection = data_connection_vms,
              anchor = list(select_item_1 = start_date_range, select_item_2 = end_date_range, select_item_3 = vessel_number)
            )
            # Disconnection to the bases
            DBI::dbDisconnect(data_connection_vms[[2]])
            # Force date type, otherwise empty dataframe sets to charactere format
            vms$vms_date <- as.Date(vms$vms_date)
            # Removal of duplicate lines for date and vessel (id, time, position, ... not considered here)
            vms_route <- vms %>%
              dplyr::select(vms_id, vms_date, vessel_code, vms_codevessel, vessel_type, vessel_statut) %>%
              dplyr::distinct() %>%
              dplyr::relocate(vessel_code)
            list_return <- append(list_return, list(vms = vms, vms_route = vms_route))
          }
        }
        # If trips have been found return them otherwise return FALSE
        if (dim(trip_select)[1] > 0) {
          return(list_return)
        } else {
          return(FALSE)
        }
      }
    })
  })
}

# Shiny function : Performs all calculations to test for inconsistencies
calcul_check_server <- function(id, text_error_trip_select, trip_select, config_data, referential_file, check_info_selected, sql_info_selected, column_user_info, parent_in) {
  moduleServer(id, function(input, output, session) {
    # 0 - Global variables assignement ----
    trip_id <- NULL
    eventReactive(trip_select(), {
      # 1 - Arguments verification ----
      # Recovery of reactive values
      check_info <- check_info_selected()
      sql_info <- sql_info_selected()[["sql_info"]]
      # Check arguments in sql_info
      name_file_sql <- sapply(sql_info, `[[`, "file")
      lapply(sql_info, function(sql) {
        # Check that the sublist contains an 'file' element
        if (!("file" %in% names(sql))) {
          stop(
            format(
              x = Sys.time(),
              format = "%Y-%m-%d %H:%M:%S"
            ),
            " - Impossible to identify the sql because there is no element in the sub-list named 'file'.",
            "\n Present element : ",
            paste0(paste(names(sql), sql, sep = " : "), collapse = ", "),
            sep = ""
          )
        }
        if (!codama::r_type_checking(
          r_object = sql[["file"]],
          type = "character",
          length = 1L,
          output = "logical"
        )) {
          return(codama::r_type_checking(
            r_object = sql[["file"]],
            type = "character",
            length = 1L,
            output = "error"
          ))
        }
        if (sql[["file"]] %in% names(config_data())) {
          stop(
            format(
              x = Sys.time(),
              format = "%Y-%m-%d %H:%M:%S"
            ),
            " - The name of the SQL file and the name of the parameters in the configuration file cannot be identical, as both are likely to be used as anchor in SQL queries, leading to a conflict.",
            "\n Name SQL file : ",
            sql[["file"]],
            "\n Name parameters configuration file : ",
            paste0(names(config_data()), collapse = ", "),
            sep = ""
          )
        }
        if (sql[["file"]] %in% names(referential_file())) {
          stop(
            format(
              x = Sys.time(),
              format = "%Y-%m-%d %H:%M:%S"
            ),
            " - The name of the SQL file and the names of reference data sets cannot be identical, as both can be used as input to a check function, leading to a conflict.",
            "\n Name SQL file : ",
            sql[["file"]],
            "\n Names of reference data sets : ",
            paste0(names(referential_file()), collapse = ", "),
            sep = ""
          )
        }
        if (sql[["file"]] %in% names(trip_select())) {
          stop(
            format(
              x = Sys.time(),
              format = "%Y-%m-%d %H:%M:%S"
            ),
            " - The name of the SQL file and the names of SQL initializing the user's query, as both can be used as input to a check function, leading to a conflict.",
            "\n Name SQL file : ",
            sql[["file"]],
            "\n Names of SQL initializing the user's query : ",
            paste0(names(trip_select()), collapse = ", "),
            sep = ""
          )
        }
        if (sql[["file"]] %in% c("trip_selected", "vessel_selected")) {
          stop(
            format(
              x = Sys.time(),
              format = "%Y-%m-%d %H:%M:%S"
            ),
            " - The name of the SQL file and the values from the SQL initializing the connection cannot be identical, as both are likely to be used as anchor in SQL queries, leading to a conflict.",
            "\n Name SQL file : ",
            sql[["file"]],
            "\n Values from the SQL initializing connection : ",
            paste0(c("trip_selected", "vessel_selected"), collapse = ", "),
            sep = ""
          )
        }
        if (sql[["file"]] %in% c("check", "plot")) {
          stop(
            format(
              x = Sys.time(),
              format = "%Y-%m-%d %H:%M:%S"
            ),
            " - The name of the SQL file and the values from the display function or data plot function cannot be identical, as both can be used as input to a display function or data plot function, leading to a conflict.",
            "\n Name SQL file : ",
            sql[["file"]],
            "\n Values from the display function or data plot function : ",
            paste0(c("check", "plot"), collapse = ", "),
            sep = ""
          )
        }
        if (system.file("sql", paste0(sql[["file"]], ".sql"), package = "AkadoR") == "") {
          stop(
            format(
              x = Sys.time(),
              format = "%Y-%m-%d %H:%M:%S"
            ),
            " - Unable to find the following SQL file in package AkadoR",
            "\n Name SQL file : ",
            sql[["file"]],
            sep = ""
          )
        }
        # Check that element 'anchor' in the sub-list is list
        if ("anchor" %in% names(sql)) {
          if (!codama::r_type_checking(
            r_object = sql[["anchor"]],
            type = "list",
            output = "logical"
          )) {
            return(codama::r_type_checking(
              r_object = sql[["anchor"]],
              type = "list",
              output = "error"
            ))
          }
        }
        # Check that the anchor reference is correct
        if ("anchor" %in% names(sql)) {
          # For SQL that allows the extraction of other SQL (use_selection_other_sql is TRUE), only references to data frames initializing the user query or configuration file arguments are allowed
          if (!is.null(sql[["use_selection_other_sql"]]) && sql[["use_selection_other_sql"]]) {
            if (any(!c(unlist(sql[["anchor"]]) %in% c("trip_selected", "vessel_selected", names(config_data()))))) {
              stop(
                format(
                  x = Sys.time(),
                  format = "%Y-%m-%d %H:%M:%S"
                ),
                " - An anchor is invalid, for SQL that allows the extraction of other SQL (use_selection_other_sql are TRUE), only references to data frames initializing the user query or configuration file arguments are allowed.",
                "\n Anchor : ",
                paste0(paste(names(sql[["anchor"]]), sql[["anchor"]], sep = " : "), collapse = ", "),
                "\n References allowed : ",
                paste0(c("trip_selected", "vessel_selected", names(config_data())), collapse = ", "),
                sep = ""
              )
            }
          } else {
            # For SQL that doesn't allow the extraction of other SQL (use_selection_other_sql is FALSE), only references to data frames initializing the user query, configuration file arguments or the name of another SQL file (with use_selection_other_sql is TRUE, the column indicated in id can be used as anchor) are allowed
            if (any(!c(unlist(sql[["anchor"]]) %in% c("trip_selected", "vessel_selected", names(config_data()), name_file_sql[sapply(sql_info, `[[`, "use_selection_other_sql")])))) {
              stop(
                format(
                  x = Sys.time(),
                  format = "%Y-%m-%d %H:%M:%S"
                ),
                " - An anchor is invalid, for SQL that doesn't allow the extraction of other SQL (use_selection_other_sql is FALSE), only references to data frames initializing the user query, configuration file arguments or the name of another SQL file (with use_selection_other_sql is TRUE, the column indicated in id can be used as anchor) are allowed.",
                "\n Anchor : ",
                paste0(paste(names(sql[["anchor"]]), sql[["anchor"]], sep = " : "), collapse = ", "),
                "\n References allowed : ",
                paste0(c("trip_selected", "vessel_selected", names(config_data()), sapply(sql_info, `[[`, "file")), collapse = ", "),
                sep = ""
              )
            }
          }
        }
      })
      if (any(names(trip_select()) %in% c("check", "plot"))) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - The names of SQL initializing the user's query and the values from the display function or data plot function cannot be identical, as both can be used as input to a display function or data plot function, leading to a conflict.",
          "\n Names of SQL initializing the user's query : ",
          paste0(names(trip_select()), collapse = ", "),
          "\n Values from the display function or data plot function : ",
          paste0(c("check", "plot"), collapse = ", "),
          sep = ""
        )
      }
      if (any(names(trip_select()) %in% names(referential_file()))) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - The names of SQL initializing the user's query and the names of reference data sets cannot be identical, as both can be used as input to a display function or data plot function, leading to a conflict.",
          "\n Names of SQL initializing the user's query : ",
          paste0(names(trip_select()), collapse = ", "),
          "\n Names of reference data sets : ",
          paste0(names(referential_file()), collapse = ", "),
          sep = ""
        )
      }
      if (any(names(config_data()) %in% names(trip_select()))) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - The name of the parameters in the configuration file and the names of SQL initializing the user's query cannot be identical, as both can be used as input to a check function, leading to a conflict.",
          "\n Name of the parameters in the configuration file : ",
          paste0(names(config_data()), collapse = ", "),
          "\n Names of SQL initializing the user's query : ",
          paste0(names(trip_select()), collapse = ", "),
          sep = ""
        )
      }
      if (any(names(config_data()) %in% names(referential_file()))) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - The name of the parameters in the configuration file and the names of reference data sets cannot be identical, as both can be used as input to a check function, leading to a conflict.",
          "\n Name of the parameters in the configuration file : ",
          paste0(names(config_data()), collapse = ", "),
          "\n Names of reference data sets : ",
          paste0(names(referential_file()), collapse = ", "),
          sep = ""
        )
      }
      # 2 - Data extraction ----
      # If there was no error in the trip selection and that there are trips for user settings, performs consistency tests
      if (text_error_trip_select() == TRUE && !is.logical(trip_select())) {
        # If the selected controls use data from SQL indicate this in sql_info
        if (!identical(sql_info, list())) {
          # Connection to the base
          config_observe_database <- config_data()[["databases_configuration"]]
          data_connection <- list()
          for (observe_database in config_observe_database[c(parent_in$`tab-data_base_observe`)]){
            data_connection <- append(data_connection, list(furdeb::postgresql_dbconnection(
              db_user = observe_database[["login"]],
              db_password = observe_database[["password"]],
              db_dbname = observe_database[["dbname"]],
              db_host = observe_database[["host"]],
              db_port = observe_database[["port"]]
            )))
          }
          # Retrieve only data that will also be used to retrieve other SQLs
          data_select_sql <- lapply(stats::setNames(sql_info[unlist(sapply(sql_info, `[[`, "use_selection_other_sql"))], paste0("data_", sapply(sql_info, `[[`, "file"))[unlist(sapply(sql_info, `[[`, "use_selection_other_sql"))]), function(sql) {
            # Print message
            cat(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start sql id ", sql[["file"]], " \n", sep = "")
            # Retrieves anchor values to be supplied to SQL
            if (!is.null(sql[["anchor"]])) {
              anchor <- lapply(stats::setNames(sql[["anchor"]], names(sql[["anchor"]])), function(anchor) {
                if (anchor == "trip_selected") {
                  # Recovers trip_id values available directly in the SQL initializing the user's request
                  trip_select()$trip_selected$trip_id
                } else if (anchor == "vessel_selected") {
                  # Recovers vessel_id values available directly in the SQL initializing the user's request
                  trip_select()$trip_selected$vessel_id
                } else if (anchor %in% names(config_data())) {
                  # Retrieves parameter values from configuration file
                  config_data()[[anchor]]
                }
              })
            } else {
              anchor <- NULL
            }
            # Execute SQL query
            data <- furdeb::data_extraction(type = "database",
                                            file_path = system.file("sql",
                                                                    paste0(sql[["file"]], ".sql"),
                                                                    package = "AkadoR"),
                                            database_connection = data_connection,
                                            anchor = anchor)
            # Checks that the name of the column to be used to supply values when used in other SQL anchors exists in the dataset
            if (!(sql[["column_anchor"]] %in% colnames(data)) && (!("vector" %in% names(sql)) || !sql[["vector"]])) {
              # Disconnection to the bases
              for (i in seq(from = 1, to = length(data_connection))) {
                DBI::dbDisconnect(data_connection[[i]][[2]])
              }
              stop(
                format(
                  x = Sys.time(),
                  format = "%Y-%m-%d %H:%M:%S"
                ),
                " - The SQL ", sql[["file"]], " file does not contain the ", sql[["column_anchor"]], " column indicated in sub-list named 'column_anchor'.",
                sep = ""
              )
            }
            # Transforms dataframe into vector if vector argument is TRUE
            if (!is.null(sql[["vector"]]) && sql[["vector"]]) {
              if (ncol(data) == 1) {
                data <- dplyr::pull(data)
              } else {
                # Disconnection to the bases
                for (i in seq(from = 1, to = length(data_connection))) {
                  DBI::dbDisconnect(data_connection[[i]][[2]])
                }
                stop(
                  format(
                    x = Sys.time(),
                    format = "%Y-%m-%d %H:%M:%S"
                  ),
                  " - The SQL ", sql[["file"]], " file cannot be transformed into a vector because there are ", ncol(data), " columns",
                  "\n The sub-list named 'vector' must not be TRUE",
                  sep = ""
                )
              }
            }
            return(data)
          })
          # Retrieve only data that will not be used to retrieve other SQL
          data_sql <- lapply(stats::setNames(sql_info[!unlist(sapply(sql_info, `[[`, "use_selection_other_sql"))], paste0("data_", sapply(sql_info, `[[`, "file"))[!unlist(sapply(sql_info, `[[`, "use_selection_other_sql"))]), function(sql) {
            # Print message
            cat(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start sql id ", sql[["file"]], " \n", sep = "")
            # Retrieves anchor values to be supplied to SQL
            if (!is.null(sql[["anchor"]])) {
              anchor <- lapply(stats::setNames(sql[["anchor"]], names(sql[["anchor"]])), function(anchor) {
                if (anchor == "trip_selected") {
                  # Recovers trip_id values available directly in the SQL initializing the user's request
                  trip_select()$trip_selected$trip_id
                } else if (anchor == "vessel_selected") {
                  # Recovers vessel_id values available directly in the SQL initializing the user's request
                  trip_select()$trip_selected$vessel_id
                } else if (anchor %in% names(config_data())) {
                  # Retrieves parameter values from configuration file
                  config_data()[[anchor]]
                } else {
                  # Retrieves the name of the column to be used as the anchor value
                  name_id <- sql_info[[which(sapply(sql_info, `[[`, "file") == anchor)]][["column_anchor"]]
                  if (!is.null(name_id)) {
                    # Extract anchor values in other data SQL
                    data_select_sql[[paste0("data_", anchor)]][[name_id]]
                  } else {
                    # Extract anchor values in other vector SQL
                    data_select_sql[[paste0("data_", anchor)]]
                  }
                }
              })
            } else {
              anchor <- NULL
            }
            # Execute SQL query
            data <- furdeb::data_extraction(type = "database",
                                            file_path = system.file("sql",
                                                                    paste0(sql[["file"]], ".sql"),
                                                                    package = "AkadoR"),
                                            database_connection = data_connection,
                                            anchor = anchor)
            # Transforms dataframe into vector if vector argument is TRUE
            if (!is.null(sql[["vector"]]) && sql[["vector"]]) {
              if (ncol(data) == 1) {
                data <- dplyr::pull(data)
              } else {
                # Disconnection to the bases
                for (i in seq(from = 1, to = length(data_connection))) {
                  DBI::dbDisconnect(data_connection[[i]][[2]])
                }
                stop(
                  format(
                    x = Sys.time(),
                    format = "%Y-%m-%d %H:%M:%S"
                  ),
                  " - The SQL ", sql[["file"]], " file cannot be transformed into a vector because there are ", ncol(data), " columns",
                  "\n The sub-list named 'vector' must not be TRUE",
                  sep = ""
                )
              }
            }
            return(data)
          })
          data_sql <- c(data_sql, data_select_sql)
          rm(data_select_sql)
          # Disconnection to the bases
          for (i in seq(from = 1, to = length(data_connection))) {
            DBI::dbDisconnect(data_connection[[i]][[2]])
          }
        } else {
          data_sql <- list()
        }
        # 3 - Data design ----
        if (!is.null(data_sql$data_previous_trip)) {
          # Reconstructs info from previous trips in different databases
          if (!codama::r_table_checking(
            r_table = data_sql$data_previous_trip,
            type = "data.frame",
            column_name = c("trip_previous_id", "trip_id", "vessel_code", "trip_enddate", "harbour_id_landing_trip_previous", "harbour_label_landing_trip_previous", "harbour_id_landing", "harbour_label_landing"),
            column_type = c("character", "character", "character", "Date", "character", "character", "character", "character"),
            output = "logical"
          )) {
            codama::r_table_checking(
              r_table = data_sql$data_previous_trip,
              type = "data.frame",
              column_name = c("trip_previous_id", "trip_id", "vessel_code", "trip_enddate", "harbour_id_landing_trip_previous", "harbour_label_landing_trip_previous", "harbour_id_landing", "harbour_label_landing"),
              column_type = c("character", "character", "character", "Date", "character", "character", "character", "character"),
              output = "error"
            )
          }
          for (i in data_sql$data_previous_trip[is.na(data_sql$data_previous_trip$trip_previous_id), "trip_id", drop = TRUE]) {
            # Recovers info from trip that has no previous trip
            current_data <- data_sql$data_previous_trip[data_sql$data_previous_trip$trip_id %in% i, ]
            # Search for trips from the same vessel and with a date lower than the current trip
            date_previous_trip <- data_sql$data_previous_trip[data_sql$data_previous_trip$vessel_code %in% current_data$vessel_code & current_data$trip_enddate > data_sql$data_previous_trip$trip_enddate, "trip_enddate", drop = TRUE]
            if (length(date_previous_trip) > 0) {
              date_min_full_trip <- max(date_previous_trip, na.rm = TRUE)
              previous_trip <- data_sql$data_previous_trip[data_sql$data_previous_trip$vessel_code %in% current_data$vessel_code & data_sql$data_previous_trip$trip_enddate == date_min_full_trip, ]
              # Assigns information from previous trip
              data_sql$data_previous_trip[data_sql$data_previous_trip$trip_id %in% i, c("trip_previous_id", "harbour_id_landing_trip_previous", "harbour_label_landing_trip_previous")] <- previous_trip[, c("trip_id", "harbour_id_landing", "harbour_label_landing"), drop = TRUE]
            }
          }
          # Remove trips not selected by the user but which have been useful for finding previous trips located in another database
          data_sql$data_previous_trip <- data_sql$data_previous_trip %>%
            dplyr::filter(trip_id %in% trip_select()$trip_selected$trip_id)
          # Checks data consistency
          if (nrow(data_sql$data_previous_trip) != length(trip_select()$trip_selected$trip_id)) {
            warning(text_object_more_or_less(id = trip_select()$trip_selected$trip_id, result_check = data_sql$data_previous_trip$trip_id))
          }
        }
        if (!is.null(data_sql$data_full_trip)) {
          # Reconstructs full trips from different databases
          if (!codama::r_table_checking(
            r_table = data_sql$data_full_trip,
            type = "data.frame",
            column_name = c("trip_end_full_trip_id", "trip_id", "vessel_id", "trip_enddate"),
            column_type = c("character", "character", "character", "Date"),
            output = "logical"
          )) {
            codama::r_table_checking(
              r_table = data_sql$data_full_trip,
              type = "data.frame",
              column_name = c("trip_end_full_trip_id", "trip_id", "vessel_id", "trip_enddate"),
              column_type = c("character", "character", "character", "Date"),
              output = "error"
            )
          }
          for (i in data_sql$data_full_trip[is.na(data_sql$data_full_trip$trip_end_full_trip_id) & !is.na(data_sql$data_full_trip$trip_id), "trip_id", drop = TRUE]) {
            # Recover trip info that doesn't belong to a finished full trip
            current_data <- data_sql$data_full_trip[data_sql$data_full_trip$trip_id %in% i, ]
            # Search for trips on the same vessel, in a finished full trip and with a date later than the current trip
            date_full_trip <- data_sql$data_full_trip[data_sql$data_full_trip$vessel_id %in% current_data$vessel_id & !is.na(data_sql$data_full_trip$trip_end_full_trip_id) & current_data$trip_enddate <= data_sql$data_full_trip$trip_enddate, "trip_enddate", drop = TRUE]
            if (length(date_full_trip) > 0) {
              date_min_full_trip <- min(date_full_trip, na.rm = TRUE)
              # Assigns the identifier of the finished full trip
              data_sql$data_full_trip[data_sql$data_full_trip$trip_id %in% i, "trip_end_full_trip_id"] <- data_sql$data_full_trip[data_sql$data_full_trip$vessel_id %in% current_data$vessel_id & !is.na(data_sql$data_full_trip$trip_end_full_trip_id) & data_sql$data_full_trip$trip_enddate == date_min_full_trip, "trip_end_full_trip_id", drop = TRUE]
            }
          }
        }
        # Execute check
        table_finish <- sapply(check_info, function(check) {
          if (!is.null(check[["function_check"]])) {
            # If the control needs VMS but the connection to the VMS database could not be made to create the data_vms dataset, then the control is not carried out
            if (!is.null(check[["need_vms"]]) && check[["need_vms"]] && !("vms" %in% names(trip_select()))) {
              return(stats::setNames(list(data.frame()), check[["id"]]))
            } else {
              # Print message
              cat(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check id ", check[["id"]], " \n", sep = "")
              tryCatch({
                # Finds the data frames to supply as arguments to the control function
                argument <- lapply(stats::setNames(check[["argument_function_check"]], names(check[["argument_function_check"]])), function(argument) {
                  if (paste0("data_", argument) %in% names(data_sql)) {
                    # Extract data from data SQL
                    data_sql[[paste0("data_", argument)]]
                  } else if (argument %in% names(referential_file())) {
                    # Retrieves parameter values from referential file
                    referential_file()[[argument]]
                  } else if (argument %in% names(config_data())) {
                    # Retrieves parameter values from configuration file
                    config_data()[[argument]]
                  } else if (argument %in% names(trip_select())) {
                    # Extract data from data SQL retrieved directly via user selections
                    trip_select()[[argument]]
                  } else {
                    argument
                  }
                })
                # Executes the control function
                check_result <- do.call(check[["function_check"]], argument)
                # Save in a new variable the data frame that will serve as the basis for the table displayed in the shiny application
                if (inherits(x = check_result, what = "list")) {
                  table_result <- check_result[[1]]
                } else {
                  table_result <- check_result
                }
                # Executes the function that modifies the table display specifically for this check
                if (!is.null(check[["function_display"]])) {
                  if (!is.null(check[["argument_function_display"]])) {
                    # Finds the data frames to supply as arguments to the display function
                    argument <- lapply(stats::setNames(check[["argument_function_display"]], names(check[["argument_function_display"]])), function(argument) {
                      if (paste0("data_", argument) %in% names(data_sql)) {
                        # Extract data
                        data_sql[[paste0("data_", argument)]]
                      } else if (argument == "check") {
                        table_result
                      } else if (argument == "plot") {
                        check_result[[2]]
                      } else {
                        argument
                      }
                    })
                    table_result <- do.call(check[["function_display"]], argument)
                  } else {
                    table_result <- do.call(check[["function_display"]], list(table_result))
                  }
                }
                if (!is.null(check[["function_plot"]])) {
                  if (!is.null(check[["function_data_plot"]])) {
                    # Finds the data frames to supply as arguments to the data plot function
                    argument <- lapply(stats::setNames(check[["argument_function_data_plot"]], names(check[["argument_function_data_plot"]])), function(argument) {
                      if (paste0("data_", argument) %in% names(data_sql)) {
                        # Extract data
                        data_sql[[paste0("data_", argument)]]
                      } else if (argument %in% names(trip_select())) {
                        trip_select()[[argument]]
                      } else if (argument == "check") {
                        table_result
                      } else if (argument == "plot") {
                        check_result[[2]]
                      } else {
                        argument
                      }
                    })
                    # Executes the data plot function
                    data_plot <- do.call(check[["function_data_plot"]], argument)
                  } else {
                    data_plot <- check_result[[2]]
                  }
                  # Name of the table containing the plot information in calcul_check_server
                  if (nrow(table_result) > 0) {
                    table_result$name_table <- check[["id"]]
                    # Add button and data for plot in table
                    argument <- list(id = check[["id"]], data = table_result, colname_id = grep("_id$", colnames(table_result), value = TRUE), colname_info = "name_table")
                    if (!is.null(check[["choice_display_plot"]])) {
                      argument <- c(argument, choice_select_row = check[["choice_display_plot"]])
                    }
                    table_result <- do.call(data_button_plot, argument)
                  }
                }
                # Finds the table containing the information needed for the user to identify the rows
                if (paste0("data_", check[["table_user_id"]]) %in% names(data_sql)) {
                  # Extract data in data_sql
                  table_display_data_info <- data_sql[[paste0("data_", check[["table_user_id"]])]][,  c(sql_info[[which(sapply(sql_info, `[[`, "file") == check[["table_user_id"]])]][["column_user_id"]], check[["additional_column_user"]])]
                } else if (check[["table_user_id"]] %in% names(trip_select())) {
                  # Extract data in trip_select
                  table_display_data_info <- trip_select()[[check[["table_user_id"]]]][,  c(check[["additional_column_user"]])]
                } else {
                  stop(
                    format(
                      x = Sys.time(),
                      format = "%Y-%m-%d %H:%M:%S"
                    ),
                    " - The sub-list named 'table_user_id' must be a name of the file SQL (", paste0(names(data_sql), collapse = ", "), ") or a name from the SQL initializing the user's query trip_select() (", paste0(names(trip_select()), collapse = ", "), ")",
                    "\n check id : ",
                    check[["id"]],
                    "\n table_user_id : ",
                    check[["table_user_id"]]
                    ,
                    sep = ""
                  )
                }
                # Uses a function to format the table (adds data_info columns allowing the user to identify rows (predefined column depending on the table specified in table_user_id for control purposes, with the possibility of adding columns in additional_column_user but which must already be present in the SQL linked to table_user_id) and modifies the display of certain columns (addition of icon, coordinate conversion, rename))
                table_result <- table_display(data = table_result, data_info = table_display_data_info, type_inconsistency = check[["type"]], rename = column_user_info[["rename_id_column_user"]], order = column_user_info[["order_id_column_user"]])
                # Modify the table for display purposes: rename column
                if (!is.null(check[["rename_column_user"]])) {
                  table_result <- table_result %>% dplyr::rename_with(~ unlist(check[["rename_column_user"]]), names(check[["rename_column_user"]]))
                }
                # return data check and data plot check if exist
                if (!is.null(check[["function_data_plot"]])) {
                  return(stats::setNames(list(list(table = table_result, list_plot = data_plot)), check[["id"]]))
                } else {
                  return(stats::setNames(list(list(table = table_result)), check[["id"]]))
                }
              }, error = function(error_message) {
                message("Error, check failure: ", conditionMessage(error_message))
                return(stats::setNames(list(list(error = (paste("Control failure :  ", error_message)))), check[["id"]]))
              })
            }
          }
        })
        table_finish
      }
    })
  })
}

# Shiny function : Error message if the data base selection elements are not correctly filled in
text_error_data_base_select_server <- function(id, parent_in, config_data) {
  moduleServer(id, function(input, output, session) {
    eventReactive(input$start_data_base, {
      # If the connection file is missing
      if (!isTruthy(config_data())) {
        text <- "Error: There is no configuration file for the connection to the base"
        showNotification(id = "notif_warning", ui = text, type = "error")
        return(paste0(text, ", please either select one using the \"settings\" tab or put it in ", file.path(path.expand("~"), ".appconfig", "akador", "configuration_file.yml")))
      }
      # If the database selection is missing
      if (is.null(parent_in$`tab-data_base_observe`)) {
        return("Error: please select a database Observe")
      }
      # If connection information is missing for at least one base in the configuration file
      if (!all(parent_in$`tab-data_base_observe` %in% names(config_data()[["databases_configuration"]]))) {
        text <- paste0("Error: connection information for the database : ", paste0(parent_in$`tab-data_base_observe`[!parent_in$`tab-data_base_observe` %in% names(config_data()[["databases_configuration"]])], collapse = ", "), ", are not available in the connection file")
        showNotification(id = "notif_warning", ui = text, type = "error")
        return(paste0(text, ", please modify the configuration file or do not select this base"))
      }
      return(TRUE)
    })
  })
}

# Shiny function : retrieve trip section parameters from database(s)
parameters_trip_select_server <- function(id, parent_in, text_error_data_base_select, config_data) {
  # 1 - Data design ----
  moduleServer(id, function(input, output, session) {
    eventReactive(input$start_data_base, {
      # If the connection data exists and there was no error in the data base selection, makes the connection
      req(config_data())
      if (text_error_data_base_select() == TRUE) {
        config_observe_database <- config_data()[["databases_configuration"]]
        data_connection <- list()
        try_connection <- tryCatch({
          # Try connection
          for (observe_database in config_observe_database[c(parent_in$`tab-data_base_observe`)]){
            data_connection <- append(data_connection, list(furdeb::postgresql_dbconnection(
              db_user = observe_database[["login"]],
              db_password = observe_database[["password"]],
              db_dbname = observe_database[["dbname"]],
              db_host = observe_database[["host"]],
              db_port = observe_database[["port"]]
            )))
          }
        }, error = function(e) {
          # Return error
          message("Error : ", e$message)
          return(paste("Error : ", e$message))
        })
        if (!is.character(try_connection)) {
          cat(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start load trip selection parameters \n", sep = "")
          trip_parameter <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "trip_parameter.sql",
                                    package = "AkadoR"),
            database_connection = data_connection,
            anchor = list(select_item_1 = config_data()[["logbook_program"]])
          )
          # Disconnection to the base
          for (i in seq(from = 1, to = length(data_connection))) {
            DBI::dbDisconnect(data_connection[[i]][[2]])
          }
          # If trips have been found return them otherwise return FALSE
          if (dim(trip_parameter)[1] > 0) {
            return(trip_parameter)
          } else {
            return(FALSE)
          }
        } else {
          return(try_connection)
        }
      }
    })
  })
}

# Shiny function : Displays the errors and notifications that occur when you want to start connection data base
error_data_base_select_serveur <- function(id, text_error_data_base_select, parameters_trip_select) {
  moduleServer(id, function(input, output, session) {
    output$text <- renderText({
      # If there are errors in the selection parameters
      if (is.character(text_error_data_base_select())) {
        showNotification(id = "notif_warning", ui = text_error_data_base_select(), type = "error")
        return(paste0("<span style=\"color:red\">", text_error_data_base_select(), "</span>"))
      }
      # If there are errors in connection for selection parameters
      if (text_error_data_base_select() == TRUE && is.character(parameters_trip_select())) {
        showNotification(id = "notif_warning", ui = parameters_trip_select(), type = "error")
        return(paste0("<span style=\"color:red\">", parameters_trip_select(), "</span>"))
      }
      # If the data is not found in the database
      if (text_error_data_base_select() == TRUE && !is.data.frame(parameters_trip_select()) && parameters_trip_select() == FALSE) {
        text <- "Error: no data was found for these data base"
        showNotification(id = "notif_warning", ui = text, type = "error")
        return(paste0("<span style=\"color:red\">", text, "</span>"))
      }
      # If recovery of parameters is finished
      if (isTruthy(parameters_trip_select())) {
        text <- "Load trip selection parameters finished"
        showNotification(id = "notif_default", ui = text, type = "default")
        cat("\033[0;32m", format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Process load trip selection parameters ran successfully.\n", "\033[0m", sep = "")
        return(paste0("<span style=\"color:#34C909\">", text, "</span>"))
      }
    })
  })
}

# Shiny function : Displays the errors and notifications that occur when you want to start the calculation
error_trip_select_serveur <- function(id, text_error_trip_select, trip_select, calcul_check) {
  moduleServer(id, function(input, output, session) {
    output$text <- renderText({
      # If there are errors in the selection parameters
      if (is.character(text_error_trip_select())) {
        showNotification(id = "notif_warning", ui = text_error_trip_select(), type = "error")
        return(paste0("<span style=\"color:red\">", text_error_trip_select(), "</span>"))
      }
      # If the selected trip is not found in the database
      if (text_error_trip_select() == TRUE && !is.list(trip_select()) && trip_select() == FALSE) {
        text <- "Error: no trip was found for these parameters"
        showNotification(id = "notif_warning", ui = text, type = "error")
        return(paste0("<span style=\"color:red\">", text, "</span>"))
      }
      # If the different manipulations on the data are finished
      if (isTruthy(calcul_check())) {
        text <- "Calculation finished"
        showNotification(id = "notif_default", ui = text, type = "default")
        cat("\033[0;32m", format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Process AkadoR ran successfully.\n", "\033[0m", sep = "")
        return(paste0("<span style=\"color:#34C909\">", text, "</span>"))
      }
    })
  })
}

# Shiny function : Selection window for choosing the type of file to download
window_button_download <- function() {
  modalDialog(downloadButton(outputId = "download_csv", label = "CSV"),
              downloadButton(outputId = "download_excel", label = "Excel"),
              fade = TRUE,
              easyClose = TRUE,
              footer = NULL,
              title = "Download Table")
}

# Shiny function : creation tab, menu and content
tab <- function(id, tab_info, check_info, type_check_info, calcul_check, referential_file, config_data, res_auth, parameters_trip_select) {
  # 1 - Arguments verification ----
  if (!codama::r_type_checking(
    r_object = id,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = id,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  # 2 - Data design ----
  # Instantiating the radio button to select the display of controls
  mod_radiobuttons_type_check_server(id = id, type_check_info = type_check_info)
  # Instantiating the menu and retrieving reactive values
  mod_tab_menu_server(id = id, tab_info = tab_info)
  # Instantiating the tab
  mod_tab_content_server(id = id, tab_info = tab_info, check_info = check_info, type_check_info = type_check_info, config_data = config_data, res_auth = res_auth, parameters_trip_select = parameters_trip_select)
}

#' @name table_display
#' @title Transforms table display
#' @description Shiny function : Function that formats the display of check results, positions, row order and column names
#' @param data {\link[base]{data.frame}} expected. data frame containing check information
#' @param data_info {\link[base]{data.frame}} expected. data frame containing information to enable the user to identify lines
#' @param type_inconsistency {\link[base]{character}} expected. Control type. You can choose between "error", "warning" or "info".
#' @param rename {\link[base]{list}} expected. List of column names to be renamed (the left part is the column names, the right part is the new names)
#' @param order {\link[base]{character}} expected. Column names according to which rows are to be sorted.
#' @returns The function returns a {\link[base]{data.frame}}
#' @export
table_display <- function(data, data_info, type_inconsistency, rename, order) {
  # 0 - Global variables assignement ----
  . <- NULL
  `:=` <- NULL
  X <- NULL
  Y <- NULL
  button <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = data,
    type = "data.frame",
    column_name = c("logical"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = data,
      type = "data.frame",
      column_name = c("logical"),
      output = "error"
    )
  }
  if (!codama::r_table_checking(
    r_table = data_info,
    type = "data.frame",
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = data_info,
      type = "data.frame",
      output = "error"
    )
  }
  if (!codama::r_type_checking(
    r_object = type_inconsistency,
    type = "character",
    allowed_value = c("error", "warning", "info"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = type_inconsistency,
      type = "character",
      allowed_value = c("error", "warning", "info"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = rename,
    type = "list",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = rename,
      type = "list",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = order,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = order,
      type = "character",
      output = "error"
    ))
  }
  # 2 - Data design ----
  # Retrieves the name of the column containing the ID
  colname_id <- grep("_id$", colnames(data), value = TRUE)
  # Deletes duplicate columns
  all_colname <- c(colnames(data), colnames(data_info))
  colname_double <- table(all_colname)[table(all_colname) > 1]
  colname_double <- names(colname_double)[!(names(colname_double) %in% colname_id)]
  data <- data[, !(colnames(data) %in% colname_double)]
  # Combines the consistency test on the data and data identification information
  data <- dplyr::inner_join(data_info, data, by = dplyr::join_by(!!colname_id))
  # Add icons according to the success of the test
  data$logical[data$logical == TRUE] <- as.character(icon("check"))
  if (type_inconsistency == "error") {
    data$logical[data$logical == FALSE] <- as.character(icon("xmark"))
  }
  if (type_inconsistency == "warning") {
    data$logical[data$logical == FALSE] <- as.character(icon("exclamation"))
  }
  if (type_inconsistency == "info") {
    data$logical[data$logical == FALSE] <- as.character(icon("info"))
  }
  # Changes position display for all column position, converting DD (Decimal Degrees) coordinates in DDM (Degrees, Decimal Minutes)
  column_name_position <- grep("_position", colnames(data), value = TRUE)
  for (column in column_name_position){
    # Formats spatial data position
    data_geo <- data %>%
      dplyr::filter(!is.na(!!as.name(column))) %>%
      dplyr::select(column, colname_id) %>%
      dplyr::distinct() %>%
      sf::st_as_sf(wkt = column, crs = 4326, remove = FALSE) %>%
      dplyr::mutate(dplyr::as_tibble(sf::st_coordinates(.)))
    new_name_column <- paste0(column, "_ddm")
    # Change position display format from decimal degrees to degrees, minutes, and decimal seconds
    data_geo <- data_geo %>%
      dplyr::mutate(!!new_name_column := paste0(coordinate_dd_to_dmd(coordinate = Y, latitude = TRUE), "<br>", coordinate_dd_to_dmd(coordinate = X, latitude = FALSE)))
    data <- dplyr::left_join(data, data.frame(data_geo)[, c(colname_id, new_name_column)], by = dplyr::join_by(!!colname_id))
    data <- data %>%
      dplyr::relocate(new_name_column, .after = column) %>%
      dplyr::select(-column)
  }
  # Sort rows
  order_tmp <- order[order %in% colnames(data)]
  data <- data %>% dplyr::arrange_at(order_tmp)
  # Rename column
  rename_tmp <- rename[names(rename) %in% colnames(data)]
  data <- data %>% dplyr::rename_with(~ unlist(rename_tmp), names(rename_tmp))
  # Modify the table for display purposes specifically for logical : rename column
  if (length(grep("logical", colnames(data), value = TRUE)) != 0) {
    data <- dplyr::rename(
      .data = data,
      Check = logical
    )
  }
  # Modify the table for display purposes specifically for button : rename column
  if (length(grep("button", colnames(data), value = TRUE)) != 0) {
    data <- dplyr::rename(
      .data = data,
      `Details problem` = button
    )
  }
  # Retrieves the name of the column containing the ID
  colname_id <- grep("_id$", colnames(data), value = TRUE)
  # Modify the table for display purposes: delete column
  data <- subset(data, select = -c(eval(parse(text = colname_id))))
  return(data)
}

#' @name data_button_plot
#' @title Create the button in the table
#' @description Shiny function : Function to create the button in the table that will create the plot
#' @param id {\link[base]{character}} expected. Button identifier
#' @param data {\link[base]{data.frame}} expected. data frame where the button will be added
#' @param colname_id {\link[base]{character}} expected. Name of column used as unique identifier for data rows
#' @param colname_info {\link[base]{character}} expected. Name of data columns to be transmitted via button name
#' @param name_button {\link[base]{character}} expected. Default values: NULL, allows you to specify a name for the button, otherwise use the prefix "button_" and the supplied id
#' @param choice_select_row {\link[base]{character}} expected. Default values: "error", the possible values are "all" to create a button on all lines, "error" to create a button only on lines considered inconsistent, "valid" to create a button only on lines considered consistent
#' @returns The function returns a {\link[base]{data.frame}}
#' @export
data_button_plot <- function(id, data, colname_id, colname_info, name_button = NULL, choice_select_row = "error") {
  # 1 - Arguments verification ----
  if (!codama::r_type_checking(
    r_object = id,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = id,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = colname_id,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = colname_id,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = colname_info,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = colname_info,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = name_button,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = name_button,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = choice_select_row,
    type = "character",
    allowed_value = c("all", "error", "valid"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = choice_select_row,
      type = "character",
      allowed_value = c("all", "error", "valid"),
      output = "error"
    ))
  }
  if (!codama::r_table_checking(
    r_table = data,
    type = "data.frame",
    column_name = c(colname_id, colname_info),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = data,
      type = "data.frame",
      column_name = c(colname_id, colname_info),
      output = "error"
    )
  }
  # 2 - Data design ----
  # Select the lines that will display a plot
  if (choice_select_row == "all") {
    select_row <- rep(TRUE, nrow(data))
  }
  if (choice_select_row == "error") {
    select_row <- data$logical == FALSE
  }
  if (choice_select_row == "valid") {
    select_row <- data$logical == TRUE
  }
  data <- data %>% dplyr::mutate(button = NA)
  if (is.null(name_button)) {
    name_button <- paste0("button_", id)
  }
  # Add button in table
  if (any(select_row)) {
    data$button[select_row] <- sapply(which(select_row), function(num_row) {
      as.character(shiny::actionButton(inputId = paste0(data[num_row, c(colname_info, colname_id)], collapse = "&"), label = "Detail", onclick = paste0('Shiny.setInputValue(\"', shiny::NS(namespace = id, id = name_button), '\", this.id, {priority: \"event\"})')))
    })
  }
  data <- data %>% dplyr::select(!c(colname_info))
  return(data)
}
