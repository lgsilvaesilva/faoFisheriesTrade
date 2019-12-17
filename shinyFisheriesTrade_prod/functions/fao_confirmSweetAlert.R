fao_confirmSweetAlert <- function (session, inputId, title = NULL, text = NULL, type = NULL, 
                                   danger_mode = FALSE, btn_labels = c("Cancel", "Confirm"), 
                                   closeOnClickOutside = FALSE, html = FALSE, p_selector = "#placeholderSweetAlert") 
{
  insertUI(selector = p_selector, where = "afterBegin", ui = useSweetAlert(),
           immediate = TRUE, session = session)
  # insertUI(selector = "body", where = "afterBegin", ui = use_sweet_alert(), 
  #          immediate = TRUE, session = session)
  if (is.null(type)) 
    type <- jsonlite::toJSON(NULL, auto_unbox = TRUE, null = "null")
  if ("shiny.tag" %in% class(text)) 
    html <- TRUE
  if (!html) {
    text <- jsonlite::toJSON(text, auto_unbox = TRUE, null = "null")
    session$sendCustomMessage(type = "sweetalert-sw-confirm", 
                              message = list(id = inputId, title = title, text = text, 
                                             icon = type, buttons = btn_labels, dangerMode = danger_mode, 
                                             closeOnClickOutside = closeOnClickOutside))
  }
  else {
    id <- paste0("placeholder-", sample.int(1e+06, 1))
    session$sendCustomMessage(type = "sweetalert-sw-confirm", 
                              message = list(id = inputId, title = title, icon = type, 
                                             sw_id = id, text = as.character(tags$div(id = id)), 
                                             buttons = btn_labels, as_html = html, dangerMode = danger_mode, 
                                             closeOnClickOutside = closeOnClickOutside))
    insertUI(session = session, selector = paste0("#", id), 
             ui = text, immediate = TRUE)
  }
}

# fao_confirmSweetAlert <-function (session, inputId, title = NULL, text = NULL, type = "question", 
#                                   btn_labels = c("Cancel", "Confirm"), btn_colors = NULL, closeOnClickOutside = FALSE, 
#                                   showCloseButton = FALSE, html = FALSE, p_selector = "#placeholderSweetAlert",...) 
# {
#   
#   insertUI(selector = p_selector, where = "afterBegin", ui = useSweetAlert(),
#            immediate = TRUE, session = session)
#   if (is.null(type)) 
#     type <- jsonlite::toJSON(NULL, auto_unbox = TRUE, null = "null")
#   if ("shiny.tag" %in% class(text)) 
#     html <- TRUE
#   if (inherits(session, "session_proxy")) {
#     if (!starts_with(inputId, session$ns(""))) 
#       inputId <- session$ns(inputId)
#   }
#   if (!isTRUE(html)) {
#     session$sendCustomMessage(type = "sweetalert-sw-confirm", 
#                               message = list(id = inputId, as_html = html, swal = shinyWidgets:::dropNullsOrNA(list(title = title, 
#                                                                                                                     text = text, type = type, confirmButtonText = btn_labels[2], 
#                                                                                                                     cancelButtonText = btn_labels[1], showConfirmButton = !is.na(btn_labels[2]), 
#                                                                                                                     showCancelButton = !is.na(btn_labels[1]), confirmButtonColor = btn_colors[2], 
#                                                                                                                     cancelButtonColor = btn_colors[1], showCloseButton = showCloseButton, 
#                                                                                                                     allowOutsideClick = closeOnClickOutside))))
#   }
#   else {
#     id <- paste0("placeholder-", sample.int(1e+06, 1))
#     session$sendCustomMessage(type = "sweetalert-sw-confirm", 
#                               message = list(id = inputId, as_html = html, sw_id = id, 
#                                              swal = shinyWidgets:::dropNullsOrNA(list(title = title, type = type, 
#                                                                                       html = as.character(tags$div(id = id)), confirmButtonText = btn_labels[2], 
#                                                                                       cancelButtonText = btn_labels[1], showConfirmButton = !is.na(btn_labels[2]), 
#                                                                                       showCancelButton = !is.na(btn_labels[1]), confirmButtonColor = btn_colors[2], 
#                                                                                       cancelButtonColor = btn_colors[1], showCloseButton = showCloseButton, 
#                                                                                       allowOutsideClick = closeOnClickOutside))))
#     insertUI(session = session, 
#              selector = paste0("#", id),
#              ui = text, immediate = TRUE)
#   }
# }