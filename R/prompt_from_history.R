#' Build prompt from chat history
#'
#' @param chat_obj Chat object
#' @param add_text Additional text to append
#' @param add_role Role for add_text ("user" or "assistant")
#' @param start_turn_index Starting turn index (default 1)
#' @return Formatted prompt string
#' @export
prompt_from_history <- function(chat_obj,
                                add_text = NULL,
                                add_role = "user",
                                start_turn_index = 1) {
  chat_his <- extract_chat_history(chat_obj,
    include_tokens = FALSE,
    include_time = FALSE
  )

  if (nrow(chat_his) == 0) {
    if (!is.null(add_text)) {
      return(paste0(add_role, ":\n", add_text))
    }
    return("")
  }

  # Ensure start_turn_index is valid
  start_turn_index <- max(1, min(start_turn_index, nrow(chat_his)))

  text <- ""
  for (i in start_turn_index:nrow(chat_his)) {
    text <- paste0(
      text,
      chat_his$role[i], ":\n",
      chat_his$text[i], "\n\n"
    )
  }

  # Add new text
  if (!is.null(add_text)) {
    text <- paste0(text, add_role, ":\n", add_text)
  }

  return(text)
}
