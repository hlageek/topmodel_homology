name_topics <- function(topics_df) {

  topics_df |> 
    mutate(across(starts_with("topic"), ~ na_if(.x, 0))) |>
      tidytable::pivot_longer(starts_with("topic"), names_to = "topic", values_drop_na = T) |>
      group_by(topic) |>
      arrange(topic, desc(value)) |>
      summarize(topic_desc = paste(term, collapse = " "))

}