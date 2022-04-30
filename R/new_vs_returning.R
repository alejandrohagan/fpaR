#' new vs. returning variable analysis
#'
#' @param df dataframe
#' @param group grouping logic
#' @param date date to sort by
#' @param new_label label of new variable
#' @param returning_label label of returning variable
#' @param date_unit data dimension
#'
#' @return
#' @export
#'
#' @examples
new_vs_returning <- function(df,group,date,new_label,returning_label,date_unit){



  df %>%
    group_by({{group}}) %>%
    mutate(first_date=min({{ date}})) %>%
    ungroup() %>%
    mutate(group_status=
             case_when(
               {{date}}> first_date ~ {{returning_label}},
               {{date}}==first_date ~ {{new_label}},
               TRUE ~ "other")
    ) %>%

    group_by(date_group=
               lubridate::ceiling_date({{date}},unit={{date_unit}})
    ) %>%
    summarize(new=
                n_distinct({{group}}[group_status=={{new_label}}]),
              returning=
                n_distinct({{group}}[group_status=={{returning_label}}]),
              other=
                n_distinct({{group}}[group_status=="other"])
    ) %>%
    pivot_longer(cols = -c(date_group),names_to = "group_status")

}
