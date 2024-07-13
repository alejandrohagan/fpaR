


#' Produces ABC graph based on abc() function
#'
#' @param df data frame
#' @param group_label label for grouping variable
#' @param dim_label label for dimension variable
#' @param size ggplot size
#' @param shape ggplot shape
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
abc_graph <- function(.data){


  abc_coordinates <- .data |>
    group_by(dim_category) |>
    summarize(first_cum_per_of_total=first(cum_prop_total),
              last_cum_per_of_total=last(cum_prop_total),
              first_cum_unit_percent=first(cum_unit_prop),
              last_cum_unit_percent=last(cum_unit_prop),
              n=n(),
              threshold=first(dim_threshold)
    ) |> dplyr::collect()


  .data |>
    ggplot(aes(x=cum_unit_prop,
               y=cum_prop_total))+
    geom_point()+
    geom_line()+
    annotate(geom = "rect",
             xmin = abc_coordinates$first_cum_unit_percent,
             xmax = abc_coordinates$last_cum_unit_percent,
             ymin = abc_coordinates$first_cum_per_of_total,
             ymax= abc_coordinates$last_cum_per_of_total,
             alpha=.3,
             fill=c("#007e2f","#ffcd12","#a40000")
    )+

    #A dotted line---------------------------------------------------------------------------

  geom_vline(xintercept = abc_coordinates$last_cum_unit_percent[1],
             linetype="dashed",
             col="#007e2f")+
    #B dotted line--------------------------------------------------------------------------

  geom_vline(xintercept = abc_coordinates$last_cum_unit_percent[2],
             linetype="dashed",
             col="#ffcd12")+

    # C dotted line, I think i take this out???------------------------------------------------------------------------
  geom_vline(xintercept = abc_coordinates$last_cum_unit_percent[3],
             linetype="dashed",
             col="#a40000")+
    # A text -----------------------------------------------------------------------------

    #format scales---------------------------------------------------------------------------------------------
  scale_x_continuous(labels = scales::percent_format())+
    scale_y_continuous(labels = scales::percent_format())
}






create_abc_anototates <- function(.data,group_label,dim_label,...){


  abc_coordinates <- .data |>
    group_by(dim_category) |>
    summarize(first_cum_per_of_total=first(cum_prop_total),
              last_cum_per_of_total=last(cum_prop_total),
              first_cum_unit_percent=first(cum_unit_prop),
              last_cum_unit_percent=last(cum_unit_prop),
              n=n(),
              threshold=first(dim_threshold)
    ) |> dplyr::collect()




       annotate(geom="text",
           label=
             glue::glue(
               "initial {scales::percent(abc_coordinates$last_cum_unit_percent[1])} ({scales::comma(abc_coordinates$n[1])}) of\n{group_label} drive\n{scales::percent(abc_coordinates$threshold[1])} of {dim_label}"
             ),
           x = abc_coordinates$last_cum_unit_percent[1]+.05,
           y = abc_coordinates$last_cum_unit_percent[1]+.05,
           hjust=0)+

    # B text ----------------------------------------------------------------------------

  annotate(geom="text",
           label=
             glue::glue(
               "cumlative {scales::percent(abc_coordinates$last_cum_unit_percent[2])} of\n{group_label} drive\n{scales::percent(abc_coordinates$threshold[2])} of {dim_label}"
             ),
           hjust=0,
           size=size,
           x =abc_coordinates$last_cum_unit_percent[2]+.05,
           y=abc_coordinates$last_cum_unit_percent[2])+
    # C text ----------------------------------------------------------------------
  annotate(geom="text",
           label=
             glue::glue("remaining {scales::percent(abc_coordinates$threshold[3])} of\n{dim_label} driven by {scales::percent(1-(abc_coordinates$last_cum_unit_percent[1]+abc_coordinates$last_cum_unit_percent[2]))}\n({scales::comma(abc_coordinates$n[3])}) of {group_label}"),
           hjust=0,
           size=size,
           x =abc_coordinates$last_cum_unit_percent[3]-.3,
           y=abc_coordinates$last_cum_unit_percent[3]-.25)
}
