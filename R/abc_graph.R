

#' Produces ABC graph based on abc() function
#'
#' @param .data a tibble or DBI object producteed by abc() function
#'
#' @return ggplot
#' @export
#'
#' @examples
#' abc(ggplot2::diamonds,cut,dim=price,func="n") |> abc_graph()
abc_graph <- function(.data){


  abc_coordinates <- .data |>
    dplyr::group_by(dim_category) |>
    dplyr::summarize(first_cum_per_of_total=dplyr::first(cum_prop_total),
              last_cum_per_of_total=dplyr::last(cum_prop_total),
              first_cum_unit_percent=dplyr::first(cum_unit_prop),
              last_cum_unit_percent=dplyr::last(cum_unit_prop),
              n=dplyr::n(),
              threshold=dplyr::first(dim_threshold)
    ) |> dplyr::collect()


  .data |>
    ggplot2::ggplot(ggplot2::aes(x=cum_unit_prop,
               y=cum_prop_total))+
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::annotate(geom = "rect",
             xmin = abc_coordinates$first_cum_unit_percent,
             xmax = abc_coordinates$last_cum_unit_percent,
             ymin = abc_coordinates$first_cum_per_of_total,
             ymax= abc_coordinates$last_cum_per_of_total,
             alpha=.3,
             fill=c("#007e2f","#ffcd12","#a40000")
    )+

    #A dotted line---------------------------------------------------------------------------

  ggplot2::geom_vline(xintercept = abc_coordinates$last_cum_unit_percent[1],
             linetype="dashed",
             col="#007e2f")+
    #B dotted line--------------------------------------------------------------------------

  ggplot2::geom_vline(xintercept = abc_coordinates$last_cum_unit_percent[2],
             linetype="dashed",
             col="#ffcd12")+

    # C dotted line, I think i take this out???------------------------------------------------------------------------
  ggplot2::geom_vline(xintercept = abc_coordinates$last_cum_unit_percent[3],
             linetype="dashed",
             col="#a40000")+
    # A text -----------------------------------------------------------------------------

    #format scales---------------------------------------------------------------------------------------------
  ggplot2::scale_x_continuous(labels = scales::percent_format())+
    ggplot2::scale_y_continuous(labels = scales::percent_format())
}

