#' Uses Kmeans & UMAP to create segmentation
#'
#' @param df is dataframe with the id column and grid-like numeric columns.
#' @param id_col is the unique id column that you want to segment by.
#' @param kmeans_centers_init is the kmeans center.
#' @param kmeans_nstart is the kmeans nstart arg.
#' @param kmeans_iter.max  is the kmenas inter.max arg.
#' @param centers_grid_range is the grid range you want the centers to iterate though, as grid range eg 1:15
#' @param ... additional arguments for umap, kmeans, or ggrepel as named arguments
#'
#' @return a list of graphs and tbls
#' @export
#'
#' @examples
#'brand_segmentation <-make_segmentation(
#'df=mtcars %>%
#'tibble::rownames_to_column(var="column 1"),
#'id_col="column 1",
#'kmeans_centers_init = 3,
#'kmeans_nstart = 100,
#'kmeans_iter.max = 100,
#'centers_grid_range = 1:15)

make_segmentation <- function(.data,id_col,kmeans_centers_init=3,kmeans_nstart=100,kmeans_iter.max=100,centers_grid_range=1:15,...) {

# logic checks

  ## check if id col is character

  if (!base::any(!base::is.character(.data[[id_col]]) || !base::is.factor(.data[[id_col]]))) {
    stop("\033[91m Error:\033[0m 'id_col' argument needs to be character class to enable joins, please convert it to character with as.character()")
  }

  #check if there are at least two numeric columns
  if(base::sum(purrr::map_lgl(.data, \(x) base::is.numeric(x) || base::is.integer(x))) < 2){
    stop("\033[91m Error:\033[0m \033[1m df must have at least 2 numeric column, please ensure df is in grid matrix format\033[0m")
  }

#checks there are enough rows for the clusters selected
if(base::nrow(df) < kmeans_centers_init){
    stop(base::paste0("Error: number of rows in df (", base::nrow(.data), ") is less than kmeans_centers_init (", kmeans_centers_init, "), please ensure df has enough rows for k-means clustering"))
  }


#checks for missing data
if (base::sum(purrr::map_lgl(.data, \(x) base::any(base::is.na(x)))) > 0) {
  stop("\033[91mError:\033[0m \033[1mdf contains missing values, please ensure df does not have any missing values\033[0m")
}

  # checks to ensure initial centers is in range of grid

  if(!base::is.numeric(kmeans_centers_init)){
    stop("\033[91mError:\033[0m \033[1mstats_kmeans:\033[0m \033[91m'kmeans_centers_init' argument needs to be numeric, please input a numeric value\033[0m")
  } else if(kmeans_centers_init < base::min(centers_grid_range) || kmeans_centers_init > base::max(centers_grid_range)) {
    stop(base::paste0("\033[91m Error:\033[0m \033[1mstats_kmeans:\033[0m \033[91m'kmeans_centers_init' argument needs to be within the range of 'centers_grid_range' (", base::min(centers_grid_range), "-", base::max(centers_grid_range), ")\033[0m"))
  }



  # checks to ensure initial centers is in range of grid

  if(!base::is.numeric(centers_grid_range)){
    stop("\033[91mError:\033[0m \033[1mstats_kmeans:\033[0m \033[91m'centers_grid_range' argument needs to be numeric, please input a numeric value\033[0m")
  } else if(base::max(centers_grid_range) > base::nrow(.data)) {
    stop(base::paste0("\033[91m Error:\033[0m \033[1mstats_kmeans:\033[0m \033[91m'centers_grid_range' argument needs to be within the range of 'centers_grid_range' (", base::min(centers_grid_range), "-", base::max(centers_grid_range), ")\033[0m"))
  }


  #check id col is in df

  if(!id_col %in% base::colnames(.data)){
    stop("\033[1m\033[91m Error:\033[0m \033[1m'id_col' argument not found in df columns, please ensure id_col exists in df\033[0m")
  }


# checks column types

  if(!base::all(purrr::map_lgl(.data, \(x) base::class(x) %in% c("numeric","integer","character","factor","ordered")))){
    stop("Error: columns must be class numeric, integric, character, factor or ordered")
  }

# start the formula>>>
  #only extract numeric columns
  tbl_numeric <-  .data |>
    dplyr::select(dplyr::where(base::is.numeric))


  #create function to calculate kmeans objection
  mapper_kmeans <- function(kmeans_centers=3,kmeans_nstart=100,kmeans_iter.max=100,...) {

    tbl_numeric |>
      stats::kmeans(x=.,
                    centers=kmeans_centers,
                    nstart = kmeans_nstart,
                    iter.max=kmeans_iter.max,...)
  }

  #make tbl grid to iterate through different centers options
  tbl_grid <- tibble::tibble(centers_input=centers_grid_range) %>%
    dplyr::mutate(kmeans_models=purrr::map(.x=centers_input,
                                           ~mapper_kmeans(kmeans_centers=.x),
                                           .progress = TRUE),
    kmeans_results=purrr::map(kmeans_models,broom::glance)
    ) |>
    tidyr::unnest(kmeans_results) %>%
    dplyr::select(centers_input,
                  kmeans_models,
                  tot.withinss)

  base::cat("\033[93m[1] Success:\033[0m \033[1mKmeans done successfully\033[0m\n")
  #plots grid of different centers and tot.withinss
  plot_grid_kmeans <- tbl_grid |>
    ggplot2::ggplot(ggplot2::aes(y=tot.withinss,x=centers_input))+
    ggplot2::geom_point()+
    ggplot2::geom_line()


  # creates umap objection
  obj_umap <- tbl_numeric |>
    umap::umap(...)


  #extracts out positions


  tbl_umap <-   obj_umap$layout |>
    tibble::as_tibble() |>
    rlang::set_names((c("x","y"))) |>
    dplyr::bind_cols(.data |>
                       dplyr::select({{id_col}})
                     )
  base::cat("\033[93m[2] Success:\033[0m \033[1mUMAP done successfully\033[0m\033[32m\033[0m\n")

  # select the kmeans model based on the inputted figure and extract their clusters

tbl_kmeans <-   tbl_grid |>
  dplyr::filter(centers_input=={{kmeans_centers_init}}) |>
  dplyr::pull(kmeans_models) |>
  purrr::pluck(1) |>
  broom::augment(df) |>
  dplyr::select({{id_col}},.cluster)


  ## join umap and kmeans tables



tbl_segmentation_id <-  tbl_kmeans |>
  dplyr::left_join(tbl_umap,by=id_col)


  ## plot kmeans clusters and 2 dimensional modeling
plot_id_segmentation <-  tbl_segmentation_id |>
   ggplot2::ggplot(ggplot2::aes(x=x,
               y=y,
               col=.cluster,
               label=df[[id_col]])
           )+
  ggplot2::geom_point()+
  ggrepel::geom_text_repel(...)


  #save outputs to a list to be accessed later
  segmentation_output <- base::list()


  segmentation_output$plot_grid_centers <- plot_grid_kmeans
  segmentation_output$plot_id_segmentation <- plot_id_segmentation
  segmentation_output$tbl_id_segmentation <- tbl_segmentation_id
  segmentation_output$tbl_grid <- tbl_grid
  segmentation_output$tbl_umap <- tbl_umap
  base::cat("\033[93m[3] Success:\033[0m\033[1m Assign this formula to an object and then use the '$' to access the saved tbls and plots\033[0m\033[32m\033[0m\n")

base::return(segmentation_output)
}
