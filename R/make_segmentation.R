
#' Make a segmentation with kmeans and umap
#'
#' @param .data a tibble dataframe with an ID column and numeric variables
#' @param id_col the ID column to join the kmeans and umap results together
#' @param kmeans_centers_init the k means center to start with
#' @param kmeans_nstart random variable to start iteration
#' @param kmeans_iter.max the maximum number of iterations
#' @param centers_grid_range range of kmeans center
#' @param ...
#'
#' @return a list of objects including tibbles and ggplot
#' @export
#'
#' @examples
#' make_segmentation(
#'.data =mtcars |>
#'tibble::rownames_to_column(var="column1"),
#'id_col=column1,
#'kmeans_nstart = 10,
#'kmeans_centers_init = 5,
#'kmeans_iter.max = 100,
#'centers_grid_range = 1:15)
make_segmentation <- function(.data,id_col,kmeans_centers_init,kmeans_nstart=100,kmeans_iter.max=100,centers_grid_range=1:15,max.overlaps=10,...) {

# logic checks

  id_col_str <- deparse(substitute(id_col))


  ## check if id col is character

  # if (!base::any(!base::is.character(.data[[id_col_str]]) || !base::is.factor(.data[[id_col_str]]))) {
  #   stop("\033[91m Error:\033[0m 'id_col' argument needs to be character class to enable joins, please convert it to character with as.character()")
  # }

  #check if there are at least two numeric columns
  # if(base::sum(purrr::map_lgl(.data, \(x) base::is.numeric(x) || base::is.integer(x))) < 2){
  #   stop("\033[91m Error:\033[0m \033[1m df must have at least 2 numeric column, please ensure df is in grid matrix format\033[0m")
  # }

#checks there are enough rows for the clusters selected
# if(base::nrow(df) < kmeans_centers_init){
#     stop(base::paste0("Error: number of rows in df (", base::nrow(.data), ") is less than kmeans_centers_init (", kmeans_centers_init, "), please ensure df has enough rows for k-means clustering"))
#   }


#checks for missing data
# if (base::sum(purrr::map_lgl(.data, \(x) base::any(base::is.na(x)))) > 0) {
#   stop("\033[91mError:\033[0m \033[1mdf contains missing values, please ensure df does not have any missing values\033[0m")
# }

  # checks to ensure initial centers is in range of grid
#
#   if(!base::is.numeric(kmeans_centers_init)){
#     stop("\033[91mError:\033[0m \033[1mstats_kmeans:\033[0m \033[91m'kmeans_centers_init' argument needs to be numeric, please input a numeric value\033[0m")
#   } else if(kmeans_centers_init < base::min(centers_grid_range) || kmeans_centers_init > base::max(centers_grid_range)) {
#     stop(base::paste0("\033[91m Error:\033[0m \033[1mstats_kmeans:\033[0m \033[91m'kmeans_centers_init' argument needs to be within the range of 'centers_grid_range' (", base::min(centers_grid_range), "-", base::max(centers_grid_range), ")\033[0m"))
#   }



  # checks to ensure initial centers is in range of grid

  # if(!base::is.numeric(centers_grid_range)){
  #   stop("\033[91mError:\033[0m \033[1mstats_kmeans:\033[0m \033[91m'centers_grid_range' argument needs to be numeric, please input a numeric value\033[0m")
  # } else if(base::max(centers_grid_range) > base::nrow(.data)) {
  #   stop(base::paste0("\033[91m Error:\033[0m \033[1mstats_kmeans:\033[0m \033[91m'centers_grid_range' argument needs to be within the range of 'centers_grid_range' (", base::min(centers_grid_range), "-", base::max(centers_grid_range), ")\033[0m"))
  # }


  #check id col is in df

  # if(!id_col_str %in% base::colnames(.data)){
  #   stop("\033[1m\033[91m Error:\033[0m \033[1m'id_col' argument not found in df columns, please ensure id_col exists in df\033[0m")
  # }


# checks column types
#
#   if(!base::all(purrr::map_lgl(.data, \(x) base::class(x) %in% c("numeric","integer","character","factor","ordered")))){
#     stop("Error: columns must be class numeric, integric, character, factor or ordered")
#   }

# start the formula  >>>

  tbl_numeric <- select(.data,-c({{id_col}}))
  print(tbl_numeric)

  mapper_kmeans <- function(kmeans_centers=3,kmeans_nstart=10,kmeans_iter.max=100) {


    stats::kmeans(
      x=tbl_numeric
      ,centers=kmeans_centers
      ,nstart = kmeans_nstart
      ,iter.max=kmeans_iter.max
    )
  }



  # centers_grid_range <- 1:10
  #make tbl grid to iterate through different centers options

  tbl_grid <-  tibble::tibble(centers_input=centers_grid_range) |>
    dplyr::mutate(
      kmeans_models=purrr::map(
        .x=centers_input
        ,\(x) mapper_kmeans(kmeans_centers=x)
        ,.progress = TRUE)
      ,kmeans_results=purrr::map(kmeans_models,\(x) broom::glance(x))
    ) |>
    tidyr::unnest(kmeans_results) |>
    dplyr::select(
      centers_input
      ,kmeans_models
      ,tot.withinss
    )


  print("success3")
  base::cat("\033[93m[1] Success:\033[0m \033[1mKmeans done successfully\033[0m\n")

    #plots grid of different centers and tot.withinss
  plot_grid_kmeans <-
    tbl_grid |>
    ggplot2::ggplot(
      ggplot2::aes(y=tot.withinss,x=centers_input)
      )+
    ggplot2::geom_point()+
    ggplot2::geom_line()


  # creates umap objection
  obj_umap <- tbl_numeric |>
    umap::umap()


  #extracts out positions


  tbl_umap <-   obj_umap$layout |>
    tibble::as_tibble() |>
    rlang::set_names((c("x","y"))) |>
    dplyr::bind_cols(.data)

  ## replace with CLI
  base::cat("\033[93m[2] Success:\033[0m \033[1mUMAP done successfully\033[0m\033[32m\033[0m\n")

  # select the kmeans model based on the inputted figure and extract their clusters



tbl_kmeans <-   tbl_grid |>
  dplyr::filter(centers_input==kmeans_centers_init) |>
  dplyr::pull(kmeans_models) |>
  purrr::pluck(1) |>
  broom::augment(.data) |>
  dplyr::select({{id_col}},.cluster)



  ## join umap and kmeans tables
#
#
tbl_segmentation_id <-
  tbl_kmeans |>
  dplyr::left_join(tbl_umap,by=join_by({{id_col}}=={{id_col}}))

#
#   ## plot kmeans clusters and 2 dimensional modeling
plot_id_segmentation <-
  tbl_segmentation_id |>
   ggplot2::ggplot(
     ggplot2::aes(
       x=x
       ,y=y
       ,col=.cluster
     )
           )+
  ggplot2::geom_point()+
  ggrepel::geom_text_repel(
    aes(label={{id_col}})
    ,max.overlaps=max.overlaps
    ,...
  )
#
#
#   #save outputs to a list to be accessed later
  segmentation_output <- base::list()


  segmentation_output$plot_grid_centers <- plot_grid_kmeans
  segmentation_output$plot_id_segmentation <- plot_id_segmentation
  segmentation_output$tbl_id_segmentation <- tbl_segmentation_id
  segmentation_output$tbl_grid <- tbl_grid
  segmentation_output$tbl_umap <- tbl_umap
#
#   base::cat("\033[93m[3] Success:\033[0m\033[1m Assign this formula to an object and then use the '$' to access the saved tbls and plots\033[0m\033[32m\033[0m\n")

base::return(segmentation_output)
}
