
#' Classify a group by proportion of a variable (A,B,C)
#'
#' @param dim dimension to classify by (should be additive and positive)
#' @param a initial segment threshold expressed as a percentage
#' @param b incremental segment threshold expressed as a percentage
#' @param c final segment threshold expressed as a percentage
#' @param na.rm to remove na values
#' @param .data data frame or tibble
#' @param ... groups columns  to group by
#' @param order_by aggregate function to order by such as "sum","n","sd","mean","mad","aad","prop05","prop25","prop_50","prop_75","prop95"
#'
#' @return a tibble or obj
#' @export
#'
#' @examples
#'
#' customer_abc <- abc(contoso,customer_key,dim=margin,a=.7,b=.2,c=.1)
abc <- function(.data,...,dim,a=.7,b=.26,c=.04,na.rm=TRUE,order_by=c("sum")){

  # input validation

  assertthat::assert_that(a+b+c==1,msg = "A, B, and C must sum to 1")

  ## numeric dim validation check

  # declar variables

  dim_str <- base::deparse(base::substitute(dim))

  .data <- contoso_fact_sales_db

  dbi_flag <- if_else(
    base::any(
      base::class(.data) %in% "tbl_dbi"
      )
    ,TRUE
    ,FALSE
    )
  ## sql
  if(dbi_flag){


  glimpse_sql <- glue::glue_sql("DESCRIBE TABLE {.data}")



  ## validation of dim type check
  column_type_vec <- DBI::dbGetQuery(.data$src[[1]],glimpse_sql) |>
    tibble::as_tibble() |>
    dplyr::filter(
      column_name==dim_str
    ) |>
    pull(column_type)



  assertthat::assert_that(
    base::any(
      stringr::str_to_lower(column_type_vec) %in% c("date","double","integer")
      )
    ,msg = "dim must be a numeric column"
    )

  }

  ## non-sql

  assertthat::assert_that(is.numeric(.data[[dim_str]]),msg = "dim must be a numeric column")

  ## begin function

  ### non DBI

  if(!dbi_flag){

temp1 <-.data |>  #passes the dataframe through

    dplyr::group_by(dplyr::pick(...)) |>  #group's the dataframe by the input group column

    #creates a bunch of columns
   dplyr::summarize(
      dplyr::across({{ dim }}, #make sure this dimension can be aggregated, later version will handle ratios
             list(sum=~base::sum(.,na.rm=na.rm),
                  mean=~ base::mean(.,na.rm=na.rm),
                  n =  ~ dplyr::n(),
                  sd =  ~ stats::sd(.,na.rm=na.rm),
                  prop_05 = ~stats::quantile(., .05,na.rm=na.rm),
                  prop_25 = ~stats::quantile(., .25,na.rm=na.rm),
                  prop_50 =  ~ stats::median(.,na.rm=na.rm),
                  prop_75 = ~stats::quantile(., .75,na.rm=na.rm),
                  prop_95 = ~stats::quantile(., .95,na.rm=na.rm)
             ),
             .names = "{.fn}") #gives each column their name
    ) %>%
    dplyr::ungroup()


# order_by <- "sum"

order_by_value <- base::match.arg(arg = order_by,choices = c("sum","n","sd","mean","mad","aad","prop05","prop25","prop_50","prop_75","prop95"),)

final_tbl <- temp1 %>%

    dplyr::arrange(dplyr::desc(dplyr::pick(order_by_value))) %>% # assuming positive values, descends highest to lowest

    dplyr::mutate(cum_sum=base::cumsum(pick(order_by_value)), #cumlative value, if ratio, need some sort of check - need specify

           prop_total=pick(order_by)/max(cum_sum), #assumes positive values, need check

           cum_prop_total=base::cumsum(prop_total), #cumsum percent of total

           cum_unit_prop=dplyr::row_number()/max(dplyr::row_number()), #unit percent

           group_classification_by_dim= #classify the cumlative percent by threshold
             dplyr::case_when(
               cum_prop_total<=a ~"A",
               cum_prop_total<=(a+b) ~"B",
               na.rm ~ "C"),
           dim_threshold= #adds a column of the thresholds
             dplyr::case_when(group_classification_by_dim=="A"~a,
                       group_classification_by_dim=="B"~(a+b),
                       na.rm ~ c)
    ) %>%
    dplyr::select(-c(prop_total,cum_sum)) %>%  #removes unused columns
    dplyr::relocate(dim_threshold,group_classification_by_dim,cum_prop_total,cum_unit_prop) #orders columns
# need to double check this
return(final_tbl)

  }

  if(dbi_flag){


      temp1 <-.data |>  #passes the dataframe through

        dplyr::group_by(dplyr::pick(...)) |>  #group's the dataframe by the input group column

        #creates a bunch of columns
        dplyr::summarize(
          dplyr::across({{ dim }}, #make sure this dimension can be aggregated, later version will handle ratios
                        list(sum=~base::sum(.,na.rm=na.rm),
                             mean=~ base::mean(.,na.rm=na.rm),
                             n =  ~ dplyr::n(),
                             sd =  ~ stats::sd(.,na.rm=na.rm),
                             mad =  ~ stats::mad(.,na.rm=na.rm,constant = 1), #median absolute deviation
                             aad =  ~ stats::mad(., center =base::mean(.,na.rm=na.rm),na.rm=na.rm,constant=1), #average absolute deviation
                             prop_05 = ~stats::quantile(., .05,na.rm=na.rm),
                             prop_25 = ~stats::quantile(., .25,na.rm=na.rm),
                             prop_50 =  ~ stats::median(.,na.rm=na.rm),
                             prop_75 = ~stats::quantile(., .75,na.rm=na.rm),
                             prop_95 = ~stats::quantile(., .95,na.rm=na.rm)
                        ),
                        .names = "{.fn}") #gives each column their name
        ) %>%
        dplyr::ungroup()


      # order_by <- "sum"

      order_by_value <- base::match.arg(arg = order_by,choices = c("sum","n","sd","mean","mad","aad","prop05","prop25","prop_50","prop_75","prop95"),)

      final_tbl <- temp1 %>%

        dplyr::arrange(dplyr::desc(dplyr::pick(order_by_value))) %>% # assuming positive values, descends highest to lowest

        dplyr::mutate(cum_sum=base::cumsum(pick(order_by_value)), #cumlative value, if ratio, need some sort of check - need specify

                      prop_total=pick(order_by)/max(cum_sum), #assumes positive values, need check

                      cum_prop_total=base::cumsum(prop_total), #cumsum percent of total

                      cum_unit_prop=dplyr::row_number()/max(dplyr::row_number()), #unit percent

                      group_classification_by_dim= #classify the cumlative percent by threshold
                        dplyr::case_when(
                          cum_prop_total<=a ~"A",
                          cum_prop_total<=(a+b) ~"B",
                          na.rm ~ "C"),
                      dim_threshold= #adds a column of the thresholds
                        dplyr::case_when(group_classification_by_dim=="A"~a,
                                         group_classification_by_dim=="B"~(a+b),
                                         na.rm ~ c)
        ) %>%
        dplyr::select(-c(prop_total,cum_sum)) %>%  #removes unused columns
        dplyr::relocate(dim_threshold,group_classification_by_dim,cum_prop_total,cum_unit_prop) #orders columns
      # need to double check this
      return(final_tbl)










  }




}
