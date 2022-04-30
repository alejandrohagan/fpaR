
#' ABC classification
#'
#' @param df dataframe
#' @param group grouping variable to classify
#' @param dim dimension to classify by (should be additive and positive)
#' @param a initial segment threshold expressed as a percentage
#' @param b incremental segment threshold expressed as a percentage
#' @param c final segment threshold expressed as a percentage
#' @param na.rm to remove na values
#'
#' @return
#' @export
#'
#' @examples
#'
#' customer_abc <- abc(contoso,customer_key,margin,a=.7,b=.2,c=.1)
abc <- function(df,group,dim,a=.7,b=.26,c=.04,na.rm=TRUE){


  #error check to ensure a,b,c sum up to one and that dim is numeric

  ##declare variable outside of stopfunction for context transition
  dim_check <- df %>% pull({{dim}})

  stopifnot("a,b,c must sum to 1, please try again"=a+b+c==1,
            "dim must be numeric"=is.numeric(dim_check),
            "please change dim to positive value"= sum(dim_check)>0)

  df %>% #passes the dataframe through

    group_by({{group}}) %>% #group's the dataframe by the input gruop column

    #creates a bunch of columns
    summarize(
      across({{ dim }}, #make sure this dimension can be aggregated, later version will handle ratios
             list(sum=~sum(.,na.rm=na.rm),
                  mean=~mean(.,na.rm=na.rm),
                  n =  ~ n(),
                  sd =  ~ sd(.,na.rm=na.rm),
                  mad =  ~ mad(.,na.rm=na.rm), #median absolute deviation
                  aad =  ~ mad(., center =mean(.,na.rm=na.rm),na.rm=na.rm), #average absolute deviation
                  IQR05 = ~quantile(., .05,na.rm=na.rm),
                  IQR25 = ~quantile(., .25,na.rm=na.rm),
                  median =  ~ median(.,na.rm=na.rm),
                  IQR75 = ~quantile(., .75,na.rm=na.rm),
                  IQR95 = ~quantile(., .95,na.rm=na.rm)
             ),
             .names = "{.fn}") #gives each column their name
    ) %>%
    ungroup() %>%

    arrange(desc(sum)) %>% # assuming positive values, descends highest to lowest

    mutate(cum_sum=cumsum(sum), #cumlative value, if ratio, need some sort of check - need specify

           prop_total=sum/max(cum_sum), #assumes positive values, need check

           cum_prop_total=cumsum(prop_total), #cumsum percent of total

           cum_unit_prop=row_number()/max(row_number()), #unit percent

           group_classification_by_dim= #classify the cumlative percent by threshold
             case_when(
               cum_prop_total<=a ~"A",
               cum_prop_total<=(a+b) ~"B",
               na.rm ~ "C"),
           dim_threshold= #adds a column of the thresholds
             case_when(group_classification_by_dim=="A"~a,
                       group_classification_by_dim=="B"~(a+b),
                       na.rm ~ c)
    ) %>%
    select(-c(prop_total,cum_sum)) %>%  #removes unused columns
    relocate(dim_threshold,group_classification_by_dim,cum_prop_total,cum_unit_prop) #orders columns

}

