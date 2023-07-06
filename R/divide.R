
#' Divide function with error handling for divide by zero or NA
#'
#' @param numerator numeric numerator
#' @param denominator numeric denominator
#' @param alternative_result alternative results if divide by 0 can by numeric or character but beware of class coercion
#'
#' @return
#' @export
#'
#' @examples
#' divide(100,0,"error")
divide <- function(numerator,denominator,alternative_result=NA_integer_){


  temp <- `/`(numerator,denominator)

  inf_vec <- which(is.infinite(temp)|is.na(temp))

  if(sum(inf_vec,na.rm=TRUE)>0){
    cli::cli_alert_info("FYI: Inf or NA values detected")
  }


  temp <- replace(temp,inf_vec,alternative_result)
  return(temp)

}


