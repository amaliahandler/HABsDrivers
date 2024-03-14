#' Create a formula with many covariates for the HABs drivers project using spmodel
#'
#' @param dep_var The dependent variable of the model as a string.
#' @param ind_vars A vector string of covariate names.
#' @param log TRUE/FASLE Should the dependent variable be log10 transformed. Default is FALSE.
#' @param const Numeric. If log-transforming, a constant to add to the dependent variable. Default is 0.
#' @param interact Any interactions to include in the model taking the string form of "var_name1 * var_name2". Default is NULL.
#'
#' @return A formula object
#' @export
#'
#' @examples
#' create_formula(dep_var = "B_G_DENS", ind_vars = c("NTL", "MAXDEPTH", "agr_ws"), log = T, const = 1000)
create_formula <- function(dep_var, ind_vars, log = F, const = 0, interact = NULL){
  # Log = F
  form <- paste(dep_var, " ~ ", paste(ind_vars, collapse = " + "))

  # Log = T
  if(log == T){
    form <- paste("log10(", dep_var, "+", const, ") ~ ", paste(ind_vars, collapse = " + "))
  }

  # Adding an interaction term
  if(is.null(interact) == F){
    form <- paste(form, "+", interact)
  }
  # Create formula object
  form_obj <- as.formula(form)
  return(form_obj)
}
