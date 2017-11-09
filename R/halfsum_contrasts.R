#' Half-sum Contrasts
#'
#' contr.sum()/2.
#' @param ... Arguments passed to contr.sum
#'
#' @return See Value in contr.sum().
#' @export
#'
#' @examples
halfsum_contrasts = function(
	...
){
	contr.sum(...)*.5
}
