#' Half-sum Contrasts
#'
#' Returns contrasts whereby coeffients will be equal to the difference between
#' levels (in the case of 2-level factors). Simply contr.sum()/2.
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
