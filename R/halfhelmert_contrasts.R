#' Half-Helmert Contrasts
#'
#' contr.helmert()/2.
#' @param ... Arguments passed to contr.helmert
#'
#' @return See Value in contr.helmert().
#' @export
#'
#' @examples
halfhelmert_contrasts = function(
	...
){
	contr.helmert(...)*.5
}
