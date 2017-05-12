#' Terminate an errant Big Stan session
#'
#' @return No value is returned
#' @export
#'
#' @examples
killBigStan = function(){
	system2(
		command = "killall"
		, args = "R"
	)
}
