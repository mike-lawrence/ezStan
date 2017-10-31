#' Terminate an errant Stan session
#'
#' @return No value is returned
#' @export
#'
#' @examples
kill_stan = function(){
	system2(
		command = "killall"
		, args = "R"
	)
}
