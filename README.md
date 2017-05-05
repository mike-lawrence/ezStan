# ezStan
An R package containing helper functions for Stan.

This package contains code I develop through my teaching and personal use of Stan/RStan to make some common tasks easier. It's very much a work in progress and probably won't ever be on CRAN.

## TO DO:
- startBigStan: each chain should delete its sample file once it has saved its rda
- collectBigStan: collect contents of stderr files
- watchBigStan: only list a chain as having an error if it is not complete
- watchBigStan: "chains with errors" message is only ever showing one number
