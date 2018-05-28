# ezStan
An R package containing helper functions for Stan.

This package contains code I develop through my teaching and personal use of Stan/RStan to make some common tasks easier. It's very much a work in progress and probably won't ever be on CRAN.

Install via:
```{r}
devtools::install_github('mike-lawrence/ezStan')
devtools::install_github('mike-lawrence/loggr') #temporarily necessary until smbache/loggr is updated and uploaded to CRAN 

```

Usage Example:
```{r}
#compile a model; shouldn't block the RStudio UI like rstan::stan_model()
my_mod = build_stan(my_stan_file)

#start the model sampling
start_stan(
  data = my_data_for_stan 
  , mod = my_mod
  # can add other arguments to rstan::sampling() here if nessary, for example:
  , iter = 4e3
)

#now watch the sampling (see ?watch_stan for options, like killing when post-warmup divergences are encountered)
watch_stan()
#it's ok to stop the watcher; you can resume watching by simply running watch_stan() again

#if anything goes awry with the sampling, you can call kill_stan() to stop all chains

#when the sampling is done, collect results
my_post = collect_stan()

#once collected, you can recover some disk space by running:
clean_stan()

#slightly cleaner looking summary table:
stan_summary(
 post = my_post
 , par = 'some_parameter'
)

```

## TO DO:
- collect_stan: be robust against bad chains
- watch_stan: dynamically compute effective sample size and rhat (& other diagnostics?) for each parameter during post-warmup sampling period
