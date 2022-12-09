
``` r
activity[, mean(power)] 
activity[, median(power)]
activity[, normalized_power(power)]
activity[, max(rollapply(power, 60*20, mean))]
activity[, max(rollapply(power, 60*5, mean))]
activity[, max(rollapply(power, 60, mean))]
activity[, max(rollapply(power, 15, mean))]
```

    ## [1] 275.2829
    ## [1] 296
    ## [1] 346.9586
    ## [1] 308.9733
    ## [1] 356.5567
    ## [1] 440.2667
    ## [1] 714.8667
