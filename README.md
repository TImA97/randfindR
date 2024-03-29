
# randfindR

randfindR is a package for the programming language R in development for
the analysis of randomness in sequences of distinct options (e.g.,
numbers or letters). randfindR can help you determine with several
indices how systematic a sequence of options is according to different
benchmarks of randomness. Its focus lies on the detection of patterns
that are typically found in human generated sequences.

## Installation

Once accepted (!), you can install the released version of randfindR
from [CRAN](https://CRAN.R-project.org) with (this is not yet possible):

``` r
#install.packages("randfindR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("TImA97/randfindR")
```

## Getting started

This quick guide is meant to show you the indices of randomness that can
be computed with randfindR. As of now, randfindR offers 15 different
indices of randomness that can be easily computed as can be seen in the
following explanation.

All functions take as first argument a vector (your random sequence). In
most cases you also have to specify the number of possible `options` a
sequence can contain. The following index indicates how regular a
sequence is with a value of 0 indicating no regularity and a value of 1
maximum regularity.

``` r
library(randfindR)

reg_index(c(1,2,1,2,1,1,2,2,2), options = 2)
#> [1] 0.4654518
```

The `options`argument is important because many indices will reflect the
fact that one or several options were omitted in a sequence. Let’s take
the example from above, but assume this time that there are three
possible options, i.e., one option was completely omitted!

``` r
reg_index(c(1,2,1,2,1,1,2,2,2), options = 3)
#> [1] 0.5773503
```

As you can see, the resulting index is now higher, indicating increased
regularity.

Some indices have additional arguments or do not have the
`options`argument. More information and examples on how to use all
indices can be found in the documentation.

## All-inclusive option

If you want all available indices or a list of indices of your choice to
be computed, you can use the `all_rand()` function. This function will
by default return all indices of randomness for a given `vector` or
`data.frame` (in the latter case indices are computed using a row-wise
format).

Let’s take the example from above again, but this time call the
`all_rand()` function:

``` r
round(all_rand(c(1,2,1,2,1,1,2,2,2), options = 2), 2)
#>       digram_rep      repetitions           series    cluster_ratio 
#>             5.00             3.00            12.00             0.69 
#>       null_score        reg_index       runs_index     coupon_score 
#>             0.00             0.47             0.25             2.33 
#>        gap_score      poker_score        rng_index       rng2_index 
#>             2.00             1.00             0.59             0.55 
#>         tp_index redundancy_index       var_digits 
#>            85.71             0.89             0.25
```

But you can also explicitely specify the indices that should be
computed:

``` r
all_rand(c(1,2,1,2,1,1,2,2,2), options = 2, indices = c("reg_index", "rng_index"))
#> reg_index rng_index 
#> 0.4654518 0.5869447
```

You can just enter all relevant arguments into the `all_rand()` function
and it will take care of everything else and insert your specified
arguments in the right places!

## List of randomness indices

The following list contains all indices that are implemented in
randfindR. More indices may be added in the future.

-   digram_rep
-   repetitions
-   series
-   cluster_ratio
-   null_score
-   reg_index
-   runs_index
-   coupon_score
-   gap_score
-   poker_score
-   rng_index
-   rng2_index
-   tp_index
-   redundancy_index
-   var_digits

Details on how to use these algorithms can be found in the documentation
by entering `?index_name`into your R-console.

## References

The indices implemented in this package are based on the following
publications:

Towse, J.N., Neil, D. Analyzing human random generation behavior: A
review of methods used and a computer program for describing
performance. Behavior Research Methods, Instruments, & Computers 30,
583–591 (1998). <https://doi.org/10.3758/BF03209475>

Ginsburg N, Karpiuk P. Random Generation: Analysis of the Responses.
Perceptual and Motor Skills. 1994;79(3):1059-1067.
<https://doi.org/10.2466/pms.1994.79.3.1059>

Skliar, Osvaldo, Ricardo E. Monge, Guillermo Oviedo, and Víctor Medina.
2009. Indices of regularity and indices of randomness for m-ary strings.
<https://doi.org/10.15517/rmta.v16i1.1418>
