
# spells

A small TidyTuesday exercise, fun in its own right, but mostly an
exercise in teaching myself how to use the targets package.

## basic workflow

Quick look at the targets that are defined for this pipeline using
`tar_manifest()`:

``` r
library(targets)
tar_manifest()
#> # A tibble: 8 × 2
#>   name            command                                                   
#>   <chr>           <chr>                                                     
#> 1 input           "\"spells.csv\""                                          
#> 2 output          "set_output_dir()"                                        
#> 3 spells          "read_csv(input, show_col_types = FALSE)"                 
#> 4 scholastic_dat  "scholastic_data(spells)"                                 
#> 5 dice_dat        "dice_data(spells)"                                       
#> 6 scholastic_clus "scholastic_clusters(scholastic_dat)"                     
#> 7 dice_pic        "dice_plot(dice_dat, output)"                             
#> 8 scholastic_pic  "scholastic_plot(scholastic_dat, scholastic_clus, output)"
```

Under the hood this file uses `tar_destroy()` at the beginning, in order
to remove the entire `_targets` directory. We’re starting from a
completely fresh state, so when I call `tar_outdated()` it shows that
everything is outdated:

``` r
tar_outdated()
#> [1] "scholastic_clus" "scholastic_dat"  "spells"          "scholastic_pic" 
#> [5] "output"          "dice_pic"        "input"           "dice_dat"
```

Our build function is `tar_make()`, so here goes:

``` r
tar_make()
#> ▶ dispatched target input
#> ● completed target input [0.284 seconds, 302.514 kilobytes]
#> ▶ dispatched target output
#> ● completed target output [0.002 seconds, 144 bytes]
#> ▶ dispatched target spells
#> ● completed target spells [0.084 seconds, 73.966 kilobytes]
#> ▶ dispatched target scholastic_dat
#> ● completed target scholastic_dat [0.013 seconds, 401 bytes]
#> ▶ dispatched target dice_dat
#> ● completed target dice_dat [0.017 seconds, 33.486 kilobytes]
#> ▶ dispatched target scholastic_clus
#> ● completed target scholastic_clus [0.019 seconds, 634 bytes]
#> ▶ dispatched target dice_pic
#> ● completed target dice_pic [0.905 seconds, 153 bytes]
#> ▶ dispatched target scholastic_pic
#> ● completed target scholastic_pic [0.195 seconds, 156 bytes]
#> ▶ ended pipeline [1.617 seconds]
```

Having run everything, we check status again:

``` r
tar_outdated()
#> character(0)
```

Nothing is outdated, so when we call `tar_make()` nothing happens:

``` r
tar_make()
#> ✔ skipped target input
#> ✔ skipped target output
#> ✔ skipped target spells
#> ✔ skipped target scholastic_dat
#> ✔ skipped target dice_dat
#> ✔ skipped target scholastic_clus
#> ✔ skipped target dice_pic
#> ✔ skipped target scholastic_pic
#> ✔ skipped pipeline [0.068 seconds]
```

## user tools

Targets executes R code within an isolated R process (using callr), so
none of the objects appear in the parent workspace when `tar_make()` is
called. That’s a good thing, insofar as it makes it easier to manage
state in your pipelines. But it also makes it a little trickier to debug
or tinker. To that end there are some user-facing tools that let you
pull the tracked objects into your workspace. There’s two versions. The
`tar_load()` function imports the object with its original name:

``` r
tar_load("dice_dat")
dice_dat
#> # A tibble: 236 × 8
#>    name           level description dice_txt position dice_num dice_die dice_val
#>    <chr>          <dbl> <chr>       <fct>       <int>    <dbl>    <dbl>    <dbl>
#>  1 Acid Splash        0 "You creat… 1d6             1        1        6      3.5
#>  2 Acid Splash        0 "You creat… 2d6             2        2        6      7  
#>  3 Acid Splash        0 "You creat… 3d6             3        3        6     10.5
#>  4 Acid Splash        0 "You creat… 4d6             4        4        6     14  
#>  5 Alter Self         2 "You alter… 1d6             1        1        6      3.5
#>  6 Animate Objec…     5 "Objects a… 1d4             1        1        4      2.5
#>  7 Animate Objec…     5 "Objects a… 1d6             2        1        6      3.5
#>  8 Animate Objec…     5 "Objects a… 1d12            3        1       12      6.5
#>  9 Animate Objec…     5 "Objects a… 2d6             4        2        6      7  
#> 10 Animate Objec…     5 "Objects a… 2d12            5        2       12     13  
#> # ℹ 226 more rows
```

As an alternative, `tar_read()` allows you to assign the object to
whatever variable name you like:

``` r
dd <- tar_read("dice_dat")
dd
#> # A tibble: 236 × 8
#>    name           level description dice_txt position dice_num dice_die dice_val
#>    <chr>          <dbl> <chr>       <fct>       <int>    <dbl>    <dbl>    <dbl>
#>  1 Acid Splash        0 "You creat… 1d6             1        1        6      3.5
#>  2 Acid Splash        0 "You creat… 2d6             2        2        6      7  
#>  3 Acid Splash        0 "You creat… 3d6             3        3        6     10.5
#>  4 Acid Splash        0 "You creat… 4d6             4        4        6     14  
#>  5 Alter Self         2 "You alter… 1d6             1        1        6      3.5
#>  6 Animate Objec…     5 "Objects a… 1d4             1        1        4      2.5
#>  7 Animate Objec…     5 "Objects a… 1d6             2        1        6      3.5
#>  8 Animate Objec…     5 "Objects a… 1d12            3        1       12      6.5
#>  9 Animate Objec…     5 "Objects a… 2d6             4        2        6      7  
#> 10 Animate Objec…     5 "Objects a… 2d12            5        2       12     13  
#> # ℹ 226 more rows
```

## the actual images

As an aside, here’s the outputs:

![](output/dice_pic.png?raw=true)

![](output/scholastic_pic.png?raw=true)
