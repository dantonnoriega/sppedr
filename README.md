
<!-- README.md is generated from README.Rmd. Please edit that file -->
Installation
============

    install.packages('devtools') # install devtools if needed
    devtools::install_github("dantonnoriega/sppedr") # install the package

Prerequisites
=============

This package assumes you have access to a directory with the following files and structure

    spped
    NA
    NA
    NA
        RawData
    NA
    NA
    NA
    NA
    NA

The directory `spped` is considered the data "main directory" (or `main_dir`) by the package. Wherever this is located on your computer, ensure the package knows where it is by running the `set_main_dir()` command.

    #> [1] "/Users/danton/Dropbox/ra-work/spped"

This will allow you to run the package functions from scratch should you want to generate new data.

Package Datasets
================

The package, when loaded, also attaches the following datasets. These data sets can be called directly once you have loaded `library(sppedr)`.

Dataset Descriptions
--------------------

-   `cps_2008_2016`: school data for years 2008 to 2016. this is a csv version of the dta file `cps_2008_2016.dta`
-   `cps_school_year`: this contains dates about the academic school year. it is a list object with two data frames, `cps_school_year$school_year` and `cps_school_year$days_off`.
-   `cps_address`: contains lat/lon coordinates and full addresses as understood by the google maps API.
-   `cps_security_personnel`
-   `cps_crosswalk`: this file helps link the `cps_security_personnel` data with the all other data sources by merging on the `unit_number` variable.
-   `cpd_crime_codes`: contains crime codes from the Chicago PD (IUCR codes) and the FBI. it is a list of two data frames, `cpd_crime_codes$iucr` and `cpd_crime_codes$fbi`.

Example: `cps_address` dataset
------------------------------

``` r
# first 10 full school address for academic year 2009 - 2010
cps_address %>% 
  dplyr::filter(school_year == 2010) %>%
  dplyr::select(schoolname, school_year, address_full, lat, lon) %>%
  head(10)
#> # A tibble: 10 x 5
#>                          schoolname school_year                              address_full      lat
#>                               <chr>       <dbl>                                     <chr>    <dbl>
#>  1             Amundsen High School        2010      5110 N Damen Ave, Chicago, IL, 60625 41.97509
#>  2                Bogan High School        2010        3939 W 79th St, Chicago, IL, 60652 41.74876
#>  3       Carver Military Academy HS        2010      13100 S Doty Ave, Chicago, IL, 60827 41.65661
#>  4 Crane Technical Prep High School        2010   2245 W Jackson Blvd, Chicago, IL, 60612 41.87685
#>  5       Farragut Career Academy HS        2010 2345 S Christiana Ave, Chicago, IL, 60623 41.84934
#>  6       Fenger Academy High School        2010    11220 S Wallace St, Chicago, IL, 60628 41.68968
#>  7              Foreman High School        2010   3235 N Leclaire Ave, Chicago, IL, 60641 41.93977
#>  8            Gage Park High School        2010    5630 S Rockwell St, Chicago, IL, 60629 41.79103
#>  9      Harlan Community Academy HS        2010   9652 S Michigan Ave, Chicago, IL, 60628 41.71819
#> 10               Harper High School        2010        6520 S Wood St, Chicago, IL, 60636 41.77499
#> # ... with 1 more variables: lon <dbl>
```

Other Info
==========

In the [package directory `data-raw`](https://github.com/dantonnoriega/sppedr/tree/master/data-raw), there is a `README.txt` file that explains where all the internal datasets come from and/or how they are built. Scripts that build most of these data are also in `data-raw`. Note that this directory is accessible via the github repo, not the package itself.

Functions
=========

The only function that one really needs call is `get_cpd_crime()`. This is because these data are much larger and could not be included as part of the package. It will import these data from the `main_dir` set using the `set_main_dir()` function.

The following functions do not need to be called *unless you want to regenerate these data again from scratch*.

-   `get_cps_2008_2016()`
-   `get_cps_address()`
-   `get_cps_security_personnel()`

To do so, you need to set `force = TRUE`. Otherwise, the function will just load the package version. *I would recommend NOT to use the force option*.

Example
-------

Here is an example where the CPS security personnel data is spread and the merged to the crosswalk data.

``` r

personnel_wide <- cps_security_personnel %>%
  dplyr::left_join(., cps_crosswalk, by = 'unit_number') %>%
  dplyr::mutate(job_description = gsub('school ', '', job_description)) %>%
  dplyr::mutate(job_description = gsub('[[:space:]]', '_', job_description)) %>%
  dplyr::filter(!is.na(schoolidr_c_d_t_s)) %>%
  tidyr::spread(job_description, n) %>%
  dplyr::mutate_if(is.integer, dplyr::funs(replace(., is.na(.), 0))) %>% # replace NAs with 0
  dplyr::select(-unit_number, -abbreviated_name) %>%
  dplyr::mutate(school_year = as.integer(school_year))

# list first 10 security_officer counts
personnel_wide %>%
  dplyr::select(school_year, school_name, security_officer) %>%
  head(10)
#> # A tibble: 10 x 3
#>    school_year                               school_name security_officer
#>          <int>                                     <chr>            <dbl>
#>  1        2008     Columbia Explorers Elementary Academy                1
#>  2        2008                         Abbott Elementary                2
#>  3        2008             Jane Addams Elementary School                3
#>  4        2008         Louis A Agassiz Elementary School                0
#>  5        2008       Louisa May Alcott Elementary School                1
#>  6        2008             Alcott Humanities High School                1
#>  7        2008          John P Altgeld Elementary School                3
#>  8        2008        Phillip D Armour Elementary School                1
#>  9        2008               New Field Elementary School                0
#> 10        2008 George Armstrong International Studies ES                2
```
