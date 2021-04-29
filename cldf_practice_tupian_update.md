cldf\_practice\_tupian\_update
================
Manuel Fernandez
4/29/2021

## In-class Assignment 04/28/21

Read data and add columns.

1.  Macro\_Region/South America
2.  Family/Tupian
3.  Country/Brazil \| Paraguay

``` r
# Clear R memory
rm(list = ls(all = T))

# Load tidyverse
suppressPackageStartupMessages(
        sapply(c("tidyverse", "readr",
                 "maps", "mapdata", "maptools"),
               require, character.only = T))
```

    ## tidyverse     readr      maps   mapdata  maptools 
    ##      TRUE      TRUE      TRUE      TRUE      TRUE

``` r
# read data
if (file.exists("cldf-practice-tupian-old.csv")) {
        cldf_practice_tupian <- read_csv("cldf-practice-tupian-old.csv")
} else {
        cldf_practice_tupian <- read_csv("cldf-practice-tupian.csv")
}
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   ID = col_double(),
    ##   Language = col_character(),
    ##   Glottocode = col_character(),
    ##   Cognate_Set = col_character(),
    ##   Form = col_character(),
    ##   Concept = col_character(),
    ##   Concept_ID = col_double(),
    ##   Source = col_character(),
    ##   Source_Page = col_double(),
    ##   Comment = col_character()
    ## )

``` r
# Languages.csv from https://zenodo.org/record/4061165
languages <- read_csv("glottolog-cldf/cldf/languages.csv")
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   ID = col_character(),
    ##   Name = col_character(),
    ##   Macroarea = col_character(),
    ##   Latitude = col_double(),
    ##   Longitude = col_double(),
    ##   Glottocode = col_character(),
    ##   ISO639P3code = col_character(),
    ##   Countries = col_character(),
    ##   Family_ID = col_character(),
    ##   Language_ID = col_character()
    ## )

``` r
# preview both
head(cldf_practice_tupian)
```

    ## # A tibble: 6 x 10
    ##      ID Language    Glottocode Cognate_Set Form  Concept Concept_ID Source      
    ##   <dbl> <chr>       <chr>      <chr>       <chr> <chr>        <dbl> <chr>       
    ## 1     1 Ak = Akunt… akun1241   I-1         õn    I/ME          2301 Galucio et …
    ## 2     2 Ar = Aruá   arua1261   I-1         õõt   I/ME          2301 Galucio et …
    ## 3     3 Aw = Awetí  awet1244   I-2         ito   I/ME          2301 Galucio et …
    ## 4     4 Gv = Gavião gavi1246   I-1         õõt   I/ME          2301 Galucio et …
    ## 5     5 Ju = Jurúna juru1256   I-1         una   I/ME          2301 Galucio et …
    ## 6     6 Ka = Káro   karo1305   I-1         ʔõn   I/ME          2301 Galucio et …
    ## # … with 2 more variables: Source_Page <dbl>, Comment <chr>

``` r
head(languages)
```

    ## # A tibble: 6 x 10
    ##   ID     Name     Macroarea Latitude Longitude Glottocode ISO639P3code Countries
    ##   <chr>  <chr>    <chr>        <dbl>     <dbl> <chr>      <chr>        <chr>    
    ## 1 kond1… Konda-Y… <NA>          NA        NA   kond1302   <NA>         <NA>     
    ## 2 cani1… Canicha… South Am…    -14.3     -64.9 cani1243   caz          BO       
    ## 3 mong1… Mongoli… <NA>          NA        NA   mong1349   <NA>         <NA>     
    ## 4 cadd1… Caddoan  <NA>          NA        NA   cadd1255   <NA>         <NA>     
    ## 5 yuki1… Yuki-Wa… <NA>          NA        NA   yuki1242   <NA>         <NA>     
    ## 6 grea1… Great A… <NA>          NA        NA   grea1241   <NA>         <NA>     
    ## # … with 2 more variables: Family_ID <chr>, Language_ID <chr>

``` r
# Select WALS languoid location data
languages <- select(languages, "Glottocode", "Latitude", "Longitude")

# Merge languoid location data into practice tibble
cldf_practice_tupian <- merge(cldf_practice_tupian, languages,
                              by = "Glottocode",
                              all.x = T, sort = T, no.dups = F) %>%
        tibble() %>% arrange(ID)

# Add Macro_Region, Family
cldf_practice_tupian <- cldf_practice_tupian %>%
        mutate(Macro_Region = "South America",
               Family = "Tupian")
head(cldf_practice_tupian)
```

    ## # A tibble: 6 x 14
    ##   Glottocode    ID Language    Cognate_Set Form  Concept Concept_ID Source      
    ##   <chr>      <dbl> <chr>       <chr>       <chr> <chr>        <dbl> <chr>       
    ## 1 akun1241       1 Ak = Akunt… I-1         õn    I/ME          2301 Galucio et …
    ## 2 arua1261       2 Ar = Aruá   I-1         õõt   I/ME          2301 Galucio et …
    ## 3 awet1244       3 Aw = Awetí  I-2         ito   I/ME          2301 Galucio et …
    ## 4 gavi1246       4 Gv = Gavião I-1         õõt   I/ME          2301 Galucio et …
    ## 5 juru1256       5 Ju = Jurúna I-1         una   I/ME          2301 Galucio et …
    ## 6 karo1305       6 Ka = Káro   I-1         ʔõn   I/ME          2301 Galucio et …
    ## # … with 6 more variables: Source_Page <dbl>, Comment <chr>, Latitude <dbl>,
    ## #   Longitude <dbl>, Macro_Region <chr>, Family <chr>

``` r
# Add Country with map.where
cldf_practice_tupian$Country <- NA
for (i in 1:nrow(cldf_practice_tupian)) {
        if (!is.na(cldf_practice_tupian$Latitude[i])) {
                cldf_practice_tupian$Country[i] <- map.where("world",
                        x = cldf_practice_tupian$Longitude[i],
                        y = cldf_practice_tupian$Latitude[i])
        }
}; rm(i)
cldf_practice_tupian[is.na(cldf_practice_tupian$Country),]
```

    ## # A tibble: 4 x 15
    ##   Glottocode    ID Language  Cognate_Set Form  Concept Concept_ID Source        
    ##   <chr>      <dbl> <chr>     <chr>       <chr> <chr>        <dbl> <chr>         
    ## 1 zoro1244      23 Zo = Zoró I-1         õõt   I/ME          2301 Galucio et al…
    ## 2 zoro1244      46 Zo = Zoró FISH-1      bolíp FISH           227 Galucio et al…
    ## 3 zoro1244      69 Zo = Zoró WOMAN-3     ßãzet WOMAN          962 Galucio et al…
    ## 4 zoro1244      92 Zo = Zoró LONG-2      -atóò LONG          1203 Galucio et al…
    ## # … with 7 more variables: Source_Page <dbl>, Comment <chr>, Latitude <dbl>,
    ## #   Longitude <dbl>, Macro_Region <chr>, Family <chr>, Country <chr>

``` r
# Manually add Zoró country data
cldf_practice_tupian[cldf_practice_tupian$Glottocode == "zoro1244",]$Country <-
        "Brazil"

# See results
cldf_practice_tupian
```

    ## # A tibble: 92 x 15
    ##    Glottocode    ID Language    Cognate_Set Form  Concept Concept_ID Source     
    ##    <chr>      <dbl> <chr>       <chr>       <chr> <chr>        <dbl> <chr>      
    ##  1 akun1241       1 Ak = Akunt… I-1         õn    I/ME          2301 Galucio et…
    ##  2 arua1261       2 Ar = Aruá   I-1         õõt   I/ME          2301 Galucio et…
    ##  3 awet1244       3 Aw = Awetí  I-2         ito   I/ME          2301 Galucio et…
    ##  4 gavi1246       4 Gv = Gavião I-1         õõt   I/ME          2301 Galucio et…
    ##  5 juru1256       5 Ju = Jurúna I-1         una   I/ME          2301 Galucio et…
    ##  6 karo1305       6 Ka = Káro   I-1         ʔõn   I/ME          2301 Galucio et…
    ##  7 kari1311       7 Kt = Karit… I-1         ĩn    I/ME          2301 Galucio et…
    ##  8 kuru1309       8 Ku = Kuruá… I-1         õn    I/ME          2301 Galucio et…
    ##  9 maku1278       9 Ma = Makur… I-1         õn    I/ME          2301 Galucio et…
    ## 10 saki1248      10 Me = Mekéns I-1         õn    I/ME          2301 Galucio et…
    ## # … with 82 more rows, and 7 more variables: Source_Page <dbl>, Comment <chr>,
    ## #   Latitude <dbl>, Longitude <dbl>, Macro_Region <chr>, Family <chr>,
    ## #   Country <chr>

``` r
# Copy original data if not already done so
if (!file.exists("cldf-practice-tupian-old.csv")) {
        file.copy("cldf-practice-tupian.csv", "cldf-practice-tupian-old.csv")
}

# Write results
write_csv(cldf_practice_tupian, "cldf-practice-tupian.csv")
```
