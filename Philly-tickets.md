Philly Tickets
================

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.0     v purrr   0.3.2
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts -------------------------------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   violation_desc = col_character(),
    ##   issue_datetime = col_datetime(format = ""),
    ##   fine = col_double(),
    ##   issuing_agency = col_character(),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   zip_code = col_double()
    ## )

``` r
by_type<-tickets %>%
  group_by(violation_desc) %>%
  tally() %>%
  arrange(desc(n)) %>%
  mutate(percent=round(n/(sum(n))*100, 1))
by_type
```

    ## # A tibble: 95 x 3
    ##    violation_desc            n percent
    ##    <chr>                 <int>   <dbl>
    ##  1 METER EXPIRED CC     281060    22.3
    ##  2 METER EXPIRED        181329    14.4
    ##  3 OVER TIME LIMIT      156859    12.4
    ##  4 EXPIRED INSPECTION   138575    11  
    ##  5 STOP PROHIBITED CC   115898     9.2
    ##  6 STOPPING PROHIBITED   47395     3.8
    ##  7 PARKING PROHBITED     47232     3.7
    ##  8 PARKING PROHBITED CC  45082     3.6
    ##  9 OVER TIME LIMIT CC    24585     1.9
    ## 10 PASSENGR LOADNG ZONE  24359     1.9
    ## # ... with 85 more rows

``` r
sub_by_type<- by_type[1:15,]


library(RColorBrewer)
cols <- 15
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(cols)


p<-ggplot(data=sub_by_type, aes(reorder(x=violation_desc, percent), y=percent, fill=violation_desc)) +
  geom_bar(stat="identity", show.legend = FALSE) +
theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = mycolors) +
  labs(title = "Most common parking tickets in Philly") +
  xlab(NULL)
p
```

![](Philly-tickets_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->
