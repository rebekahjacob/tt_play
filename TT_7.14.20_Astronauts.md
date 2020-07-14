Tidy Tuesday Astronauts
================

``` r
library(tidyverse) #data mgmt
```

    ## -- Attaching packages --------------------------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.1     v dplyr   1.0.0
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## Warning: package 'ggplot2' was built under R version 3.6.3

    ## Warning: package 'tibble' was built under R version 3.6.3

    ## Warning: package 'tidyr' was built under R version 3.6.3

    ## Warning: package 'purrr' was built under R version 3.6.3

    ## Warning: package 'dplyr' was built under R version 3.6.3

    ## Warning: package 'forcats' was built under R version 3.6.3

    ## -- Conflicts ------------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(extrafont) #add different font
```

    ## Warning: package 'extrafont' was built under R version 3.6.2

    ## Registering fonts with R

``` r
library(ggpubr) #plot pic to plot background
library(jpeg) #read in background image

nauts<-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   name = col_character(),
    ##   original_name = col_character(),
    ##   sex = col_character(),
    ##   nationality = col_character(),
    ##   military_civilian = col_character(),
    ##   selection = col_character(),
    ##   occupation = col_character(),
    ##   mission_title = col_character(),
    ##   ascend_shuttle = col_character(),
    ##   in_orbit = col_character(),
    ##   descend_shuttle = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
nauts
```

    ## # A tibble: 1,277 x 24
    ##       id number nationwide_numb~ name  original_name sex   year_of_birth
    ##    <dbl>  <dbl>            <dbl> <chr> <chr>         <chr>         <dbl>
    ##  1     1      1                1 Gaga~ <U+0413><U+0410><U+0413><U+0410><U+0420><U+0418><U+041D> <U+042E><U+0440><U+0438><U+0439>~ male           1934
    ##  2     2      2                2 Tito~ <U+0422><U+0418><U+0422><U+041E><U+0412> <U+0413><U+0435><U+0440><U+043C><U+0430><U+043D>~ male           1935
    ##  3     3      3                1 Glen~ Glenn, John ~ male           1921
    ##  4     4      3                1 Glen~ Glenn, John ~ male           1921
    ##  5     5      4                2 Carp~ Carpenter, M~ male           1925
    ##  6     6      5                2 Niko~ <U+041D><U+0418><U+041A><U+041E><U+041B><U+0410><U+0415><U+0412> <U+0410><U+043D><U+0434>~ male           1929
    ##  7     7      5                2 Niko~ <U+041D><U+0418><U+041A><U+041E><U+041B><U+0410><U+0415><U+0412> <U+0410><U+043D><U+0434>~ male           1929
    ##  8     8      6                4 Popo~ <U+041F><U+041E><U+041F><U+041E><U+0412><U+0418><U+0427> <U+041F><U+0430><U+0432><U+0435>~ male           1930
    ##  9     9      6                4 Popo~ <U+041F><U+041E><U+041F><U+041E><U+0412><U+0418><U+0427> <U+041F><U+0430><U+0432><U+0435>~ male           1930
    ## 10    10      7                3 Schi~ Schirra, Wal~ male           1923
    ## # ... with 1,267 more rows, and 17 more variables: nationality <chr>,
    ## #   military_civilian <chr>, selection <chr>, year_of_selection <dbl>,
    ## #   mission_number <dbl>, total_number_of_missions <dbl>, occupation <chr>,
    ## #   year_of_mission <dbl>, mission_title <chr>, ascend_shuttle <chr>,
    ## #   in_orbit <chr>, descend_shuttle <chr>, hours_mission <dbl>,
    ## #   total_hrs_sum <dbl>, field21 <dbl>, eva_hrs_mission <dbl>,
    ## #   total_eva_hrs <dbl>

### number of astronauts in df

``` r
length(unique(nauts$name))
```

    ## [1] 564

is it the same as “number variable”… yes

``` r
length(unique(nauts$number))
```

    ## [1] 564

### Nationality

How many different nationalities

``` r
nauts %>%
  distinct(number, .keep_all=TRUE) %>% #keep each unique naut
  group_by(nationality) %>% 
  tally() %>%
  arrange(desc(n))
```

    ## # A tibble: 39 x 2
    ##    nationality        n
    ##    <chr>          <int>
    ##  1 U.S.             344
    ##  2 U.S.S.R/Russia   122
    ##  3 Japan             12
    ##  4 China             11
    ##  5 Germany           11
    ##  6 Canada            10
    ##  7 France            10
    ##  8 Italy              7
    ##  9 U.K./U.S.          3
    ## 10 Belgium            2
    ## # ... with 29 more rows

Looks like some could be added into US or Russia

``` r
nauts %>%
  filter(grepl("U.S.", nationality)) %>%
  group_by(nationality) %>%
  tally()
```

    ## # A tibble: 4 x 2
    ##   nationality         n
    ##   <chr>           <int>
    ## 1 U.K./U.S.           6
    ## 2 U.S.              854
    ## 3 U.S.S.R/Russia    273
    ## 4 U.S.S.R/Ukraine     1

Create 3 nationality groups (US, Russia, Other)

``` r
nauts<- nauts %>%
  mutate(nation_bi=case_when(
    grepl("U.S.$", nationality)~"US",
    grepl("U.S.S.R", nationality)~ "USSR/Russia",
    TRUE~"Other"))

nauts %>%
  group_by(nation_bi) %>%
  tally()
```

    ## # A tibble: 3 x 2
    ##   nation_bi       n
    ##   <chr>       <int>
    ## 1 Other         143
    ## 2 US            860
    ## 3 USSR/Russia   274

Average age at first mission by nation

``` r
age_first_df <-nauts %>%
  distinct(number, .keep_all=TRUE) %>% #keep each unique naut
  group_by(nation_bi) %>% 
  summarise(age=mean((year_of_mission-year_of_birth), na.rm=TRUE)) %>%
  mutate(time="First mission")
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
age_first_df 
```

    ## # A tibble: 3 x 3
    ##   nation_bi     age time         
    ##   <chr>       <dbl> <chr>        
    ## 1 Other        39.8 First mission
    ## 2 US           41.0 First mission
    ## 3 USSR/Russia  38.7 First mission

Average age at last mission

``` r
age_last_df<- nauts %>%
  arrange(desc(id, number)) %>%
  distinct(number, .keep_all=TRUE) %>% #keep each unique naut
  group_by(nation_bi) %>% 
  summarise(age=mean((year_of_mission-year_of_birth), na.rm=TRUE)) %>%
  mutate(time="Last mission")
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
age_last_df
```

    ## # A tibble: 3 x 3
    ##   nation_bi     age time        
    ##   <chr>       <dbl> <chr>       
    ## 1 Other        43.0 Last mission
    ## 2 US           45.5 Last mission
    ## 3 USSR/Russia  43.3 Last mission

Merge dfs for visual

``` r
naut_age_df<- rbind(age_first_df, age_last_df)

naut_age_df
```

    ## # A tibble: 6 x 3
    ##   nation_bi     age time         
    ##   <chr>       <dbl> <chr>        
    ## 1 Other        39.8 First mission
    ## 2 US           41.0 First mission
    ## 3 USSR/Russia  38.7 First mission
    ## 4 Other        43.0 Last mission 
    ## 5 US           45.5 Last mission 
    ## 6 USSR/Russia  43.3 Last mission

Theme

``` r
library(ggdark)
```

    ## Warning: package 'ggdark' was built under R version 3.6.3

``` r
my_theme<- dark_theme_gray(base_family = "Comic Sans MS", base_size = 14) + 
  theme(plot.title = element_text(family = "Comic Sans MS"),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank())
```

    ## Inverted geom defaults of fill and color/colour.
    ## To change them back, use invert_geom_defaults().

``` r
#pic
myimg<- readJPEG("C:\\Users\\rebekahjacob\\Box\\T drive data\\Tidy Tuesdays\\space_pic.jpg")
```

``` r
fig1 <- ggplot(data = naut_age_df, aes(x = nation_bi, y=age, fill = time)) +
  background_image(myimg) + #add image to background BEFORE geom_bar
            geom_bar(stat = "identity",  width = 0.7, position = "dodge", alpha=0.5) +
          geom_text(aes(label = round(age, 1)), size = 4 , hjust = 0.5 , vjust = -0.25, 
                    position = position_dodge(width = 0.7))+
          my_theme +
        scale_fill_manual(values = c("mediumpurple", "yellow"))+
        labs(title = "Average age at first and last mission by nationality",
             x = "", y = "Mean age in years",
             caption = "@RRJacob_STL")+
   ylim(c(0, 50)) 
  
fig1   
```

![](TT_7.14.20_Astronauts_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
