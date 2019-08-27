Simpsons Guest Stars
================

# Load Data and Packages

``` r
library(tidyverse) #data management and ggplot
library(igraph) # bipartite networks
library(RColorBrewer)

simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")

simpsons
```

    ## # A tibble: 1,386 x 6
    ##    season number  production_code episode_title    guest_star   role       
    ##    <chr>  <chr>   <chr>           <chr>            <chr>        <chr>      
    ##  1 1      002–102 7G02            Bart the Genius  Marcia Wall~ Edna Kraba~
    ##  2 1      003–103 7G03            Homer's Odyssey  Sam McMurray Worker     
    ##  3 1      003–103 7G03            Homer's Odyssey  Marcia Wall~ Edna Kraba~
    ##  4 1      006–106 7G06            Moaning Lisa     Miriam Flynn Ms. Barr   
    ##  5 1      006–106 7G06            Moaning Lisa     Ron Taylor   Bleeding G~
    ##  6 1      007–107 7G09            The Call of the~ Albert Broo~ Cowboy Bob 
    ##  7 1      008–108 7G07            The Telltale He~ Marcia Wall~ Edna Kraba~
    ##  8 1      009–109 7G11            Life on the Fas~ Albert Broo~ Jacques    
    ##  9 1      010–110 7G10            Homer's Night O~ Sam McMurray Gulliver D~
    ## 10 1      011–111 7G13            The Crepes of W~ Christian C~ Gendarme O~
    ## # ... with 1,376 more rows

# Inspect data

  - *variable* class description
  - *season* integer Season of the show
  - *number* character Episode number
  - *production\_code* character Production code for the episode
  - *episode\_title* character Episode Title
  - *guest\_star* character Guest star (actual name)
  - *role* character Role in the show, either a character or themself

<!-- end list -->

``` r
#how many roles have each guest star played?
simpsons %>%
  group_by(guest_star) %>%
  summarize(length(role))
```

    ## # A tibble: 795 x 2
    ##    guest_star              `length(role)`
    ##    <chr>                            <int>
    ##  1 'N Sync                              1
    ##  2 "\"Weird Al\" Yankovic"              2
    ##  3 50 Cent                              1
    ##  4 Aaron Paul                           1
    ##  5 Adam Driver                          1
    ##  6 Adam Savage                          1
    ##  7 Adam Silver                          1
    ##  8 Adam West                            2
    ##  9 Aerosmith                            1
    ## 10 Al Roker                             1
    ## # ... with 785 more rows

``` r
# How many guest stars per season
simpsons %>%
  group_by(as.numeric(season)) %>% #make season numeric so in proper numerical order
  tally()
```

    ## Warning: NAs introduced by coercion

    ## # A tibble: 31 x 2
    ##    `as.numeric(season)`     n
    ##                   <dbl> <int>
    ##  1                    1    14
    ##  2                    2    31
    ##  3                    3    48
    ##  4                    4    44
    ##  5                    5    40
    ##  6                    6    41
    ##  7                    7    43
    ##  8                    8    38
    ##  9                    9    51
    ## 10                   10    55
    ## # ... with 21 more rows

# Grab data for SNA

*only picking last 5 seasons for ease of viewing connections*

``` r
#select data to graph
guests<- simpsons %>%
  filter(season %in% 26:30) %>%
  select(guest_star, number)

#create contigency matrix
guest_mat<- as.data.frame.matrix(table(guests))

#graph incidence
guests_inc1<- graph.incidence(guest_mat)

# creat bipartite networks
guests_pr1<- bipartite.projection(guests_inc1)

#select just project1 from bipartite (vertex names= guest star names)
collab_guests<- guests_pr1$proj1

#crude plot, will need some serious formatting
plot(collab_guests)
```

![](TT-8.27.19_Simpsons-Guest-Stars_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#filter for degree of less than 5
less_deg<- which(degree(collab_guests)<5)

#pull out those who do not have many connections
collab_guests_deg5<- delete.vertices(collab_guests, less_deg)

#formatting for better visual
plot(collab_guests_deg5, 
     vertex.label=V(collab_guests_deg5)$name, 
     vertex.size=degree(collab_guests_deg5)/3, 
     vertex.color="darkred", 
     layout=layout_with_kk, 
     main="Simpsons Guest Starring Network (Seasons 26 through 30)", 
     vertex.label.cex = .60)
```

![](TT-8.27.19_Simpsons-Guest-Stars_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
#look at total degree by guest
 prom.df <- data.frame(
            names= V(collab_guests_deg5)$name,
            deg.guests = degree(collab_guests_deg5))
 
 
prom.df<- prom.df %>%
   filter(deg.guests>8)
   
#bar charts_ top degree
p <- ggplot(prom.df, aes(x = reorder(names, +deg.guests), y = deg.guests)) +
         geom_bar(stat = "identity", fill="Salmon") + theme_minimal()
p+ coord_flip() + ggtitle("Prominent Guest Stars in the Simpsons' Guest Star Network\n (Seasons 26-30)") + labs(x="Guest Star Names", y= "Number of Episodes shared with other Guests (Degree)")
```

![](TT-8.27.19_Simpsons-Guest-Stars_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->
