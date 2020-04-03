Untitled
================
Rebekah Jacob
4/2/2020

\`\`\`{r message=FALSE} library(tidyverse)

beer\_states \<-
readr::read\_csv(‘<https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv>’)
beer\_states

```` 


```{r}
around_mo_kegs<- beer_states %>%
  filter(state %in% c("MO", "IL", "AK", "KS", "IA","NE", "OK", "TN", "KY") & type=="Kegs and Barrels") %>%
  mutate(barrels_perK=barrels/1000)


around_mo_kegs %>%
  ggplot(aes(x = year, y = barrels_perK, color=state)) + 
    geom_line(size=1.5) +
      scale_x_continuous(breaks=c(2007:2019)) +
      theme_minimal() +
        scale_color_manual(values=c("#FF01FF", "#FF0180", "#FF0101", "#FF8001", "#FFFF01", "#80FF01", "#600060", "#800180", "#400040")) +
          ylab("Barrels (in thousands)") +
           ggtitle("Beer Kegs and Barrels Produced in 9 Contiguous States")
````

``` {r}
around_mo_kegs<- beer_states %>%
  filter(state %in% c("MO", "IL", "AK", "KS", "IA","NE", "OK", "TN", "KY") & type=="Bottles and Cans") %>%
  mutate(barrels_perK=barrels/1000)


around_mo_kegs %>%
  ggplot(aes(x = year, y = barrels_perK, color=state)) + 
    geom_line(size=1.5) +scale_x_continuous(breaks=c(2007:2019)) +
      theme_minimal() +
  scale_color_manual(values=c("#FF01FF", "#FF0180", "#FF0101", "#FF8001", "#FFFF01", "#80FF01", "#600060", "#800180", "#400040")) +
  ylab("Barrels (in thousands)") +
  ggtitle("Beer Bottles and Cans Produced in 9 Contiguous States")
```

``` {r}
around_mo_kegs<- beer_states %>%
  filter(state %in% c("MO", "IL", "AK", "KS", "IA","NE", "OK", "TN", "KY") & type=="On Premises") %>%
  mutate(barrels_perK=barrels/1000)


around_mo_kegs %>%
  ggplot(aes(x = year, y = barrels_perK, color=state)) + 
    geom_line(size=1.5) +scale_x_continuous(breaks=c(2007:2019)) +
      theme_minimal() +
  scale_color_manual(values=c("#FF01FF", "#FF0180", "#FF0101", "#FF8001", "#FFFF01", "#80FF01", "#600060", "#800180", "#400040")) +
  ylab("Barrels (in thousands)") +
  ggtitle("Beer Produced On Premises in 9 Contiguous States")
```

Okay, all states…. Hi Missouri…

``` {r}
all<- beer_states %>%
  filter(type=="Bottles and Cans" & state!="total" & !is.na(barrels)) %>%
  mutate(barrels_perK=barrels/1000)

all %>%
  ggplot(aes(x = year, y = barrels_perK, fill=state)) + 
    geom_line(size=0.5) +scale_x_continuous(breaks=c(2007:2019)) +
      theme_minimal() +
  ylab("Barrels (in thousands)") +
  ggtitle("Producion of Bottles and Cans, Missouri") +
  geom_line(data=subset(all, state == "MO"), colour="#80FF01", size=1.5)
```
