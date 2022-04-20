Painted Ladies
================
Grace Karbowski, Meredith Moore, Larz von Huene

4/20/2022


Import the Data

``` r
G1 <- read_xlsx("BIO_373_caterpillar_data.xlsx", sheet = "Group_1" , na= c("NA",""))
G2 <- read_xlsx("BIO_373_caterpillar_data.xlsx", sheet = "Group_2", na= c("NA",""))
G3 <- read_xlsx("BIO_373_caterpillar_data.xlsx", sheet = "Group_3", na= c("NA",""))
G4 <- read_xlsx("BIO_373_caterpillar_data.xlsx", sheet = "Group_4", na= c("NA",""))
G5 <- read_xlsx("BIO_373_caterpillar_data.xlsx", sheet = "Group_5", na= c("NA",""))
```

``` r
G5 <- G5 %>% 
  select(-...8)
```

``` r
catdata <- rbind(G1,G2,G3,G4,G5)

catdata<- catdata %>% 
  filter(!is.na(treatment)) %>%
  mutate(hatched = "2022-02-14", 
         survival= factor(survival),
         malformations = factor(malformations),
         treatment = factor(treatment)) %>% 
   filter(!treatment == "cold") %>% 
  mutate(treatment = recode(treatment, cold= "cold", hot= "heatwave"))
```

Survivability

``` r
survivalcounts<- catdata %>% 
  group_by(treatment,survival) %>% 
  summarise(number=n()) %>% 
  mutate(survived= ifelse(survival=="n", "no", "yes"))
```

    ## `summarise()` has grouped output by 'treatment'. You can override using the `.groups` argument.

``` r
survivalwider <- pivot_wider(survivalcounts, names_from = treatment, values_from = number)
```

``` r
chisq.test(survivalwider[c(3,4)],correct=F)
```
    ##  Pearson's Chi-squared test
    ## X-squared = 13.333, df = 1, p-value = 0.0002607

``` r
library(cowplot)
ggplot(data=survivalcounts, aes(x=treatment,y=number, fill=survived))+
  geom_col()+
    scale_fill_manual(labels = c("No", "Yes"), values = c("#EAC24A", "#A7CB92"))+
  xlab("")+
  ylab("Count of caterpillars")+
  labs(fill="Survived?")+
  theme_cowplot()
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- --> 

Development Time
``` r
dtcatdata <- catdata %>% 
  select(lid, treatment, hatched, pupa, emergence) %>% 
  mutate(hatch_day = yday(hatched),
         pupa_day = yday(pupa),
         em_day = yday(emergence),
         pdt = pupa_day - hatch_day,
         edt = em_day - hatch_day) %>% 
  select(lid, treatment, hatched, pdt, edt)

dtcatdata <- dtcatdata %>% 
  group_by(treatment) %>% 
  mutate (meanpdt = mean(pdt, na.rm = TRUE), 
          meanedt = mean(edt, na.rm = TRUE)) 
```

``` r
ggplot(dtcatdata, aes (x=treatment, y=edt, fill=treatment))+
  geom_boxplot()+
  geom_jitter()+
  scale_fill_manual(values = c("lightgoldenrod1","#ffb347"))+
  xlab("")+ 
  ylab("Development Time (days)")+
  theme(legend.position="none")+
  labs(fill="Treatment")+
  theme_cowplot()
```


![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
kruskal.test(edt~treatment, data = dtcatdata)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  edt by treatment
    ## Kruskal-Wallis chi-squared = 0.17853, df = 1, p-value = 0.6726
