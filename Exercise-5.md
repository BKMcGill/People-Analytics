Exercise 5 BK
================

## Loading previous data

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(lubridate)
library(dplyr)
library(arrow)
```

    ## 
    ## Attaching package: 'arrow'
    ## 
    ## The following object is masked from 'package:lubridate':
    ## 
    ##     duration
    ## 
    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

``` r
applications <- read_feather("/Users/brettkwan/Desktop/McGill/People Analytics/app_data_starter.feather")
```

## Gender Distribution by TC

``` r
# Assuming your dataset is called 'applications' 

# Filter the dataset to include only the relevant columns ('tc' and 'gender')
filtered_data <- applications[, c("tc", "gender")]

# Group the data by 'tc' and 'gender', and calculate the count of each gender within each group
gender_distribution <- table(filtered_data$tc, filtered_data$gender)

# Print the gender distribution by 'tc'
print(gender_distribution)
```

    ##       
    ##        female   male
    ##   1600 218597 228909
    ##   1700 218350 434432
    ##   2100  81660 258830
    ##   2400  52620 221220

## Show as Ratio and Histogram

``` r
# Assuming your dataset is called 'applications' and you have loaded the 'ggplot2' package

# Filter the dataset to include only the relevant columns ('tc' and 'gender')
filtered_data <- applications[, c("tc", "gender")]

# Calculate the gender distribution as a ratio
gender_distribution <- prop.table(table(filtered_data$tc, filtered_data$gender), margin = 1)

# Print the gender distribution as a ratio
print(gender_distribution)
```

    ##       
    ##           female      male
    ##   1600 0.4884784 0.5115216
    ##   1700 0.3344915 0.6655085
    ##   2100 0.2398308 0.7601692
    ##   2400 0.1921560 0.8078440

``` r
# Convert the gender distribution to a data frame
gender_df <- as.data.frame(gender_distribution)

# Rename the columns for clarity
colnames(gender_df) <- c("tc", "gender", "ratio")

# Plot the stacked histogram
ggplot(gender_df, aes(x = tc, y = ratio, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(x = "tc", y = "Gender Ratio", title = "Gender Distribution by tc") +
  scale_fill_manual(values = c("pink", "blue"))  # Customize the colors for each gender
```

![](Exercise-5_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## TC Gender Average

``` r
tc_gender_avg <- applications %>%
  group_by(tc, gender) %>%
  filter(gender != "n/a") %>%
  summarize(average = n(), .groups = "drop") %>%
  group_by(tc) %>%
  mutate(percentage = average/sum(average) * 100, .groups = "drop")

print(tc_gender_avg)
```

    ## # A tibble: 8 × 5
    ## # Groups:   tc [4]
    ##      tc gender average percentage .groups
    ##   <dbl> <chr>    <int>      <dbl> <chr>  
    ## 1  1600 female  218597       48.8 drop   
    ## 2  1600 male    228909       51.2 drop   
    ## 3  1700 female  218350       33.4 drop   
    ## 4  1700 male    434432       66.6 drop   
    ## 5  2100 female   81660       24.0 drop   
    ## 6  2100 male    258830       76.0 drop   
    ## 7  2400 female   52620       19.2 drop   
    ## 8  2400 male    221220       80.8 drop

## Gender distribution by TC

``` r
distinct_examiner_ids <- applications %>%
  filter(!is.na(gender)) %>%
  group_by(tc, gender) %>%
  filter(!is.na(examiner_id)) %>%
  summarize(distinct_count = n_distinct(examiner_id), .groups = "drop")

print(distinct_examiner_ids)
```

    ## # A tibble: 8 × 3
    ##      tc gender distinct_count
    ##   <dbl> <chr>           <int>
    ## 1  1600 female            456
    ## 2  1600 male              497
    ## 3  1700 female            503
    ## 4  1700 male              859
    ## 5  2100 female            346
    ## 6  2100 male             1325
    ## 7  2400 female            238
    ## 8  2400 male              897

I remembered the example you gave in class about classroom composition
perspection (90/10 and 50/50) but was confused as it how to get there.

1600 - 456 Female and 497 Male Total 953 456 Squared + 497 Squared / 953
squared = 50% 1700 - 503 Female and 859 Male Total 1362 = 53% 2100 - 346
Female and 1325 Male Total 1671 = 67% 2400 - 238 Female and 897 male.
Total 1135 = 67%

## TC Gender Segregation

``` r
tc_gender_segregation <- tc_gender_avg %>%
  group_by(tc) %>%
  summarize(segregation_index = sum(percentage^2)/10000)
print(tc_gender_segregation)
```

    ## # A tibble: 4 × 2
    ##      tc segregation_index
    ##   <dbl>             <dbl>
    ## 1  1600             0.500
    ## 2  1700             0.555
    ## 3  2100             0.635
    ## 4  2400             0.690

## Visualize the gender representation and segregation using a bar plot.

``` r
ggplot(tc_gender_avg, aes(x = tc, y = percentage, fill = gender)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Technical Center", y = "Percentage", fill = "Gender") +
  ggtitle("Gender Representation by Technical Center") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](Exercise-5_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

\##Visualize the gender segregation index using a bar plot. We will plot
the segregation index for each TC, where higher values indicate higher
segregation.

``` r
ggplot(tc_gender_segregation, aes(x = tc, y = segregation_index)) +
  geom_bar(stat = "identity") +
  labs(x = "Technical Center", y = "Segregation Index") +
  ggtitle("Gender Segregation by Technical Center") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](Exercise-5_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Examiner Art Unit average gender representation

``` r
art_unit_gender_avg <- applications %>%
  drop_na(examiner_id) %>%
  group_by(examiner_art_unit, gender, .drop = TRUE) %>%
  summarize(average = n(), .groups = "drop") %>%
  group_by(examiner_art_unit, .drop = TRUE) %>%
  mutate(percentage = average/sum(average) * 100, .groups = "drop")
print(art_unit_gender_avg)
```

    ## # A tibble: 851 × 5
    ## # Groups:   examiner_art_unit [290]
    ##    examiner_art_unit gender average percentage .groups
    ##                <dbl> <chr>    <int>      <dbl> <chr>  
    ##  1              1600 female      22      21.8  drop   
    ##  2              1600 male        79      78.2  drop   
    ##  3              1609 female      22      40.7  drop   
    ##  4              1609 male        28      51.9  drop   
    ##  5              1609 <NA>         4       7.41 drop   
    ##  6              1611 female    4333      62.4  drop   
    ##  7              1611 male      2524      36.4  drop   
    ##  8              1611 <NA>        85       1.22 drop   
    ##  9              1612 female    3145      34.4  drop   
    ## 10              1612 male      4944      54.1  drop   
    ## # ℹ 841 more rows

## Examiner Art Gender Segregation

``` r
art_unit_gender_segregation <- art_unit_gender_avg %>%
  group_by(examiner_art_unit, .drop = TRUE) %>%
  summarize(segregation_index = sum(percentage^2)/10000)
```

## Visualize gender segregation index

``` r
ggplot(art_unit_gender_segregation, aes(x = examiner_art_unit, y = segregation_index)) +
  geom_bar(stat = "identity") +
  labs(x = "Examiner Art Unit", y = "Segregation Index") +
  ggtitle("Gender Segregation by Examiner Art Unit") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](Exercise-5_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Identifying top 25 AU scoring highest on segregation index

``` r
top_25_segregated_units <- art_unit_gender_segregation %>%
  arrange(desc(segregation_index)) %>%
  head(25)
print(top_25_segregated_units)
```

    ## # A tibble: 25 × 2
    ##    examiner_art_unit segregation_index
    ##                <dbl>             <dbl>
    ##  1              2403             1    
    ##  2              2100             0.957
    ##  3              2111             0.839
    ##  4              1736             0.830
    ##  5              1700             0.82 
    ##  6              2427             0.809
    ##  7              2491             0.795
    ##  8              2183             0.790
    ##  9              2415             0.782
    ## 10              2112             0.780
    ## # ℹ 15 more rows

``` r
top_25_least_segregated_units <- art_unit_gender_segregation %>%
  arrange(segregation_index) %>%
  head(25)
print(top_25_least_segregated_units)
```

    ## # A tibble: 25 × 2
    ##    examiner_art_unit segregation_index
    ##                <dbl>             <dbl>
    ##  1              2171             0.336
    ##  2              1622             0.336
    ##  3              1641             0.338
    ##  4              1632             0.338
    ##  5              2179             0.339
    ##  6              1672             0.341
    ##  7              2168             0.342
    ##  8              2162             0.345
    ##  9              1676             0.348
    ## 10              1709             0.349
    ## # ℹ 15 more rows

## Art Unit Gender Distribution to examine differences in individual perceptions

``` r
distinct_examiner_ids <- applications %>%
  filter(!is.na(gender)) %>%
  filter(examiner_art_unit %in% sample(unique(examiner_art_unit), 5)) %>%
  group_by(examiner_art_unit, gender) %>%
  filter(!is.na(examiner_id)) %>%
  summarize(count = n(), .groups = "drop") %>%
  group_by(examiner_art_unit) %>%
  mutate(percentage = count / sum(count) * 100)

print(distinct_examiner_ids)
```

    ## # A tibble: 10 × 4
    ## # Groups:   examiner_art_unit [5]
    ##    examiner_art_unit gender count percentage
    ##                <dbl> <chr>  <int>      <dbl>
    ##  1              1678 female  1963       76.9
    ##  2              1678 male     589       23.1
    ##  3              1729 female  2046       35.5
    ##  4              1729 male    3718       64.5
    ##  5              2138 female   636       25.2
    ##  6              2138 male    1890       74.8
    ##  7              2166 female  1429       37.4
    ##  8              2166 male    2392       62.6
    ##  9              2192 female  1497       25.9
    ## 10              2192 male    4286       74.1

         Versus Individ. Perception

1628 50.6% 1726 53.2% 1791 54.3% 2169 54.3% 2177 56.0 \## Observations

\###TC Gender Average I didn’t do distinct examinder id as I was
concerned about examiners working at different TCs over the time span.
Equality appears greatest in 1600, and gets worse as you ascend TC. As
noted in earlier exercises, it is notably worse in 2100 and 2400.

### Segregation Index

I used something called a segregation index, which calculates the
proportion of one group’s population that would need to move to achieve
an even distribution across all geographic areas. A value of 0 indicates
complete integration, while a value of 1 indicates complete
segregation.1600 was lowest with about .3499, while the others were
between .41 and .45. This once again shows that 1600 is more equal. This
is likely to it being a more established TC, and has been able to
undergo more gradual evolutions, in line with societal change, than the
other ’tc” which are newer, and may take time to catch up to societal
averages.

I used this index to identify the top 25 and bottom 25 scoring Art Units
according to the Segregation Index. The results match the TC gender
average, inso far as the top 25 integrated art units are concentrated in
1600, while TC 2100 dominates the most most segregated. There are some
outliers, notably 2171, 2168,2179, 2176 in the top 10 of most
integrated. They all belong to either 2140/2170 Graphical User Interface
and Document Processing or 2150/2160 Data Bases & File Management.
Additionally 4/5 of these Art Units are lead either by someone other
than a white male. This diverse leadership could influence the gender
distribution of the art unit.

Overall, Ive noticed that gender segregation/gender equality is most
pronounced at the art unit level. This is evidenced by the appearance of
art units in the 1600s appearing in the top 25 least integrated, and the
appearance of 2100 art units in the top 25 most integrated. It seems
like when baselines are set, it takes time to move towards equality, and
will be done gradually. it will increase at a fast pace when there is
more equal distribution. Other factors likely have to do with
gender-based leadership.

\###Individual Perception of Gender Looking at gender distribution from
individual perception, the differences in gender seem to be amplified in
1600 and 1700, but downplayed in 2100 and 2400. I am not so familiar
with this concept so will ask for further explanation in class.
