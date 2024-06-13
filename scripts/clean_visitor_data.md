Clean insect visitors dataset
================
B. Karina Montero
2024-06-13

### LOAD LIBRARIES

``` r
rm(list = ls())

# basics
library(tidyverse)
library(stringr)
library(vegan)
library(magrittr)
library(ggplot2)
library(ggpubr)

# diversity install.packages('iNEXT')
library("iNEXT")
library(hilldiv)

# rmd
library(details)
library(knitr)
library(kableExtra)
```

### WORKING DIRECTORIES

``` r
source("/home/kari/Dropbox/PollenUCR/01_scripts/UCR_workingDirectories.R")
```

### LOAD DATA

``` r
bichos <- read.csv(paste0(dataDir, "/Base_unificada.csv"))

# remove messy columns
bichos <- bichos[, -c(35:45)]

# Figure out sampling unit
bichos$Proj_site <- paste(bichos$Project, bichos$Fruitcrop_transect,
    sep = "_")

bichos$Sampling_unit <- paste(bichos$Project,
    bichos$Sampling_session, bichos$Fruitcrop_transect,
    bichos$Month, sep = "_")

bichos$Sampling_unit[1:3]
```

    ## [1] "Silvestres_1_Suria_June" "Silvestres_1_Suria_June"
    ## [3] "Silvestres_1_Suria_June"

``` r
check_sampling <- as.data.frame(table(bichos$Sampling_unit))
names(check_sampling) <- c("Sampling_unit", "n_visitors")

n_plants_sampling <- bichos %>%
    group_by(Sampling_unit) %>%
    summarise(n_plants = n_distinct(Tree_code))

check_sampling <- left_join(check_sampling, n_plants_sampling,
    by = "Sampling_unit")

head(check_sampling)
```

    ##                           Sampling_unit n_visitors n_plants
    ## 1   Cultivos_10_Malus domestica_January        124       29
    ## 2  Cultivos_10_Persea americana_January          9        2
    ## 3  Cultivos_10_Prunus domestica_January        140       23
    ## 4 Cultivos_11_Persea americana_February        153       36
    ## 5                 Cultivos_15_Rubus_May         30        5
    ## 6     Cultivos_17_Prunus domestica_June          1        1

``` r
bichos$Proj_season <- paste(bichos$Project, bichos$Season,
    sep = "_")

(sample_size_project <- bichos %>%
    group_by(Project) %>%
    summarise(n_sampling = n_distinct(Sampling_unit)))
```

    ## # A tibble: 2 × 2
    ##   Project    n_sampling
    ##   <chr>           <int>
    ## 1 Cultivos           33
    ## 2 Silvestres         34

``` r
(sample_size_fruitcrop <- bichos %>%
    group_by(Fruitcrop_transect) %>%
    summarise(n_sampling = n_distinct(Sampling_unit)))
```

    ## # A tibble: 6 × 2
    ##   Fruitcrop_transect n_sampling
    ##   <chr>                   <int>
    ## 1 Malus domestica             6
    ## 2 Persea americana           16
    ## 3 Prunus domestica            5
    ## 4 Rubus                       6
    ## 5 Suria                      20
    ## 6 Tajo                       14

``` r
(sample_size_sampling <- bichos %>%
    group_by(Proj_site) %>%
    summarise(n_sampling = n_distinct(Sampling_unit)))
```

    ## # A tibble: 6 × 2
    ##   Proj_site                 n_sampling
    ##   <chr>                          <int>
    ## 1 Cultivos_Malus domestica           6
    ## 2 Cultivos_Persea americana         16
    ## 3 Cultivos_Prunus domestica          5
    ## 4 Cultivos_Rubus                     6
    ## 5 Silvestres_Suria                  20
    ## 6 Silvestres_Tajo                   14

``` r
sum(sample_size_sampling$n_sampling)
```

    ## [1] 67

``` r
(sample_size_farm <- bichos %>%
    group_by(Proj_site, Farm_transect) %>%
    summarise(n_sampling = n_distinct(Sampling_unit)))
```

    ## # A tibble: 9 × 3
    ## # Groups:   Proj_site [6]
    ##   Proj_site                 Farm_transect n_sampling
    ##   <chr>                     <chr>              <int>
    ## 1 Cultivos_Malus domestica  Lauraceas              6
    ## 2 Cultivos_Malus domestica  Pablo                  1
    ## 3 Cultivos_Persea americana CasaMonge              1
    ## 4 Cultivos_Persea americana Lauraceas             16
    ## 5 Cultivos_Prunus domestica Lauraceas              5
    ## 6 Cultivos_Rubus            Jaboncillos            3
    ## 7 Cultivos_Rubus            SBosque                3
    ## 8 Silvestres_Suria          Suria                 20
    ## 9 Silvestres_Tajo           Tajo                  14

``` r
(sample_size_crop <- bichos %>%
    filter(Project == "Cultivos") %>%
    group_by(Plant_species) %>%
    summarise(n_sampling = n_distinct(Sampling_unit)))
```

    ## # A tibble: 4 × 2
    ##   Plant_species    n_sampling
    ##   <chr>                 <int>
    ## 1 Malus domestica           6
    ## 2 Persea americana         16
    ## 3 Prunus domestica          5
    ## 4 Rubus                     6

``` r
(sample_size_proj_season <- bichos %>%
    group_by(Proj_season) %>%
    summarise(n_sampling = n_distinct(Sampling_unit)))
```

    ## # A tibble: 4 × 2
    ##   Proj_season      n_sampling
    ##   <chr>                 <int>
    ## 1 Cultivos_dry              4
    ## 2 Cultivos_rainy           29
    ## 3 Silvestres_dry           11
    ## 4 Silvestres_rainy         23

``` r
# add rownames to dataframe
rownames(bichos) <- bichos$ID_code_unique

# check data
nrow(bichos)
```

    ## [1] 4112

``` r
table(bichos$Project)
```

    ## 
    ##   Cultivos Silvestres 
    ##       2806       1306

``` r
table(bichos$Fruitcrop_transect)
```

    ## 
    ##  Malus domestica Persea americana Prunus domestica            Rubus 
    ##              273             2092              166              275 
    ##            Suria             Tajo 
    ##              588              718

``` r
table(bichos$Collected_or_sighting)
```

    ## 
    ## Avistamiento      Colecta 
    ##          369         3743

``` r
table(bichos$Project, bichos$Collected_or_sighting)
```

    ##             
    ##              Avistamiento Colecta
    ##   Cultivos             11    2795
    ##   Silvestres          358     948

``` r
giras <- read.csv(paste0(dataDir, "/Datos_giras.csv"))

# Figure out sampling unit
giras$Proj_site <- paste(giras$Project, giras$Fruitcrop_transect,
    sep = "_")

giras$Sampling_unit <- paste(giras$Project, giras$Sampling_session,
    giras$Fruitcrop_transect, giras$Day, giras$Month,
    sep = "_")

(summary_farms <- giras %>%
    group_by(Farm_transect, Fruitcrop_transect,
        ) %>%
    summarise(n_sampling = n_distinct(Sampling_unit)) %>%
    data.frame)
```

    ##   Farm_transect Fruitcrop_transect n_sampling
    ## 1     CasaMonge   Persea americana          3
    ## 2   Jaboncillos              Rubus          3
    ## 3     Lauraceas    Malus domestica         10
    ## 4     Lauraceas   Persea americana         21
    ## 5     Lauraceas   Prunus domestica          6
    ## 6         Pablo    Malus domestica          1
    ## 7       SBosque              Rubus          5
    ## 8         Suria              Suria         22
    ## 9          Tajo               Tajo         15

``` r
(summary_giras <- giras %>%
    filter(Project == "Cultivos") %>%
    group_by(Month_num, Plant_species) %>%
    summarise(n_sampling = n_distinct(Sampling_unit)) %>%
    data.frame)
```

    ##    Month_num    Plant_species n_sampling
    ## 1          1  Malus domestica          2
    ## 2          1 Persea americana          2
    ## 3          1 Prunus domestica          2
    ## 4          2 Persea americana          2
    ## 5          5            Rubus          1
    ## 6          6 Prunus domestica          1
    ## 7          6            Rubus          1
    ## 8          7  Malus domestica          6
    ## 9          7 Persea americana          6
    ## 10         7 Prunus domestica          1
    ## 11         8  Malus domestica          2
    ## 12         8 Persea americana          9
    ## 13         8 Prunus domestica          2
    ## 14         8            Rubus          1
    ## 15         9 Persea americana          2
    ## 16        10            Rubus          2
    ## 17        11            Rubus          3

``` r
days_per_sampling <- giras %>%
    group_by(Project, Sampling_session) %>%
    summarise(days_per_sampling = n_distinct(Day)) %>%
    data.frame
(total_hours_project <- giras %>%
    group_by(Project, Sampling_unit) %>%
    summarise(hours_per_sampling = min(Sampling_hours)) %>%
    group_by(Project) %>%
    summarise(total_hours = sum(hours_per_sampling)) %>%
    data.frame)
```

    ##      Project total_hours
    ## 1   Cultivos      162.10
    ## 2 Silvestres      102.45

``` r
(total_hours_plant_transect <- giras %>%
    group_by(Fruitcrop_transect, Sampling_unit) %>%
    summarise(hours_per_sampling = min(Sampling_hours)) %>%
    group_by(Fruitcrop_transect) %>%
    summarise(total_hours = sum(hours_per_sampling)) %>%
    data.frame)
```

    ##   Fruitcrop_transect total_hours
    ## 1    Malus domestica       14.50
    ## 2   Persea americana      105.15
    ## 3   Prunus domestica       11.45
    ## 4              Rubus       31.00
    ## 5              Suria       54.45
    ## 6               Tajo       48.00

``` r
giras_to_merge <- giras %>%
    group_by(Project, Sampling_session, Fruitcrop_transect,
        Day) %>%
    summarise(giras_to_merge = n_distinct(paste(Sampling_session,
        Day))) %>%
    data.frame

giras_subset <- giras %>%
    select(Project, Sampling_session, Day, Fruitcrop_transect,
        Sampling_hours) %>%
    distinct()

giras_to_merge <- right_join(giras_to_merge, giras_subset,
    by = c("Project", "Sampling_session", "Fruitcrop_transect",
        "Day"))
str(giras_to_merge)
```

    ## 'data.frame':    82 obs. of  6 variables:
    ##  $ Project           : chr  "Cultivos" "Cultivos" "Cultivos" "Cultivos" ...
    ##  $ Sampling_session  : chr  "10" "10" "10" "10" ...
    ##  $ Fruitcrop_transect: chr  "Malus domestica" "Malus domestica" "Persea americana" "Persea americana" ...
    ##  $ Day               : int  18 20 18 19 18 19 10 11 16 19 ...
    ##  $ giras_to_merge    : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Sampling_hours    : num  0.5 3 3 0.15 0.5 6 6 6 4 0.3 ...

### CLEAN TAXONOMY

``` r
tax <- bichos %>%
    select(Order, Family, Genus, Species, Lowest_assignment)

tax[is.na(tax)] <- "NA"
str(tax)
```

    ## 'data.frame':    4112 obs. of  5 variables:
    ##  $ Order            : chr  "Hymenoptera" "Hymenoptera" "Hymenoptera" "Hymenoptera" ...
    ##  $ Family           : chr  "Apidae" "Apidae" "Ichneumonidae" "Apidae" ...
    ##  $ Genus            : chr  "Bombus" "Thygater" "NA" "Bombus" ...
    ##  $ Species          : chr  "Bombus pullatus" "NA" "NA" "Bombus ephippiatus " ...
    ##  $ Lowest_assignment: chr  "Bombus pullatus" "Thygater sp1" "NA" "Bombus ephippiatus " ...

``` r
table(tax$Order)
```

    ## 
    ##  Apodiformes   Coleoptera      Diptera    Hemiptera  Hymenoptera  Lepidoptera 
    ##            3          248         2403           41         1362           16 
    ##  Psocoptera  Thysanoptera  Trichoptera 
    ##            1           37            1

``` r
# num Families minus 1 (NA)
nrow(data.frame(table(tax$Family))) - 1
```

    ## [1] 93

``` r
# num Genera minus 1 (NA)
nrow(data.frame(table(tax$Genus))) - 1
```

    ## [1] 144

``` r
# Explore unsassigned
family_na <- tax %>%
    filter(Family == "NA")

# num of specimens unassigned to Family
# level
nrow(family_na)
```

    ## [1] 77

``` r
table(family_na$Order)
```

    ## 
    ##   Coleoptera      Diptera    Hemiptera  Hymenoptera  Lepidoptera  Psocoptera  
    ##            6           23            4            1            4            1 
    ## Thysanoptera  Trichoptera 
    ##           37            1

Remove individuals that did not get assigned to Family level and
hummingbirds.

``` r
tax_filter <- tax %>%
    filter(Family != "NA" & Order != "Apodiformes")
str(tax_filter)
```

    ## 'data.frame':    4032 obs. of  5 variables:
    ##  $ Order            : chr  "Hymenoptera" "Hymenoptera" "Hymenoptera" "Hymenoptera" ...
    ##  $ Family           : chr  "Apidae" "Apidae" "Ichneumonidae" "Apidae" ...
    ##  $ Genus            : chr  "Bombus" "Thygater" "NA" "Bombus" ...
    ##  $ Species          : chr  "Bombus pullatus" "NA" "NA" "Bombus ephippiatus " ...
    ##  $ Lowest_assignment: chr  "Bombus pullatus" "Thygater sp1" "NA" "Bombus ephippiatus " ...

``` r
tax_filter %>%
    summarise(n_Order = n_distinct(Order), n_Family = n_distinct(Family),
        n_Genus = n_distinct(Genus), n_Lowest = n_distinct(Lowest_assignment),
        n_species = n_distinct(Species))
```

    ##   n_Order n_Family n_Genus n_Lowest n_species
    ## 1       5       92     144      268        26

Explore unassigned Genera

``` r
# num of individuals unassigned to Genus
# level

genus_na <- tax %>%
    filter(Family != "NA", Genus == "NA")
nrow(genus_na)
```

    ## [1] 799

``` r
# Perc of individuals unnasigned to Genus
(799/4112) * 100
```

    ## [1] 19.43093

``` r
count_genus_na <- as.data.frame(table(genus_na$Family))
count_genus_na$Perc_na <- (count_genus_na$Freq/799) *
    100
count_genus_na$Perc_total <- (count_genus_na$Freq/4112) *
    100

head(count_genus_na)
```

    ##           Var1 Freq   Perc_na Perc_total
    ## 1  Agromyzidae    6 0.7509387 0.14591440
    ## 2 Anisopodidae    2 0.2503129 0.04863813
    ## 3 Anthocoridae    1 0.1251564 0.02431907
    ## 4    Aphididae   13 1.6270338 0.31614786
    ## 5       Apidae    1 0.1251564 0.02431907
    ## 6   Bibionidae    2 0.2503129 0.04863813

``` r
(top_unassigned <- count_genus_na %>%
    filter(Perc_na > 3))
```

    ##              Var1 Freq   Perc_na Perc_total
    ## 1 Ceratopogonidae   32  4.005006  0.7782101
    ## 2   Ichneumonidae   25  3.128911  0.6079767
    ## 3      Mycteridae   76  9.511890  1.8482490
    ## 4   Sarcophagidae  149 18.648310  3.6235409
    ## 5     Scatopsidae   61  7.634543  1.4834630
    ## 6       Sciaridae  131 16.395494  3.1857977

``` r
sum(top_unassigned$Perc_total)
```

    ## [1] 11.52724

Explore unassigned Lowest assignment (Species / Morphospecies)

``` r
# num of specimens unassigned to Genus level
lowest_na <- tax %>%
    filter(Family != "NA", Lowest_assignment ==
        "NA")
nrow(lowest_na)
```

    ## [1] 1064

``` r
# Perc of specimens unnasigned to Genus
(1064/4112) * 100
```

    ## [1] 25.87549

``` r
count_lowest_na <- as.data.frame(table(lowest_na$Genus))
count_lowest_na$Perc_na <- (count_lowest_na$Freq/1064) *
    100
count_lowest_na$Perc_total <- (count_lowest_na$Freq/4112) *
    100

head(count_lowest_na, 3)
```

    ##            Var1 Freq    Perc_na Perc_total
    ## 1    Allograpta  156 14.6616541 3.79377432
    ## 2 Argentinomyia   30  2.8195489 0.72957198
    ## 3        Bracon    2  0.1879699 0.04863813

``` r
(top_unassigned <- count_lowest_na %>%
    filter(Perc_na > 10))
```

    ##         Var1 Freq  Perc_na Perc_total
    ## 1 Allograpta  156 14.66165   3.793774
    ## 2         NA  799 75.09398  19.430934

``` r
sum(top_unassigned$Perc_total)
```

    ## [1] 23.22471

Replace NA with highest taxonomic assinment

``` r
for (i in 1:nrow(tax_filter)) {
    if (tax_filter[i, 2] == "NA") {
        order <- paste("Order_", tax_filter[i,
            1], sep = "")
        tax_filter[i, 2:4] <- order
    } else if (tax_filter[i, 3] == "NA") {
        family <- paste("Family_", tax_filter[i,
            2], sep = "")
        tax_filter[i, 3:4] <- family
    } else if ((tax_filter[i, 4] == "NA")) {
        tax_filter$Lowest_assignment[i] <- paste("Genus_",
            tax_filter[i, 2], sep = "")
    } else if (tax_filter[i, 4] == "NA") {
    }
}

head(tax_filter, 3)
```

    ##           Order        Family                Genus              Species
    ## S_1 Hymenoptera        Apidae               Bombus      Bombus pullatus
    ## S_2 Hymenoptera        Apidae             Thygater                   NA
    ## S_3 Hymenoptera Ichneumonidae Family_Ichneumonidae Family_Ichneumonidae
    ##     Lowest_assignment
    ## S_1   Bombus pullatus
    ## S_2      Genus_Apidae
    ## S_3                NA

``` r
tail(tax_filter, 3)
```

    ##             Order        Family                Genus              Species
    ## C_896 Hymenoptera Ichneumonidae Family_Ichneumonidae Family_Ichneumonidae
    ## C_897 Hymenoptera    Braconidae    Family_Braconidae    Family_Braconidae
    ## C_898     Diptera      Muscidae         Neodexiopsis                   NA
    ##       Lowest_assignment
    ## C_896                NA
    ## C_897                NA
    ## C_898    Genus_Muscidae

``` r
# merge with df
bichos_long <- bichos %>%
    select(Project, Sampling_session, Sampling_unit,
        Day, Month, Year, Season, Proj_site, Proj_season,
        Farm_transect, Fruitcrop_transect, Plant_species)

head(rownames(bichos_long))
```

    ## [1] "S_1" "S_2" "S_3" "S_4" "S_5" "S_6"

``` r
head(rownames(tax_filter))
```

    ## [1] "S_1" "S_2" "S_3" "S_4" "S_5" "S_6"

``` r
bichos_long <- merge(bichos_long, tax_filter,
    by = 0, all.x = FALSE)
names(bichos_long)
```

    ##  [1] "Row.names"          "Project"            "Sampling_session"  
    ##  [4] "Sampling_unit"      "Day"                "Month"             
    ##  [7] "Year"               "Season"             "Proj_site"         
    ## [10] "Proj_season"        "Farm_transect"      "Fruitcrop_transect"
    ## [13] "Plant_species"      "Order"              "Family"            
    ## [16] "Genus"              "Species"            "Lowest_assignment"

``` r
sampling_lowest <- bichos_long %>%
    group_by(Sampling_unit) %>%
    summarise(count_lowest = n_distinct(Lowest_assignment)) %>%
    data.frame

head(sampling_lowest)
```

    ##                           Sampling_unit count_lowest
    ## 1   Cultivos_10_Malus domestica_January           10
    ## 2  Cultivos_10_Persea americana_January            3
    ## 3  Cultivos_10_Prunus domestica_January            9
    ## 4 Cultivos_11_Persea americana_February           16
    ## 5                 Cultivos_15_Rubus_May            5
    ## 6     Cultivos_17_Prunus domestica_June            1

``` r
head(check_sampling)
```

    ##                           Sampling_unit n_visitors n_plants
    ## 1   Cultivos_10_Malus domestica_January        124       29
    ## 2  Cultivos_10_Persea americana_January          9        2
    ## 3  Cultivos_10_Prunus domestica_January        140       23
    ## 4 Cultivos_11_Persea americana_February        153       36
    ## 5                 Cultivos_15_Rubus_May         30        5
    ## 6     Cultivos_17_Prunus domestica_June          1        1

``` r
check_sampling$Sampling_unit <- as.character(check_sampling$Sampling_unit)

check_sampling_df <- left_join(sampling_lowest,
    check_sampling, by = "Sampling_unit")
check_sampling_df <- check_sampling_df[order(check_sampling_df$n_plants,
    check_sampling_df$n_visitors), ]
head(check_sampling_df, 12)
```

    ##                           Sampling_unit count_lowest n_visitors n_plants
    ## 6     Cultivos_17_Prunus domestica_June            1          1        1
    ## 25     Cultivos_2_Prunus domestica_July            1          1        1
    ## 31   Cultivos_4_Prunus domestica_August            1          1        1
    ## 50           Silvestres_20_Suria_August            1          1        1
    ## 34         Silvestres_11_Suria_February            3          3        1
    ## 49               Silvestres_1_Tajo_June            4          6        2
    ## 2  Cultivos_10_Persea americana_January            3          9        2
    ## 29    Cultivos_4_Malus domestica_August            6         10        2
    ## 56              Silvestres_2_Suria_July            2          5        3
    ## 58         Silvestres_5_Suria_September            4          5        3
    ## 14     Cultivos_19_Malus domestica_July            7         20        3
    ## 19   Cultivos_20_Malus domestica_August            7         34        3

``` r
# write.csv(check_sampling_df,
# paste0(dataDir, '/check_sampling_df.csv'))
```

### MERGE AND FILTER

Merge insect data with sampling session info.

``` r
# add counts to df
bichos_long <- left_join(bichos_long, check_sampling_df,
    by = "Sampling_unit")

# add sampling hours to df
bichos_long <- left_join(bichos_long, giras_to_merge,
    by = c("Project", "Sampling_session", "Day",
        "Fruitcrop_transect"), keep = FALSE)

Sampling_hours_total <- bichos_long %>%
    group_by(Project, Sampling_unit) %>%
    distinct(Sampling_hours) %>%
    arrange(Sampling_unit) %>%
    mutate(Sampling_hours_total = sum(Sampling_hours)) %>%
    select(-Sampling_hours) %>%
    distinct(Sampling_unit, Sampling_hours_total) %>%
    data.frame


bichos_long <- right_join(bichos_long, Sampling_hours_total,
    by = c("Sampling_unit", "Project"))

check_sampling_df_2 <- bichos_long %>%
    distinct(Sampling_unit, count_lowest, n_visitors,
        n_plants, Sampling_hours_total) %>%
    arrange(Sampling_hours_total)
head(check_sampling_df_2, 15)
```

    ##                         Sampling_unit count_lowest n_visitors n_plants
    ## 1  Cultivos_4_Prunus domestica_August            1          1        1
    ## 2          Silvestres_20_Suria_August            1          1        1
    ## 3   Cultivos_17_Prunus domestica_June            1          1        1
    ## 4   Cultivos_4_Malus domestica_August            6         10        2
    ## 5    Cultivos_2_Prunus domestica_July            1          1        1
    ## 6             Silvestres_2_Suria_July            2          5        3
    ## 7        Silvestres_11_Suria_February            3          3        1
    ## 8             Silvestres_1_Suria_June            8         11        4
    ## 9              Silvestres_1_Tajo_June            4          6        2
    ## 10   Cultivos_19_Malus domestica_July            7         20        3
    ## 11   Cultivos_18_Malus domestica_July            3         11        6
    ## 12          Silvestres_3_Suria_August            9         30        8
    ## 13        Silvestres_8_Suria_December            9         44       17
    ## 14            Silvestres_17_Tajo_June           11         46       12
    ## 15           Silvestres_17_Suria_June            9         27        7
    ##    Sampling_hours_total
    ## 1                  0.15
    ## 2                  0.20
    ## 3                  0.30
    ## 4                  0.50
    ## 5                  0.50
    ## 6                  0.50
    ## 7                  0.50
    ## 8                  1.00
    ## 9                  1.00
    ## 10                 1.50
    ## 11                 2.00
    ## 12                 2.00
    ## 13                 2.00
    ## 14                 2.00
    ## 15                 2.00

``` r
write.csv(check_sampling_df_2, paste0(dataDir,
    "/check_sampling_fadf.csv"))
```

Remove sampling sessions with very low sampling effort (\< 2hours)

``` r
# n sampling units before filtering
bichos_long %>%
    group_by(Fruitcrop_transect) %>%
    summarise(n_distinct(Sampling_unit))
```

    ## # A tibble: 6 × 2
    ##   Fruitcrop_transect `n_distinct(Sampling_unit)`
    ##   <chr>                                    <int>
    ## 1 Malus domestica                              6
    ## 2 Persea americana                            16
    ## 3 Prunus domestica                             5
    ## 4 Rubus                                        6
    ## 5 Suria                                       20
    ## 6 Tajo                                        14

``` r
# remove sampling units with very low
# sampling effort
bichos_long_filter <- bichos_long %>%
    filter(Sampling_hours_total >= 2)

# n sampling units after filtering
bichos_long_filter %>%
    group_by(Fruitcrop_transect) %>%
    summarise(n_distinct(Sampling_unit))
```

    ## # A tibble: 6 × 2
    ##   Fruitcrop_transect `n_distinct(Sampling_unit)`
    ##   <chr>                                    <int>
    ## 1 Malus domestica                              4
    ## 2 Persea americana                            16
    ## 3 Prunus domestica                             2
    ## 4 Rubus                                        6
    ## 5 Suria                                       16
    ## 6 Tajo                                        13

``` r
bichos_long_filter %>%
    group_by(Project) %>%
    summarise(n_distinct(Sampling_unit))
```

    ## # A tibble: 2 × 2
    ##   Project    `n_distinct(Sampling_unit)`
    ##   <chr>                            <int>
    ## 1 Cultivos                            28
    ## 2 Silvestres                          29

``` r
(total_hours_project <- bichos_long_filter %>%
    group_by(Project, Sampling_unit) %>%
    distinct(Sampling_hours_total) %>%
    group_by(Project) %>%
    summarise(total_hours = sum(Sampling_hours_total)) %>%
    data.frame)
```

    ##      Project total_hours
    ## 1   Cultivos      136.65
    ## 2 Silvestres       97.25

``` r
136.65 + 97.25
```

    ## [1] 233.9

Supplementary Tables

``` r
y <- c("Persea americana", "Rubus", "Malus domestica",
    "Prunus domestica", "Suria", "Tajo")

oldnames <- c("August", "July", "September", "December",
    "June", "November", "October", "January",
    "February", "May", "April", "March")
newnames <- c("Aug", "Jul", "Sep", "Dec", "Jun",
    "Nov", "Oct", "Jan", "Feb", "May", "Apr",
    "Mar")


(sample_size_month <- bichos_long_filter %>%
    group_by(Year, Fruitcrop_transect, Month) %>%
    summarise(n_sampling = n_distinct(Sampling_unit)) %>%
    pivot_wider(names_from = Month, values_from = n_sampling,
        values_fill = 0) %>%
    arrange(match(Fruitcrop_transect, y)) %>%
    data.frame %>%
    rename_at(all_of(oldnames), ~newnames) %>%
    select(Year, Fruitcrop_transect, Jan, Feb,
        Mar, Apr, May, Jun, Jul, Aug, Sep, Oct,
        Nov, Dec))
```

    ##    Year Fruitcrop_transect Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
    ## 1  2021   Persea americana   0   0   0   0   0   0   0   3   2   0   0   0
    ## 2  2022   Persea americana   1   1   0   0   0   0   5   4   0   0   0   0
    ## 3  2022              Rubus   0   0   0   0   1   1   0   1   0   2   1   0
    ## 4  2021    Malus domestica   0   0   0   0   0   0   1   0   0   0   0   0
    ## 5  2022    Malus domestica   1   0   0   0   0   0   1   1   0   0   0   0
    ## 6  2021   Prunus domestica   0   0   0   0   0   0   0   1   0   0   0   0
    ## 7  2022   Prunus domestica   1   0   0   0   0   0   0   0   0   0   0   0
    ## 8  2021              Suria   0   0   0   0   0   0   0   1   1   1   1   1
    ## 9  2022              Suria   1   1   1   1   1   1   2   1   1   1   0   0
    ## 10 2021               Tajo   0   0   0   0   0   0   0   0   1   1   1   1
    ## 11 2022               Tajo   1   1   1   1   1   1   1   1   1   0   0   0

``` r
silvestres_info <- read.csv(paste0(dataDir, "/silvestres_info.csv"))
head(silvestres_info)
```

    ##      Plant_Family        Plant_species Fruitcrop_ruderal
    ## 1      Asteraceae Ageratina bustamenta                no
    ## 2      Asteraceae Ageratum conyzoides                 no
    ## 3 Caryophyllaceae  Arenaria lanuginosa               yes
    ## 4      Asteraceae        Bidens pilosa               yes
    ## 5      Asteraceae       Bidens reptans                no
    ## 6      Solanaceae   Brugmansia arborea               yes

``` r
bichos_long_filter <- left_join(bichos_long_filter,
    silvestres_info, by = "Plant_species")
names(bichos_long_filter)
```

    ##  [1] "Row.names"            "Project"              "Sampling_session"    
    ##  [4] "Sampling_unit"        "Day"                  "Month"               
    ##  [7] "Year"                 "Season"               "Proj_site"           
    ## [10] "Proj_season"          "Farm_transect"        "Fruitcrop_transect"  
    ## [13] "Plant_species"        "Order"                "Family"              
    ## [16] "Genus"                "Species"              "Lowest_assignment"   
    ## [19] "count_lowest"         "n_visitors"           "n_plants"            
    ## [22] "giras_to_merge"       "Sampling_hours"       "Sampling_hours_total"
    ## [25] "Plant_Family"         "Fruitcrop_ruderal"

``` r
flowering_transects <- bichos_long_filter %>%
    filter(Project == "Silvestres") %>%
    group_by(Plant_Family, Plant_species, Month) %>%
    summarise(n_sampling = n_distinct(Sampling_unit)) %>%
    pivot_wider(names_from = Month, values_from = n_sampling,
        values_fill = 0) %>%
    data.frame %>%
    rename_at(all_of(oldnames), ~newnames) %>%
    select(Plant_Family, Plant_species, Jan, Feb,
        Mar, Apr, May, Jun, Jul, Aug, Sep, Oct,
        Nov, Dec)
flowering_transects <- left_join(flowering_transects,
    silvestres_info, by = c("Plant_Family", "Plant_species"))

write.csv(flowering_transects, paste0(dataDir,
    "/flowering_transects.csv"), row.names = FALSE)

names(bichos_long_filter)
```

    ##  [1] "Row.names"            "Project"              "Sampling_session"    
    ##  [4] "Sampling_unit"        "Day"                  "Month"               
    ##  [7] "Year"                 "Season"               "Proj_site"           
    ## [10] "Proj_season"          "Farm_transect"        "Fruitcrop_transect"  
    ## [13] "Plant_species"        "Order"                "Family"              
    ## [16] "Genus"                "Species"              "Lowest_assignment"   
    ## [19] "count_lowest"         "n_visitors"           "n_plants"            
    ## [22] "giras_to_merge"       "Sampling_hours"       "Sampling_hours_total"
    ## [25] "Plant_Family"         "Fruitcrop_ruderal"

``` r
table(bichos_long_filter$Order)
```

    ## 
    ##  Coleoptera     Diptera   Hemiptera Hymenoptera Lepidoptera 
    ##         242        2363          37        1319          12

``` r
tax_summary <- bichos_long_filter %>%
    group_by(Order, Family, Genus, Lowest_assignment) %>%
    mutate(Genus = ifelse(str_starts(Genus, "Family_"),
        NA, Genus), Lowest_assignment = ifelse(str_starts(Lowest_assignment,
        "Family_"), NA, Lowest_assignment)) %>%
    group_by(Order, Family, Genus) %>%
    summarise(count_lowest = n_distinct(Lowest_assignment,
        na.rm = TRUE), abundance = n()) %>%
    data.frame

table(tax_summary$Order)
```

    ## 
    ##  Coleoptera     Diptera   Hemiptera Hymenoptera Lepidoptera 
    ##          14         134          10          49           7

``` r
tax_summary_coleoptera <- tax_summary %>%
    filter(Order == "Coleoptera")

tax_summary_diptera <- tax_summary %>%
    filter(Order == "Diptera")

tax_summary_hemiptera <- tax_summary %>%
    filter(Order == "Hemiptera")

tax_summary_hymenoptera <- tax_summary %>%
    filter(Order == "Hymenoptera")

tax_summary_lepidoptera <- tax_summary %>%
    filter(Order == "Lepidoptera")

write.csv(tax_summary_coleoptera, paste0(outputDir,
    "/list_coleoptera.csv"), row.names = FALSE)
write.csv(tax_summary_diptera, paste0(outputDir,
    "/list_diptera.csv"), row.names = FALSE)
write.csv(tax_summary_hemiptera, paste0(outputDir,
    "/list_hemiptera.csv"), row.names = FALSE)
write.csv(tax_summary_hymenoptera, paste0(outputDir,
    "/list_hymenoptera.csv"), row.names = FALSE)
write.csv(tax_summary_lepidoptera, paste0(outputDir,
    "/list_lepidoptera.csv"), row.names = FALSE)
```

### OUTPUT FILES

Save clean data files for diversity analyses

``` r
write.csv(bichos_long, paste0(dataDir, "/bichos_long.csv"))
write.csv(bichos_long_filter, paste0(dataDir,
    "/bichos_long_filter.csv"))
```

## Session Info

``` r
sessioninfo::session_info() %>%
    details::details(summary = "Current session info",
        open = FALSE)
```

<details closed>
<summary>
<span title="Click to Expand"> Current session info </span>
</summary>

``` r

─ Session info ───────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.4.0 (2024-04-24)
 os       Linux Mint 21.3
 system   x86_64, linux-gnu
 ui       X11
 language en_US
 collate  en_IN.UTF-8
 ctype    en_IN.UTF-8
 tz       Europe/Berlin
 date     2024-06-13
 pandoc   3.1.13 @ /usr/local/bin/ (via rmarkdown)

─ Packages ───────────────────────────────────────────────────────────────────
 package           * version    date (UTC) lib source
 abind               1.4-5      2016-07-21 [1] CRAN (R 4.4.0)
 ape                 5.8        2024-04-11 [1] CRAN (R 4.4.0)
 backports           1.4.1      2021-12-13 [1] CRAN (R 4.4.0)
 base64enc           0.1-3      2015-07-28 [1] CRAN (R 4.4.0)
 broom               1.0.6      2024-05-17 [1] CRAN (R 4.4.0)
 car                 3.1-2      2023-03-30 [1] CRAN (R 4.4.0)
 carData             3.0-5      2022-01-06 [1] CRAN (R 4.4.0)
 checkmate           2.3.1      2023-12-04 [1] CRAN (R 4.4.0)
 cli                 3.6.2      2023-12-11 [1] CRAN (R 4.4.0)
 clipr               0.8.0      2022-02-22 [1] CRAN (R 4.4.0)
 cluster             2.1.6      2023-12-01 [1] CRAN (R 4.4.0)
 clusterGeneration   1.3.8      2023-08-16 [1] CRAN (R 4.4.0)
 coda                0.19-4.1   2024-01-31 [1] CRAN (R 4.4.0)
 codetools           0.2-20     2024-03-31 [1] CRAN (R 4.4.0)
 colorspace          2.1-0      2023-01-23 [1] CRAN (R 4.4.0)
 combinat            0.0-8      2012-10-29 [1] CRAN (R 4.4.0)
 corpcor             1.6.10     2021-09-16 [1] CRAN (R 4.4.0)
 data.table          1.15.4     2024-03-30 [1] CRAN (R 4.4.0)
 desc                1.4.3      2023-12-10 [1] CRAN (R 4.4.0)
 deSolve             1.40       2023-11-27 [1] CRAN (R 4.4.0)
 details           * 0.3.0      2022-03-27 [1] CRAN (R 4.4.0)
 digest              0.6.35     2024-03-11 [1] CRAN (R 4.4.0)
 doParallel          1.0.17     2022-02-07 [1] CRAN (R 4.4.0)
 dplyr             * 1.1.4      2023-11-17 [1] CRAN (R 4.4.0)
 evaluate            0.23       2023-11-01 [1] CRAN (R 4.4.0)
 expm                0.999-9    2024-01-11 [1] CRAN (R 4.4.0)
 fansi               1.0.6      2023-12-08 [1] CRAN (R 4.4.0)
 fastmap             1.2.0      2024-05-15 [1] CRAN (R 4.4.0)
 fastmatch           1.1-4      2023-08-18 [1] CRAN (R 4.4.0)
 fdrtool             1.2.17     2021-11-13 [1] CRAN (R 4.4.0)
 forcats           * 1.0.0      2023-01-29 [1] CRAN (R 4.4.0)
 foreach             1.5.2      2022-02-02 [1] CRAN (R 4.4.0)
 foreign             0.8-86     2023-11-28 [1] CRAN (R 4.4.0)
 formatR             1.14       2023-01-17 [1] CRAN (R 4.4.0)
 Formula             1.2-5      2023-02-24 [1] CRAN (R 4.4.0)
 FSA                 0.9.5      2023-08-26 [1] CRAN (R 4.4.0)
 geiger              2.0.11     2023-04-03 [1] CRAN (R 4.4.0)
 generics            0.1.3      2022-07-05 [1] CRAN (R 4.4.0)
 ggplot2           * 3.5.1      2024-04-23 [1] CRAN (R 4.4.0)
 ggpubr            * 0.6.0      2023-02-10 [1] CRAN (R 4.4.0)
 ggrepel             0.9.5      2024-01-10 [1] CRAN (R 4.4.0)
 ggsignif            0.6.4      2022-10-13 [1] CRAN (R 4.4.0)
 glasso              1.11       2019-10-01 [1] CRAN (R 4.4.0)
 glue                1.7.0      2024-01-09 [1] CRAN (R 4.4.0)
 gridExtra           2.3        2017-09-09 [1] CRAN (R 4.4.0)
 gtable              0.3.5      2024-04-22 [1] CRAN (R 4.4.0)
 gtools              3.9.5      2023-11-20 [1] CRAN (R 4.4.0)
 hilldiv           * 1.5.3      2024-05-16 [1] Github (anttonalberdi/hilldiv@551c284)
 Hmisc               5.1-2      2024-03-11 [1] CRAN (R 4.4.0)
 hms                 1.1.3      2023-03-21 [1] CRAN (R 4.4.0)
 htmlTable           2.4.2      2023-10-29 [1] CRAN (R 4.4.0)
 htmltools           0.5.8.1    2024-04-04 [1] CRAN (R 4.4.0)
 htmlwidgets         1.6.4      2023-12-06 [1] CRAN (R 4.4.0)
 httr                1.4.7      2023-08-15 [1] CRAN (R 4.4.0)
 igraph              2.0.3.9016 2024-05-16 [1] https://igraph.r-universe.dev (R 4.4.0)
 iNEXT             * 3.0.1      2024-03-24 [1] CRAN (R 4.4.0)
 iterators           1.0.14     2022-02-05 [1] CRAN (R 4.4.0)
 jpeg                0.1-10     2022-11-29 [1] CRAN (R 4.4.0)
 kableExtra        * 1.4.0      2024-01-24 [1] CRAN (R 4.4.0)
 knitr             * 1.46       2024-04-06 [1] CRAN (R 4.4.0)
 lattice           * 0.22-6     2024-03-20 [1] CRAN (R 4.4.0)
 lavaan              0.6-17     2023-12-20 [1] CRAN (R 4.4.0)
 lifecycle           1.0.4      2023-11-07 [1] CRAN (R 4.4.0)
 lubridate         * 1.9.3      2023-09-27 [1] CRAN (R 4.4.0)
 magrittr          * 2.0.3      2022-03-30 [1] CRAN (R 4.4.0)
 maps                3.4.2      2023-12-15 [1] CRAN (R 4.4.0)
 MASS                7.3-60.2   2024-04-26 [1] CRAN (R 4.4.0)
 Matrix              1.7-0      2024-04-26 [1] CRAN (R 4.4.0)
 mgcv                1.9-1      2023-12-21 [1] CRAN (R 4.4.0)
 mnormt              2.1.1      2022-09-26 [1] CRAN (R 4.4.0)
 munsell             0.5.1      2024-04-01 [1] CRAN (R 4.4.0)
 mvtnorm             1.2-4      2023-11-27 [1] CRAN (R 4.4.0)
 nlme                3.1-164    2023-11-27 [1] CRAN (R 4.4.0)
 nnet                7.3-19     2023-05-03 [1] CRAN (R 4.4.0)
 numDeriv            2016.8-1.1 2019-06-06 [1] CRAN (R 4.4.0)
 optimParallel       1.0-2      2021-02-11 [1] CRAN (R 4.4.0)
 pbapply             1.7-2      2023-06-27 [1] CRAN (R 4.4.0)
 pbivnorm            0.6.0      2015-01-23 [1] CRAN (R 4.4.0)
 permute           * 0.9-7      2022-01-27 [1] CRAN (R 4.4.0)
 phangorn            2.11.1     2023-01-23 [1] CRAN (R 4.4.0)
 phytools            2.1-1      2024-01-09 [1] CRAN (R 4.4.0)
 pillar              1.9.0      2023-03-22 [1] CRAN (R 4.4.0)
 pkgconfig           2.0.3      2019-09-22 [1] CRAN (R 4.4.0)
 plyr                1.8.9      2023-10-02 [1] CRAN (R 4.4.0)
 png                 0.1-8      2022-11-29 [1] CRAN (R 4.4.0)
 psych               2.4.3      2024-03-18 [1] CRAN (R 4.4.0)
 purrr             * 1.0.2      2023-08-10 [1] CRAN (R 4.4.0)
 qgraph              1.9.8      2023-11-03 [1] CRAN (R 4.4.0)
 quadprog            1.5-8      2019-11-20 [1] CRAN (R 4.4.0)
 R6                  2.5.1      2021-08-19 [1] CRAN (R 4.4.0)
 RColorBrewer        1.1-3      2022-04-03 [1] CRAN (R 4.4.0)
 Rcpp                1.0.12     2024-01-09 [1] CRAN (R 4.4.0)
 readr             * 2.1.5      2024-01-10 [1] CRAN (R 4.4.0)
 reshape2            1.4.4      2020-04-09 [1] CRAN (R 4.4.0)
 rlang               1.1.4      2024-06-04 [1] CRAN (R 4.4.0)
 rmarkdown           2.27       2024-05-17 [1] CRAN (R 4.4.0)
 rpart               4.1.23     2023-12-05 [1] CRAN (R 4.4.0)
 rstatix             0.7.2      2023-02-01 [1] CRAN (R 4.4.0)
 rstudioapi          0.16.0     2024-03-24 [1] CRAN (R 4.4.0)
 scales              1.3.0      2023-11-28 [1] CRAN (R 4.4.0)
 scatterplot3d       0.3-44     2023-05-05 [1] CRAN (R 4.4.0)
 sessioninfo         1.2.2      2021-12-06 [1] CRAN (R 4.4.0)
 stringi             1.8.4      2024-05-06 [1] CRAN (R 4.4.0)
 stringr           * 1.5.1      2023-11-14 [1] CRAN (R 4.4.0)
 subplex             1.8        2022-04-12 [1] CRAN (R 4.4.0)
 svglite             2.1.3      2023-12-08 [1] CRAN (R 4.4.0)
 systemfonts         1.1.0      2024-05-15 [1] CRAN (R 4.4.0)
 tibble            * 3.2.1      2023-03-20 [1] CRAN (R 4.4.0)
 tidyr             * 1.3.1      2024-01-24 [1] CRAN (R 4.4.0)
 tidyselect          1.2.1      2024-03-11 [1] CRAN (R 4.4.0)
 tidyverse         * 2.0.0      2023-02-22 [1] CRAN (R 4.4.0)
 timechange          0.3.0      2024-01-18 [1] CRAN (R 4.4.0)
 tzdb                0.4.0      2023-05-12 [1] CRAN (R 4.4.0)
 utf8                1.2.4      2023-10-22 [1] CRAN (R 4.4.0)
 vctrs               0.6.5      2023-12-01 [1] CRAN (R 4.4.0)
 vegan             * 2.6-6.1    2024-05-21 [1] CRAN (R 4.4.0)
 viridisLite         0.4.2      2023-05-02 [1] CRAN (R 4.4.0)
 withr               3.0.0      2024-01-16 [1] CRAN (R 4.4.0)
 xfun                0.44       2024-05-15 [1] CRAN (R 4.4.0)
 xml2                1.3.6      2023-12-04 [1] CRAN (R 4.4.0)
 yaml                2.3.8      2023-12-11 [1] CRAN (R 4.4.0)

 [1] /home/kari/R/x86_64-pc-linux-gnu-library/4.4
 [2] /usr/local/lib/R/site-library
 [3] /usr/lib/R/site-library
 [4] /usr/lib/R/library

──────────────────────────────────────────────────────────────────────────────
```

</details>

<br>
