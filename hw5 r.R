#1
> library(tidyverse)
> papers <- readRDS("C:/Users/73113/Downloads/ssrn_cjn.rds")
> papers <- papers %>% mutate(word_count = str_count(title, "\\S+"))
> ggplot(papers, aes(x = as.factor(year), y = word_count)) +geom_boxplot() +
labs(x = "Year", y = "Word Count") + theme_minimal()
#From the boxplots,there hasn't been a huge change in variability of title
#sizes from 2018 to 2023.there are a number of outliers each year, with some 
#titles having a word count significantly high.
> one <- papers %>% filter(word_count == 1)
# A tibble: 19 x 6
title                  year downloads page_count url           word_count
<chr>                 <dbl>     <int>      <int> <chr>              <int>
  1 Sentencing             2023         0          0 https://pape~          1
2 Introduction           2023         4          4 https://pape~          1
3 Restitution            2023        27         39 https://pape~          1
4 Retributivism          2022       230         30 https://pape~          1
5 Justice-as-a-Platform  2022        86         28 https://pape~          1
6 'Ruined'               2022      1451         44 https://pape~          1
7 Terrorism              2022       224         13 https://pape~          1
8 Trauma                 2021        80         24 https://pape~          1
9 #WeToo                 2021       164         73 https://pape~          1
10 Introduction           2021        12          7 https://pape~          1
11 Guncrime               2020        52         27 https://pape~          1
12 Omissions              2020       147         39 https://pape~          1
13 Instigation            2020        93         37 https://pape~          1
14 Cyber-Nuisance         2020       106         92 https://pape~          1
15 Insanity               2019         0          0 https://pape~          1
16 #SororityToo           2019       185         56 https://pape~          1
17 CYBER!                 2019        54         89 https://pape~          1
18 Abortion               2019         0          0 https://pape~          1
19 Fraud                  2019         0          0 https://pape~          1
#there are 19 one word title

#2
> library(dplyr)
> library(tidyr)
> keywords <- tibble(keyword = c("crimin", "justice", "polic", "eviden",
+"violen", "right", "international")
> keywords <- keywords %>% mutate(keyword = tolower(keyword))
> papers <- papers %>% mutate(title = tolower(title))
> crossed_data <- crossing(year = unique(papers$year), keyword =
keywords$keyword)
> crossed_data <- crossed_data %>% left_join(papers, by = c("year" = "year")) 
%>% mutate(keyword_in_title = str_detect(title, keyword))
> result <- crossed_data %>%
  +     group_by(year, keyword) %>%
  +     summarise(n = sum(keyword_in_title, na.rm = TRUE)) %>%
  +     ungroup()
> pivot_table <- result %>%
  +     pivot_wider(names_from = keyword, values_from = n)
# A tibble: 6 x 8
year crimin eviden international justice polic right violen
<dbl>  <int>  <int>         <int>   <int> <int> <int>  <int>
  1  2018    152     48            52      56    70    42     27
2  2019    350     73            89     174   134    95     67
3  2020    386     87           121     202   197   121    118
4  2021    408     90           110     174   175   131     83
5  2022    289     66            50     128   142    58     71
6  2023    201     43            51      88   104    62     33

#3
> melted_data <- pivot_table %>%
  +  pivot_longer(cols = -year, names_to = "keyword", values_to = "count")
 
> line_plot <- ggplot(melted_data, aes(x = year, y = count, color = keyword)) +
  geom_line() + labs(x = "Year", y = "Count", title = "Keyword Counts Over Time") +
 theme_minimal()
> print(line_plot)
#most common is Crimin, all the keywords trend are the same, before 2020 and 
#2021, which are increasing in interest. After that, they tend to decline.

#4
> papers_year <- papers %>%
  +     group_by(year) %>%
  +     summarise(total_downloads = sum(downloads))
> crossed_data <- crossed_data %>%
  +     left_join(papers_year, by = "year")
crossed_data <- crossed_data %>%
  +     group_by(year, keyword) %>%
  +     summarise(prop = sum(keyword_in_title) / sum(downloads)) %>%
  +     ungroup()
>line_plot <- ggplot(crossed_data, aes(x = year, y = prop, color = keyword)) +
  +     geom_line() +
  +     labs(x = "Year", y = "Proportion", title = "Proportion of Downloads by
Keyword") + theme_minimal()
# this plot is different from q3 and its trend are continuing increasing.