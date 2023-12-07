## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(knitr)
opts_chunk$set(fig.align = 'center', 
               fig.show = 'hold', fig.width = 7, fig.height = 5)
options(warnPartialMatchArgs = FALSE,
        tibble.print.max = 4,
        tibble.print.min = 4,
        dplyr.summarise.inform = FALSE)

## -----------------------------------------------------------------------------
# install.packages("ggthemeUL", repos="http://R-Forge.R-project.org")
library(ggthemeUL)

library(ggplot2)
library(scales)

## ----echo=FALSE, results='asis'-----------------------------------------------
primaryColors <- c(
  `red`     = "#E03127",
  `antracit` = "#58595b",
  `medium`     = "#A7A8AA",
  `lajt`    = "#E8E9EA"
)

for (i in seq_along(primaryColors)) {
  cat('<div style="width: 100px; height: 100px; background-color:', primaryColors[i], 
      '; display: flex; justify-content: center; align-items: center; color: white; margin: 10px; display: inline-block;">', 
      names(primaryColors)[i], "<br>", primaryColors[i], '</div>')
}

## ----echo=FALSE, results='asis'-----------------------------------------------
coldColors <- c(
  `darkblue`   = "#0033a0",
  `navyblue`   = "#0082C0",
  `turquoise`   = "#00B1AC",
  `green`   = "#00694E"
)

for (i in seq_along(coldColors)) {
  cat('<div style="width: 100px; height: 100px; background-color:', coldColors[i], 
      '; display: flex; justify-content: center; align-items: center; color: white; margin: 10px; display: inline-block;">', 
      names(coldColors)[i], "<br>", coldColors[i],  '</div>')
}

## ----echo=FALSE, results='asis'-----------------------------------------------
warmColors <- c(
  `yellow`   = "#EACE12",
  `orange`   = "#CB511C",
  `burgundy`   = "#9A2F31",
  `pink`   = "#C43788"
)

for (i in seq_along(warmColors)) {
  cat('<div style="width: 100px; height: 100px; background-color:', warmColors[i], 
      '; display: flex; justify-content: center; align-items: center; color: white; margin: 10px; display: inline-block;">', 
      names(warmColors)[i],"<br>", warmColors[i],  '</div>')
}

## -----------------------------------------------------------------------------
ul_color("lajt")

## ----include=FALSE------------------------------------------------------------
set.seed(1)  
n <- 100 
age <- rnorm(n, mean = 35, sd = 5)
height <- rnorm(n, mean = 150 + 0.1 * age, sd = 5)
weight <- rnorm(n, mean = 100 + 0.5 * height - 1 * age, sd = 5)/2

SatisfactionLevels <- c("Highly Disagree", "Disagree", "Agree", "Highly Agree")
SatisfactionLevelsWithNeutral <- c("Highly Disagree", "Disagree", "Neutral", "Agree", "Highly Agree")
df <- data.frame(
  respondent_id = 1:n,
  country = sample(x = c("Slovenia", "Croatia", "Austria", "France"), n, replace = TRUE),
  gender = sample(x = c("Male", "Female"), n, replace = TRUE),
  height = height,
  weight = weight,
  age = age,
  satisfaction = sample(SatisfactionLevels, n, replace = TRUE),
  satisfactionWithNeutral = sample(SatisfactionLevelsWithNeutral, n, replace = TRUE)
)
df$satisfaction <- factor(df$satisfaction, levels = SatisfactionLevels)
df$satisfactionWithNeutral <- factor(df$satisfactionWithNeutral, levels = SatisfactionLevelsWithNeutral)

## ----fig.show='hold'----------------------------------------------------------
basicChart <- ggplot(df, aes(x = country)) +  
  geom_bar() + 
  geom_hline(yintercept = 5) +
  labs(x = element_blank(),        
       y = "Frequency",        
       title = "Lorem ipsum dolor sit amet",        
       caption = "Data source: this is all fake data.",        
       subtitle = "Eiusmod tempor incididunt ut labore et dolore magna.")

basicChart

basicChart +
  geom_bar(fill = ul_color("navyblue")) +   
  geom_hline(yintercept = 5, color = ul_color("red")) +
  theme_ul(plot.background.fill =  "#E8E9EA")

## ----fig.show='hold'----------------------------------------------------------
basicChart +
  geom_bar(fill = ul_color("navyblue")) +   
  geom_hline(yintercept = 5, color = ul_color("red")) +
  theme_ul(plot.background.fill =  "#E8E9EA") +
  theme(plot.title = element_text(color="red")) 

## ----fig.show='hold'----------------------------------------------------------
basicChart <- ggplot(df, aes(x = country, y = height, fill = country)) +
  facet_grid(.~gender) +
  geom_boxplot(show.legend = FALSE) +
  labs(y = "Sentiment", x = element_blank()) +
  theme_ul(legend.justification = c(0, 1)) +
  scale_y_continuous(labels = dollar_format(suffix=" cm",prefix="")) +
  labs(x = element_blank(),        
       y = element_blank(),        
       title = "Height ipsum dolor sit amet",        
       caption = "Data source: this is all fake data.",        
       subtitle = "Eiusmod tempor incididunt ut labore et dolore magna.") 

basicChart + scale_fill_ul() 
basicChart + scale_fill_ul("cold") 

## ----fig.show='hold'----------------------------------------------------------
basicChart + scale_fill_ul("navyblue", reverse = TRUE) 

## ----fig.show='hold'----------------------------------------------------------
basicChartCont <- ggplot(df, aes(x = age, y = height, color = weight)) +   
  geom_point(size = 5) +   
  theme_ul(legend.justification = c(0, 1)) +
  labs(x = "Age (years)",        
       y = "Height (cm)",        
       title = "Lorem ipsum dolor sit amet",        
       caption = "Data source: this is all fake data.",        
       subtitle = "Eiusmod tempor incididunt ut labore et dolore magna.",
       color = "Weight") 

basicChartCont + scale_color_ul(palette = "navyblue", discrete = FALSE)
basicChartCont + scale_color_ul(palette = "navyblue", discrete = FALSE, values = c(0, 0.8, 1))

## ----fig.show='hold'----------------------------------------------------------
df$country <- factor(df$country, levels = rev(c("Slovenia", "Croatia", "France", "Austria")))
ggplot(df, aes(y = country, fill = satisfaction)) +
  scale_x_continuous(labels = dollar_format(suffix=" %",prefix="", scale = 100)) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  scale_fill_ul("redGreen") +
  theme_ul(panel.background.fill = "white", panel.grid.major.color = "white") +
    labs(x = element_blank(),        
       y = element_blank(),        
       title = "Lorem ipsum dolor sit amet",        
       caption = "Data source: this is all fake data.",        
       subtitle = "Eiusmod tempor incididunt ut labore et dolore magna.",
       fill = "Agreement") 

## ----fig.show='hold'----------------------------------------------------------
basicChart <- ggplot(df, aes(y = country, fill = satisfactionWithNeutral)) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_ul() +
  labs(fill = element_blank()) +
  scale_x_continuous(labels = dollar_format(suffix=" %",prefix="", scale = 100)) +
  theme_ul(plot.background.fill = ul_color("lajt")) +
      labs(x = element_blank(),        
       y = element_blank(),        
       title = "Lorem ipsum dolor sit amet",        
       caption = "Data source: this is all fake data.",        
       subtitle = "Eiusmod tempor incididunt ut labore et dolore magna.",
       fill = "Agreement") 

basicChart + scale_fill_ul("redGreen")

## ----fig.show='hold'----------------------------------------------------------
ggplot(df, aes(y = country, fill = satisfactionWithNeutral)) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  guides(fill = guide_legend(nrow = 1)) +
  scale_x_continuous(labels = dollar_format(suffix=" %",prefix="", scale = 100)) +
  scale_fill_ul("redGreen", neutralColor = "lajt") +   
  theme_ul(legend.key = element_rect(color = ul_color("antracit"), 
                                     fill = "transparent"), 
           plot.background.fill = ul_color("lajt")) +
  labs(fill = element_blank()) +
      labs(x = element_blank(),        
       y = element_blank(),        
       title = "Lorem ipsum dolor sit amet",        
       caption = "Data source: this is all fake data.",        
       subtitle = "Eiusmod tempor incididunt ut labore et dolore magna.",
       fill = "Agreement") 

ggplot(df, aes(y = country, fill = satisfactionWithNeutral)) +
  geom_bar(position = position_fill(reverse = TRUE), color = ul_color("antracit")) +
  guides(fill = guide_legend(nrow = 1)) +
  scale_x_continuous(labels = dollar_format(suffix=" %",prefix="", scale = 100)) +
  scale_fill_ul("redGreen", neutralColor = "lajt") +   
  theme_ul(plot.background.fill = ul_color("lajt")) +
  labs(fill = element_blank()) +
      labs(x = element_blank(),        
       y = element_blank(),        
       title = "Lorem ipsum dolor sit amet",        
       caption = "Data source: this is all fake data.",        
       subtitle = "Eiusmod tempor incididunt ut labore et dolore magna.",
       fill = "Agreement") 

## ----fig.show='hold'----------------------------------------------------------
basicChartCont + scale_color_ul(palette = "redGreen", discrete = FALSE)

## ----fig.show='hold'----------------------------------------------------------
basicChartCont + scale_color_ul(palette = "redGreen", discrete = FALSE, midpoint = 70)
basicChartCont + scale_color_ul(palette = "redGreen", 
                            discrete = FALSE, 
                            values = c(0, 
                                       scales::rescale(70, to = c(0, 1), from = range(df$weight)),
                                       1))

