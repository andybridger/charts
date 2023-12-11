# Libraries to load
library(OECD)
library(tidyverse)
library(ggplot2)
library(emojiflag)
library(dplyr)
library(emoji)
library(countrycode)
library(purrr)
library(ggimage)

# Note need to run the mystyle code to make charts
# chart2 is the version of the chart saved on GitHub
# chart1 code was ran purely out of interest

# Obtain data
df <- get_dataset("CPL")

# Transform data
df_chart <- df %>% filter(CPL_COUNTRY == "AUS") %>%
  filter(Time == "2023-10") %>% 
  mutate(ObsValue = as.numeric(ObsValue),
         pct = ObsValue - 100)

df_chart$iso2 <- countrycode(df_chart$LOCATION, "iso3c", "iso2c")
df_chart$type <- ifelse(df_chart$pct >= 0, "pos", "neg")

#Make chart 1
chart1 <- df_chart %>% ggplot(aes(x = reorder(LOCATION, pct), y = pct, fill = type) ) + 
  geom_flag(size = 0.03, y = -75, aes(image = iso2))  +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.2, size = 4.0) +
  my_style() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  ylim(c(-75, 50)) +
  #my_y_continuous(limits=c(-80,50), breaks = seq(-75,50, by = 25)) +
  labs(title = "The UK is 4% cheaper than Australia",
       subtitle = "Comparative Price Levels (CPL) for OECD countries relative to Australia",
       x = "% difference relative to Australia",
       caption = "Note: Data for October 2023. CPLs are defined as the ratios of Purchasing Power Parities for private final consumption expenditure to
exchange rates. They provide measures of differences in price levels between countries.
Source: OECD, Monthly Comparative Price Levels, October 2023") +
  coord_flip() +
  scale_fill_manual(values = c("#56B4E9","#cc79a7"))

#save chart1
finalise_plot(plot_name = chart1,
              save_filepath = "/Users/andrewbridger/Desktop/R/chartoftheweek/chartcpl.png",
              width_pixels = 640,
              height_pixels = 900)


#Make chart 2
chart2 <- df_chart %>% ggplot(aes(x = reorder(LOCATION, pct), y = pct, fill = type) ) + 
  geom_flag(size = 0.03, y = -75, aes(image = iso2))  +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.2, size = 4.0) +
  my_style() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  ylim(c(-75, 50)) +
  #my_y_continuous(limits=c(-80,50), breaks = seq(-75,50, by = 25)) +
  labs(title = "Comparative price levels in OECD countries relative to Australia",
       subtitle = "How to read? The price level in the UK is 4% lower than in Australia. That means you 
would spend A$100 in Australia to buy the same basket of goods and services when you 
spend $A96 in the UK.",
       caption = "Note: Data for October 2023. CPLs are defined as the ratios of Purchasing Power Parities for private final consumption expenditure to
exchange rates. They provide measures of differences in price levels between countries.
Source: OECD, Monthly Comparative Price Levels, October 2023") +
  coord_flip() +
  scale_fill_manual(values = c("#56B4E9","#cc79a7"))

#save chart2
finalise_plot(plot_name = chart2,
              save_filepath = "/Users/andrewbridger/Desktop/R/chartoftheweek/chartcpl2.png",
              width_pixels = 640,
              height_pixels = 900)