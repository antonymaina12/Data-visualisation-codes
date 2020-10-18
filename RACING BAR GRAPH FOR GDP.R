##R CODE FOR ANIMATED RACING BAR GRAPH FOR COUNTRIES GDP



library(readxl)
library(reshape2)
library(dplyr)
library(ggplot2)
library(gganimate)
library(extrafont)
library(grid)
library(png)

# Read Data
df <- readxl::read_excel("GDP.xlsx")

# Prepare Data
t2 <- melt(df,id.vars = "Country",measure.vars = colnames(df[,-1]) , variable.name="Year", value.name="GDP",na.rm = TRUE)

gdp <- t2 %>%
        group_by(Year) %>%
        mutate(rank = min_rank(-GDP) * 1,
               GDP_lbl = paste0(" ",round(GDP/1000))) %>%
        filter(rank<=20) %>%
        ungroup()


# Load Font
loadfonts(device = "win")

# Read PNG file
img <- png::readPNG("orig.png")
g <- grid::rasterGrob(img, width=unit(0.5,"npc"), 
                      height=unit(1,"npc"), 
                      just = "left")

p =     ggplot(gdp, aes(rank, group = Country, fill = as.factor(Country), color = as.factor(Country))) +
        annotation_custom(g, -Inf, Inf, -Inf, Inf) +
        geom_tile(aes(y = GDP/2, height = GDP, width = 0.9), alpha = 0.8, color = NA, size=4) +
        geom_text(aes(y = 0, label = paste(Country, " ")), vjust = 0.2, hjust = 1, size=6, family = "Roboto Condensed") +
        geom_text(aes(y=GDP,label = GDP_lbl, hjust=0)) +
        coord_flip(clip = "off", expand = FALSE) +
        scale_x_reverse() +
        guides(color = FALSE, fill = FALSE) +
        labs(title= "{closest_state} GDP (PPP) by Country", 
             subtitle ="in Billions International Dollars", 
             x = NULL, 
             y = NULL,
             caption = "Sources: IMF's WEO Database") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 28, colour = "#DC143C", family = "Roboto Condensed"),
              plot.subtitle = element_text(hjust = 0.5, size = 12, colour = "#DC143C", family = "Roboto Condensed"),
              plot.caption = element_text(size = 10, colour = "#000000", family = "Roboto Condensed"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y  = element_blank(),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              plot.margin = margin(1,1,1,4, "cm")) +
        transition_states(Year, transition_length = 4, state_length = 1) +
        ease_aes('cubic-in-out')

animate(p, fps = 25, duration = 30, width = 800, 
        height = 600, renderer = gifski_renderer("gdpanimation.gif"))