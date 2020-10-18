##CODE FOR ANIMATED LINE GRAPH


df = data.frame(A=sample(1:75, 50, replace=TRUE),
                B=sample(1:100, 50, replace=TRUE),
                stringsAsFactors = FALSE)


library(ggplot2)
library(tidyverse)
library(gganimate)
library(directlabels)
library(png)
library(transformr)
library(grid)

ggplot(df, aes(A, B)) +
        geom_line() +
        transition_reveal(A) +
        labs(title = 'A: {frame_along}')