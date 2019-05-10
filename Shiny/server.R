#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(readr)
library(dplyr)
library(wordcloud)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(formattable)
library(DT)
library(RColorBrewer)
library(randomForest)
library(e1071)
library(rpart)
library(MASS)
library(glmnet)
library(PerformanceAnalytics)
library(corrplot)
library(car)
library(kernlab)
library(keras)
library(xgboost)
library(stringr)
library(caret)
library(Matrix)
library(ROCR)
library(pROC)
f1 = list(
  family = "Old Standard TT, serif",
  size = 14,
  color = "grey"
)
f2 = list(
  family = "Old Standard TT, serif",
  size = 10,
  color = "black"
)
a = list(
  titlefont = f1,
  showticklabels = T,
  tickangle = -45,
  tickfont = f2
)

m = list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)

# annotations for subplot
a1 = list(x = 0.5, y = 1.0,
          showarrow = FALSE, 
          text = "Distribution of bugdet", 
          xanchor = "center", 
          xref = "paper", 
          yanchor = "bottom", 
          yref = "paper", 
          font = f1)

b1 = list(x = 0.5, y = 1.0, 
          showarrow = FALSE, 
          text = "Distribution of gross", 
          xanchor = "center", 
          xref = "paper", 
          yanchor = "bottom", 
          yref = "paper",
          font = f1)


# creating a function called scatter_plot for
# plotting scatter plots using ggplot and plotly

scatter_plot = function(x, y, xlabel, ylabel, title,
                        text1, text2, text3,
                        alpha = NULL){
  if(is.null(alpha)) alpha = 0.4
  gp = ggplot(data = movie, mapping = aes(x = x, y = y,
                                          text = paste(text1, x,
                                                       text2, y,
                                                       text3, movie_title)))
  plot = gp + geom_point(position = "jitter", 
                         show.legend = F, shape = 21, 
                         stroke = .2, alpha = alpha) + 
    xlab(xlabel) +
    ylab(ylabel) +
    ggtitle(title) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(size = 12, face = "bold", 
                                    family = "Times", 
                                    color = "darkgrey")) 
  
  ggplotly(plot, tooltip = "text") %>%
    layout(m, xaxis = a, yaxis = a)
}

# creating function for plotting scatter plot using facet wrap
facet_plot = function(x, y, xlabel, ylabel, alpha = NULL){
  if(is.null(alpha)) alpha = 1
  
  fp = ggplot(data = movie, mapping = aes(x = x, y = y))
  fp + geom_point(aes(fill = genres), position = "jitter", 
                  show.legend = F, shape = 21,
                  stroke = 0.2, alpha = alpha) +
    xlab(xlabel) +
    ylab(ylabel) +
    facet_wrap(~genres, scales = "free") +
    theme_minimal()
}

# creating a function for plotting a simple histogram
hist_plot = function(x, xlabel, bwidth, fill = NULL, color = NULL){
  if(is.null(fill)) fill = "orange"
  if(is.null(color)) color = "black"
  hp = ggplot(data = movie, mapping = aes(x = x))
  gp = hp + geom_histogram(binwidth = bwidth, fill = fill, 
                           color = color, 
                           size = 0.2,
                           alpha = 0.7,
                           show.legend = F) +
    xlab(xlabel) +
    theme_minimal()
  
  ggplotly(gp) %>%
    layout(margin = m, xaxis = a, yaxis = a)
}

# creating a function for plotting histogram using facet wrap
facet_hist_plot = function(x, xlabel, bwidth){
  
  hp = ggplot(data = movie, mapping = aes(x = x))
  hp + geom_histogram(aes(fill = genres), binwidth = bwidth,
                      show.legend = F, 
                      color = "black", size = 0.2, 
                      alpha = 0.8) +
    xlab(xlabel) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text = element_text(size = 12, angle = 20),
          axis.title = element_text(size = 14, 
                                    family = "Times", 
                                    color = "darkgrey",
                                    face = "bold")) + 
    facet_wrap(~ genres, scales = "free_y", ncol = 4)    
}


# creating function for plotting histograms for budget and gross
budg_gross_hist = function(x){
  
  bh = ggplot(movie, aes(x = x))
  bh + geom_histogram(binwidth = 0.05, 
                      fill = sample(brewer.pal(11, "Spectral"), 1),
                      color = "black", 
                      size = 0.09,
                      alpha = 0.7) + 
    scale_x_log10() + 
    theme_minimal()
  
  ggplotly() %>% 
    layout(m, xaxis = a, yaxis = a) 
}


# creating a function for ploting bar graphs
bar_plot = function(data, x, y, info, xlabl, ylabl, title, 
                    deci = NULL, suf = NULL){
  if(is.null(suf)) suf = ""
  if(is.null(deci)) deci = 0
  b1 = ggplot(data, aes(x = reorder(genres, x), 
                        y = y, 
                        text = paste("Genre:", genres,
                                     info, 
                                     round(y, deci), suf)))
  
  b1 + geom_bar(aes(fill = genres), stat = "identity", 
                show.legend = F, color = "black", size = 0.2,
                width = 0.7, alpha = 0.7) +
    xlab(xlabl) +
    ylab(ylabl) +
    ggtitle(title) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(size = 14, 
                                    color = "grey", family = "Times")) +
    scale_fill_brewer(palette = "Spectral")
  
  ggplotly(tooltip = "text") %>%
    layout(margin = m, xaxis = a, yaxis = a)
}


# creating a function for plotting plotly line graph for title_year
line_graph = function(data, y, name){
  
  scat_p1 = plot_ly(data, x = ~title_year, y = ~ y, 
                    name = name, type = 'scatter', mode = 'lines',
                    line = list(color = sample(brewer.pal(11, "Spectral"), 1))) %>%
    layout(xaxis = list(title = "Title Year", zeroline = F,
                        showline = F, 
                        showticklabels = T),
           yaxis = list(title = "Average Score"),
           title = "Line Graph for Avg Score/Avg Votes/Avg User Review by Title Year", 
           font = list(family = "Serif", color = "grey"),
           legend = list(orientation = "h", size = 6,
                         bgcolor = "#E2E2E2",
                         bordercolor = "darkgrey",
                         borderwidth = 1),
           margin = m)
  scat_p1
}
colors = c(brewer.pal(n = 11, name = "Spectral"))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  movie = read_csv("../Data/movie.csv")
  output$genre <- renderPlotly({
    p = movie %>%
      group_by(genres) %>%
      summarise(count = n()) %>% 
      arrange(desc(count)) %>%
      head(10) %>%
      plot_ly(labels = ~genres, values = ~count,
              insidetextfont = list(color = 'Black'),
              marker = list(colors = colors, 
                            line = list(color = 'Black', width = .5)), 
              opacity = 0.8) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "", 
             titlefont = list(family = "Times", size = 20, color = "grey"),
             xaxis = list(showgrid = T, zeroline = F, showticklabels = F),
             yaxis = list(showgrid = T, zeroline = F, showticklabels = F),
             showlegend = T, 
             margin = list(t = 50, b = 50))
    
  })
  
  output$profit = renderPlotly({
    p = movie %>%
      group_by(if_profit) %>%
      summarise(count = n()) %>% 
      plot_ly(labels = ~factor(if_profit), values = ~count,
              insidetextfont = list(color = 'Black'),
              marker = list(colors = sample(brewer.pal(11, "Spectral")), 
                            line = list(color = 'Black', width = .5)), 
              opacity = 0.8) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "", 
             titlefont = list(family = "Times", size = 20, color = "grey"),
             xaxis = list(showgrid = T, zeroline = F, showticklabels = F),
             yaxis = list(showgrid = T, zeroline = F, showticklabels = F),
             margin = list(t = 50, b = 50))
  })
  
  output$score = renderPlotly({
    p = hist_plot(movie$imdb_score, bwidth = 0.1, 
                                        "IMDB Score", 
                                        fill = sample(brewer.pal(11, "Spectral"), 1),
                                        color = "black") 
    })
  
  output$money = renderPlotly({
    options(scipen = 999)  
    # transformed budget histograms
    p1 = budg_gross_hist(movie$budget) %>% 
      layout(annotations = a1)
    
    # transformed gross histograms
    p2 = budg_gross_hist(movie$gross) %>%
      layout(annotations = b1) 
    
    p = subplot(p1, p2, widths = c(0.5, 0.5))
  })
  
  output$year = renderPlotly({
    movie$before_n_after_2000 = ifelse(movie$title_year >= 2000, 1, 0)
    
    # plotting a histogram separated by before_n_after_2000
    his = movie %>%
      ggplot(aes(x = imdb_score, fill = factor(before_n_after_2000))) +
      geom_histogram(binwidth = 0.1, 
                     color = "black",
                     position = "dodge",
                     size = 0.2,
                     alpha = 0.7) +
      xlab("IMDB Score") +
      ggtitle("") +
      scale_fill_manual(values = c(sample(brewer.pal(11, "Spectral")))) +
      theme_minimal() + 
      theme(plot.title = element_text(size = 14,
                                      colour = "darkgrey",
                                      family = "Times"))
    
    
    p = ggplotly(his) %>%
      layout(margin = list(t = 50, b = 100), 
             xaxis = a, yaxis = a, 
             legend = list(orientation = "h", size = 4,
                           bgcolor = "#E2E2E2",
                           bordercolor = "darkgrey",
                           borderwidth = 1,
                           x = 0,
                           y = -0.1)) 
  })
})
