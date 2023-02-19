library(shiny)
library(rsconnect)
library(tidyverse)
library(readr)
library(googleLanguageR)
library(deeplr)
library(tidytext)
library(tm)
library(stringr)
library(topicmodels)
library(reshape2)
library(viridisLite)
library(wordcloud)
library(dendroextras)
library(heatmaply)
library(RColorBrewer)
library(dendextend)
library(cluster)
library(plotly)
library(DT)
library(shinythemes)
library(wordcloud2)

df <- read_csv("air.csv")
df_topics <- df %>% filter(
  Country %in% c("United States", "United Kingdom")
) %>% select(
  Description,
  Summary,
  Notes,
  Neighborhood.Overview,
  House.Rules,
  Room.Type,
  Number.of.Reviews,
  Review.Scores.Rating,
  Cancellation.Policy,
  Minimum.Nights,
  Maximum.Nights,
  ID,
  Price
)

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("What determines the price of an Airbnb listing"),

  sidebarLayout(
      sidebarPanel(
        selectInput("textField", "Selected text field",
                    selected= "House.Rules",
                    choices = c(
                      "Description",
                      "Summary",
                      "Notes",
                      "Neighborhood.Overview",
                      "House.Rules")),
        sliderInput("i", "# words in word count", min = 10, max = 50, value = 10),
        sliderInput("k", "# of topics", min = 2, max = 10, value = 2),
        sliderInput("n", "# of words in each topic", min = 5,
                    max = 15,
                    value = 5),
        selectInput("alg", "LDA method: \"Gibbs\" (slow, high quality) or
                    \"VEM\" (fast, bad quality)",
                    choices = c("Gibbs", "VEM"),
                    selected = "VEM"),
        numericInput('size', 'Size of wordcloud (between 0.2 and 1.5)',
                     value = 0.4,
                     min = 0.2,
                     max = 1,
                     step = .1)),
      mainPanel(
        tabsetPanel(
          tabPanel("Word Cloud", wordcloud2Output('wordcloud2')),
          tabPanel("Word Count", plotOutput("wordCountPlot")),
          tabPanel("Topics LDA", plotOutput("ldaPlot")),
          tabPanel("Row Data", DTOutput("tbl"))
          )
        )
      )
  )


server <- function(input, output) {
  rval_text <- reactive({input$textField})
  rval_k <- reactive({input$k})
  rval_n <- reactive({input$n})
  rval_alg <- reactive({input$alg})
  rval_i <- reactive({input$i})
  rval_size <- reactive({
    if (input$size > 1.5){
      1.5
    } else if (input$size < 0.2 ){
      0.2
    } else {
      input$size
    }
    })
  df_create <- function(df, arg1) {
    df_temp <- df %>%
      unnest_tokens(word, arg1)
    
    df_temp_without_stop_words <- df_temp %>% 
      anti_join(stop_words)
    
    my_stop_words <- tribble(
      # Column names should match stop_words
      ~word, ~lexicon,
      "home", "CUSTOM",
      "amp", "CUSTOM",
      "", "Serhii"
    )
    
    stop_words2 <- stop_words %>% 
      bind_rows(my_stop_words)
    
    df_temp_without_stop_words <- df_temp_without_stop_words %>%
      mutate(word = removeNumbers(removePunctuation(word))) %>%
      anti_join(stop_words2)
    
    df_temp_without_stop_words <- df_temp_without_stop_words %>%
      mutate(
        word = str_replace(word, pattern = 'room', replacement = "")
      ) %>% filter(word != "")
    
   # words_copy_top_50 <- df_temp_without_stop_words %>% 
    #  count(word) %>% 
     # top_n(50, n)
    
    #words_copy <- words_copy_top_50$word
    
    #df_temp_without_stop_words <- df_temp_without_stop_words %>% 
    #  mutate(
      #  word = stemDocument(df_temp_without_stop_words$word)
      #)
    
    #df_temp_without_stop_words <- df_temp_without_stop_words %>% 
     # mutate(
      #  word = stemCompletion(df_temp_without_stop_words$word, words_copy)
      #) %>% filter(word != "")
    #return(df_temp_without_stop_words)
  }
  
  plot_top_n <- function(df, number, title = "", subtitle = "", color = plasma(1, alpha = 0.6)) {
    df %>% 
      count(word) %>% 
      top_n(rval_i(), n) %>% 
      arrange(desc(n)) %>% 
      mutate(word_fct = fct_reorder(word, n)) %>% 
      ggplot(aes(word_fct, n)) +
      geom_col(
        fill = color,
        show.legend = FALSE
      ) +
      coord_flip() +
      labs(
        title = title,
        subtitle = subtitle,
        x = "Word",
        y = "Count"
      ) +
      theme_classic() +
      scale_y_discrete(
        expand = c(0, 0)
      )
  }
  
  lda <- function(title="", color=plasma(1, alpha = 0.6)) {
    dtm <- df_temp_without_stop_words() %>%
      count(word, ID) %>% 
      cast_dtm(ID, word, n)
    
    M <- as.matrix(dtm)
    lda_topics <- dtm %>%
      LDA(
        k = rval_k(), 
        method = rval_alg(),
        control = list(seed = 42)) %>% 
      tidy(matrix = "beta") %>% 
      arrange(desc(beta))
    word_probs <- lda_topics %>% 
      group_by(topic) %>% 
      top_n(rval_n(), beta) %>% 
      ungroup() %>% 
      mutate(term = fct_reorder(term, beta))
    
    topicLabels <- c("Topic # 1", "Topic # 2", "Topic # 3")
    names(topicLabels) <- c(1,2,3)
    
    word_probs %>% ggplot(
      aes(term,
          beta,
          fill = as.factor(topic)
      )) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic,
                 scales = "free",
                 labeller = labeller(topic = topicLabels)) +
      labs(title = title) + 
      coord_flip() +
      theme_bw()
  }
  df_temp_without_stop_words <- reactive({df_create(df_topics, rval_text())})
  word_by_price <- reactive({df_temp_without_stop_words() %>% 
    group_by(word) %>% 
    summarise(
      #freq = mean(Price)
      `Sum Price` = sum(Price)
      #n = n()
      )})
  output$tbl <- renderDT({
    word_by_price()
    })
  output$wordCountPlot <- renderPlot({
    plot_top_n(df_temp_without_stop_words(), 25, color = "#fcba03", title = rval_text(), subtitle = "word count")
  })
  output$ldaPlot <- renderPlot({
    lda(title = rval_text())
  })
  set.seed(42)
  output$wordcloud2 <- renderWordcloud2({
    wordcloud2(word_by_price(), size=rval_size())
  })
}

shinyApp(ui = ui, server = server)