library(shiny)
library(tidyverse)
library(tidytext)
library(gutenbergr)
library(janitor)
library(wordcloud)
library(wordcloud2)
library(ECharts2Shiny)
library(shinythemes)

books <- gutenberg_download(c(140, 219), 
                              meta_fields = "title",
                              strip = TRUE,    
                              mirror = "http://mirrors.xmission.com/gutenberg/")

books <- books %>% 
    unnest_tokens(word,text)


ui <- fluidPage(theme = shinytheme("darkly"),
                navbarPage("Book Text Analysis",
                           tabPanel("Summary",
                                    mainPanel("The purpose of this Shiny app is to 
                                              conduct text and sentiment analysis of data 
                                              on seven classical novels.\n
                                              Data on the seven novels was downloaded from Project GutenbergR, 
                                              a publicly available repository of online book data.\n
                                              In this app, you’ll have the opportunity to make a word cloud of most popular
                                              word by novel, to xxxxx, and to see the most popular 
                                              words that contribute to positive and negative sentiment by book")),
                           tabPanel("Word-Cloud",
                                    sidebarLayout(
                                        sidebarPanel("Create a Word Cloud!",
                                                     selectInput(inputId = "pick_book",
                                                                 label = "Select Book:",
                                                                 choices = unique(books$title)
                                                                 )
                                                     ),
                                        mainPanel("Heres your Word Cloud",
                                                  plotOutput("wc_plot"))
                                    )),
                           tabPanel("Sentiment-analysis",
                                    sidebarLayout(
                                        sidebarPanel("",
                                                     selectInput(inputId = "pick_book2",
                                                                 label = "Select Book:",
                                                                 choices = unique(books$title)
                                                     )
                                        ),
                                        mainPanel("Comparison of lexicons",
                                                  plotOutput("sa_plot"))
                                    )
                                    ),
                           tabPanel("Word-Count",
                                    sidebarLayout(
                                        sidebarPanel("",
                                                     textInput(inputId = "text",
                                                               label = "Word Input",
                                                               value = "Enter word ..."
                                                               )
                                                               ),
                                                     
                                
                                        mainPanel("Word Count",
                                                  plotOutput("words_plot"))
                                    ))
                           )
)

  

# Define server logic required to draw a histogram
server <- function(input, output) {
    ### Input 1
    wc_reactive <- reactive({
        books %>% 
            filter(title %in% input$pick_book)
        
    })
    ### Input 2
    words_reactive <- reactive({
        books %>% 
            filter(word %in% input$text) %>% 
            group_by(title) %>% 
            summarise(count = n())
    })
    
    ###Input 3
    
    sa_reactive <- reactive({
        books %>% 
            filter(title %in% input$pick_book2) %>% 
            inner_join(get_sentiments("bing")) %>%
            count(word, sentiment, sort = TRUE) %>%
            ungroup() %>% 
            group_by(sentiment) %>%
            top_n(10) %>%
            ungroup() %>%
            mutate(word = reorder(word, n))
        
    })
    ### output 1
    output$wc_plot <- renderPlot({
        wc_reactive() %>%
            select(word) %>% 
            anti_join(stop_words) %>%
            count(word) %>%
            with(wordcloud(word, n, max.words = 100))
    })
    ### output 2
    output$words_plot <- renderPlot({
        ggplot(data = words_reactive(), aes(x = title, y = count)) +
            geom_col(aes(fill = title))
    })
    ### output 3
    
    output$sa_plot <- renderPlot({
        ggplot(sa_reactive(), aes(n, word, fill = sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y") +
            labs(x = "Contribution to sentiment",
                 y = NULL)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
