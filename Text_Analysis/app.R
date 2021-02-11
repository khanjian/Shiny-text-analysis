library(shiny)
library(tidyverse)
library(tidytext)
library(gutenbergr)
library(janitor)
library(wordcloud)
library(wordcloud2)
library(ECharts2Shiny)
library(shinythemes)
library(textdata)
library(slider)
library(kableExtra)
library(knitr)

ids <- c(140, 219, 64317, 1342, 16, 76, 55, 345, 205, 2701)

books_initial <- gutenberg_download(ids, 
                              meta_fields = c("title", "author"),
                              strip = TRUE,    
                              mirror = "http://mirrors.xmission.com/gutenberg/")

books <- books_initial %>% 
    unnest_tokens(word,text)


ui <- fluidPage(theme = shinytheme("darkly"),
                navbarPage("Book Text Analysis",
                           tabPanel("Summary", ### 1
                                    mainPanel("The purpose of this Shiny app is to 
                                              conduct text and sentiment analysis of data 
                                              on seven classical novels.\n
                                              Data on the seven novels was downloaded from Project GutenbergR, 
                                              a publicly available repository of online book data.\n
                                              In this app, you’ll have the opportunity to make a word cloud of most popular
                                              word by novel, to xxxxx, and to see the most popular 
                                              words that contribute to positive and negative sentiment by book")),
                           tabPanel("Word-Cloud", ### 2
                                    sidebarLayout(
                                        sidebarPanel("",
                                                     selectInput(inputId = "pick_book",
                                                                 label = "Select Book:",
                                                                 choices = unique(books$title)
                                                                 )
                                                     ),
                                        mainPanel("Here’s a word cloud showing the most popular words that show up in your book! The largest words are the ones that show up the most often.",
                                                  plotOutput("wc_plot"))
                                    )),
                           tabPanel("Sentiment-analysis", ### 3
                                    sidebarLayout(
                                        sidebarPanel("",
                                                     selectInput(inputId = "pick_book2",
                                                                 label = "Select Book:",
                                                                 choices = unique(books$title)
                                                     )
                                        ),
                                        mainPanel("Here’s a graph which shows the top 10 words which contribute to negative or positive sentiment in your book!",
                                                  plotOutput("sa_plot"))
                                    )
                                    ),
                           tabPanel("Word-Count", ### 4
                                    sidebarLayout(
                                        sidebarPanel("Enter a word to see how often it shows up across the 10 books listed below",
                                                     textInput(inputId = "text",
                                                               label = "Word Input",
                                                               value = "Enter word ..."
                                                               ),
                                                     tableOutput('table_words')
                                                               ),
                                                     
                                
                                        mainPanel("Here’s a graph which shows how many times your specified word shows up in each book!",
                                                  plotOutput("words_plot"))
                                    )
                                    ),
                           tabPanel("Pick a Book", ### 5
                                    sidebarLayout(
                                      sidebarPanel("",
                                                   textInput(inputId = "pick_book3",
                                                             label = "Book input",
                                                             value = "Enter a title ..."
                                                   ),
                                                   actionButton("choose", "Show me!")
                                      ),
                                      
                                      
                                      mainPanel("",
                                                plotOutput("pb_plot"))
                                    ))
                           ,
                           tabPanel("Times series", ### 6
                                    sidebarLayout(
                                      sidebarPanel(HTML("<p>To check if a book title of your choosing is available in this database, please visit the <a href='https://www.gutenberg.org/'>Project Gutenberg website</a> and type your desired book into the search bar. If your book is available in the Project Gutenberg library, enter the title as it appears on Project Gutenberg into the search bar below.</p>"),
                                                   textInput(inputId = "pick_book4",
                                                             label = "Book input",
                                                             value = "Enter a title ..."
                                                   ),
                                                   sliderInput(inputId = "moving_avg",
                                                               label = "Choose a moving average window",
                                                               min = 51, 
                                                               max = 1001, 
                                                               value = 101,
                                                               step = 10
                                                   ),
                                                   actionButton("choose2", "Show me!")
                                      ),
                                      
                                      
                                      mainPanel("",
                                                plotOutput("ts_plot"))
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
    
    ###Input 4
    
    pb_reactive <- eventReactive(input$choose,{
      gutenberg_works(title %in% input$pick_book3) %>%
        select(gutenberg_id) %>%
        as.numeric() %>%
        gutenberg_download(meta_fields = "title",
                           strip = TRUE,
                           mirror = "http://mirrors.xmission.com/gutenberg/") %>%
        unnest_tokens(word, text) %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup() %>% 
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word = reorder(word, n))
    })
    
    ###Input 5
    
    ts_reactive <- eventReactive(input$choose2,{
      gutenberg_works(title %in% input$pick_book4) %>%
        select(gutenberg_id) %>%
        as.numeric() %>%
        gutenberg_download(meta_fields = "title",
                           strip = TRUE,
                           mirror = "http://mirrors.xmission.com/gutenberg/") %>%
        unnest_tokens(word, text) %>%
        full_join(lexicon_nrc_vad(), by = c("word" = "Word")) %>%
        drop_na(Valence) %>% 
        mutate(index = seq(1, length(word) ,1)) %>% 
        mutate(moving_avg = as.numeric(slide(Valence, 
                                             mean, 
                                             .before = (input$moving_avg - 1)/2 , 
                                             .after = (input$moving_avg - 1)/2 ))) 
        # count(word, sentiment, sort = TRUE) %>%
        # ungroup() %>% 
        # group_by(sentiment) %>%
        # top_n(10) %>%
        # ungroup() %>%
        # mutate(word = reorder(word, n))
    })
    
    ### input table for book
    table_reactive <- reactive({
      books_initial %>% 
        distinct(title, .keep_all = TRUE) %>% 
        select(c(title, author))
      
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
            geom_col(aes(fill = title), show.legend = FALSE) + 
        coord_flip() 
    })
    ### output 3
    
    output$sa_plot <- renderPlot({
        ggplot(sa_reactive(), aes(n, word, fill = sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y") +
            labs(x = "Contribution to sentiment",
                 y = NULL)
    })
    ### output 4
    
    output$pb_plot <- renderPlot({
      ggplot(pb_reactive(), aes(n, word, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(x = "Contribution to sentiment",
             y = NULL)
    })
    ### output 5
    output$ts_plot <- renderPlot({
      ggplot(data = ts_reactive(), aes(x = index)) +
        geom_line(aes(y = moving_avg), color = "blue")
    })
    
    output$table_words <- renderTable(
      table_reactive()
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
