library(shiny)
library(tidyverse)
library(tidytext)
library(gutenbergr)
library(janitor)
library(shinythemes)
library(textdata)
library(slider)
library(viridis)
library(DT)
library(ggwordcloud)

all_books1 <- gutenberg_works() %>%
  clean_names() %>%
  filter(language == "en") %>%
  filter(has_text == TRUE) %>%
  drop_na(author, title, gutenberg_author_id) %>% 
  dplyr::select(gutenberg_id, title, author)

# choose only american and english lit
top_subj <- gutenberg_subjects %>%
  filter(subject %in% c("PS","PR")) 

# join em
all_books <- all_books1 %>% 
  inner_join(top_subj, by = "gutenberg_id") %>% 
  select(title, author, gutenberg_id)

all_books_table <- all_books1 %>% 
  inner_join(top_subj, by = "gutenberg_id") %>% 
  select(title, author, gutenberg_id) %>% 
  rename(Title = title,
         `Author (Last, First)` = author)


ui <- fluidPage(theme = shinytheme("flatly"),
                shinyFeedback::useShinyFeedback(),
                navbarPage("What's in a Word?",
                           tabPanel(title = " Summary", 
                                    icon = icon("home"),
                                    mainPanel(HTML("<h2> Welcome to What’s in a Word!</h2> <br><br>  <h4>This Shiny App will take you on a journey through American literature with text and sentiment analysis. Data for this project is provided by Project GutenbergR, a publicly available repository of online book data.</h4> <br>  <h4>Click on each tab when you’re ready to start exploring what’s in a word!</h4> <br><br>")),
                                    fluidRow(column(DT::dataTableOutput("all_books"),
                                                    width = 12)
                                             )
                                    ),
                           ### 2
                           tabPanel(title = " Word Cloud",
                                    icon = icon("cloud"),
                                    sidebarLayout(
                                        sidebarPanel("",
                                                     textInput(inputId = "pick_book",
                                                                 label = "Enter a title",
                                                                 value = ""
                                                                 ),
                                                     radioButtons(inputId = "radio_wc",
                                                                  label = "Choose number of words in your word cloud",
                                                                  choices = list(
                                                                    "25" = 25,
                                                                    "50" = 50,
                                                                    "75" = 75,
                                                                    "100" = 100
                                                                  )
                                                                  ),
                                                     actionButton("choose_word_cloud", "Show me!"),
                                                     fluidRow(column(DT::dataTableOutput("all_books_tab1"),
                                                                     width = 12)
                                                     )
                                                     ),
                                        mainPanel("Here’s a word cloud showing the most popular words that show up in your book! The largest words are the ones that show up the most often. This widget allows you to select a book to view the top most frequently occurring words. The larger the word in the cloud, the more often it appears in the text!",
                                                  plotOutput("wc_plot"))
                                    )),
                           ### 3
                           tabPanel("Word-Count", 
                                    sidebarLayout(
                                        sidebarPanel(  "On this page, enter a word (any word!) to compare how common it is in up to 4 different books. For example, enter the word 'love' and see how many times that word occurs in each of the selected books!",
                                                       textInput(inputId = "pick_book_wc1",
                                                                 label = "Enter first title",
                                                                 value = ""),
                                                       textInput(inputId = "pick_book_wc2",
                                                                 label = "Enter second title",
                                                                 value = ""),
                                                       textInput(inputId = "pick_book_wc3",
                                                                 label = "Enter third title",
                                                                 value = ""),
                                                       textInput(inputId = "pick_book_wc4",
                                                                 label = "Enter fourth title",
                                                                 value = ""),
                                                       textInput(inputId = "text",
                                                                 label = "Enter a word!",
                                                                 value = ""),
                                                       
                                                       actionButton("choose_word", "Show me!"),
                                                       fluidRow(column(DT::dataTableOutput("all_books_tab2"),
                                                                       width = 12)
                                                       )
                                                     
                                        ), 
                                                     
                                
                                        mainPanel( plotOutput("words_plot"))
                                    )
                                    ),
                           ### 4
                           tabPanel("Sentiment Analysis",
                                    sidebarLayout(
                                      sidebarPanel("On this page, select a book to explore the frequency of positively and negatively associated words. The most common words related with positive or negative sentiments will appear at the top!",
                                                   textInput(inputId = "pick_book3",
                                                             label = "Enter a title:",
                                                             value = ""
                                                   ),
                                                   actionButton("choose", "Show me!"),
                                                   fluidRow(column(DT::dataTableOutput("all_books_tab3"),
                                                                   width = 12)
                                                   )
                                      ),
                                      
                                      
                                      mainPanel("",
                                                plotOutput("pb_plot"))
                                    ))
                           ,
                           ### 5
                           tabPanel("Sentiment Trajectory", 
                                    sidebarLayout(
                                      sidebarPanel("",
                                                   textInput(inputId = "pick_book4",
                                                             label = "Enter a title:",
                                                             value = ""
                                                   ),
                                                   HTML("In statistics, a moving average is a calculation to analyze data points by creating a series of averages of different subsets of the full data set."),
                                                   sliderInput(inputId = "moving_avg",
                                                               label = "Choose a moving average window:",
                                                               min = 51, 
                                                               max = 1001, 
                                                               value = 101,
                                                               step = 10
                                                   ),
                                                   actionButton(inputId = "choose2", 
                                                                label = "Show me!"),
                                                   fluidRow(column(DT::dataTableOutput("all_books_tab4"),
                                                                   width = 12)
                                                   )
                                      ),
                                      
                                      
                                      mainPanel(HTML("Type in a book from the Project GutenbergR database below and see how sentiment changes throughout the novel! <br>

Wherever the graph dips and peaks are likely to be major changes occurring in the book’s plot: a dip indicates a very negative part of the story, where a peak indicates a very positive part of the story. <br>

Read your book to see if the plot matches up with what you find in the sentiment analysis!"),
                                                plotOutput("ts_plot"))
                                      
                                    ))
                           )

)
  
server <- function(input, output) {
    
    ### table on main page
    output$all_books <- DT::renderDataTable(
      DT::datatable({all_books_table[,1:2]},
                    options = list(lengthMenu = list(c(5,10),c('5','10')),
                                   dom = 'ltipr'),
                    style = "bootstrap",
                    filter = "top",
                    class = 'cell-border stripe',
                    rownames = FALSE,
                    caption = 'Start searching below to check if your book exists in the Project GutenbergR database:'))
    
    ### table on wc page
    output$all_books_tab1 <- DT::renderDataTable(
      DT::datatable({all_books_table[,1:2]},
                    options = list(lengthMenu = list(c(5),c('5')),
                                   dom = 'tipr'),
                    style = "bootstrap",
                    filter = "top",
                    class = 'cell-border stripe',
                    rownames = FALSE))
    
    ### table on word count page
    output$all_books_tab2 <- DT::renderDataTable(
      DT::datatable({all_books_table[,1:2]},
                    options = list(lengthMenu = list(c(5),c('5')),
                                   dom = 'tipr'),
                    style = "bootstrap",
                    filter = "top",
                    class = 'cell-border stripe',
                    rownames = FALSE))
    
    
    ### table on sentimen page
    output$all_books_tab3 <- DT::renderDataTable(
      DT::datatable({all_books_table[,1:2]},
                    options = list(lengthMenu = list(c(5),c('5')),
                                   dom = 'tipr'),
                    style = "bootstrap",
                    filter = "top",
                    class = 'cell-border stripe',
                    rownames = FALSE))
    
    ### table on ts page
    output$all_books_tab4 <- DT::renderDataTable(
      DT::datatable({all_books_table[,1:2]},
                    options = list(lengthMenu = list(c(5),c('5')),
                                   dom = 'tipr'),
                    style = "bootstrap",
                    filter = "top",
                    class = 'cell-border stripe',
                    rownames = FALSE))

    
    ### Input 1
    wc_reactive <- eventReactive(input$choose_word_cloud,{
      all_books %>%
        filter(title %in% c(input$pick_book,
                            str_to_lower(input$pick_book),
                            str_to_title(input$pick_book))) %>% # changes the users input to title case
        select(gutenberg_id) %>%
        as.numeric() %>%
        gutenberg_download(meta_fields = "title",
                           strip = TRUE,
                           mirror = "http://mirrors.xmission.com/gutenberg/") %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        mutate(word = str_extract(word, "[a-z']+")) %>% 
        count(word) %>% 
        slice_max(n, n = as.numeric(input$radio_wc))
    
    })
    ### Input 2
    words_reactive <-  eventReactive(input$choose_word,{
      all_books %>%
        filter(title %in% c(input$pick_book_wc1,
                            str_to_lower(input$pick_book_wc1),
                            str_to_title(input$pick_book_wc1),
                            input$pick_book_wc2,
                            str_to_lower(input$pick_book_wc2),
                            str_to_title(input$pick_book_wc2),
                            input$pick_book_wc3,
                            str_to_lower(input$pick_book_wc3),
                            str_to_title(input$pick_book_wc3),
                            input$pick_book_wc4,
                            str_to_lower(input$pick_book_wc4),
                            str_to_title(input$pick_book_wc4))) %>% # changes the users input to title case
        select(gutenberg_id) %>%
        # as.numeric() %>%
        gutenberg_download(meta_fields = "title",
                           strip = TRUE,
                           mirror = "http://mirrors.xmission.com/gutenberg/") %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        mutate(word = str_extract(word, "[a-z']+")) %>%
        filter(word %in% c(str_to_lower(input$text),
                           input$text)) %>%
        group_by(title) %>%
        summarise(count = n())
    })
    
    ###Input 3
    
    pb_reactive <- eventReactive(input$choose,{
      all_books %>%
        filter(title %in% c(input$pick_book3,
                            str_to_title(input$pick_book3),
                            str_to_lower(input$pick_book3))) %>% # changes the users input to title case
        select(gutenberg_id) %>%
        as.numeric() %>%
        gutenberg_download(meta_fields = "title",
                           strip = TRUE,
                           mirror = "http://mirrors.xmission.com/gutenberg/") %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        mutate(word = str_extract(word, "[a-z']+")) %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup() %>% 
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word = reorder(word, n))
    })
    
    ###Input 4
    
    ts_reactive <- eventReactive(input$choose2,{
      all_books %>%
        filter(title %in% c(input$pick_book4, 
                            str_to_title(input$pick_book4),
                            str_to_lower(input$pick_book4))) %>%  
       # shinyFeedback::feedbackWarning("n", (is.null(title_chosen) == TRUE), 
       #                               "Please enter a valid title!")
       #  req(title_chosen)
        select(gutenberg_id) %>%
        as.numeric() %>%
        gutenberg_download(meta_fields = "title",
                           strip = TRUE,
                           mirror = "http://mirrors.xmission.com/gutenberg/") %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        mutate(word = str_extract(word, "[a-z']+")) %>%
        inner_join(lexicon_nrc_vad(), by = c("word" = "Word")) %>%
        drop_na(Valence) %>% 
        mutate(index = seq(1, length(word) ,1)) %>% 
        mutate(moving_avg = as.numeric(slide(Valence, 
                                             mean, 
                                             .before = (input$moving_avg - 1)/2 , 
                                             .after = (input$moving_avg - 1)/2 )))
      
    })

    ### output 1
    
    output$wc_plot <- renderPlot({
      ggplot(data = wc_reactive(), aes(label = word)) +
        geom_text_wordcloud(aes(size = n)) +
        scale_size_area(max_size = 6) +
        theme_void()
      
      
    })
    
    ### output 2
    output$words_plot <- renderPlot({
        ggplot(data = words_reactive(), aes(x = title, y = count)) +
            geom_col(aes(fill = title), show.legend = FALSE) + 
            coord_flip() +
            labs(x = "Title",
                 y = "Counts",
                 title = paste('Number of times the word',input$text,'shows up in the selected books.')) +
        theme_minimal() +
        theme(axis.text = element_text(size = 15,
                                         face = "bold",
                                         color = "black"),
              axis.title.y = element_blank(),
              axis.title.x = element_text(face = "bold",
                                          color = "black",
                                          size = 16),
              plot.title = element_text(face = "bold",
                                          size = 18),
              panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank())
    })

    ### output 3
    
    output$pb_plot <- renderPlot({
      ggplot(pb_reactive(), aes(n, word, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(x = "Contribution to sentiment",
             y = NULL)
    })
    ### output 4
    output$ts_plot <- renderPlot({
      ggplot(data = ts_reactive(), aes(x = index, color = moving_avg)) +
        geom_line(aes(y = moving_avg), size = 1) +
        geom_abline(aes(intercept = 0.5, slope = 0), col = "black", alpha = 0.7) +
        scale_color_viridis("viridis")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

