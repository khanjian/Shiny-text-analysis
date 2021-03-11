library(shiny)
library(tidyverse)
library(tidytext)
library(here)
library(gutenbergr)
library(janitor)
library(shinythemes)
library(textdata)
library(slider)
library(viridis)
library(DT)
library(ggwordcloud)
library(shinyWidgets)
library(bslib)
library(shinyFeedback)

all_books1 <- gutenberg_works() %>%
  clean_names() %>%
  filter(language == "en") %>%
  filter(has_text == TRUE) %>%
  drop_na(author, title, gutenberg_author_id) %>% 
  dplyr::select(gutenberg_id, title, author)

# choose only American and English lit
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

# subset all the colors, without the numbers. 
all_colors <- colors()
my_colors <-  all_colors[!str_detect(colors(), "[0-9']+")]

# test id top 100 downloaded books work 
# top_100 <- read_table(here("data", "top_ebooks_30.txt"),
#                       col_names = FALSE)  %>% 
#   clean_names() %>% 
#   separate(x1, c("title", "author"), sep ="\\s[by]\\s", remove = FALSE) %>% 
#   select(title)

my_theme <- bs_theme(
  bg = "bisque",
  fg = "navy",
  primary = "green",
  secondary = "red",
  base_font = font_google("Akaya Kanadaka")
)

flatly_theme <- bs_theme(version = 3, 
                         bootswatch = "flatly", 
                         base_font = font_google("Antic Didone"),
                         primary = "#663300",
                         success = "darkslategrey") 


ui <- fluidPage(theme = flatly_theme,
                shinyFeedback::useShinyFeedback(),
                navbarPage("What's in a Word?",
                           ##### 1
                           tabPanel(title = " Summary", 
                                    icon = icon("home"),
                                    mainPanel(HTML("<h2> Welcome to What’s in a Word!</h2> <br><br> By: Roupen Khanjian, Michelle Handy, Roni Shen  <h4>This Shiny App will take you on a journey through American and English literature with text and sentiment analysis. Data for this project is provided by Project GutenbergR, a publicly available repository of online book data.</h4> <br>  <h4>Click on each tab when you’re ready to start exploring what’s in a word!</h4> <br><br>")),
                                    fluidRow(column(DT::dataTableOutput("all_books"),
                                                    width = 12)
                                             ),
                                    mainPanel(HTML('<h3> References: </h3> <br><br>  
                                                   
                                                  <ol start="1">  
                                                  
                                                  <li> Silge, J., & Robinson, D. (2017). <em>Text mining with R: A tidy approach.</em> "OReilly Media, Inc.". <a href="https://www.tidytextmining.com/index.html">https://www.tidytextmining.com/index.html</a> </li>  
                                                  <li> Mohammad, S. M., & Turney, P. D. (2013). Crowdsourcing a word–emotion association lexicon. <em>Computational Intelligence</em>, 29(3), 436-465. </li> 
                                                  <li> Robinson, D. (2016). gutenbergr: Download and Process Public Domain Works from Project Gutenberg. </li>   
                                                  <li> Hu, M. and Liu, B. (2004, August). Mining and summarizing customer reviews. <em>Proceedings of the ACM SIGKDD International Conference on Knowledge Discovery & Data Mining.</em> </li>  
                                                  <li> Moving average. (2021, March 4). In Wikipedia. <a href="https://en.wikipedia.org/wiki/Moving_average">https://en.wikipedia.org/wiki/Moving_average</a> </li>  </ol> '))
                                    ),
                           ### 2
                           tabPanel(title = " Word Cloud",
                                    icon = icon("cloud"),
                                    sidebarLayout(
                                        sidebarPanel("",
                                                     textInput(inputId = "pick_book",
                                                                 label = "Enter a book title",
                                                                 value = ""
                                                                 ),
                                                     radioButtons(inputId = "wc_shape",
                                                                  label = "Choose a shape for your word cloud",
                                                                  choices = list("circle" = "circle", 
                                                                                 "diamond" = "diamond",
                                                                                 "square" = "square", 
                                                                                 "triangle" = "triangle-upright",
                                                                                 "pentagon" = "pentagon" , 
                                                                                 "star" = "star")
                                                                  ),
                                                     knobInput(inputId = "wc_count",
                                                               label = "Choose the number of words that will show up in your word cloud",
                                                               value = 50,
                                                               min = 10,
                                                               max = 80,
                                                               step = 1,
                                                               thickness = 0.31,
                                                               displayPrevious = TRUE,
                                                               lineCap = "round",
                                                               fgColor = "dodgerblue",
                                                               inputColor = "dodgerblue",
                                                               post = " words",
                                                               fontSize = "21px",
                                                               immediate = FALSE
                                                       
                                                     ),
                                                     selectInput(inputId = "pick_color",
                                                                 label = "Choose a color for your word cloud",
                                                                 choices = my_colors,
                                                                 selected = "dodgerblue"
                                                                 ),
                                                     actionButton("choose_word_cloud", "Show me!"),
                                                     fluidRow(column(DT::dataTableOutput("all_books_tab1"),
                                                                     width = 12)
                                                     )
                                                     ),
                                        mainPanel("Here’s a word cloud showing the most popular words that show up in the book you've entered. The larger the word in the cloud, the more often it appears in the text!",
                                                  plotOutput("wc_plot"))
                                    )),
                           ### 3
                           tabPanel(title = "Word-Count", 
                                    icon = icon("calculator"),
                                    sidebarLayout(
                                        sidebarPanel(  "On this page, enter a word - ANY word! - to compare how common it is in up to 4 different books. For example, enter the word 'love' and view how many times it occurs in each of your specified books!",
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
                                                                 label = "Enter a word",
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
                           tabPanel(title = "Sentiment Analysis",
                                    icon = icon("smile-beam"),
                                    sidebarLayout(
                                      sidebarPanel("On this page, enter a book title to explore the frequency of positively and negatively associated words that show up in that book.",
                                                   textInput(inputId = "pick_book3",
                                                             label = "Enter a title:",
                                                             value = ""
                                                   ),
                                                   radioButtons(inputId = "pick_sentiment",
                                                                label = "Choose a sentiment lexicon",
                                                                choices = list(
                                                                  "bing: binary, categorizes words into either positive or negative" = "bing",
                                                                  "nrc: categorizes words into one of eight emotions, and as either positive or negative affect" = "nrc"
                                                                )
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
                           tabPanel(title = "Sentiment Trajectory", 
                                    icon = icon("chart-line"),
                                    sidebarLayout(
                                      sidebarPanel("",
                                                   textInput(inputId = "pick_book4",
                                                             label = "Enter a title:",
                                                             value = ""
                                                   ),
                                                   HTML("In statistics, a moving average is a calculation used to analyze data points by creating a series of averages of different subsets of the full dataset."),
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

Wherever the graph dips and peaks are likely to be major changes occurring in the book’s plot: a dip indicates a very negative part of the story, whereas a peak indicates a very positive part of the story. <br>

Read (or Sparknote) your book to see if its plot matches up with what you find in the sentiment analysis!"),
                                                plotOutput("ts_plot"))
                                      
                                    ))
                           )

)
  
server <- function(input, output) {
    
    ### table on main page
    output$all_books <- DT::renderDataTable(
      DT::datatable({all_books_table[,1:2]},
                    options = list(lengthMenu = list(c(5, 10, 25),
                                                     c('5','10', '25')),
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
      req(input$pick_book)
      is_here <- shiny::isTruthy(all_books$title %in% c(input$pick_book,
                                                        str_to_title(input$pick_book),
                                                        str_to_lower(input$pick_book)))
      shinyFeedback::feedbackDanger("pick_book", !is_here, "Unknown title, please double check your spelling")
      req(is_here, cancelOutput = TRUE)
      
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
        count(word, title) %>% 
        slice_max(n, n = 100)
    
    })
    ### Input 2
    words_reactive <-  eventReactive(input$choose_word,{
      req(input$text)
      req(c(input$pick_book_wc1, input$pick_book_wc2,
            input$pick_book_wc3, input$pick_book_wc4))

      is_here <- shiny::isTruthy(all_books$title %in% c(input$pick_book_wc1,
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
                                                        str_to_title(input$pick_book_wc4)))
      shinyFeedback::feedbackDanger("text", !is_here, "Please enter a valid title in one of the four boxes")
      req(is_here, cancelOutput = TRUE)
      
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
        group_by(title, word) %>%
        summarise(count = n())
    })
    
    ###Input 3
    
    pb_reactive <- eventReactive(input$choose,{
      req(input$pick_book3)
      is_here <- shiny::isTruthy(all_books$title %in% c(input$pick_book3,
                                   str_to_title(input$pick_book3),
                                   str_to_lower(input$pick_book3)))
      shinyFeedback::feedbackDanger("pick_book3", !is_here, "Unknown title")
      req(is_here, cancelOutput = TRUE)
      
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
        inner_join(get_sentiments(input$pick_sentiment)) %>%
        count(word, sentiment, title, sort = TRUE) %>%
        ungroup() %>% 
        group_by(sentiment) %>%
        top_n(8) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>% 
        mutate(dict = input$pick_sentiment)
    })
    
    ###Input 4
    
    ts_reactive <- eventReactive(input$choose2,{
      req(input$pick_book4)
      is_here <- shiny::isTruthy(all_books$title %in% c(input$pick_book4,
                                                        str_to_title(input$pick_book4),
                                                        str_to_lower(input$pick_book4)))
      shinyFeedback::feedbackDanger("pick_book4", !is_here, "Unknown title, please double check your spelling")
      req(is_here, cancelOutput = TRUE)
      
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
      ggplot(data = head(wc_reactive(), input$wc_count), 
             aes(label = head(word, input$wc_count ))) +
        geom_text_wordcloud(aes(size = head(n, input$wc_count)),
                            color = input$pick_color,
                            shape = input$wc_shape) +
        scale_size_area(max_size = 10) +
        theme_void() +
        labs(caption = paste('Wordcloud for', wc_reactive()[1,"title"],
                           'with',input$wc_count, "words.")) +
        theme(plot.caption = element_text(size = 16))
      
      
    })
    
    ### output 2
    output$words_plot <- renderPlot({
        ggplot(data = words_reactive(), aes(x = title, y = count)) +
            geom_col(aes(fill = title), show.legend = FALSE) + 
            coord_flip() +
            labs(x = "Title",
                 y = "Number of occurences",
                 title = paste('Number of times the word', words_reactive()[1, "word"], 'shows up in the selected books.')) +
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
      dictionary <- as.character(pb_reactive()[1, "dict"])
      switch(dictionary,
             
           nrc = (ggplot(pb_reactive(), aes(n, word, fill = sentiment)) +
             geom_col(show.legend = FALSE) +
             facet_wrap(~sentiment, scales = "free") +
             labs(x = "Number of occurences in book",
                  title = paste("Sentiment Analysis for", 
                                pb_reactive()[1,"title"])) +
             theme_minimal() +
               theme(axis.text.x = element_text(size = 12,
                                              face = "bold",
                                              color = "black"),
                     axis.text.y = element_text(size = 10.5,
                                                face = "bold"),
                     axis.title.y = element_blank(),
                     axis.title.x = element_text(face = "bold",
                                                 color = "black",
                                                 size = 13),
                     plot.title = element_text(face = "bold",
                                               size = 19),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.major.y = element_blank(),
                     strip.text = element_text(size = 13,
                                               face = "bold",
                                               color = "black"))),
           
           bing = (ggplot(pb_reactive(), aes(n, word, fill = sentiment)) +
                     geom_col(show.legend = FALSE) +
                     facet_wrap(~sentiment, scales = "free") +
                     scale_fill_manual(values = c("firebrick", "forestgreen")) +
                     labs(x = "Number of occurences in book",
                          title = paste("Sentiment Analysis for",
                                        pb_reactive()[1,"title"])) +
                     theme_minimal() +
                     theme(axis.text = element_text(size = 15,
                                                    face = "bold",
                                                    color = "black"),
                           axis.title.y = element_blank(),
                           axis.title.x = element_text(face = "bold",
                                                       color = "black",
                                                       size = 16),
                           plot.title = element_text(face = "bold",
                                                     size = 19),
                           panel.grid.minor.y = element_blank(),
                           panel.grid.major.y = element_blank(),
                           strip.text = element_text(size = 15,
                                                     face = "bold",
                                                     color = "black")))
      )
    })  
    ### output 4
    output$ts_plot <- renderPlot({
      ggplot(data = ts_reactive(), aes(x = index, color = moving_avg)) +
        geom_line(aes(y = moving_avg), size = 1) +
        geom_abline(aes(intercept = 0.5, slope = 0), 
                    col = "black", alpha = 0.7) +
        scale_color_viridis() +
        # scale_y_continuous(breaks = seq(0, 1, 0.2)) +
        labs(x = "Novel Progression",
             y = "NRC Valence Sentiment Moving Average",
             caption =  paste('Using only the valence scores from the NRC Valence, Arousal, and Dominance\nLexicon, this graph depicts the sentiment of',  ts_reactive()[1, "title"] ,'as the novel progresses.\n\n*Note: Valence scores can range from 0 (displeasure) to 1 (pleasure), but tend to start\nabove 0.5 because books usually contain more positive than negative words.'),
             color = "Valence Score") +
        theme_minimal() +
        theme(plot.caption = element_text(size = 15,
                                         hjust = 0),
              axis.title = element_text(face = "bold",
                                          color = "black",
                                          size = 12),
              axis.text.x = element_blank(),
              axis.text.y = element_text(face = "bold",
                                         size = 11),
              
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              legend.title = element_text(size = 12,
                                          face = "bold"),
              legend.text = element_text(face = "bold",
                                         size = 11))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

