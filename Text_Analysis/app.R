library(shiny) # Web Application Framework for R, CRAN v1.6.0
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0
library(tidytext) # Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools, CRAN v0.3.0
library(here) # A Simpler Way to Find Your Files, CRAN v1.0.1
library(gutenbergr) # Download and Process Public Domain Works from Project Gutenberg, CRAN v0.2.0
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data, CRAN v2.1.0
library(shinythemes) # Themes for Shiny, CRAN v1.2.0
library(textdata) # Download and Load Various Text Datasets, CRAN v0.4.1
library(slider) # Sliding Window Functions, CRAN v0.1.5
library(viridis) # Default Color Maps from 'matplotlib', CRAN v0.5.1
library(DT) # A Wrapper of the JavaScript Library 'DataTables', CRAN v0.17
library(ggwordcloud) # A Word Cloud Geom for 'ggplot2', CRAN v0.5.0
library(shinyWidgets) # Custom Inputs Widgets for Shiny, CRAN v0.5.7
library(bslib) # Custom 'Bootstrap' 'Sass' Themes for 'shiny' and 'rmarkdown', CRAN v0.2.4
library(shinyFeedback) # Display User Feedback in Shiny Apps, CRAN v0.3.0

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

### test id top 100 downloaded books work

# Need this for shinyapps.io
# here("Text_Analysis", "www", "top_ebooks_30.txt")
# "www/top_ebooks_30.txt"
top_100 <- read_table("www/top_ebooks_30.txt",
                      col_names = FALSE)  %>%
  clean_names() %>%
  mutate(title = str_split_fixed(x1, "by", 2)) %>% 
  select(title) 

test <- top_100$title
test <- test[1:100]
test <- enframe(test)

top_100 <- test %>% 
  select(value) %>% 
  rename(title = "value") 

top_100$title <- str_squish(top_100$title)

top_100_join <- top_100 %>% 
  inner_join(all_books, by = "title") %>% 
  filter(title != "The Masque of the Red Death") %>% 
  filter(title != "The Works of Edgar Allan Poe — Volume 2") %>% 
  filter(title != "A Modest Proposal")

# double checked and all top 50 books works!
# Table for summary page
top_100_table <- top_100_join %>% 
  rename(Title = title,
         `Author (Last, First)` = author)

###

# Theme
flatly_theme <- bs_theme(version = 3, 
                         bootswatch = "flatly", 
                         base_font = font_google("Fondamento", wght = "400"),
                         primary = "#663300",
                         success = "lightslategrey") 

### NRC lexicons

# nrc_emotions <- get_sentiments("nrc")
# nrc_vad <- lexicon_nrc_vad()

# Bing lexicons
bing_lexicon <- get_sentiments("bing")


# Need this for shinyapps.io
# write_csv(nrc_emotions, "nrc_emotions.csv")
# write_csv(nrc_vad, "nrc_vad.csv")

nrc_emotions <- read_csv("www/nrc_emotions.csv")
nrc_vad  <- read_csv("www/nrc_vad.csv")

# App UI
ui <- fluidPage(theme = flatly_theme,
                shinyFeedback::useShinyFeedback(),
                navbarPage("What's in a Word?",
                           ##### 1
                           tabPanel(title = " Summary", 
                                    icon = icon("home"),
                                    mainPanel(HTML("<h2> Welcome to What’s in a Word!</h2> <br> 
                                                   By: Roupen Khanjian, Michelle Handy, Roni Shen <br><br>"),
                                              img(src = "library.jpg",
                                                  height = "400px", width = "800px",
                                                  class="center"),
                                              HTML("<br><br>"),
                                              HTML("<h4>This Shiny App will take you on a journey through 
                                                   English and American literature with text and sentiment analysis. 
                                                   Data for this project is provided by Project GutenbergR, a publicly 
                                                   available repository of online book data. Use the search table on each 
                                                   page to double check that the book you would like to analyze is in the
                                                   Project GutenbergR database (which contains over 12,000 works of American 
                                                   and English literature alone!). Or you can let the app choose some books 
                                                   for you from a list of the top 50 most downloaded books from the Project
                                                   Gutenberg website by typing the word “random” into the text box! </h4> <br>
                                                   <h4>Click on each tab when you’re ready to start exploring
                                                   what’s in a word! </h4> <br><br>"),
                                              HTML("<br><br>")),
                                    fluidRow(column(DT::dataTableOutput("all_books"),
                                                    width = 11)
                                             ),
                                    fluidRow(column(DT::dataTableOutput("top_100"),
                                                    width = 11)
                                    ),
                                    mainPanel(HTML('<h3> References: </h3> <br><br>  
                                                   
                                                  <ol start="1">  
                                                  
                                                  <li> Silge, J., & Robinson, D. (2017). <em>Text mining with R: 
                                                  A tidy approach.</em> "OReilly Media, Inc.". 
                                                  <a href="https://www.tidytextmining.com/index.html">
                                                  https://www.tidytextmining.com/index.html</a> </li>  
                                                  
                                                  <li> Mohammad, S. M., & Turney, P. D. (2013). Crowdsourcing a word–emotion
                                                  association lexicon. <em>Computational Intelligence</em>, 29(3), 436-465. </li> 
                                                  
                                                  <li> Robinson, D. (2016). gutenbergr: Download and Process Public Domain Works 
                                                  from Project Gutenberg. </li>   
                                                  
                                                  <li> Hu, M. and Liu, B. (2004, August). Mining and summarizing customer reviews. 
                                                  <em>Proceedings of the ACM SIGKDD International Conference on Knowledge 
                                                  Discovery & Data Mining.</em> </li>  
                                                  
                                                  <li> Moving average. (2021, March 4). In Wikipedia. <a href="
                                                  https://en.wikipedia.org/wiki/Moving_average">https://en.wikipedia.
                                                  org/wiki/Moving_average</a> </li>
                                                  
                                                  <li> Photograph of Real Gabinete Portugues de Leitura in Rio de Janeiro, Brazil by Massimo Listri <em>theguardian.com</em>  <a href = "https://www.theguardian.com/artanddesign/gallery/2018/jul/31/libraries-world-most-beautiful-in-pictures#img-6">link</a> </li> 
                                                  </ol> '))
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
                                        mainPanel(HTML("<h4>Here’s a word cloud showing the most popular words that
                                                       show up in the book you've entered. The larger the word in the 
                                                       cloud, the more often it appears in the text!</h4>"),
                                                  plotOutput("wc_plot",
                                                             height = "700px"))
                                    )),
                           ### 3
                           tabPanel(title = "Word-Count", 
                                    icon = icon("calculator"),
                                    sidebarLayout(
                                        sidebarPanel(  "",
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
                                                                 label = HTML("<br><br>Enter a word"),
                                                                 value = ""),
                                                       actionButton("choose_word", "Show me!"),
                                                       fluidRow(column(DT::dataTableOutput("all_books_tab2"),
                                                                       width = 12)
                                                       )
                                                     
                                        ), 
                                                     
                                
                                        mainPanel(HTML("<h4>On this page, enter a word - ANY word! - to 
                                                       compare how common it is in up to 4 different books. 
                                                       For example, enter the word 'love' and view how many times 
                                                       it occurs in each of your specified books! <br> If you type in the word 
                                                       'random' in the first title box, word counts from up to 4 randomly selected books will appear. </h4>"),
                                                  plotOutput("words_plot",
                                                             height = "700px"))
                                    )
                                    ),
                           ### 4
                           tabPanel(title = "Sentiment Analysis",
                                    icon = icon("smile-beam"),
                                    sidebarLayout(
                                      sidebarPanel("",
                                                   textInput(inputId = "pick_book3",
                                                             label = "Enter a title:",
                                                             value = ""
                                                   ),
                                                   radioButtons(inputId = "pick_sentiment",
                                                                label = "Choose a sentiment lexicon",
                                                                choices = list(
                                                                  "bing: categorizes words into either 
                                                                  positive or negative affect." = "bing",
                                                                  "nrc: categorizes words into one of eight emotions, 
                                                                  and as either positive or negative affect." = "nrc"
                                                                )
                                                                ),
                                                   actionButton("choose", "Show me!"),
                                                   fluidRow(column(DT::dataTableOutput("all_books_tab3"),
                                                                   width = 12)
                                                   )
                                      ),
                                      
                                      
                                      mainPanel(HTML("<h4>On this page, enter a book title to 
                                                     explore the frequency of positively and negatively 
                                                     associated words that show up in that book.</h4>"),
                                                plotOutput("pb_plot",
                                                           height = "700px"))
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
                                                   HTML("In statistics, a moving average is a calculation used to 
                                                        analyze data points by creating a series of averages of different subsets of the full dataset."),
                                                   sliderInput(inputId = "moving_avg",
                                                               label = "Choose a moving average window:",
                                                               min = 51, 
                                                               max = 1051, 
                                                               value = 501,
                                                               step = 10
                                                   ),
                                                   actionButton(inputId = "choose2", 
                                                                label = "Show me!"),
                                                   fluidRow(column(DT::dataTableOutput("all_books_tab4"),
                                                                   width = 12)
                                                   )
                                      ),
                                      
                                      
                                      mainPanel(HTML("<h4>Type in a book from the Project GutenbergR database 
                                                     below and see how sentiment changes throughout the novel! <br>

Wherever the graph dips and peaks are likely to be major changes occurring in the book’s plot: a 
dip indicates a very negative part of the story, whereas a peak indicates a very positive part of the story. <br>

Read (or Sparknote) your book to see if its plot matches up with what you find in the sentiment analysis!</h4>"),
                                                plotOutput("ts_plot",
                                                           height = "700px"))
                                      
                                    )),
                            ### 6
                            tabPanel(title = HTML("</a></li><li><a href = 
                                                  'https://github.com/khanjian/Shiny-text-analysis'><i 
                                                  class='fa fa-github' style='color:white'></i>")
                                     )
                           )

)
 
tags$li(a(href = 'https://github.com/BrennieDev/OHI_shiny_app',
          icon("github"),
          title = "GitHub"),
        class = "dropdown")

 
server <- function(input, output) {
    
    ### table on main page
    output$all_books <- DT::renderDataTable(
      DT::datatable({all_books_table[,1:2]},
                    options = list(lengthMenu = list(c(5, 10, 25),
                                                     c('5','10', '25')),
                                   dom = 'ltipr', searchHighlight = TRUE),
                    style = "bootstrap",
                    filter = "top",
                    class = 'cell-border stripe',
                    rownames = FALSE,
                    caption = HTML('<h4> <p style="color:#663300;">Start searching below to
                                   check if your book exists in the Project GutenbergR database:</p> </h4>')))
    
    ### table on main page 2
    output$top_100 <- DT::renderDataTable(
      DT::datatable({top_100_table[,1:2]},
                    options = list(lengthMenu = list(c(5),
                                                     c('5')),
                                   dom = 'tipr', searchHighlight = TRUE),
                    style = "bootstrap",
                    filter = "top",
                    class = 'cell-border stripe',
                    rownames = FALSE,
                    caption = HTML('<h4><p style="color:#663300;">Below is the table of top 
                                   50 books that will be chosen at random when you type in the 
                                   word "random" in the title box:</p></h4>')))
    
    ### table on wc page
    output$all_books_tab1 <- DT::renderDataTable(
      DT::datatable({all_books_table[,1:2]},
                    options = list(lengthMenu = list(c(5),c('5')),
                                   dom = 'tipr', searchHighlight = TRUE),
                    style = "bootstrap",
                    filter = "top",
                    class = 'cell-border stripe',
                    rownames = FALSE))
    
    ### table on word count page
    output$all_books_tab2 <- DT::renderDataTable(
      DT::datatable({all_books_table[,1:2]},
                    options = list(lengthMenu = list(c(5),c('5')),
                                   dom = 'tipr', searchHighlight = TRUE),
                    style = "bootstrap",
                    filter = "top",
                    class = 'cell-border stripe',
                    rownames = FALSE))
    
    
    ### table on sentimen page
    output$all_books_tab3 <- DT::renderDataTable(
      DT::datatable({all_books_table[,1:2]},
                    options = list(lengthMenu = list(c(5),c('5')),
                                   dom = 'tipr', searchHighlight = TRUE),
                    style = "bootstrap",
                    filter = "top",
                    class = 'cell-border stripe',
                    rownames = FALSE))
    
    ### table on ts page
    output$all_books_tab4 <- DT::renderDataTable(
      DT::datatable({all_books_table[,1:2]},
                    options = list(lengthMenu = list(c(5),c('5')),
                                   dom = 'tipr', searchHighlight = TRUE),
                    style = "bootstrap",
                    filter = "top",
                    class = 'cell-border stripe',
                    rownames = FALSE))

    
    ### Input 1
    wc_reactive <- eventReactive(input$choose_word_cloud,{
      req(input$pick_book)
      # error message
      is_here <- shiny::isTruthy((all_books$title %in% c(input$pick_book,
                                                        str_to_title(input$pick_book),
                                                        str_to_lower(input$pick_book))
                                  ) | (input$pick_book == "random")
                                 )
      shinyFeedback::feedbackDanger("pick_book", !is_here, "Please enter a valid title")
      req(is_here, cancelOutput = TRUE)
      
      if(input$pick_book == "random"){
        top_100_join %>% 
          select(gutenberg_id) %>%
          filter(gutenberg_id == sample(gutenberg_id, 1)) %>% 
          as.numeric() %>%
          gutenberg_download(meta_fields = "title",
                             strip = TRUE,
                             mirror = "http://mirrors.xmission.com/gutenberg/") %>%
          unnest_tokens(word, text) %>%
          anti_join(stop_words) %>%
          mutate(word = str_extract(word, "[a-z']+")) %>% 
          count(word, title) %>% 
          slice_max(n, n = 100)
      }
      else{
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
      }
   
    
    })
    
    ### Input 2
    words_reactive <-  eventReactive(input$choose_word,{
      req(input$text)
      # error message
      req(c(input$pick_book_wc1, input$pick_book_wc2,
            input$pick_book_wc3, input$pick_book_wc4))

      is_here <- shiny::isTruthy((all_books$title %in% c(input$pick_book_wc1,
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
                                                        str_to_title(input$pick_book_wc4))
                                  ) | (c(input$pick_book_wc1, input$pick_book_wc2,
                                         input$pick_book_wc3, input$pick_book_wc4) %in% "random")  
                                 )
      shinyFeedback::feedbackDanger("text", !is_here, "Please enter a valid title in one of the four boxes")
      req(is_here, cancelOutput = TRUE)

    
    if(c(input$pick_book_wc1, input$pick_book_wc2,
         input$pick_book_wc3, input$pick_book_wc4) %in% "random") {
      top_100_join %>% 
        select(gutenberg_id) %>%
        filter(gutenberg_id %in% sample(gutenberg_id, 4)) %>% 
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
    }
    
    else{
      
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
    }
})

    ###Input 3
    
    pb_reactive <- eventReactive(input$choose,{
      req(input$pick_book3)
      # error message
      is_here <- shiny::isTruthy((all_books$title %in% c(input$pick_book3,
                                   str_to_title(input$pick_book3),
                                   str_to_lower(input$pick_book3))
                                  ) | (input$pick_book3 == "random")
                                 )
      shinyFeedback::feedbackDanger("pick_book3", !is_here, "Please enter a valid title")
      req(is_here, cancelOutput = TRUE)
  
      if(input$pick_sentiment == "bing"){
        lexicon_choice = bing_lexicon
      }
      else{
        lexicon_choice = nrc_emotions
      }
      
      if(input$pick_book3 == "random"){
        top_100_join %>% 
          select(gutenberg_id) %>%
          filter(gutenberg_id == sample(gutenberg_id, 1)) %>% 
          as.numeric() %>%
          gutenberg_download(meta_fields = "title",
                                             strip = TRUE,
                                             mirror = "http://mirrors.xmission.com/gutenberg/") %>%
          unnest_tokens(word, text) %>%
          anti_join(stop_words) %>%
          mutate(word = str_extract(word, "[a-z']+")) %>%
          inner_join(lexicon_choice) %>%
          count(word, sentiment, title, sort = TRUE) %>%
          ungroup() %>% 
          group_by(sentiment) %>%
          top_n(8) %>%
          ungroup() %>%
          mutate(word = reorder(word, n)) %>% 
          mutate(dict = input$pick_sentiment)
      }
      else{
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
        inner_join(lexicon_choice) %>%
        count(word, sentiment, title, sort = TRUE) %>%
        ungroup() %>% 
        group_by(sentiment) %>%
        top_n(8) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>% 
        mutate(dict = input$pick_sentiment)
      }
    })
    
    ###Input 4
    
    ts_reactive <- eventReactive(input$choose2,{
      req(input$pick_book4)
      # error message
      is_here <- shiny::isTruthy((all_books$title %in% c(input$pick_book4,
                                                        str_to_title(input$pick_book4),
                                                        str_to_lower(input$pick_book4))
                                  ) | (input$pick_book4 == "random")
                                 )
      shinyFeedback::feedbackDanger("pick_book4", !is_here, "Please enter a valid title")
      req(is_here, cancelOutput = TRUE)
      
      if(input$pick_book4 == "random"){
        top_100_join %>% 
          select(gutenberg_id) %>%
          filter(gutenberg_id == sample(gutenberg_id, 1)) %>% 
          as.numeric() %>%
          gutenberg_download(meta_fields = "title",
                             strip = TRUE,
                             mirror = "http://mirrors.xmission.com/gutenberg/") %>%
          unnest_tokens(word, text) %>%
          anti_join(stop_words) %>%
          mutate(word = str_extract(word, "[a-z']+")) %>%
          inner_join(nrc_vad, by = c("word" = "Word")) %>%
          drop_na(Valence) %>% 
          mutate(index = seq(1, length(word) ,1)) 
      }
      
      else{
      
      all_books %>%
        filter(title %in% c(input$pick_book4, 
                            str_to_title(input$pick_book4),
                            str_to_lower(input$pick_book4))) %>%  
        select(gutenberg_id) %>%
        as.numeric() %>%
        gutenberg_download(meta_fields = "title",
                           strip = TRUE,
                           mirror = "http://mirrors.xmission.com/gutenberg/") %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        mutate(word = str_extract(word, "[a-z']+")) %>%
        inner_join(nrc_vad, by = c("word" = "Word")) %>%
        drop_na(Valence) %>% 
        mutate(index = seq(1, length(word) ,1)) 
      }
    })

    ### output 1
    
    output$wc_plot <- renderPlot({
      ggplot(data = head(wc_reactive(), input$wc_count), 
             aes(label = head(word, input$wc_count ))) +
        geom_text_wordcloud(aes(size = head(n, input$wc_count)),
                            color = input$pick_color,
                            shape = input$wc_shape) +
        scale_size_area(max_size = 13) +
        theme_void() +
        labs(caption = paste('Wordcloud for', wc_reactive()[1,"title"],
                           'with',input$wc_count, "words.")) +
        theme(plot.caption = element_text(size = 16))
      
      
    })
    
    
    ### output 2
    output$words_plot <- renderPlot({
        ggplot(data = words_reactive(), aes(x = fct_reorder(title, count), y = count)) +
            geom_col(aes(fill = title), show.legend = FALSE) + 
            coord_flip() +
            labs(x = "Title",
                 y = "Number of occurences",
                 title = paste('Number of times the word', words_reactive()[1, "word"], 
                               'shows up in the selected books.')) +
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
           nrc = (ggplot(pb_reactive(), aes(n, word, fill = n)) +
             geom_col(color = "white") +
             facet_wrap(~sentiment, scales = "free",
                        nrow = 2) +
             scale_fill_gradient(low = "lightblue", high = "midnightblue") +
             labs(x = "Number of occurences in book",
                  title = paste("Sentiment Analysis for", 
                                pb_reactive()[1,"title"])) +
             theme_minimal() +
               theme(axis.text.x = element_text(size = 12,
                                              face = "bold",
                                              color = "black"),
                     axis.text.y = element_text(size = 10.25,
                                                face = "bold",
                                                color = "black"),
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
                                               color = "black"),
                     legend.title = element_blank(),
                     legend.text = element_text(face = "bold",
                                                size = 11,
                                                color = "black"),
                     legend.position = "bottom")),
           
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
      ts_reactive() %>% 
        mutate(moving_avg = as.numeric(slide(Valence, # added moving average option
                                             mean, 
                                             .before = (input$moving_avg - 1)/2 , 
                                             .after = (input$moving_avg - 1)/2 ))) %>% 
      
      ggplot(aes(x = index, color = moving_avg)) +
        geom_line(aes(y = moving_avg), size = 1) +
        geom_abline(aes(intercept = 0.5, slope = 0), 
                    col = "black", alpha = 0.7) +
        
        scale_color_viridis() +
        labs(x = "Novel Progression",
             y = "NRC Valence Sentiment Moving Average",
             caption =  paste('Using only the valence scores from the NRC Valence, Arousal, and Dominance\nLexicon, this graph depicts the sentiment of',  ts_reactive()[1, "title"] ,'as the novel progresses.\n\n*Note: Valence scores can range from 0 (displeasure) to 1 (pleasure), but tend to start\nabove 0.5 because books usually contain more positive than negative words.'),
             color = "Valence Score") +
        theme_minimal() +
        theme(plot.caption = element_text(size = 15,
                                          hjust = 0),
              axis.title = element_text(face = "bold",
                                        color = "black",
                                        size = 12.5),
              axis.text.x = element_blank(),
              axis.text.y = element_text(face = "bold",
                                         size = 12.5),
              
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              legend.title = element_text(size = 12.5,
                                          face = "bold"),
              legend.text = element_text(face = "bold",
                                         size = 12))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)


