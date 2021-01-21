dashboardPage(
    dashboardHeader(title="Ice cream data"),
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            style = "position:fixed;width: 220px;", # make sidebar fixed
            selectInput(inputId = "brand_in", 
                        label = "Select a brand",
                        choices = list("Ben & Jerry's" = "bj", 
                                    "Haagen-Dazs" = "hd",
                                    "Breyers" = "breyers", 
                                    "Talenti" = "talenti"),
                        selected = "Ben & Jerry's"),
            tags$div(
                imageOutput("brandImage", width="100", height="100"), style="display:flex;justify-content:center;margin-bottom:20px;"
            ),
            menuItem(padMenuItem("Brand overview"), tabName = "overview", icon = icon(paste("home", iconsize))),
            menuItem(padMenuItem("Settings"), tabName = "settings", icon = icon(paste("cog", iconsize))),
            menuItem(padMenuItem("Reviews (NLP)"), tabName = "nlp", icon = icon(paste("comment", iconsize)), startExpanded=FALSE,
                     menuSubItem(padMenuItem("Basic statistics"), tabName = "basic-stats", icon = icon(paste("sort-alpha-down", iconsize))),
                     menuSubItem(padMenuItem("Word insights"), tabName = "words", icon = icon(paste("book", iconsize))),
                     menuSubItem(padMenuItem("Sentiment analysis"), tabName = "sentiment", icon = icon(paste("grin-hearts", iconsize))),
                     menuSubItem(padMenuItem("Topic modeling"), tabName = "topic-model", icon = icon(paste("comments", iconsize)))
                     ),
            menuItem(padMenuItem("Products"), tabName = "products", icon = icon(paste("ice-cream", iconsize)), startExpanded=FALSE,
                     menuSubItem(padMenuItem("Leaderboard"), tabName = "leaderboard", icon = icon(paste("list-ol", iconsize))),
                     menuSubItem(padMenuItem("Trends"), tabName = "trends", icon = icon(paste("line-chart", iconsize))),
                     menuSubItem(padMenuItem("Comparsion"), tabName = "comparison", icon = icon(paste("balance-scale-right", iconsize)))
            ),
            menuItem(padMenuItem("About"), tabName = "about", icon = icon(paste("info-circle", iconsize)))
        )
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(
            # By default, there is either no limit or a really large limit on textInput character length. We'll limit this using javascript.
            # Also by default, switching tabs does not scroll to top. This is a fix.
            tags$script("$(document).ready(function () {
            $('input[type=\"text\"][id^=\"group\"]').attr('maxlength', 100);
         $('a[data-toggle=\"tab\"]').bind('click', function (e) {
               $(document).scrollTop(0);
               });
               });"),
            tags$style(HTML(
            paste(
            "
            .prod_input .selectize-input {
                height: 80px;
            }
            #prod_2 .option:first-child{
                font-weight:bold;
            }
            .prod-container {
            position: relative;
            width: 170px;
            height: 170px;
            }
            .prod-overlay {
            background: rgba(255,255,255,0.8);
            position: absolute;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            text-align: center;
            display: flex;
            flex-direction: column;
            justify-content: center;
            z-index: 10;
            opacity: 0;
            transition: all 200ms ease-in-out;
            }
            .prod-overlay:hover {
            opacity: 1;
            }
            ",
            paste(".navbar, .logo {background-color:", primary_col, "!important;}")
             ))
        )),
        tabItems(
            tabItem(tabName = "overview",
                tags$h1("Brand overview"),
                tags$h2("At a glance"),
                fluidRow(
                    valueBoxOutput("n_revs"),
                    valueBoxOutput("n_prods"),
                    valueBoxOutput("rev_per_prod"),
                    valueBoxOutput("avg_rating")
                ),
                tags$h2("Most reviewed"),
                fluidRow(
                    uiOutput("most_reviewed")
                ),
                tags$h2("Ratings"),
                highchartOutput("overview_dist"),
                tags$h2("Rating trends"),
                ts_UI(inputId = "overview", with_stat = TRUE, smoothing_choice = 0),
                highchartOutput("overview_ts"),
                tags$h2("Ratings by product"),
                radioButtons(inputId = "overview_by_prod_stat",
                             label = "Statistic",
                             choices = c("Number of reviews", "Average rating"),
                             selected = "Number of reviews",
                             inline = TRUE),
                highchartOutput("overview_by_prod")
            ), 
            
            tabItem(tabName = "settings",
                    fluidRow(
                        box(width=12,
                            div(style="width:100%; text-align:center; text-decoration:underline;", tags$h1("SETTINGS")),
                            tags$h2("Preprocessing"),
                            tags$h3("Text normalization"),
                            checkboxInput(inputId = "lemmatization",
                                        label = "Lemmatization",
                                        value = TRUE),
                            tags$h3("Word filters"),
                            radioButtons(inputId = "standard_sw",
                                         label = "Standard stopwords (English)",
                                         choices = c("None", "Snowball (174 words)", "SMART (571 words)"),
                                         selected = "SMART (571 words)",
                                         inline = TRUE
                                         ),
                            checkboxGroupInput(inputId = "task_sw",
                                               label = "Task-specific stopwords",
                                               choices = list("Product names" = "name", "Product ingredients" = "ingredients"),
                                               selected = c("name", "ingredients"),
                                               inline = TRUE),
                            checkboxInput(inputId = "filter_words",
                                        label = "Filter by document frequency",
                                        value = FALSE),
                            sliderInput(inputId = "filter_words_num", 
                                        label = "Document frequency range", 
                                        min = 0,
                                        max = 1,
                                        value = c(0.01,0.5), 
                                        step = 0.01),
                            numericInput(inputId = "max_tokens",
                                         label = "Max. vocabulary size (input must be less than 20,000)",
                                         min = 1000,
                                         max = 20000,
                                         value = 5000,
                                         step = 100),
                            checkboxInput(inputId = "valence_shifters",
                                          label = "Keep valence shifters",
                                          value = FALSE),
                            actionButton("submitButton", "Update", onclick = "document.getElementById('preprocessed_rev_table').scrollIntoView({behavior: 'smooth'});"),
                            tags$p("Here's how your filter choices have impacted the amount of remaining data.", style = "margin-top: 24px;"),
                            dataTableOutput("preprocessed_rev_table") %>% withSpinner(color="#0dc5c1", proxy.height = "252px"),
                            tags$h2("Words and N-grams"),
                            checkboxInput(inputId = "within_sentence",
                                          label = "Restrict N-grams & coocurrences to within sentences",
                                          value = TRUE),
                            checkboxInput(inputId = "ngram_repeats",
                                          label = "Allow repeats in N-grams",
                                          value = FALSE),
                            tags$p("Repeated words are common in longer N-grams.
                            If this input is not toggled then 'flavor_favorite_flavor' will no longer be a valid 3-gram."),
                            radioButtons(inputId = "words_display_method",
                                        label = "Display method",
                                        choices = c("Table", "Wordcloud"),
                                        selected = "Table",
                                        inline = TRUE)
                        )
                    ),
            ),
            
            tabItem(tabName = "basic-stats",
                    tags$h1("Basic statistics"),
                    tags$h2("Word counts"),
                    selectInput(inputId = "count_stat",
                                label = "Statistic",
                                choices = c("Characters", "Words", "Sentences"),
                                selected = "Words"),
                    highchartOutput("counts_plot1"),
                    highchartOutput("counts_plot2"),
                    
                    tags$h2("Language diversity"),
                    dataTableOutput("diversity_table1"),
                    selectInput(inputId = "diversity_stat",
                                label = "Statistic",
                                choices = c("Unique words", "Unique words density"),
                                selected = "Unique words"),
                    highchartOutput("diversity_plot1"),
                    tags$h2("Rank-frequency distribution"),
                    highchartOutput("rank_freq"),
                    tags$h2("Top words", style = "margin-top: 50px;"),
                    tags$p("Note: hovering over the wordcloud shows log10(freq) rather than freq."),
                    checkboxGroupInput(inputId = "rank_freq_wordcloud_pos",
                                label = "POS",
                                choices = c("Noun" = "NOUN",
                                            "Adjective" = "ADJ", 
                                            "Verb" = "VERB",
                                            "Other" = "other"),
                                selected = c("NOUN", "ADJ", "VERB"),
                                inline = TRUE),
                    highchartOutput("rank_freq_wordcloud")
                    ),
            
            tabItem(tabName = "words",
                    tags$h1("Word insights"),
                    selectInput(inputId = "ngram_length",
                                label = "N-gram length",
                                choices = c(1, 2, 3),
                                selected = 2),
                    checkboxGroupInput(inputId = "ngram_pos",
                                       label = "POS",
                                       choices = c("Noun" = "NOUN",
                                                   "Adjective" = "ADJ", 
                                                   "Verb" = "VERB",
                                                   "Other" = "other"),
                                       selected = c("NOUN", "ADJ", "VERB"),
                                       inline = TRUE),
                    tags$h2("Top N-grams"),
                    conditionalPanel('input.words_display_method=="Table"', dataTableOutput("ngram_table")),
                    conditionalPanel('input.words_display_method=="Wordcloud"', highchartOutput("ngram_wordcloud")),
                    tags$h2("Positive and negative N-grams"),
                    highchartOutput("pos_neg_comparison"),
                    tags$h2("N-gram tracker"),
                    textInput(inputId = "group1",
                              label = "Group 1",
                              value = "favorite flavor, amazing"),
                    textInput(inputId = "group2",
                              label = "Group 2",
                              value = "bad batch, disappointed"),
                    textInput(inputId = "group3",
                              label = "Group 3",
                              value = "expensive, pricey"),
                    ts_UI(inputId = "ngram", with_stat = FALSE, smoothing_choice = 3),
                    actionButton("ngram_button", "Update"),
                    highchartOutput("ngram_tracker"),
                    tags$h2("Noun-adjective coocurrences"),
                    selectInput(inputId = "cooccurrence_distance",
                                label = "Coocurrence distance",
                                choices = c(3, 2, 1, -1, -2 ,-3),
                                selected = -1),
                    tags$p("Coocurrence distance is the distance of the adjective from the noun.
                           A distance of -2 means the adjective is two words BEFORE the noun, whereas
                           a distance of 2 means the adjective is two words AFTER the noun.
                           Note: distance calculated after preprocessing may be much different
                           depending on the strictness of the filters chosen."),
                    conditionalPanel('input.words_display_method=="Table"', dataTableOutput("cooccurrences_table")),
                    conditionalPanel('input.words_display_method=="Wordcloud"', highchartOutput("cooccurrences_wordcloud")),
                    tags$h2("Dependency parsing"),
                    conditionalPanel('input.words_display_method=="Table"', dataTableOutput("dep_parse_table")),
                    conditionalPanel('input.words_display_method=="Wordcloud"', highchartOutput("dep_parse_wordcloud"))
                    ),
            
            tabItem(tabName = "sentiment",
                    tags$h1("Sentiment analysis"),
                    tags$h2("Common sentiments"),
                    highchartOutput("sentiment_radar"),
                    tags$h2("Reviews by sentiment"),
                    dataTableOutput("sentiment_table"),
                    tags$h2("Sentiment distribution"),
                    highchartOutput("sent_dist"),
                    tags$h2("Sentiment and star rating"),
                    highchartOutput("sent_dist_by_stars"),
                    tags$h2("Sentiment trends"),
                    ts_UI("sent", with_stat = FALSE, smoothing_choice = 3),
                    highchartOutput("sent_ts"),
                    tags$h2("Sentimental N-grams"),
                    selectInput(inputId = "ngram_length_valShift",
                                label = "N-gram length",
                                choices = c(1, 2, 3),
                                selected = 2),
                    tags$h3("Valence shifters"),
                    tags$p("Valence shifters play an important role in sentiment analysis. There are four types, which we illustrate by example. 
                    1. I do *not* like this flavor (Negator). 2. I *absolutely* love this flavor (Amplifier). 
                    3. I *barely* like this flavor (De-amplifier). 
                    4. I like this flavor, *but* it is way too expensive (Adversative Conjunction)."),
                    textOutput("num_valShift"),
                    tags$br(),
                    checkboxGroupInput(inputId = "valShift_type",
                                label = "Valence shifter type",
                                choices = c("Negator" = 1, 
                                            "Amplifier" = 2,
                                            "De-amplifier" = 3,
                                            "Adversative Conjunction" = 4),
                                selected = c(1,2,3,4),
                                inline = TRUE),
                    conditionalPanel('input.words_display_method=="Table"', dataTableOutput("ngram_valShift_table")),
                    conditionalPanel('input.words_display_method=="Wordcloud"', highchartOutput("ngram_valShift_wordcloud")),
                    tags$h3("Polarized words"),
                    sliderInput(inputId = "polarity_range_filter",
                                label = "Polarity value",
                                min = -5,
                                max = 5,
                                value = c(-5, -2),
                                step = 1),
                    conditionalPanel('input.words_display_method=="Table"', dataTableOutput("ngram_polarity_table")),
                    conditionalPanel('input.words_display_method=="Wordcloud"', highchartOutput("ngram_polarity_wordcloud"))
                    ),
            
            tabItem(tabName = "topic-model",
                    tags$h1("Topic modeling"),
                    selectInput(inputId = "cluster_method",
                                label = "Clustering method",
                                choices = list("Dirichlet Mixture Model (GSDMM)" = "dmm", 
                                               "K-means" = "kmeans", 
                                               "Manual clustering" = "manual"),
                                selected = "Dirichlet Mixture Model (GSDMM)"),
                    uiOutput("num_clusters"),
                    actionButton("topic_button", "Run"), 
                    tags$h2("Visualizing clusters in 2D"),
                    highchartOutput("topic_model_vectorspace"),
                    tags$h2("Cluster sizes"),
                    highchartOutput("cluster_size_dist"),
                    tags$h2("Cluster sizes over time"),
                    ts_UI("cluster_size", with_stat = FALSE, smoothing_choice = 0),
                    highchartOutput("cluster_size_ts"),
                    tags$h2("Important words"),
                    dataTableOutput("topic_model_words")),
            
            tabItem(tabName = "leaderboard",
                    tags$h1("Product leaderboard"),
                    DT::dataTableOutput("prod_leaderboard")
            ),
            
            tabItem(tabName = "trends",
                    tags$h1("Product trends"),
                    sliderInput(inputId = "trends_month_range", 
                                label = "N month performance", 
                                min = 1,
                                max = 12,
                                value = 3, 
                                step = 1),
                    uiOutput("prod_trends_text"),
                    checkboxGroupInput(inputId = "trends_column_options",
                                       label = "Metrics displayed (suffix)",
                                       choices = c("Values (0, 1)" = "value",
                                                   "Raw increase (diff)" = "diff",
                                                   "Percent increase (rel)" = "rel"),
                                       selected = c("value", "rel"),
                                       inline = TRUE),
                    DT::dataTableOutput("prod_trends_table")),
            
            tabItem(tabName = "comparison",
                    tags$h1("Product comparison"),
                    fluidRow(
                        column(width = 6, 
                               uiOutput("prod_1")),
                        column(width = 6,
                               uiOutput("prod_2"))
                    ),
                    fluidRow(
                        column(width = 6,
                               imageOutput("prod_1_img", width="170", height="170")),
                        column(width = 6,
                               imageOutput("prod_2_img", width="170", height="170"))
                    ),
                    tags$h2("At a glance"),
                    dataTableOutput("prod_comp_table"),
                    tags$h2("Ratings"),
                    highchartOutput("prod_comp_dist"),
                    tags$h2("Rating trends"),
                    ts_UI(inputId = "prod", with_stat = TRUE, smoothing_choice=0),
                    highchartOutput("prod_comp_ts"),
                    tags$h2("N-grams"),
                    selectInput(inputId = "prod_comp_ngram_length",
                                label = "N-gram length",
                                choices = c(1, 2, 3),
                                selected = 2),
                    highchartOutput("prod_comp_ngrams"),
                    tags$h2("Common sentiments"),
                    highchartOutput("prod_comp_sentiment")
            ),
            
            tabItem(tabName = "about",
                    tags$h1("About"),
                    tags$h2("Soon to come..."))
        ) # end all tab items
    )
)
