# LITE VERSION

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
            tags$script(
                      "function select_insight(el) {
                      var tab = el.getAttribute('tab');
                      Shiny.onInputChange(tab + '_insight', el.getAttribute('target_insight'));
                      // $(window).scrollTop( $('#' + tab + '_plot').offset().top);
                      }"
            ),
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
                    Map(function(i) {
                        uiOutput(paste0("top_prod_wrapper_", i))
                        }, 1:3)
                ),
                tags$h2("Insights"),
                tags$ul(
                    tags$li(tags$a(
                        href="###", onclick="select_insight(this)", tab = "overview", target_insight = "overview_dist", "Ratings")),
                    tags$li(tags$a(
                        href="###", onclick="select_insight(this)", tab = "overview", target_insight = "overview_ts", "Rating trends")),
                    tags$li(tags$a(
                        href="###", onclick="select_insight(this)", tab = "overview", target_insight = "overview_by_prod", "Ratings by product"))),
                uiOutput("overview_header"),
                uiOutput("overview_plot")
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
                            table_wrapper("preprocessed_rev_table", num_rows = 4),
                            tags$h2("Words and N-grams"),
                            checkboxInput(inputId = "within_sentence",
                                          label = "Restrict N-grams & cooccurrences to within sentences",
                                          value = TRUE),
                            checkboxInput(inputId = "ngram_repeats",
                                          label = "Allow repeats in N-grams",
                                          value = FALSE),
                            tags$p("Repeated words are common in longer N-grams.
                            If this input is not toggled then 'flavor_favorite_flavor' will no longer be a valid 3-gram.")
                        )
                    ),
            ),
            
            tabItem(tabName = "basic-stats",
                    tags$h1("Basic statistics"),
                    tags$h2("Insights"),
                    tags$ul(
                        tags$li("Word counts"),
                        tags$ul(
                            tags$li(tags$a(
                                href="###", onclick="select_insight(this)", tab = "basic-stats", target_insight = "counts_plot1", "All reviews")),
                            tags$li(tags$a(
                                href="###", onclick="select_insight(this)", tab = "basic-stats", target_insight = "counts_plot2", "By star rating"))
                        ),
                        tags$li("Language diversity"),
                        tags$ul(
                            tags$li(tags$a(
                                href="###", onclick="select_insight(this)", tab = "basic-stats", target_insight = "diversity_table1", "Aggregate metrics")),
                            tags$li(tags$a(
                                href="###", onclick="select_insight(this)", tab = "basic-stats", target_insight = "diversity_plot1", "Individual reviews"))
                        ),
                        tags$li(tags$a(
                            href="###", onclick="select_insight(this)", tab = "basic-stats", target_insight = "rank_freq", "Rank-frequency distribution")),
                        tags$li(tags$a(
                            href="###", onclick="select_insight(this)", tab = "basic-stats", target_insight = "top_words_wordcloud", "Top words"))),
                    uiOutput("basic-stats_header"),
                    uiOutput("basic-stats_plot")
                    ),
            
            tabItem(tabName = "words",
                    tags$h1("Word insights"),
                    tags$h2("Insights"),
                    tags$ul(
                        tags$li("N-grams"),
                        tags$ul(
                            tags$li("Top N-grams",
                                    "(",
                                    tags$a(href="###", onclick="select_insight(this)", tab = "words", target_insight = "ngram_table", "Table"),
                                    "|",
                                    tags$a(href="###", onclick="select_insight(this)", tab = "words", target_insight = "ngram_wordcloud", "Wordcloud"),
                                    ")"),
                            tags$li(tags$a(href="###", onclick="select_insight(this)", tab = "words", target_insight = "pos_neg_comparison", "Positive and negative N-grams")),
                            tags$li(tags$a(href="###", onclick="select_insight(this)", tab = "words", target_insight = "ngram_tracker", "N-gram tracker"))
                        ),
                        tags$li("Noun-adjective cooccurrences",
                                "(",
                                tags$a(href="###", onclick="select_insight(this)", tab = "words", target_insight = "cooccurrences_table", "Table"),
                                "|",
                                tags$a(href="###", onclick="select_insight(this)", tab = "words", target_insight = "cooccurrences_wordcloud", "Wordcloud"),
                                ")"),
                        tags$li("Dependency parsing",
                                "(",
                                tags$a(href="###", onclick="select_insight(this)", tab = "words", target_insight = "dep_parse_table", "Table"),
                                "|",
                                tags$a(href="###", onclick="select_insight(this)", tab = "words", target_insight = "dep_parse_wordcloud", "Wordcloud"),
                                ")")
                    ),
                    uiOutput("words_header"),
                    uiOutput("words_plot")
                    ),
            
            tabItem(tabName = "sentiment",
                    tags$h1("Sentiment analysis"),
                    tags$h2("Insights"),
                    tags$ul(
                        tags$li(tags$a(href="###", onclick="select_insight(this)", tab = "sentiment", target_insight = "sentiment_radar", "Common sentiments")),
                        tags$li("Sentiment modeling",
                                tags$ul(
                                    tags$li(tags$a(href="###", onclick="select_insight(this)", tab = "sentiment", target_insight = "sentiment_table", "Reviews by sentiment")),
                                    tags$li(tags$a(href="###", onclick="select_insight(this)", tab = "sentiment", target_insight = "sent_dist", "Sentiment distribution")),
                                    tags$li(tags$a(href="###", onclick="select_insight(this)", tab = "sentiment", target_insight = "sent_dist_by_stars", "Sentiment and star rating")),
                                    tags$li(tags$a(href="###", onclick="select_insight(this)", tab = "sentiment", target_insight = "sent_ts", "Sentiment trends"))
                                )),
                        tags$li("Sentimental N-grams",
                                tags$ul(
                                    tags$li("Valence shifters",
                                            "(",
                                            tags$a(href="###", onclick="select_insight(this)", tab = "sentiment", target_insight = "ngram_valShift_table", "Table"),
                                            "|",
                                            tags$a(href="###", onclick="select_insight(this)", tab = "sentiment", target_insight = "ngram_valShift_wordcloud", "Wordcloud"),
                                            ")"),
                                    tags$li("Polarized words",
                                            "(",
                                            tags$a(href="###", onclick="select_insight(this)", tab = "sentiment", target_insight = "ngram_polarity_table", "Table"),
                                            "|",
                                            tags$a(href="###", onclick="select_insight(this)", tab = "sentiment", target_insight = "ngram_polarity_wordcloud", "Wordcloud"),
                                            ")")
                                    )
                                )
                        ),
                    uiOutput("sentiment_header"),
                    uiOutput("sentiment_plot")
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
                    tags$h2("Insights"),
                    tags$ul(
                        tags$li(tags$a(
                            href="###", onclick="select_insight(this)", tab = "topic-model", target_insight = "topic_model_vectorspace", "Visualizing clusters in 2D")),
                        tags$li(tags$a(
                            href="###", onclick="select_insight(this)", tab = "topic-model", target_insight = "cluster_size_dist", "Cluster sizes")),
                        tags$li(tags$a(
                            href="###", onclick="select_insight(this)", tab = "topic-model", target_insight = "cluster_size_ts", "Cluster sizes over time")),
                        tags$li(tags$a(
                            href="###", onclick="select_insight(this)", tab = "topic-model", target_insight = "topic_model_words", "Important words"))
                        ),
                    uiOutput("topic-model_header"),
                    uiOutput("topic-model_plot")
                ),
            
            tabItem(tabName = "leaderboard",
                    tags$h1("Product leaderboard"),
                    table_wrapper("prod_leaderboard", 
                                  num_rows = 60,
                                  pagination = TRUE,
                                  filter = TRUE,
                                  bMargin = 48)
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
                    table_wrapper("prod_trends_table", 
                                  num_rows = 60,
                                  pagination = TRUE,
                                  filter = TRUE,
                                  bMargin = 48)
                    ),
            
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
                    actionButton("prod_comp_button", "Run"),
                    tags$h2("Insights"),
                    tags$ul(
                        tags$li(tags$a(
                            href="###", onclick="select_insight(this)", tab = "comparison", target_insight = "prod_comp_table", "At a glance")),
                        tags$li(tags$a(
                            href="###", onclick="select_insight(this)", tab = "comparison", target_insight = "prod_comp_dist", "Ratings")),
                        tags$li(tags$a(
                            href="###", onclick="select_insight(this)", tab = "comparison", target_insight = "prod_comp_ts", "Rating trends")),
                        tags$li(tags$a(
                            href="###", onclick="select_insight(this)", tab = "comparison", target_insight = "prod_comp_ngrams", "N-grams")),
                        tags$li(tags$a(
                            href="###", onclick="select_insight(this)", tab = "comparison", target_insight = "prod_comp_sentiment", "Common sentiments"))
                    ),
                    uiOutput("comparison_header"),
                    uiOutput("comparison_plot")
            ),
            
            tabItem(tabName = "about",
                    tags$h1("About"),
                    tags$h2("Soon to come..."))
        ) # end all tab items
    )
)
