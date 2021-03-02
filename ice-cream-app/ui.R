# FULL VERSION

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
                "
            @import url(https://fonts.googleapis.com/css?family=Roboto:400);
            #socials i {
                margin: 15px;
            }
            body {
                font-size: 18px !important;
                font-family: Roboto, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Helvetica, Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\";
                font-weight: 400;
            }
            .btn.shiny-bound-input {
                background-color: #1f77b4;
                color: white;
                margin-bottom: 20px;
            }
            ")),
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
                tags$h2("Ratings"),
                plot_wrapper("overview_dist"),
                tags$h2("Rating trends"),
                ts_UI(inputId = "overview", with_stat = TRUE, smoothing_choice = 0),
                plot_wrapper("overview_ts"),
                tags$h2("Ratings by product"),
                radioButtons(inputId = "overview_by_prod_stat",
                             label = "Statistic",
                             choices = c("Number of reviews", "Average rating"),
                             selected = "Number of reviews",
                             inline = TRUE),
                plot_wrapper("overview_by_prod")
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
                            tags$p("Here's how your filter choices have impacted the amount of remaining data."),
                            table_wrapper("preprocessed_rev_table", num_rows = 4),
                            tags$h2("Words and N-grams"),
                            checkboxInput(inputId = "within_sentence",
                                          label = "Restrict N-grams & cooccurrences to within sentences",
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
                    tags$h3("All reviews"),
                    plot_wrapper("counts_plot1"),
                    tags$h3("By star rating"),
                    plot_wrapper("counts_plot2"),
                    
                    tags$h2("Language diversity"),
                    tags$h3("Aggregate metrics"),
                    table_wrapper("diversity_table1", num_rows = 6),
                    tags$h3("Individual reviews"),
                    selectInput(inputId = "diversity_stat",
                                label = "Statistic",
                                choices = c("Unique words", "Unique words density"),
                                selected = "Unique words"),
                    plot_wrapper("diversity_plot1"),
                    tags$h2("Rank-frequency distribution"),
                    plot_wrapper("rank_freq"),
                    tags$h2("Top words", style = "margin-top: 50px;"),
                    checkboxGroupInput(inputId = "top_words_wordcloud_pos",
                                label = "POS",
                                choices = c("Noun" = "NOUN",
                                            "Adjective" = "ADJ", 
                                            "Verb" = "VERB",
                                            "Other" = "other"),
                                selected = c("NOUN", "ADJ", "VERB"),
                                inline = TRUE),
                    plot_wrapper("top_words_wordcloud")
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
                    conditionalPanel('input.words_display_method=="Table"', table_wrapper("ngram_table", 
                                                                                          num_rows = if (table_max_words) table_max_words else 1000,
                                                                                          pagination = TRUE,
                                                                                          filter = TRUE)),
                    conditionalPanel('input.words_display_method=="Wordcloud"', plot_wrapper("ngram_wordcloud")),
                    tags$h2("Positive and negative N-grams"),
                    plot_wrapper("pos_neg_comparison"),
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
                    plot_wrapper("ngram_tracker"),
                    tags$h2("Noun-adjective cooccurrences"),
                    selectInput(inputId = "cooccurrence_distance",
                                label = "Cooccurrence distance",
                                choices = c(3, 2, 1, -1, -2 ,-3),
                                selected = -1),
                    tags$p("Cooccurrence distance is the distance of the adjective from the noun.
                           A distance of -2 means the adjective is two words BEFORE the noun, whereas
                           a distance of 2 means the adjective is two words AFTER the noun.
                           Note: distance calculated after preprocessing may be much different
                           depending on the strictness of the filters chosen."),
                    conditionalPanel('input.words_display_method=="Table"', table_wrapper("cooccurrences_table", 
                                                                                          num_rows = if (table_max_words) table_max_words else 1000,
                                                                                          pagination = TRUE,
                                                                                          filter = TRUE)),
                    conditionalPanel('input.words_display_method=="Wordcloud"', plot_wrapper("cooccurrences_wordcloud")),
                    tags$h2("Dependency parsing"),
                    conditionalPanel('input.words_display_method=="Table"', table_wrapper("dep_parse_table", 
                                                                                          num_rows = if (table_max_words) table_max_words else 1000,
                                                                                          pagination = TRUE,
                                                                                          filter = TRUE)),
                    conditionalPanel('input.words_display_method=="Wordcloud"', plot_wrapper("dep_parse_wordcloud"))
                    ),
            
            tabItem(tabName = "sentiment",
                    tags$h1("Sentiment analysis"),
                    tags$p("Note: sentiment model may take up to 10-15 seconds."),
                    tags$h2("Common sentiments"),
                    plot_wrapper("sentiment_radar"),
                    tags$h2("Reviews by sentiment"),
                    table_wrapper("sentiment_table", 
                                  num_rows = 1000,
                                  pagination = TRUE,
                                  filter = TRUE,
                                  bMargin = 86),
                    tags$h2("Sentiment distribution"),
                    plot_wrapper("sent_dist"),
                    tags$h2("Sentiment and star rating"),
                    plot_wrapper("sent_dist_by_stars"),
                    tags$h2("Sentiment trends"),
                    ts_UI("sent", with_stat = FALSE, smoothing_choice = 3),
                    plot_wrapper("sent_ts"),
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
                    conditionalPanel('input.words_display_method=="Table"', table_wrapper("ngram_valShift_table", 
                                                                                          num_rows = if (table_max_words) table_max_words else 1000,
                                                                                          pagination = TRUE,
                                                                                          filter = TRUE)),
                    conditionalPanel('input.words_display_method=="Wordcloud"', plot_wrapper("ngram_valShift_wordcloud")),
                    tags$h3("Polarized words"),
                    sliderInput(inputId = "polarity_range_filter",
                                label = "Polarity value",
                                min = -5,
                                max = 5,
                                value = c(-5, -2),
                                step = 1),
                    conditionalPanel('input.words_display_method=="Table"', table_wrapper("ngram_polarity_table", 
                                                                                          num_rows = if (table_max_words) table_max_words else 1000,
                                                                                          pagination = TRUE,
                                                                                          filter = TRUE)),
                    conditionalPanel('input.words_display_method=="Wordcloud"', plot_wrapper("ngram_polarity_wordcloud"))
                    ),
            
            tabItem(tabName = "topic-model",
                    tags$h1("Topic modeling"),
                    selectInput(inputId = "cluster_method",
                                label = "Clustering method",
                                choices = list("Dirichlet mixture model" = "dmm", 
                                               "K-means" = "kmeans", 
                                               "Manual clustering" = "manual"),
                                selected = "Dirichlet mixture model"),
                    uiOutput("num_clusters"),
                    actionButton("topic_button", "Run"), 
                    tags$h2("Visualizing clusters in 2D"),
                    plot_wrapper("topic_model_vectorspace"),
                    tags$h2("Cluster sizes"),
                    plot_wrapper("cluster_size_dist"),
                    tags$h2("Cluster sizes over time"),
                    ts_UI("cluster_size", with_stat = FALSE, smoothing_choice = 0),
                    plot_wrapper("cluster_size_ts"),
                    tags$h2("Important words"),
                    table_wrapper("topic_model_words", num_rows = 10)
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
                    tags$h2("At a glance"),
                    table_wrapper("prod_comp_table", num_rows = 5),
                    tags$h2("Ratings"),
                    plot_wrapper("prod_comp_dist"),
                    tags$h2("Rating trends"),
                    ts_UI(inputId = "prod", with_stat = TRUE, smoothing_choice=0),
                    plot_wrapper("prod_comp_ts"),
                    tags$h2("N-grams"),
                    selectInput(inputId = "prod_comp_ngram_length",
                                label = "N-gram length",
                                choices = c(1, 2, 3),
                                selected = 2),
                    plot_wrapper("prod_comp_ngrams"),
                    tags$h2("Common sentiments"),
                    plot_wrapper("prod_comp_sentiment")
            ),
            
            tabItem(tabName = "about",
                    tags$h1("About"),
                    tags$h2("Overview"),
                    tags$p("The goal of this project was to offer product insights that could improve customer satisfaction for various ice cream
                           companies. Specifically, we collected and parsed customer reviews and used the ratings and text to assess product performance 
                           over various time periods."),
                    tags$p("We utilized natural language processing (NLP) to monitor common likes/dislikes and identify 
                           successful (or growing) and unsuccessful (or declining) products. We developed this web application (using R Shiny) to
                           allow for tailored insights."),
                    tags$h2("Usage"),
                    tags$p("On each page of this application we have an \"Insights\" menu to choose the visualization you want to see. The reasons
                           for this are: (i) to not overwhelm the user, (ii) many of the visualizations require expensive computation. Note: for pages
                           with a \"Run\" button, clicking an insight link will have no effect until you click the button.
                           "),
                    tags$h2("Highlights"),
                    tags$p("Below we highlight a few features we think are particularly useful or interesting."),
                    tags$h3("N-gram tracker"),
                    tags$p("The N-gram tracker is based on Google's", 
                           tags$a(href="https://books.google.com/ngrams", target="_blank", tags$i("Ngram Viewer")),
                           ", allowing the user to study trends in word usage. Note that our N-gram database only contains N-grams of length three or 
                           less. Since the data is very noisy, we allow the user to input groups of words (rather than only individual words), 
                           and also change the aggregation frequency and smoothing. The smoothing algorithm used here is identical to Google's; 
                           we use a centered moving average, where a smoothing of 2 means
                           the smoothed datapoint is the average of itself and its 2 nearest points to the left and right. The N-gram tracker
                           helps assess product performance and identify concerning trends, e.g. by querying \"expensive, pricey\" we can tell
                           if customers are concerned about pricing."),
                    tags$h3("Dependency parsing"),
                    tags$p("Dependency parsing is an NLP task that parses the grammatical structure of a sentence and represents word relationships
                            as a tree. We use dependency parsing to get corresponding noun-adjective pairs. This allows us to study how the customers 
                            are describing the products. These descriptions may include functional descriptions (the pint was expensive) or
                            sentimental descriptions (the flavor was great)."),
                    tags$p(tags$b("About word tables:"), "Each \"word table\", such as in Dependency parsing, also shows the rating, number of votes, and percent helpful computed
                            as follows: each N-gram belongs to a review with a known rating and number of helpful/non-helpful votes. For each N-gram
                           we compute, e.g. rating, by averaging the rating over all documents. It may be useful to look at N-grams which are low-rated
                           but have high helpful vote percentages; such N-grams are common complaints.
                           "),
                    tags$h3("Product leaderboard"),
                    tags$p("The product leaderboard is a data table that ranks products based on their star rating. We provide the number of ratings,
                           the average rating, and the rating standard deviation. All three values should be considered when evaluating the successfulness
                           of a product. If a product has many ratings and a low standard deviation, this indicates there is a consensus opinion. If
                           such a product has a low average rating, discontinuing this product may be appropriate.
                           On the other hand, if a product has a relatively low average rating but a high standardard deviation, this indicates 
                           the product is controversial. Discontinuing such product may lower customer satisfaction."),
                    tags$h3("Product comparison: common sentiments"),
                    tags$p("Here we study common sentiment types among the two chosen products. This gives us insights on how people feel about
                           products relative to one another. We can focus our comparison on just positive and negative sentiment, or we can look
                           at feelings of \"joy\", \"surprise\", \"anger\", etc. It may be beneficial to know what customers are finding 
                           \"surprising\"."),
                    tags$h2("Data collection"),
                    tags$p("We collected all customer reviews from four brand websites: Ben & Jerry's, Haagen Dazs, Breyers, and Talenti. 
                           The final dataset contained 21674 reviews on 241 ice cream flavors, and covered from 2015-10-02 to 2020-10-04. Product
                           information included the name of the product, the description, and the ingredients list, while review information
                           contained the review date, text, star rating, and number of \"helpful\" votes. We removed any reviews which appeared
                           to be duplicates."),
                    tags$h2("Data processing"),
                    tags$h3("Initial text processing"),
                    tags$p("To extract insights from the reviews, we needed to apply tokenization. We used spacyr, a wrapper for the Python spaCy 
                           package to simulateously perform: tokenization, lemmatization, POS tagging, and dependency parsing. We then performed
                           some initial filtering: (i) we removed pronouns, (ii) we converted to lowercase, (iii), we removed all tokens which
                           did not consist entirely of English letters, (iv) we removed tokens which consisted of less than three characters, (v) we 
                           made sure to preserve valence shifters such as \"not\"."),
                    tags$h3("Configurable settings"),
                    tags$p("In the app, the user can configure several settings related to word filtering. The user can choose to: (i) use tokens
                           or lemmas, (ii) choose from two sets of English stopwords or use neither, (iii) use task-specific stopwords which filter
                           out product names, ingredients, or common words across many reviews. Finally, the user can choose a maximum vocabulary size
                           and choose whether to keep valence shifters."),
                    tags$h3("Notes"),
                    tags$p("For the \"Sentiment analysis\" and \"Topic modeling\" sections we have different configurations. For \"Sentiment analysis\",
                           valence shifters are toggled on regardless of what the user chose, because they are essential to the sentiment model. 
                           Our work for \"Topic Modeling\" was prepared in an external Python script with fixed processing configurations (lemmatization,
                           SMART stopwords, name and ingredient stopwords, no filtering by document frequency, max vocab 5000, and 
                           do not keep valence shifters)."),
                    tags$h2("Contact"),
                    tags$p("Hi! My name is Tyson Pond and I developed the Ice cream data dashboard. To contact me for business inquiries, 
                           feature suggestions, or bug reports, you may send me an email at pondtyson@gmail.com or visit my social pages below."),
                    div(id="socials", style="width:100%; display:flex; justify-content: center; color: #1f77b4;",
                        tags$a(href="https://www.linkedin.com/in/tyson-pond/", target="_blank", icon(paste("linkedin", "fa-3x"))),
                        tags$a(href="https://twitter.com/tyson_c_pond", target="_blank", icon(paste("twitter", "fa-3x"))),
                        tags$a(href="https://github.com/tysonpond", target="_blank", icon(paste("github", "fa-3x")))
                    )
            )
        ) # end all tab items
    )
)
