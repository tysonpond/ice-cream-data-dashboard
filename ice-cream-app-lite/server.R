# LITE VERSION

shinyServer(function(input, output) {
    # --------- BRAND OVERVIEW --------
    output$brandImage <- renderImage({
        imgSrc <- paste0(PATH_TO_IMAGES, "logo ", input$brand_in)
        if (input$brand_in %in% c("bj", "hd")){
            imgSrc <- paste0(imgSrc, ".png")
        }
        else {
            imgSrc <- paste0(imgSrc, ".jpg")
        }
        list(src = imgSrc, width="100", height="100", alt="brand image")
    }, deleteFile = FALSE)
    
    rev <- reactive({
        rev_all %>% 
            filter(brand == input$brand_in) %>% 
            mutate(date = as.Date(date, "%Y-%m-%d")) %>%
            mutate(week = lubridate::floor_date(date, "week"), month = lubridate::floor_date(date, "month")) %>% 
            left_join(prod()[,c("key", "name")], by = "key")
    })
    
    rev_dateRange <- reactive({
        data <- rev()
        list(date = list(start = min(data$date), end = max(data$date)),
            month = list(start = min(data$month), end = max(data$month)),
            week = list(start = min(data$week), end = max(data$week))
        )
    })
    
    prod <- reactive({
        prod_all %>% filter(brand == input$brand_in)
    })
    
    output$n_revs <- renderValueBox({ 
        valueBox(value = nrow(rev()), subtitle = "Number of reviews", icon = icon("comment"), color = "green")
    })
    output$n_prods <- renderValueBox({ 
        valueBox(value = nrow(prod()), subtitle = "Number of products", icon = icon("ice-cream"), color = "green")
    })
    output$rev_per_prod <- renderValueBox({ 
        valueBox(value = round(nrow(rev())/nrow(prod())), subtitle = "Average reviews/product", icon = icon("divide"), color = "green")
    })
    output$avg_rating <- renderValueBox({ 
        valueBox(value = round(mean(rev()$stars),2), subtitle = "Average review rating", icon = icon("thumbs-up"), color = "green")
    })
    
    top3_prod <- reactive({
        prod() %>% slice_max(order_by = rating_count, n = 3, with_ties = FALSE)
    })
    
    # Map saves us having to copy and paste this
    # https://stackoverflow.com/questions/35737029/how-to-generate-output-for-multi-plots-within-a-loop-in-shiny-app
    Map(function(i) {
        output[[paste0("top_prod_", i)]] <- renderImage({
            data <- top3_prod()
            imgSrc <- paste0(PATH_TO_IMAGES, data$key[i], ".png")
            list(src = imgSrc, width="170", height="170", alt="brand image")
        }, deleteFile = FALSE)
    }, 1:3)
    
    Map(function(i) {
        output[[paste0("top_prod_wrapper_", i)]] <- renderUI({
            data <- top3_prod()
            column(width = 4,
                    div(class = "prod-container", style = "width:100%; display:flex; justify-content:center;",
                     imageOutput(paste0("top_prod_",i), width="170", height="170"),
                     tags$div(class="prod-overlay",
                              tags$div(data$name[i]), 
                              tags$div(paste("Reviews:", data$rating_count[i])),
                              tags$div(paste("Average rating:", data$rating[i])))
                    ))
        })
    }, 1:3)
    
    output$overview_dist_header <- renderUI({
        tagList(tags$h3("Ratings"), 
                div(style = "display: none;", textInput("overview_dist_header", label = "", value = "x")))
    })    
    
    output$overview_dist <- renderHighchart({
        req(input$overview_dist_header)
        data <- rev() %>% count(stars)
        data <- data %>% hchart("column", hcaes(x = stars, y = n)) %>% 
            hc_xAxis(title = list(text = "Star rating")) %>%
            hc_yAxis(title = list(text = "Number of reviews")) %>% 
            hc_title(text = "Distribution of star ratings") %>%
            hc_tooltip(crosshairs = TRUE, sort = TRUE, headerFormat = "", pointFormat = "<b> {point.x} Star </b> <br> {point.y}") %>%
            hc_legend(enabled = FALSE)
    })
    
    output$overview_ts_header <- renderUI({
        tagList(tags$h3("Rating trends"),
                ts_UI(inputId = "overview", with_stat = TRUE, smoothing_choice = 0),
                div(style = "display: none;", textInput("overview_ts_header", label = "", value = "x")))
    })
    output$overview_ts <- renderHighchart({
        req(input$overview_ts_header)
        data <- rev()
        agg_freq <- input$overview_ts_agg_freq 
        stat <- input$overview_ts_stat
        smoothing <- as.numeric(input$overview_ts_smoothing)
        
        data<- data %>% 
            group_by(!!as.name(agg_freq)) %>%
            summarize(mean_rating = mean(stars), n = n())
        
        start <- rev_dateRange()[[agg_freq]]$start
        end <- rev_dateRange()[[agg_freq]]$end
        data <- data %>% 
            complete( {{agg_freq}} := seq.Date(start, end, by=agg_freq), fill = list(n=0)) %>%
            mutate({{stat}} := smooth_ts_data(!!as.name(stat), p = smoothing))
        
        x <- data %>% mutate({{stat}} := smooth_ts_data(!!as.name(stat), p = smoothing))
        
        x <- as.xts(x[[stat]], x[[agg_freq]]) 
        
        stat_label <- switch(stat, "n" = "Number of reviews", "mean_rating" = "Average rating")
        make_ts_plot(series_list = list(x), 
                     yLab = stat_label, 
                     marker = if (stat == "mean_rating") TRUE else FALSE,
                     title = paste(stat_label, "per", agg_freq))
    })
    
    output$overview_by_prod_header <- renderUI({
        tagList(tags$h3("Ratings by product"),
                radioButtons(inputId = "overview_by_prod_stat",
                             label = "Statistic",
                             choices = c("Number of reviews", "Average rating"),
                             selected = "Number of reviews",
                             inline = TRUE),
                div(style = "display: none;", textInput("overview_by_prod_header", label = "", value = "x")))
    })
    
    output$overview_by_prod <- renderHighchart({
        req(input$overview_by_prod_header)
        data <- prod()
        stat <- input$overview_by_prod_stat
        data <- if (stat == "Average rating") data$rating else data$rating_count
        hchart(data) %>%
            hc_xAxis(title = list(text = stat)) %>%
            hc_yAxis(title = list(text = "Number of products")) %>% 
            hc_title(text = paste("Distribution of", tolower(stat), "by product")) %>%
            hc_tooltip(crosshairs = TRUE, sort = TRUE, headerFormat = "", pointFormat = "<b> {point.name} </b> <br> {point.y}") %>%
            hc_legend(enabled = FALSE)
    })
    
    output$overview_header <- renderUI({
        uiOutput(paste0(input$overview_insight, "_header"))
    })
    
    output$overview_plot <- renderUI({
        plot_wrapper(input$overview_insight)
    })
    # --------- END BRAND OVERVIEW ---------
    
    # ----------- SETTINGS ---------------
    observeEvent(input$filter_words, {
        if(!input$filter_words){
            shinyjs::disable("filter_words_num")
        }else{
            shinyjs::enable("filter_words_num")
        }
    })
    
    # ------------ TEXT PREPROCESSING ---------------
    preprocessed_rev <- reactive({
        # Take dependency on input$submitButton by accessing it. We will then use isolate(...) to make sure this reactive only
        # updates when the settings button is pressed. Reference: https://shiny.rstudio.com/articles/isolation.html.
        input$submitButton

        # Isolating inputs from Settings form
        lemmatization <- isolate(input$lemmatization)
        standard_sw <- isolate(input$standard_sw)
        task_sw <- isolate(input$task_sw)
        filter_words <- isolate(input$filter_words)
        filter_words_num <- isolate(input$filter_words_num)
        max_tokens <- isolate(input$max_tokens)
        valence_shifters <- isolate(input$valence_shifters)
        
        # Recode user input for lemmatization: "No" --> "token", "Yes" --> "lemma"
        lemmatization <- ifelse(lemmatization, "lemma", "token")
        
        # Load parsed reviews
        parsedtxt <- read.csv(paste0(PATH_TO_APPDATA, "parsed_reviews.csv"), encoding = "UTF-8")
        
        # Rename token column (lemma/token) to "word" and drop other column
        parsedtxt <- parsedtxt %>%
            rename("word" = lemmatization) %>%
            select(-setdiff(c("token", "lemma"), lemmatization))
        
        # Subset based on brand 
        parsedtxt <- parsedtxt %>% filter(doc_id %in% rev()$rev_id)
        
        # Filter parsed reviews based on lemmatization choice
        parsedtxt <- parsedtxt %>%
            filter(grepl("^[a-z]+$",  word)) %>% # remove tokens/lemmas which consist of anything other than letters
            filter(nchar(word) >= 3) # remove short tokens/lemmas
        
        # Save statistics before filtering
        stats_before <- get_preprocessing_stats(parsedtxt)
        
        # ---------------WORD FILTERING------------------
        # 1. Standard English stopwords (see https://github.com/igorbrigadir/stopwords for a comprehensive database)
        standard_sw <- switch(standard_sw,
                              "None" = c(),
                              "Snowball (174 words)" = read.csv(paste0(PATH_TO_APPDATA, "Snowball_sw_parsed.csv"), encoding = "UTF-8")[[lemmatization]],
                              "SMART (571 words)" = read.csv(paste0(PATH_TO_APPDATA, "SMART_sw_parsed.csv"), encoding = "UTF-8")[[lemmatization]]
    
        )
        
        # 2. Task-specific stopwords
        if (length(task_sw) > 0){
            custom_sw <- read.csv(paste0(PATH_TO_APPDATA, "task_sw_parsed.csv"), encoding = "UTF-8")
            custom_sw <- custom_sw %>% 
                filter(brand == input$brand_in & doc_type %in% task_sw)
            custom_sw <- custom_sw[[lemmatization]]
        }
        else{
            custom_sw <- c()
        }
        
        # some additional words/misspellings to include in stopwords
        other_sw <- read.csv(paste0(PATH_TO_APPDATA, "other_sw_parsed.csv"), encoding = "UTF-8")[[lemmatization]]
        
        # combine all stopwords
        sw <- unique(c(standard_sw, custom_sw, other_sw))
        
        # 3. Words to exclude from stopwords (but not if filtering by document frequency)
        exclude <- c("natural", "flavor", "flavour", "creamy", "value", "down", "downward", "sweet", "super", "like", "ingredient", "ingredients",
                     "artificial", "flavoring", "heaven", "light", "extra", "layers", "add", "added", "free", "indulgences", "indulgence", "delight",
                     "delights", "style", "layered", "layer", "maintain", "freshness", "flavored", "flavors", "everything")
        sw <- setdiff(sw, exclude)
        
        # 4. Valence shifters
        # Keep valence shifters
        valShift <- lexicon::hash_valence_shifters$x
        sw <- setdiff(sw, valShift)
        
        parsedtxt <- parsedtxt %>% anti_join(data.frame(word=sw), by = "word")
        
        # 5. Filter by document frequency and limit vocab size to `max_tokens`
        L <- length(unique(parsedtxt$doc_id)) # total number of documents (that still have words after removing stopwords, etc.)
        
        # keep words
        kw <- parsedtxt %>%
            group_by(word) %>%
            summarise(termfreq = n(), docfreq = length(unique(doc_id))/L) 
        
        valShift_present <- intersect(kw$word, valShift)
        
        if (filter_words){
            kw <- kw %>% filter(filter_words_num[1] <= docfreq & docfreq <= filter_words_num[2])
        }
        
        kw <- kw %>% arrange(desc(docfreq), desc(termfreq))
        kw <- kw %>% anti_join(data.frame(word=valShift_present), by = "word")
        
        # with valence shifters
        kw1 <- kw %>% slice_head(n = max(max_tokens - length(valShift_present), 0)) %>% .$word
        kw1 <- union(kw1, valShift_present)
        
        # no valence shifters
        kw2 <- kw %>% slice_head(n = max_tokens) %>% .$word
        
        parsedtxt_val <- parsedtxt %>% filter(word %in% kw1)
        parsedtxt <- parsedtxt %>% filter(word %in% kw2)

        # Save statistics after filtering
        stats_after <- get_preprocessing_stats(if(valence_shifters) parsedtxt_val else parsedtxt)
        
        # Join with star ratings from review data
        cols <- c("rev_id", "stars")
        if (bind_rev_numbers){
            cols <- c(cols, c("helpful_yes", "helpful_no"))
        }
        parsedtxt_val <- parsedtxt_val %>% inner_join(rev_all %>% select(cols), c("doc_id" = "rev_id"))
        parsedtxt <- parsedtxt %>% inner_join(rev_all %>% select(cols), c("doc_id" = "rev_id"))
        
        return(list(rev_data = if(valence_shifters) parsedtxt_val else parsedtxt,
                    rev_valShift = parsedtxt_val,
                    stats_before = stats_before, 
                    stats_after = stats_after))
        # -------- END WORD FILTERING --------------------------
    })
    # -------------- END TEXT PREPROCESSING --------
    
    output$preprocessed_rev_table <- renderDataTable({
        stats_before <- preprocessed_rev()[["stats_before"]]
        stats_after <- preprocessed_rev()[["stats_after"]]
        data <- data.frame(x=unlist(stats_before, use.names = FALSE), y=unlist(stats_after, use.names = FALSE))

        DT::datatable(data, 
                      rownames = c("Mean words per review", "Median words per reivew", "Vocabulary size", "Number of reviews"), 
                      colnames = c("", "Before filtering", "After filtering"),
                      class = "display nowrap",
                      selection = "none", 
                      options = list(searching = FALSE, paging = FALSE, info = FALSE, ordering = F, scrollX = TRUE))
    })
    
    # output$preprocessed_rev_table <- renderUI({ 
    #     stats_before <- preprocessed_rev()[["stats_before"]]
    #     stats_after <- preprocessed_rev()[["stats_after"]]
    #     tags$table(style = "border: 1px solid black; padding: 1%; width: 100%; height: 252px;",
    #              tags$tr(
    #                  tags$th(),
    #                  tags$th("Before filtering"),
    #                  tags$th("After filtering")
    #              ),
    #              tags$tr(
    #                  tags$td("Mean words per review"),
    #                  tags$td(stats_before[["mean_words_per_doc"]]),
    #                  tags$td(stats_after[["mean_words_per_doc"]])
    #              ),
    #                tags$tr(
    #                    tags$td("Median words per review"),
    #                    tags$td(stats_before[["median_words_per_doc"]]),
    #                    tags$td(stats_after[["median_words_per_doc"]])
    #                ),
    #                tags$tr(
    #                    tags$td("Vocabulary size"),
    #                    tags$td(stats_before[["vocab_size"]]),
    #                    tags$td(stats_after[["vocab_size"]])
    #                ),
    #                tags$tr(
    #                    tags$td("Number of reviews"),
    #                    tags$td(stats_before[["number_of_documents"]]),
    #                    tags$td(stats_after[["number_of_documents"]])
    #                )
    #                
    #     )
    # })
    # -------------- END SETTINGS ------------
    
    # --------- BASIC STATS ------------
    output$counts_plot1_header <- renderUI({
        tagList(tags$h3("Word counts"),
                selectInput(inputId = "count_stat",
                    label = "Statistic",
                    choices = c("Characters", "Words", "Sentences"),
                    selected = "Words"),
                tags$h4("All reviews"),
                div(style = "display: none;", textInput("counts_plot1_header", label = "", value = "x"))
                )
    })
    
    output$counts_plot1 <- renderHighchart({
        req(input$counts_plot1_header)
        data <- preprocessed_rev()[["rev_data"]]
        count_stat <- input$count_stat
        data <- data %>% 
            group_by(doc_id) %>%
            summarize(value = switch(count_stat,
                                     "Characters" = sum(nchar(word)),
                                     "Words" = n(),
                                     "Sentences" = length(unique(sentence_id)))
            )
        

        # dataRange <- range(data$value)
        # bins <- seq(dataRange[1] - 0.5, dataRange[2] + 0.5, length.out = dataRange[2] - dataRange[1] + 2)
        # h <- hist(data$value, breaks = bins, plot = FALSE)
        h <- hist(data$value, plot = FALSE)
        
        hchart(h) %>% 
            hc_xAxis(title = list(text = paste("Number of", tolower(count_stat)))) %>%
            hc_yAxis(title = list(text = "Number of reviews")) %>% 
            hc_title(text = paste("Distribution of", substr(tolower(count_stat), start = 1, stop = nchar(count_stat)-1), "count")) %>%
            hc_tooltip(crosshairs = TRUE, sort = TRUE, headerFormat = "", pointFormat = "<b> {point.name} </b> <br> {point.y}") %>%
            hc_legend(enabled = FALSE)
    })
    
    output$counts_plot2_header <- renderUI({
        tagList(tags$h3("Word counts"),
                selectInput(inputId = "count_stat",
                            label = "Statistic",
                            choices = c("Characters", "Words", "Sentences"),
                            selected = "Words"),
                tags$h4("By star rating"),
                div(style = "display: none;", textInput("counts_plot2_header", label = "", value = "x")))
    })
    
    output$counts_plot2 <- renderHighchart({
        req(input$counts_plot2_header)
        data <- preprocessed_rev()[["rev_data"]]
        count_stat <- input$count_stat
        
        data <- data %>% 
            group_by(stars, doc_id) %>%
            summarize(value = switch(count_stat,
                                     "Characters" = sum(nchar(word)),
                                     "Words" = n(),
                                     "Sentences" = length(unique(sentence_id)))
            ) %>%
            summarize(stat = mean(value))
        
        
        data %>%
            hchart('column', hcaes(x = stars, y = stat)) %>%
            hc_xAxis(title = list(text = "Star rating")) %>%
            hc_yAxis(title = list(text = paste("Average number of", tolower(count_stat)))) %>%
            hc_title(text = paste("Average", substr(tolower(count_stat), start = 1, stop = nchar(count_stat)-1), "count by star rating")) %>%
            hc_tooltip(crosshairs = TRUE, sort = TRUE, headerFormat = "", pointFormat = "<b> {point.x} Star </b> <br> {point.y:.2f}") %>%
            hc_legend(enabled = FALSE)
    })
    
    output$diversity_table1_header <- renderUI({
        tagList(tags$h3("Language diversity"),
                tags$h4("Aggregate metrics"),
                div(style = "display: none;", textInput("diversity_table1_header", label = "", value = "x")))
    })
    
    output$diversity_table1 <- renderDataTable({
        req(input$diversity_table1_header)
        data <- preprocessed_rev()[["rev_data"]]
        data <- data %>% 
            mutate(specie = as.numeric(as.factor(word))) 
        
        all_stats <- list(stars = "All",
                          nunq = length(unique(data$specie)),
                          nunq_dens = length(unique(data$specie))/nrow(data),
                          entropy = diversity(data$specie, index = "shannon"),
                          simpson = diversity(data$specie, index = "simpson"))
        all_stats <- c(all_stats, list(perplexity = 2^all_stats$entropy))
        
        by_star_stats <- data %>% 
            group_by(stars) %>%
            summarize(nunq = length(unique(specie)),
                      nunq_dens = length(unique(specie))/n(),
                      entropy = diversity(specie, index = "shannon"),
                      simpson = diversity(specie, index = "simpson")) %>%
            mutate(perplexity = 2^entropy)
        
        df <- rbind(all_stats, by_star_stats)
        df <- df[,c("stars","nunq", "nunq_dens", "entropy", "perplexity", "simpson")]
        
        df <- DT::datatable(df, 
                            rownames = FALSE,
                            colnames = c("Stars", "Unique words", "Unique words density", "Entropy", "Perplexity", "Simpson index"),
                            class = "display nowrap",
                            selection = "none", 
                            options = list(searching = FALSE, paging = FALSE, info = FALSE, ordering = F, scrollX = TRUE)) 
        
        df %>% formatRound(c(3,4,5), 3, mark = "") %>% formatRound(6, 6)
    })
    
    output$diversity_plot1_header <- renderUI({
        tagList(tags$h3("Language diversity"),
                tags$h4("Individual reviews"),
                selectInput(inputId = "diversity_stat",
                            label = "Statistic",
                            choices = c("Unique words", "Unique words density"),
                            selected = "Unique words"),
                div(style = "display: none;", textInput("diversity_plot1_header", label = "", value = "x")))
    })
    
    output$diversity_plot1 <- renderHighchart({
        req(input$diversity_plot1_header)
        data <- preprocessed_rev()[["rev_data"]]
        diversity_stat <- input$diversity_stat
        
        data <- data %>% 
            group_by(doc_id) %>%
            summarize(
            nunq = length(unique(word)),
            nunq_dens = length(unique(word))/n()
            )
        
        data <- if (diversity_stat == "Unique words density") data$nunq_dens else data$nunq
        h <- hist(data, plot = FALSE)
        
        hchart(h) %>%
            hc_xAxis(title = list(text = diversity_stat)) %>%
            hc_yAxis(title = list(text = "Number of reviews")) %>%
            hc_title(text = paste("Distribution of", tolower(diversity_stat))) %>%
            hc_tooltip(crosshairs = TRUE, sort = TRUE, headerFormat = "", pointFormat = "<b> {point.name} </b> <br> {point.y}") %>%
            hc_legend(enabled = FALSE)
    })
    
    output$rank_freq_header <- renderUI({
        tagList(tags$h3("Rank-frequency distribution"),
                div(style = "display: none;", textInput("rank_freq_header", label = "", value = "x")))
    })
    
    output$rank_freq <- renderHighchart({
        req(input$rank_freq_header)
        data <- preprocessed_rev()[["rev_data"]]
    
        # rank-frequency calculation
        data <- data %>%
            count(word) %>%
            arrange(desc(n)) %>%
            mutate(rank = row_number())
        
        hc <- data %>% hchart('scatter', hcaes(x = rank, y = n)) %>%
            hc_chart(zoomType = "x") %>%
            hc_xAxis(type = "logarithmic", tickInterval = 1, minRange = 1, title = list(text = "Rank")) %>%
            hc_yAxis(type = "logarithmic", title = list(text = "Frequency")) %>%
            hc_tooltip(crosshairs = TRUE, headerFormat = "", pointFormatter = JS("function() {return this.word;}")
            )
        hc
    })
    
    output$top_words_wordcloud_header <- renderUI({
        tagList(tags$h3("Top words"),
                tags$p("Note: hovering over the wordcloud shows log10(freq) rather than freq."),
                checkboxGroupInput(inputId = "top_words_wordcloud_pos",
                                   label = "POS",
                                   choices = c("Noun" = "NOUN",
                                               "Adjective" = "ADJ", 
                                               "Verb" = "VERB",
                                               "Other" = "other"),
                                   selected = c("NOUN", "ADJ", "VERB"),
                                   inline = TRUE),
                div(style = "display: none;", textInput("top_words_wordcloud_header", label = "", value = "x")))
    })
    
    output$top_words_wordcloud <- renderHighchart({
        req(input$top_words_wordcloud_header)
        data <- preprocessed_rev()[["rev_data"]]

        pos_input <- input$top_words_wordcloud_pos
        shiny::validate(
            need( length(intersect(pos_input, c("NOUN", "ADJ", "VERB", "other"))) > 0, 
                  "Please select at least one input.")
        )

        if (length(setdiff(c("NOUN", "ADJ", "VERB", "other"), pos_input)) > 0){
            data <- data %>% filter(pos %in% pos_input | (("other" %in% pos_input) & !(pos %in% c("NOUN", "ADJ", "VERB"))))
        } 
        
        data <- data %>% count(word) 

        make_wordcloud(data, monochrome = monochrome, max_words = wordcloud_max_words)
        # wordcloud2a(data = data, size = 0.25, shape = "circle",
        #             color = sample(getOption("highcharter.theme")$colors, size = wordcloud_max_words, replace = TRUE))
    })
    
    output[["basic-stats_header"]] <- renderUI({
        uiOutput(paste0(input[["basic-stats_insight"]], "_header"))
    })
    
    output[["basic-stats_plot"]] <- renderUI({
        insight <- input[["basic-stats_insight"]]
        req(insight)
        if (insight == "diversity_table1"){
            table_wrapper(insight, num_rows = 6)
        } else {
            plot_wrapper(insight)
        }
    })
    # ----------- END BASIC STATS --------
    
    # ---------- WORD INSIGHTS ---------
    all_ngrams_reactive <- reactive({
        data <- preprocessed_rev()[["rev_data"]]
        within_sentence <- input$within_sentence
        repeats <- input$ngram_repeats
        return( get_all_ngrams(data, max_ngram_length=3, within_sentence=within_sentence, repeats=repeats, 
                               with_pos = TRUE, with_sent = FALSE, bind_rev_numbers = bind_rev_numbers) )
    }) 
    
    Map(function(plotType) {
        output[[paste0("ngram_", plotType, "_header")]] <- renderUI({
            tagList(tags$h3("Top N-grams"),
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
                    div(style = "display: none;", textInput(paste0("ngram_", plotType, "_header"), label = "", value = "x")))
        })
    }, c("table", "wordcloud"))
    
    output$ngram_table <- renderDataTable({
        req(input$ngram_table_header)
        ngram_length <- as.integer(input$ngram_length)
        pos_input <- input$ngram_pos
        
        shiny::validate(
            need( length(intersect(pos_input, c("NOUN", "ADJ", "VERB", "other"))) > 0, 
                  "Please select at least one input.")
        )
        
        ngram_data <- all_ngrams_reactive()
        data <- filter_ngrams_pos(ngram_data, ngram_length, pos_input)
        data <- form_ngrams(data, ngram_length = ngram_length, sep = table_sep)
        data <- get_word_counts(data, bind_rev_numbers = bind_rev_numbers)
        
        make_wordtable(data, max_words = table_max_words)
    })
    
    output$ngram_wordcloud <- renderHighchart({
        req(input$ngram_wordcloud_header)
        ngram_length <- as.integer(input$ngram_length)
        pos_input <- input$ngram_pos
        
        shiny::validate(
            need( length(intersect(pos_input, c("NOUN", "ADJ", "VERB", "other"))) > 0, 
                  "Please select at least one choice.")
        )
        
        ngram_data <- all_ngrams_reactive()
        data <- filter_ngrams_pos(ngram_data, ngram_length, pos_input)
        data <- form_ngrams(data, ngram_length = ngram_length, sep = wordcloud_sep)
        data <- get_word_counts(data, bind_rev_numbers = FALSE)
        
        make_wordcloud(data, monochrome = monochrome, max_words = wordcloud_max_words) 
    })
    
    output$pos_neg_comparison_header <- renderUI({
        tagList(tags$h3("Positive and negative N-grams"),
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
                div(style = "display: none;", textInput("pos_neg_comparison_header", label = "", value = "x")))
    })
    
    output$pos_neg_comparison <- renderHighchart({
        req(input$pos_neg_comparison_header)
        ngram_length <- as.integer(input$ngram_length)
        pos_input <- input$ngram_pos

        shiny::validate(
            need( length(intersect(pos_input, c("NOUN", "ADJ", "VERB", "other"))) > 0, 
                  "Please select at least one input.")
        )
        
        ngram_data <- all_ngrams_reactive()
        data <- filter_ngrams_pos(ngram_data, ngram_length, pos_input)
        data <- form_ngrams(data, ngram_length = ngram_length, sep = barchart_sep)

        data <- data %>% select(doc_id, word) %>% inner_join(rev() %>% select(rev_id, stars), by = c("doc_id" = "rev_id"))
        data <- data %>% mutate(group = case_when(stars < 5 ~ "G1", TRUE ~ "G2"))
        
        data <- get_word_comparison_data(data = data, max_words = barchart_max_words)
        
        make_word_comparison_barchart(x1 = data$G2,
                                      name1 = "Positive",
                                      x2 = data$G1, 
                                      name2 = "Negative", 
                                      categories = data$categories,
                                      xLab = "Difference in N-gram frequency (Positive - Negative)",
                                      max_words = barchart_max_words)
    })
    
    output$ngram_tracker_header <- renderUI({
        tagList(tags$h3("N-gram tracker"),
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
                div(style = "display: none;", textInput("ngram_tracker_header", label = "", value = "x")))
    })
    
    output$ngram_tracker <- renderHighchart({
        req(input$ngram_tracker_header)
        input$ngram_button
        queries <- c(isolate(input$group1), isolate(input$group2), isolate(input$group3))
        agg_freq <- isolate(input$ngram_ts_agg_freq)
        smoothing <- as.integer(isolate(input$ngram_ts_smoothing))
        
        ngram_data <- all_ngrams_reactive()
        
        ngrams_formed <- form_all_ngrams(ngram_data, sep = " ")
        
        data <- ngrams_formed %>% inner_join(rev() %>% select(c("rev_id", agg_freq)), by = c("doc_id" = "rev_id"))
        normalization <- data %>% group_by(!!as.name(agg_freq)) %>% summarize(freq_all = n())
        start <- rev_dateRange()[[agg_freq]]$start
        end <- rev_dateRange()[[agg_freq]]$end
        
        x <- lapply(queries, function(q) process_group_query(data, q, agg_freq, normalization, start, end, smoothing))
        
        make_ts_plot(series_list = x, series_names = paste("Group", 1:3), yLab = "N-gram frequency", tooltip = FALSE, formatter = JS(multiple_ts_tooltip))
    })
    
    Map(function(plotType) {
        output[[paste0("cooccurrences_", plotType, "_header")]] <- renderUI({
            tagList(tags$h3("Noun-adjective cooccurrences"),
                    selectInput(inputId = "cooccurrence_distance",
                                label = "Cooccurrence distance",
                                choices = c(3, 2, 1, -1, -2 ,-3),
                                selected = -1),
                    tags$p("Cooccurrence distance is the distance of the adjective from the noun.
                           A distance of -2 means the adjective is two words BEFORE the noun, whereas
                           a distance of 2 means the adjective is two words AFTER the noun.
                           Note: distance calculated after preprocessing may be much different
                           depending on the strictness of the filters chosen."),
                    div(style = "display: none;", textInput(paste0("cooccurrences_", plotType, "_header"), label = "", value = "x")))
        })
        }, c("table", "wordcloud"))
    
    output$cooccurrences_table <- renderDataTable({
        req(input$cooccurrences_table_header)
        cooccurrence_distance <- as.numeric(input$cooccurrence_distance)
        within_sentence <- input$within_sentence
        
        data <- preprocessed_rev()[["rev_data"]]
        
        data <- get_cooccurrences(data, cooccurrence_distance, within_sentence, sep = table_sep)
        data <- get_word_counts(data = data, bind_rev_numbers = bind_rev_numbers)
        
        make_wordtable(data, max_words = table_max_words)
    })
    
    output$cooccurrences_wordcloud <- renderHighchart({
        req(input$cooccurrences_wordcloud_header)
        cooccurrence_distance <- as.numeric(input$cooccurrence_distance)
        within_sentence <- input$within_sentence
        
        data <- preprocessed_rev()[["rev_data"]]
        
        data <- get_cooccurrences(data, cooccurrence_distance, within_sentence, sep = wordcloud_sep)
        data <- get_word_counts(data, bind_rev_numbers = FALSE)
        
        make_wordcloud(data, monochrome = monochrome, max_words = wordcloud_max_words)
    })
    
    Map(function(plotType) {
        output[[paste0("dep_parse_", plotType, "_header")]] <- renderUI({
            tagList(tags$h3("Dependency parsing"),
                    div(style = "display: none;", textInput(paste0("dep_parse_", plotType, "_header"), label = "", value = "x")))
        })
    }, c("table", "wordcloud"))
    
    output$dep_parse_table <- renderDataTable({
        req(input$dep_parse_table_header)
        data <- preprocessed_rev()[["rev_data"]]
        
        data <- get_dep_parse(data, sep = table_sep)
        data <- get_word_counts(data, bind_rev_numbers = bind_rev_numbers)
        
        make_wordtable(data, max_words = table_max_words)
    })
    
    output$dep_parse_wordcloud <- renderHighchart({
        req(input$dep_parse_wordcloud_header)
        data <- preprocessed_rev()[["rev_data"]]
        
        data <- get_dep_parse(data, sep = wordcloud_sep)
        data <- get_word_counts(data, bind_rev_numbers = FALSE)
        
        make_wordcloud(data, monochrome = monochrome, max_words = wordcloud_max_words)
    })
    
    output$words_header <- renderUI({
        uiOutput(paste0(input$words_insight, "_header"))
    })
    
    output$words_plot <- renderUI({
        req(input$words_insight)
        insight <- input$words_insight
        if (substr(insight, start = nchar(insight) - 4, stop = nchar(insight)) == "table"){
            table_wrapper(insight, 
                          num_rows = if (table_max_words) table_max_words else 1000,
                          pagination = TRUE,
                          filter = TRUE)
        } else {
            plot_wrapper(insight)
        }
    })
    # ---------- END WORD INSIGHTS ------

    # ---------- SENTIMENT ANALYSIS -------
    output$sentiment_radar_header <- renderUI({
        tagList(tags$h3("Common sentiments"),
                div(style = "display: none;", textInput("sentiment_radar_header", label = "", value = "x")))
    })
    output$sentiment_radar <- renderHighchart({
        req(input$sentiment_radar_header)
        data <- preprocessed_rev()[["rev_valShift"]]
        
        data <- data %>% count(word)
        
        data <- data %>% inner_join(nrc, by = "word") %>% group_by(sentiment) %>% summarize(n_sent=sum(n))
        
        highchart() %>% 
            hc_chart(polar = TRUE) %>% 
            hc_add_series(data$n_sent, type = "area", pointPlacement = "on") %>%
            hc_xAxis(categories = data$sentiment) %>% 
            hc_yAxis(labels = list(enabled = FALSE), gridLineInterpolation = "polygon") %>% 
            hc_title(text = "Sentiment types") %>%
            hc_tooltip(crosshairs = TRUE, sort = TRUE, headerFormat = "", pointFormat = "<b> {point.category} </b> <br> {point.y}") %>%
            hc_legend(enabled = FALSE)
        
    })
        
    rev_sentiment <- reactive({
        data <- preprocessed_rev()[["rev_valShift"]]
        
        reform_sentences <- data %>% group_by(doc_id, sentence_id) %>% summarize(sentence=paste0(paste(word, collapse=' '), "."))  
        reform_docs <- reform_sentences %>% group_by(doc_id) %>% summarize(doc=paste0(sentence, collapse=' '))
        sentiment_sentences <- sentimentr::get_sentences(reform_docs)
        
        # Sentimentr model to get sentiment scores
        data <- sentiment_sentences %>% sentimentr::sentiment_by(by = "doc_id")
        
        # Join with other data
        data <- data %>%
            inner_join(reform_docs %>% select(doc_id, doc), by = "doc_id")  %>%
            inner_join(rev() %>% select(rev_id, text, stars, date, month, week, title, name, helpful_yes, helpful_no), by = c("doc_id" = "rev_id"))
        
        return(data)
    })
    
    output$sentiment_table_header <- renderUI({
        tagList(tags$h3("Reviews by sentiment"),
                div(style = "display: none;", textInput("sentiment_table_header", label = "", value = "x")))
    })
    
    output$sentiment_table <- renderDataTable({
        req(input$sentiment_table_header)
        data <- rev_sentiment() %>% 
            mutate(votes = helpful_yes + helpful_no) %>% 
            mutate(helpful = helpful_yes/pmax(1,votes)) 
        data <- data[,c("title", "text", "doc", "date", "name", "word_count", "ave_sentiment", "sd", "stars", "votes", "helpful")]
        
        data <- cbind(' ' = '\u2295', data)
        
        df <- DT::datatable(data,
                            rownames = FALSE,
                            class = "display nowrap",
                            selection = "none",
                            filter = "top",
                            options = list(
                              order = list(list(5, "asc"), list(4, "desc")),
                              scrollX = TRUE,
                              lengthChange = FALSE,
                              drawCallback = JS("function() {
                              this.api().table().column(0).nodes().to$().css({'cursor': 'pointer', 'font-size': '18px' });
                              }"),
                              columnDefs = list(
                                  list(visible = FALSE, targets = c(1,2,3)),
                                  list(orderable = FALSE, className = 'details-control', targets = 0)
                              )
                            ),
                            callback = js_sentiment_table)
        
        df %>% formatRound(c(8,9,12),2)
    })
    
    output$sent_dist_header <- renderUI({
        tagList(tags$h3("Sentiment distribution"),
                div(style = "display: none;", textInput("sent_dist_header", label = "", value = "x")))
    })
    
    output$sent_dist <- renderHighchart({
        req(input$sent_dist_header)
        data <- rev_sentiment()
        h <- hist(data$ave_sentiment, breaks = 20, plot = FALSE)
        hchart(h) %>%
            hc_xAxis(title = list(text = "Sentiment score")) %>%
            hc_yAxis(title = list(text = "Number of reviews")) %>%
            hc_title(text = "Distribution of sentiment score") %>%
            hc_tooltip(crosshairs = TRUE, sort = TRUE, headerFormat = "", pointFormat = "<b> {point.name} </b> <br> {point.y}") %>%
            hc_legend(enabled = FALSE)
    })
    
    output$sent_dist_by_stars_header <- renderUI({
        tagList(tags$h3("Sentiment and star rating"),
                div(style = "display: none;", textInput("sent_dist_by_stars_header", label = "", value = "x")))
    })
    
    output$sent_dist_by_stars <- renderHighchart({
        req(input$sent_dist_by_stars_header)
        data <- rev_sentiment()
        data <- data %>% select(stars, ave_sentiment) %>% data_to_boxplot(variable = ave_sentiment, group_var = stars, add_outliers = FALSE)
        
        highchart() %>%
            hc_add_series_list(data) %>%
            hc_xAxis(type = "category", title = list(text = "Star rating")) %>%
            hc_yAxis(title = list(text = "Sentiment score")) %>%
            hc_title(text = "Distribution of sentiment score") %>%
            hc_subtitle(text = "Outliers are not shown") %>%
            hc_tooltip(crosshairs = TRUE, headerFormat = "",
                       pointFormat = "<b> {point.name} </b> <br>
                                      Max: {point.high} <br> 
                                      Q3: {point.q3} <br>
                                      Median: {point.median} <br>
                                      Q1: {point.q1} <br>
                                      Min: {point.low}") %>%
            hc_legend(enabled = FALSE)
    })
    
    output$sent_ts_header <- renderUI({
        tagList(tags$h3("Sentiment trends"),
                ts_UI("sent", with_stat = FALSE, smoothing_choice = 3),
                div(style = "display: none;", textInput("sent_ts_header", label = "", value = "x")))
    })
    
    output$sent_ts <- renderHighchart({
        req(input$sent_ts_header)
        data <- rev_sentiment()
        agg_freq <- input$sent_ts_agg_freq
        smoothing <- as.integer(input$sent_ts_smoothing)
        data <- data %>% group_by(!!as.name(agg_freq)) %>% summarize(y = mean(ave_sentiment))
        
        # Fill (with NA) dates that are missing sentiment scores
        start <- rev_dateRange()[[agg_freq]]$start
        end <- rev_dateRange()[[agg_freq]]$end
        data <- data %>% complete({{agg_freq}} := seq.Date(start, end, by=agg_freq)) 
        
        data <- data %>% mutate(y = smooth_ts_data(y, p = smoothing))
        data <- as.xts(data$y, data[[agg_freq]]) 
        
        make_ts_plot(series_list = list(data), 
                     yLab = "Average sentiment score", 
                     title = paste("Average sentiment", "per", agg_freq),
                     marker = TRUE)
    })
    
    output$num_valShift <- renderText({
        data <- preprocessed_rev()[["rev_valShift"]]
        data <- nrow(data %>% filter(word %in% lexicon::hash_valence_shifters$x))/nrow(rev())
        paste("Average number of valence shifters per review:", round(data, 3))
    })
    
    all_ngrams_valShift_reactive <- reactive({
        data <- preprocessed_rev()[["rev_valShift"]]
        within_sentence <- input$within_sentence
        repeats <- input$ngram_repeats
        return( get_all_ngrams(data, max_ngram_length=3, within_sentence=within_sentence, repeats=repeats, 
                               with_pos = FALSE, with_sent = TRUE, bind_rev_numbers = bind_rev_numbers) )
    }) 
    
    Map(function(plotType) {
        output[[paste0("ngram_valShift_", plotType, "_header")]] <- renderUI({
            tagList(tags$h3("Sentimental N-grams"),
                    selectInput(inputId = "ngram_length_valShift",
                                label = "N-gram length",
                                choices = c(1, 2, 3),
                                selected = 2),
                    tags$h4("Valence shifters"),
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
                    div(style = "display: none;", textInput(paste0("ngram_valShift_", plotType, "_header"), label = "", value = "x")))
        })
    }, c("table", "wordcloud"))
    
    output$ngram_valShift_table <- renderDataTable({
        req(input$ngram_valShift_table_header)
        ngram_length <- as.integer(input$ngram_length_valShift)
        valShift_type <- as.integer(input$valShift_type)
        
        shiny::validate(need(length(intersect(valShift_type, 1:4)) > 0, "Please select at least one choice."))
        
        data <- all_ngrams_valShift_reactive()[[ngram_length]]
        
        # only keep ngrams that contain valence shifters
        valType_cols <- paste0("w", 1:ngram_length, "_valType")
        data <- data %>% filter_at(.vars = valType_cols, .vars_predicate = any_vars(. %in% valShift_type))
        
        data <- form_ngrams(data, ngram_length = ngram_length, sep = table_sep)
        data <- get_word_counts(data, bind_rev_numbers = bind_rev_numbers)
        
        make_wordtable(data, max_words = table_max_words)
    })
    
    output$ngram_valShift_wordcloud <- renderHighchart({
        req(input$ngram_valShift_wordcloud_header)
        ngram_length <- as.integer(input$ngram_length_valShift)
        valShift_type <- as.integer(input$valShift_type)
        
        shiny::validate(need(length(intersect(valShift_type, 1:4)) > 0, "Please select at least one choice."))
        
        data <- all_ngrams_valShift_reactive()[[ngram_length]]
        
        # only keep ngrams that contain valence shifters
        valType_cols <- paste0("w", 1:ngram_length, "_valType")
        data <- data %>% filter_at(.vars = valType_cols, .vars_predicate = any_vars(. %in% valShift_type))
        
        data <- form_ngrams(data, ngram_length = ngram_length, sep = wordcloud_sep)
        data <- get_word_counts(data, bind_rev_numbers = FALSE)
        
        make_wordcloud(data, monochrome = monochrome, max_words = wordcloud_max_words) 
    })
    
    Map(function(plotType) {
        output[[paste0("ngram_polarity_", plotType, "_header")]] <- renderUI({
            tagList(tags$h3("Sentimental N-grams"),
                    tags$h4("Polarized words"),
                    sliderInput(inputId = "polarity_range_filter",
                                label = "Polarity value",
                                min = -5,
                                max = 5,
                                value = c(-5, -2),
                                step = 1),
                    div(style = "display: none;", textInput(paste0("ngram_polarity_", plotType, "_header"), label = "", value = "x")))
        })
    }, c("table", "wordcloud"))
    
    output$ngram_polarity_table <- renderDataTable({
        req(input$ngram_polarity_table_header)
        ngram_length <- as.integer(input$ngram_length_valShift)
        polarity_range_filter <- input$polarity_range_filter
        
        data <- all_ngrams_valShift_reactive()[[ngram_length]]
        
        # only keep ngrams that contain a sentimental word in the specified polarity range
        sent_cols <- paste0("w", 1:ngram_length, "_sent")
        data <- data %>% filter_at(.vars = sent_cols, .vars_predicate = any_vars( (. >= polarity_range_filter[1]) &
                                                                                  (. <= polarity_range_filter[2]) )
                                   )

        data <- form_ngrams(data, ngram_length = ngram_length, sep = table_sep)
        data <- get_word_counts(data, bind_rev_numbers = bind_rev_numbers)
        
        make_wordtable(data, max_words = table_max_words)
    })
    
    output$ngram_polarity_wordcloud <- renderHighchart({
        req(input$ngram_polarity_table_header)
        ngram_length <- as.integer(input$ngram_length_valShift)
        polarity_range_filter <- input$polarity_range_filter
        
        data <- all_ngrams_valShift_reactive()[[ngram_length]]
        
        # only keep ngrams that contain a sentimental word in the specified polarity range
        sent_cols <- paste0("w", 1:ngram_length, "_sent")
        data <- data %>% filter_at(.vars = sent_cols, .vars_predicate = any_vars( (. >= polarity_range_filter[1]) &
                                                                                      (. <= polarity_range_filter[2]) )
        )
        
        data <- form_ngrams(data, ngram_length = ngram_length, sep = wordcloud_sep)
        data <- get_word_counts(data, bind_rev_numbers = FALSE)
        
        make_wordcloud(data, monochrome = monochrome, max_words = wordcloud_max_words) 
    })
    
    output$sentiment_header <- renderUI({
        uiOutput(paste0(input$sentiment_insight, "_header"))
    })
    
    output$sentiment_plot <- renderUI({
        req(input$sentiment_insight)
        insight <- input$sentiment_insight
        if (substr(insight, start = nchar(insight) - 4, stop = nchar(insight)) == "table"){
            if (insight == "sentiment_table"){
                table_wrapper(insight, 
                              num_rows = 1000,
                              pagination = TRUE,
                              filter = TRUE,
                              bMargin = 86)
            } else {
                table_wrapper(insight, 
                              num_rows = if (table_max_words) table_max_words else 1000,
                              pagination = TRUE,
                              filter = TRUE)
            }
        } else {
            plot_wrapper(insight)
        }
    })
    
    # -------- END SENTIMENT ANALYSIS -------
    
    # --------- TOPIC MODEL -------------
    output$num_clusters <- renderUI({
        # `choices` are 5 or 10.
        # `selected` defaults to 5 on first render, and afterwards defaults to whatever the current selection is (unless cluster_method == manual),
        # If cluster_method == manual, the only valid selection is 5.
        # If this logic isn't included then every time the user changes the cluster_method, the num_clusters selection will *RESET* to
        # whatever the default is, i.e. 5.
        selected <- if (!is.null(input$num_clusters) & input$cluster_method != "manual") input$num_clusters else 5

        uiOut <-  selectInput(inputId = "num_clusters",
                    label = "Number of clusters",
                    choices = c(5, 10),
                    selected = selected
                    )
        if(input$cluster_method == "manual"){
            return(shinyjs::disabled(uiOut))
        }else{
            return(uiOut)
        }
    })
    
    cluster_data <- reactive({
        cluster_data_all %>% filter(brand == input$brand_in)
    })
    
    cluster_parsedtxt <- reactive({
        cluster_parsedtxt_all %>% filter(brand == input$brand_in)
    })
    
    output$topic_model_vectorspace_header <- renderUI({
        tagList(tags$h3("Visualizing clusters in 2D"),
                div(style = "display: none;", textInput("topic_model_vectorspace_header", label = "", value = "x")))
    })
    
    output$topic_model_vectorspace <- renderHighchart({
        req(input$topic_model_vectorspace_header)
        req(input$topic_button) # req() ensures that this code is only run after the button is clicked
        cluster_method <- isolate(input$cluster_method)
        num_clusters <- isolate(input$num_clusters)
        
        col_name <- paste("y", cluster_method, num_clusters, sep = "_")
        data <- cluster_data() %>% select(c("doc_id", "X1", "X2", col_name))
        
        parsedtxt <- cluster_parsedtxt()
        reform_sentences <- parsedtxt %>% group_by(doc_id, sentence_id) %>% summarize(sentence=paste0(paste(word, collapse=' '), "."))  
        reform_docs <- reform_sentences %>% group_by(doc_id) %>% summarize(doc=paste0(sentence, collapse=' '))
        
        data <- data %>% 
            inner_join(reform_docs %>% select(doc_id, doc), by = "doc_id") %>%
            inner_join(rev() %>% select(rev_id, name, title, stars), by = c("doc_id" = "rev_id"))

        hchart(data, "scatter", hcaes(x = X1, y = X2, group = !!as.name(col_name))) %>%
            hc_xAxis(title = list(text = "")) %>%
            hc_yAxis(title = list(text = "")) %>% 
            hc_title(text = "A vector space of customer reviews") %>%
            hc_subtitle(text = "Reviews have been embedded into 2D and axes represent these embedded values.") %>%
            hc_tooltip(pointFormatter = JS("function() {return ('<b>Product:</b> ' + this.name + '<br>' +
                                                                '<b>Title:</b> ' + this.title + '<br>' +
                                                                '<b>Stars:</b> ' + this.stars + '<br>' +
                                                                '<b>Words:</b> ' + this.doc);}")) %>%
            hc_legend(title = list(text = "Cluster ID"))
    })
    
    output$cluster_size_dist_header <- renderUI({
        tagList(tags$h3("Cluster sizes"),
                div(style = "display: none;", textInput("cluster_size_dist_header", label = "", value = "x")))
    })
    
    output$cluster_size_dist <- renderHighchart({
        req(input$cluster_size_dist_header)
        req(input$topic_button) # req() ensures that this code is only run after the button is clicked
        cluster_method <- isolate(input$cluster_method)
        num_clusters <- as.numeric(isolate(input$num_clusters))
        
        col_name <- paste("y", cluster_method, num_clusters, sep = "_")
        data <- cluster_data() %>% count(!!as.name(col_name)) %>% ungroup()
        
        # We don't need colors for this plot, but we'll use
        data[["colors"]] <- as.vector(unlist(lapply(data[[col_name]], FUN = function(x){return(color_theme[x+1])})))
        
        hchart(data, "column", hcaes(x = !!as.name(col_name), y = n, color = colors)) %>%
            hc_xAxis(title = list(text = "Cluster ID")) %>%
            hc_yAxis(title = list(text = "Cluster size (# of reviews)")) %>% 
            hc_title(text = "Number of reviews in each cluster") %>%
            hc_subtitle(text = "Colors do not add any information, but are used to match the other figures.") %>%
            hc_tooltip(crosshairs = TRUE, sort = TRUE, headerFormat = "", pointFormat = "<b> Cluster {point.x} </b> <br> {point.y}") %>%
            hc_legend(enabled = FALSE)
    })
    
    output$cluster_size_ts_header <- renderUI({
        tagList(tags$h3("Cluster sizes over time"),
                ts_UI("cluster_size", with_stat = FALSE, smoothing_choice = 0),
                div(style = "display: none;", textInput("cluster_size_ts_header", label = "", value = "x")))
    })
    
    output$cluster_size_ts <- renderHighchart({
        req(input$cluster_size_ts_header)
        req(input$topic_button) # req() ensures that this code is only run after the button is clicked
        cluster_method <- isolate(input$cluster_method)
        num_clusters <- as.numeric(isolate(input$num_clusters))
        agg_freq <- input$cluster_size_ts_agg_freq
        smoothing <- as.integer(input$cluster_size_ts_smoothing)
        
        data <- cluster_data() %>% inner_join(rev() %>% select(c("rev_id", agg_freq)), by = c("doc_id" = "rev_id"))
        col_name <- paste("y", cluster_method, num_clusters, sep = "_")
        data <- data %>% group_by(!!as.name(col_name), !!as.name(agg_freq)) %>% summarize(n=n()) %>% ungroup()
        
        start <- rev_dateRange()[[agg_freq]]$start
        end <- rev_dateRange()[[agg_freq]]$end
        data <- data %>% 
            complete({{agg_freq}} := seq.Date(start, end, by=agg_freq), !!as.name(col_name), fill = list(n=0))
        
        series_list <- list()
        for (i in 0:(as.integer(num_clusters)-1)){
            data_i <- data %>% filter(!!as.name(col_name) == i) %>% mutate(n = smooth_ts_data(n, p = smoothing))
            data_i <- as.xts(data_i$n, data_i[[agg_freq]]) 
            series_list <- c(series_list, list(data_i))
        }
        make_ts_plot(series_list = series_list,
                     series_names = paste("Cluster",0:(num_clusters-1)),
                     yLab = "Cluster size (# of reviews)", 
                     title = paste("Cluster sizes per", agg_freq),
                     tooltip = FALSE,
                     formatter = JS(multiple_ts_tooltip))
    })
    
    output$topic_model_words_header <- renderUI({
        tagList(tags$h3("Important words"),
                div(style = "display: none;", textInput("topic_model_words_header", label = "", value = "x")))
    })
    
    output$topic_model_words <- renderDataTable({
        req(input$topic_model_words_header)
        req(input$topic_button) # req() ensures that this code is only run after the button is clicked
        cluster_method <- isolate(input$cluster_method)
        num_clusters <- as.numeric(isolate(input$num_clusters))
        brand <- input$brand_in
        
        df_name <- paste(brand, cluster_method, num_clusters, sep = "_")
        cluster_words <- as.data.frame(cluster_words_all[[df_name]])

        # note: the column names are originally numeric; however, R does not allow names to start with a number,
        # so we convert the column names to c(Cluster 1, Cluster 2, ...)
        DT::datatable(cluster_words,
                      rownames = FALSE, 
                      colnames = paste("Cluster", 0:(num_clusters-1)),
                      class = "display nowrap",
                      selection = "none", 
                      options = list(searching = FALSE, paging = FALSE, info = FALSE, ordering = F, scrollX = TRUE))
    })
    
    output[["topic-model_header"]] <- renderUI({
        req(input$topic_button) 
        uiOutput(paste0(input[["topic-model_insight"]], "_header"))
    })
    
    output[["topic-model_plot"]] <- renderUI({
        req(input$topic_button) 
        insight <- input[["topic-model_insight"]]
        req(insight)
        if (insight == "topic_model_words"){
            table_wrapper(insight, num_rows = 10)
        } else {
            plot_wrapper(insight)
        }
    })
    # ----------- END TOPIC MODELING -------------
    
    # ------------- PRODUCT LEADERBOARD ----------------
    output$prod_leaderboard <- renderDataTable({
        data <- rev() 
        data <- data %>% 
            group_by(key) %>% 
            summarize(n = n(),
                      mean_rating = mean(stars),
                      sd_rating = sd(stars),
                      first_review = min(date),
                      last_review = max(date)) %>%
            ungroup() %>%
            right_join(prod()[,c("key", "name", "description", "ingredients")], by = "key") # right join so that if a product has 0 reviews it will still be kept in the datatable
        
        data <- data[,c("key","name","description","ingredients","n","mean_rating","sd_rating","first_review","last_review")]
        data <- cbind(' ' = '\u2295', data)
        
        df <- DT::datatable(data,
                      rownames = FALSE,
                      colnames = c("", "Key", "Name", "Description", "Ingredients", "Number of reviews", "Average rating", "Rating st. dev.", "First review", "Last review"),
                      class = "display nowrap",
                      selection = "none",
                      filter = "top",
                      options = list(
                          order = list(2, "asc"),
                          scrollX = TRUE,
                          lengthChange = FALSE,
                          drawCallback = JS("function() {
                          this.api().table().column(0).nodes().to$().css({'cursor': 'pointer', 'font-size': '18px' });
                          }"),
                          columnDefs = list(
                              list(visible = FALSE, targets = c(1,3,4)),
                              list(orderable = FALSE, className = 'details-control', targets = 0)
                          )
                      ),
                      callback = js_prod_table)
        
        df %>% formatRound(c(7,8), 2) 
    })
    # ------------ END PRODUCT LEADERBOARD -----------
    
    # -------- PRODUCT TRENDS ----------
    output$prod_trends_text <- renderUI({
        data <- rev()
        trends_month_range <- input$trends_month_range
        last_date <- max(data$date)
        split_date0 <- last_date - 2*months(trends_month_range)
        split_date1 <- last_date - months(trends_month_range)
        gp0_range <- c(split_date0, split_date1)
        gp1_range <- c(split_date1, last_date)
        
        tags$p("You are comparing product performance from ", tags$b(gp1_range[1]), " to ", tags$b(gp1_range[2]), " (group 1) with 
               product performance from ", tags$b(gp0_range[1]), " to ", tags$b(gp0_range[2]), " (group 0).")
    })
    
    output$prod_trends_table <- renderDataTable({
        data <- rev()
        trends_month_range <- input$trends_month_range
        trends_column_options <- input$trends_column_options
        
        # Split reviews into last N months (date group 1) and prior N months (date group 0), where N = input$trends_month_range
        # I.e. if N = 3 and the last review was written on 2020-09-24, then date group 1 is (2020-06-24, 2020-09-24] 
        # and date group 0 is (2020-03-24, 2020-06-24]
        last_date <- max(data$date)
        split_date0 <- last_date - 2*months(trends_month_range)
        split_date1 <- last_date - months(trends_month_range)
        gp0_range <- c(split_date0, split_date1)
        gp1_range <- c(split_date1, last_date)
        
        # Encode date group 
        data <- data %>%
            mutate(date_group = case_when( ((gp0_range[1] < date)&(date <= gp0_range[2])) ~ 0,
                                           ((gp1_range[1]< date)&(date <= gp1_range[2])) ~ 1)
            ) %>% 
            filter(date_group %in% 0:1)
        
        # Calculate summary statistics for each date group
        data <- data %>%
            group_by(key, date_group) %>%
            summarize(n = n(), 
                      mean_rating = mean(stars),
                      sd_rating=sd(stars)) %>%
            ungroup()
        
        # Fill values for products that have no reviews in either or both time windows (or no reviews AT ALL)
        data <- data %>% complete(key = prod()$key, date_group, fill = list(n = 0, mean_rating = NA, sd_rating = NA))
        
        # If only 1 review, sd returns NA because it uses (n-1) denominator; however it is appropriate to say sd = 0.
        data <- data %>% mutate(sd_rating = case_when(n == 1 ~ 0, TRUE ~ sd_rating))
        
        # Convert data to wide format. Suffixes are _0 and _1, e.g. n_0, n_1, mean_rating_0...
        data <- data %>% pivot_wider(id_cols = key, 
                                     names_from = date_group, 
                                     values_from = c("n", "mean_rating", "sd_rating"))
        
        
        # Format datatable based on user input. Calculate differences & pct difference if user chooses.
        # stats <- c("n", "mean_rating", "sd_rating")
        stats <- c("n", "mean_rating")
        cols <- c("key") # columns for datatable
        round_to <- 3
        for (stat in stats){
            if (("value" %in% trends_column_options)){
                col0 <- paste0(stat, "_0")
                col1 <- paste0(stat, "_1")
                # round non-null values only
                data[!is.na(data[[col0]]), col0] <- round(data[!is.na(data[[col0]]), col0], round_to)
                data[!is.na(data[[col1]]), col1] <- round(data[!is.na(data[[col1]]), col1], round_to)
                cols <- c(cols, paste0(stat, c("_0", "_1")))
            }
            if ("diff" %in% trends_column_options){
                data[[paste0(stat, "_diff")]] <- round(data[[paste0(stat, "_1")]] - data[[paste0(stat, "_0")]], round_to) # difference
                cols <- c(cols, paste0(stat, "_diff"))
            }
            if ("rel" %in% trends_column_options){
                data[[paste0(stat, "_rel")]] <- round((data[[paste0(stat, "_1")]] - data[[paste0(stat, "_0")]])/data[[paste0(stat, "_0")]], round_to) # percent change
                cols <- c(cols, paste0(stat, "_rel"))
            }
        }
        data <- data[,cols] %>% 
            right_join(prod()[,c("key", "name", "description", "ingredients")], by = "key") # right join so that if a product has 0 reviews it will still be kept in the datatable
        
        data <- data[,c("key","name","description","ingredients",setdiff(cols,"key"))]
        data <- cbind(' ' = '\u2295', data)

        
        df <- DT::datatable(data,
                            rownames = FALSE,
                            class = "display nowrap",
                            selection = "none",
                            filter = "top",
                            options = list(
                              order = list(2, "asc"),
                              scrollX = TRUE,
                              lengthChange = FALSE,
                              drawCallback = JS("function() {
                              this.api().table().column(0).nodes().to$().css({'cursor': 'pointer', 'font-size': '18px' });
                              }"),
                              columnDefs = list(
                                  list(visible = FALSE, targets = c(1,3,4)),
                                  list(orderable = FALSE, className = 'details-control', targets = 0)
                              )
                            ),
                            callback = js_prod_table)
        
        df %>% formatRound(setdiff(names(data)[5:ncol(data)], c("n_0", "n_1")), 2) 
    })
    # ----------- END PRODUCT TRENDS -----
    
    
    # ---------- PRODUCT COMPARISON ---------
    output$prod_1 <- renderUI({
        data <- prod() %>% arrange(name)
        choices <- data$key
        names(choices) <- paste0(data$name, " (", data$rating_count, " reviews)")
        selected <- data$key[1]
        
        div(class = "prod_input",
            selectInput(inputId = "prod_1",
                        label = "Product 1",
                        choices = choices,
                        selected = selected
            )
        )
    })
    
    output$prod_2 <- renderUI({
        req(input$prod_1)
        data <- prod() %>% arrange(name)
        choices <- c("all", data$key)
        n_revs_without_prod1 <- sum(data[data$key != input$prod_1, "rating_count"])
        names(choices) <- c(paste0("All (", n_revs_without_prod1, " reviews)"), 
                            paste0(data$name, " (", data$rating_count, " reviews)"))
        selected <- ifelse(is.null(input$prod_2), "all", input$prod_2)
        
        div(class = "prod_input",
            selectInput(inputId = "prod_2",
                        label = "Product 2",
                        choices = choices,
                        selected = selected
            )
        )
    })
    
    output$prod_1_img <- renderImage({
        # Note: Upon switching `brand` input, an error message will flash (for 1/2 a sec) due to reactives being updated out of order.
        # Specifically, the UI here is trying to update before the renderUI for input$prod_1 and input$prod_2 is complete.
        # Reference:  https://stackoverflow.com/questions/50340172/how-do-i-avoid-flashing-errors-in-shiny-r-plot
        # A hacky fix:  req(input$prod_1 %in% prod()$key) will ensure that the renderUI finishes before this.
        req(input$prod_1 %in% prod()$key)
        req(input$prod_2 %in% c("all",prod()$key))
        key1 <- input$prod_1
        key2 <- input$prod_2
        shiny::validate(need(key1 != key2, message = "Choose two different products"))
        
        imgSrc <- paste0(PATH_TO_IMAGES, key1, ".png")
        
        list(src = imgSrc, width="170", height="170", alt="brand image")
    }, deleteFile = FALSE)
    
    output$prod_2_img <- renderImage({
        req(input$prod_1 %in% prod()$key)
        req(input$prod_2 %in% c("all",prod()$key))
        key1 <- input$prod_1
        key2 <- input$prod_2
        shiny::validate(need(key1 != key2, message = "Choose two different products"))
        
        if (key2 == "all"){
            imgSrc <- paste0(PATH_TO_IMAGES, "logo ", input$brand_in)
            if (input$brand_in %in% c("bj", "hd")){
                imgSrc <- paste0(imgSrc, ".png")
            }
            else {
                imgSrc <- paste0(imgSrc, ".jpg")
            }
        }
        else {
            imgSrc <- paste0(PATH_TO_IMAGES, key2, ".png")
        }
        
        list(src = imgSrc, width="170", height="170", alt="brand image")
    }, deleteFile = FALSE)
    
    output$prod_comp_table_header <- renderUI({
        tagList(tags$h3("At a glance"), 
                div(style = "display: none;", textInput("prod_comp_table_header", label = "", value = "x")))
    })
    
    output$prod_comp_table <- renderDataTable({
        req(input$prod_comp_table_header)
        req(input$prod_comp_button)
        key1 <- isolate(input$prod_1)
        key2 <- isolate(input$prod_2)
        req(key1 %in% prod()$key)
        req(key2 %in% c("all",prod()$key))
        
        shiny::validate(need(key1 != key2, message = "Choose two different products"))
        data <- rev()
        
        if (key2 != "all"){
            data <- data %>% filter(key %in% c(key1, key2)) 
        }
        data <- data %>% 
            mutate(prod_group = case_when(key == key1 ~ 1, TRUE ~ 2)) %>%
            group_by(prod_group) %>%
            summarise(n = n(), 
                      mean_rating = round(mean(stars), 2), 
                      sd_rating = round(sd(stars), 2), 
                      first_review = min(date), 
                      last_review = max(date)) %>%
            ungroup() %>%
            select(-prod_group) %>%
            t()
        
        DT::datatable(data,
                      rownames = c("Number of reviews", "Average rating", "Rating st. dev.", "First review", "Last review"),
                      colnames = c("Statistic", "Product 1", "Product 2"),
                      class = "display nowrap",
                      selection = "none", 
                      options = list(searching = FALSE, paging = FALSE, info = FALSE, ordering = F, scrollX = TRUE, lengthChange = FALSE))
    })
    
    output$prod_comp_dist_header <- renderUI({
        tagList(tags$h3("Ratings"), 
                div(style = "display: none;", textInput("prod_comp_dist_header", label = "", value = "x")))
    })
    
    output$prod_comp_dist <- renderHighchart({
        req(input$prod_comp_dist_header)
        req(input$prod_comp_button)
        key1 <- isolate(input$prod_1)
        key2 <- isolate(input$prod_2)
        req(key1 %in% prod()$key)
        req(key2 %in% c("all",prod()$key))
        shiny::validate(need(key1 != key2, message = "Choose two different products"))
        data <- rev()
        
        if (key2 != "all"){
            data <- data %>% filter(key %in% c(key1, key2)) 
        }
        data <- data %>% mutate(prod_group = case_when(key == key1 ~ "Product 1", TRUE ~ "Product 2"))
        
        data <- data %>% 
            group_by(prod_group, stars) %>% 
            summarize(n = n()) %>%
            mutate(n = n/sum(n)) %>%
            ungroup() %>%
            complete(stars, prod_group, fill = list(n = 0))
        
        # this isn't needed, but it if stars is treated as numeric then crosshair is a "line" rather than "area"
        # which contrasts the crosshair behavior in the sentiment plot
        data <- data %>% mutate(stars = as.factor(stars))
        
        hchart(data, type = "column", hcaes(x = stars, y = n, group = prod_group)) %>%
            hc_xAxis(title = list(text = "Star rating")) %>%
            hc_yAxis(title = list(text = "Fraction of reviews")) %>% 
            hc_title(text = "Distribution of star ratings") %>%
            hc_tooltip(crosshairs = TRUE, shared = TRUE)
    })
    
    output$prod_comp_ts_header <- renderUI({
        tagList(tags$h3("Rating trends"),
                ts_UI(inputId = "prod", with_stat = TRUE, smoothing_choice=0),
                div(style = "display: none;", textInput("prod_comp_ts_header", label = "", value = "x")))
    })
    
    output$prod_comp_ts <- renderHighchart({
        req(input$prod_comp_ts_header)
        req(input$prod_comp_button)
        key1 <- isolate(input$prod_1)
        key2 <- isolate(input$prod_2)
        req(key1 %in% prod()$key)
        req(key2 %in% c("all",prod()$key))
        smoothing <- as.numeric(input$prod_ts_smoothing)
        shiny::validate(need(key1 != key2, message = "Choose two different products"))
        data <- rev()
        
        agg_freq <- input$prod_ts_agg_freq
        stat <- input$prod_ts_stat
        
        if (key2 != "all"){
            data <- data %>% filter(key %in% c(key1, key2)) 
        }
        data <- data %>% mutate(prod_group = case_when(key == key1 ~ 1, TRUE ~ 2))
        
        data <- data %>% 
            group_by(prod_group, !!as.name(agg_freq)) %>%
            summarize(mean_rating = mean(stars), n = n()) %>%
            mutate(n = n/sum(n)) %>%
            ungroup()
        
        start <- rev_dateRange()[[agg_freq]]$start
        end <- rev_dateRange()[[agg_freq]]$end
        data <- data %>% complete( {{agg_freq}} := seq.Date(start, end, by=agg_freq), prod_group, fill = list(n=0))
        
        x1 <- data %>% filter(prod_group == 1) %>% mutate({{stat}} := smooth_ts_data(!!as.name(stat), p = smoothing))
        x2 <- data %>% filter(prod_group == 2) %>% mutate({{stat}} := smooth_ts_data(!!as.name(stat), p = smoothing))
        
        x1 <- as.xts(x1[[stat]], x1[[agg_freq]]) 
        x2 <- as.xts(x2[[stat]], x2[[agg_freq]]) 
        
        stat_label <- switch(stat, "n" = "Fraction of reviews", "mean_rating" = "Average rating")
        make_ts_plot(series_list = list(x1, x2), 
                     series_names = paste("Product", 1:2),
                     yLab = stat_label, 
                     title = paste(stat_label, "per", agg_freq),
                     marker = if (stat == "mean_rating") TRUE else FALSE,
                     tooltip = FALSE,
                     formatter = JS(multiple_ts_tooltip))
    })
    
    output$prod_comp_ngrams_header <- renderUI({
        tagList(tags$h3("N-grams"), 
                selectInput(inputId = "prod_comp_ngram_length",
                            label = "N-gram length",
                            choices = c(1, 2, 3),
                            selected = 2),
                div(style = "display: none;", textInput("prod_comp_ngrams_header", label = "", value = "x")))
    })
    
    output$prod_comp_ngrams <- renderHighchart({
        req(input$prod_comp_ngrams_header)
        req(input$prod_comp_button)
        key1 <- isolate(input$prod_1)
        key2 <- isolate(input$prod_2)
        req(key1 %in% prod()$key)
        req(key2 %in% c("all",prod()$key))
        shiny::validate(need(key1 != key2, message = "Choose two different products"))
        ngram_length <- as.integer(input$prod_comp_ngram_length)
        
        data <- all_ngrams_reactive()[[ngram_length]]
        data <- form_ngrams(data, ngram_length, sep = barchart_sep)
        
        data <- data %>% inner_join(rev() %>% select("rev_id","key"), by = c("doc_id" = "rev_id"))
        if (key2 != "all"){
            data <- data %>% filter(key %in% c(key1, key2)) 
        }
        
        data <- data %>% mutate(group = case_when(key == key1 ~ "G1", TRUE ~ "G2"))
        data <- get_word_comparison_data(data = data, max_words = barchart_max_words)
        
        make_word_comparison_barchart(x1 = data$G1,
                                      name1 = "Product 1",
                                      x2 = data$G2, 
                                      name2 = "Product 2", 
                                      categories = data$categories,
                                      xLab = "Difference in N-gram frequency (Prod 2. - Prod. 1)",
                                      max_words = barchart_max_words)
    })
    
    output$prod_comp_sentiment_header <- renderUI({
        tagList(tags$h3("Common sentiments"), 
                div(style = "display: none;", textInput("prod_comp_sentiment_header", label = "", value = "x")))
    })
    
    output$prod_comp_sentiment <- renderHighchart({
        req(input$prod_comp_sentiment_header)
        req(input$prod_comp_button)
        key1 <- isolate(input$prod_1)
        key2 <- isolate(input$prod_2)
        req(key1 %in% prod()$key)
        req(key2 %in% c("all",prod()$key))
        shiny::validate(need(key1 != key2, message = "Choose two different products"))
        
        data <- preprocessed_rev()[["rev_valShift"]] %>% inner_join(rev() %>% select("rev_id","key"), by = c("doc_id" = "rev_id"))
        
        if (key2 != "all"){
            data <- data %>% filter(key %in% c(key1, key2)) 
        }
        data <- data %>% mutate(prod_group = case_when(key == key1 ~ "Product 1", TRUE ~ "Product 2"))
        
        # Get word counts
        data <- data %>% 
            group_by(prod_group, word) %>% 
            summarize(n = n()) %>%
            ungroup() %>%
            complete(prod_group, word, fill = list(n = 0))
        
        # Join with sentiment types from get_sentiments("nrc").
        # Note: we've chosen to normalize after joining. After the inner join, words without sentiment types will be dropped, 
        # so our normalized frequencies represent frequencies of all recognized "sentiment words" rather than of all words.
        # This ensures that the heights of the bars for each group sum to 1. An alternative way would be to normalize before,
        # do a left-join, and rename "NA" to "Other/Unrecognized", for a total of 13 bars rather than 12.
        data <- data %>% 
            inner_join(nrc, by = "word") %>% 
            group_by(prod_group, sentiment) %>% 
            summarize(n_sent=sum(n)) %>% 
            mutate(n_sent = n_sent/sum(n_sent)) %>% 
            ungroup()
        
        hchart(data, type = "column", hcaes(x = sentiment, y = n_sent, group = prod_group)) %>%
            hc_xAxis(title = list(text = "Sentiment type")) %>%
            hc_yAxis(title = list(text = "Fraction of words")) %>% 
            hc_title(text = "Distribution of sentiment types") %>%
            hc_tooltip(crosshairs = TRUE, shared = TRUE)
    
    })
    
    output$comparison_header <- renderUI({
        req(input$prod_comp_button)
        uiOutput(paste0(input$comparison_insight, "_header"))
    })
    
    output$comparison_plot <- renderUI({
        req(input$prod_comp_button)
        req(input$comparison_insight)
        insight <- input$comparison_insight
        if (substr(insight, start = nchar(insight) - 4, stop = nchar(insight)) == "table"){
            table_wrapper(insight, num_row = 5)
        } else {
            plot_wrapper(insight)
        }
    })
    # ---------- END PRODUCT COMPARISON -------
})