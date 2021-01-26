# Ice cream data dashboard

## Overview
In this project we: 

1. Used web scraping to collect 20k+ customer reviews for Ben & Jerry's, Haagen-Dazs, Breyers, and Talenti ice cream products. I have made my dataset available on [Kaggle](https://www.kaggle.com/tysonpo/ice-cream-dataset) for public use.

2. Compared products and assessed business performance by analyzing rating distributions and trends.

3. Employed various NLP techniques to gain insights from reviews. We examined n-grams and word coocurrences to identify common likes/dislikes among products. Sentiment analysis allowed us to fine-grain the star ratings (1-5 stars) and in turn identify more severe complaints. Topic modeling was a tool that we thought would be useful in distilling the large amount of reviews into a few key insights; however, it did not work as planned.  

4. Built a dashboard using R shiny to bring user interactivity to our explorations.

Tags: data collection, web scraping (Selenium), R, shiny, web app, dashboard, EDA, visualizations (Highcharts), NLP preprocessing (stopwords, lemmatization, POS-tagging & dependency-parsing), n-grams, sentiment analysis (VADER), topic modeling/document clustering (Dirichlet mixture model, K-means), customer reviews, short text, product metrics, business analytics

## Recent changes & TO DOs
- **Built a "lite" version of the app.** The "full" version has multiple expensive computations/plots rendered on the same tab, at the same time, while the lite version allows the user to only see 1 insight at a time -- thus being more efficient. It's also easy to comment out unwanted tabs/insights (i.e. Topic Modeling, since it wasn't very useful), in case we want to make the app even "lite-r".

- There are still some glitches to work out with the lite version:
   - Prevent automatic scrolling to top on "insight" selection. Instead we should be scrolling down. Adding margin-bottom to the plot shown may help.
   - Fix interaction with buttons (in Topic Modeling & Product Comparison tabs). Clicking insight without clicking button still shows the header.
   - Glitch in Product Comparison where Product 1 & Product 2 (selectInputs + images) are blank.
   - Change default behavior when switching brands. Glitch in product comparison where whatever insight you're currently on goes blank and "breaks" -- i.e. even changing to another insight and then re-clicking doesn't show it. Perhaps we should refresh entire page/app on brand change. 
   - Would be nice to share some inputs between "insights", for example n-gram length or POS filtering between Table/Wordcloud views.

- Note: some UI/options were reformatted/removed. Mainly, we removed the words_display_method option in Settings and replaced it with separate links on each tab. We use our own server logic to select Table/Wordcloud instead of conditonalPanel.

-Need to add table_wrapper & plot_wrapper back.