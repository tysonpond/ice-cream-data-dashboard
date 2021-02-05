# Ice cream data dashboard

## Overview
In this project we: 

1. Used web scraping to collect 20k+ customer reviews for Ben & Jerry's, Haagen-Dazs, Breyers, and Talenti ice cream products. I have made my dataset available on [Kaggle](https://www.kaggle.com/tysonpo/ice-cream-dataset) for public use.

2. Compared products and assessed business performance by analyzing rating distributions and trends.

3. Employed various NLP techniques to gain insights from reviews. We examined n-grams and word coocurrences to identify common likes/dislikes among products. Sentiment analysis allowed us to fine-grain the star ratings (1-5 stars) and in turn identify more severe complaints. Topic modeling was a tool that we thought would be useful in distilling the large amount of reviews into a few key insights; however, it did not work as planned.  

4. Built a dashboard using R shiny to bring user interactivity to our explorations.

Tags: data collection, web scraping (Selenium), R, shiny, web app, dashboard, EDA, visualizations (Highcharts), NLP preprocessing (stopwords, lemmatization, POS-tagging & dependency-parsing), n-grams, sentiment analysis (VADER), topic modeling/document clustering (Dirichlet mixture model, K-means), customer reviews, short text, product metrics, business analytics

## Recent changes
- Fixed many of the glitches due to transition to "lite" version of app (mostly in Product Comparison & Topic Modeling). 
- Re-added plot + table wrappers.
- Modified topic modeling vector space plot to use `parsed_reviews_for_topic_modeling.csv` data.

## To do
- Add to About page
- Optional:
   - touch up UI (i.e., CSS for "insight" `<li>`'s)
   - optimize queries (avoid joins) & add "expected runtime" to each insight
   - add plot descriptions (why the plot is important/what we can learn from it) underneath each header
   - add a video demo to README
- Deploy