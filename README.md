# Ice cream data dashboard

## Overview
In this project we: 

1. Used web scraping to collect 20k+ customer reviews for Ben & Jerry's, Haagen-Dazs, Breyers, and Talenti ice cream products. I have made my dataset available on [Kaggle](https://www.kaggle.com/tysonpo/ice-cream-dataset) for public use.

2. Compared products and assessed business performance by analyzing rating distributions and trends.

3. Employed various NLP techniques to gain insights from reviews. We examined n-grams and word coocurrences to identify common likes/dislikes among products. Sentiment analysis allowed us to fine-grain the star ratings (1-5 stars) and in turn identify more severe complaints. Topic modeling was a tool that we thought would be useful in distilling the large amount of reviews into a few key insights; however, it did not work as planned.  

4. Built a dashboard using R shiny to bring user interactivity to our explorations.

Tags: data collection, web scraping (Selenium), R, shiny, web app, dashboard, EDA, visualizations (Highcharts), NLP preprocessing (stopwords, lemmatization, POS-tagging & dependency-parsing), n-grams, sentiment analysis (VADER), topic modeling/document clustering (Dirichlet mixture model, K-means), customer reviews, short text, product metrics, business analytics