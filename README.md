# Ice cream data dashboard 🍦

## Overview
In this project we: 

1. Used web scraping to collect all customer reviews from Ben & Jerry's, Haagen-Dazs, Breyers, and Talenti websites. I have made my dataset available on [Kaggle](https://www.kaggle.com/tysonpo/ice-cream-dataset) for public use.

2. Assessed and monitored product performance by analyzing rating distributions and trends.

3. Employed various NLP techniques to gain insights from reviews. We examined N-grams and word coocurrences to identify common likes/dislikes among products. Sentiment analysis allowed us to fine-grain the star ratings (1-5 stars) and in turn identify more severe complaints. Topic modeling was a tool that we thought would be useful in distilling the large amount of reviews into a few key insights; however, it did not work as planned.  

4. Used Highcharts for visualization and built a dashboard using R shiny to bring user interactivity to our explorations.

### Tags
Data collection, web scraping (Selenium), R, Shiny, web app, dashboard, EDA, visualizations (Highcharts), NLP preprocessing (stopwords, lemmatization, POS-tagging & dependency-parsing), n-grams, sentiment analysis (VADER), topic modeling/document clustering (Dirichlet mixture model, K-means), customer reviews, short text, product metrics, business analytics

## Data collection
We collected all customer reviews from four brand websites: Ben & Jerry's, Haagen Dazs, Breyers, and Talenti. The final dataset contained 21674 reviews on 241 ice cream flavors, and covered from 2015-10-02 to 2020-10-04. Product information included the name of the product, the description, and the ingredients list, while review information contained the review date, text, star rating, and number of "helpful" votes. We removed any reviews which appeared to be duplicates.

## Data processing
### Initial text processing
To extract insights from the reviews, we needed to apply tokenization. We used spacyr, a wrapper for the Python spaCy package to simulateously perform: tokenization, lemmatization, POS tagging, and dependency parsing. We then performed some initial filtering: (i) we removed pronouns, (ii) we converted to lowercase, (iii), we removed all tokens which did not consist entirely of English letters, (iv) we removed tokens which consisted of less than three characters, (v) we made sure to preserve valence shifters such as "not".

### Configurable settings
In the app, the user can configure several settings related to word filtering. The user can choose to: (i) use tokens or lemmas, (ii) choose from two sets of English stopwords or use neither, (iii) use task-specific stopwords which filter out product names, ingredients, or common words across many reviews. Finally, the user can choose a maximum vocabulary size and choose whether to keep valence shifters.

### Notes
For the "Sentiment analysis" and "Topic modeling" sections we have different configurations. For "Sentiment analysis", valence shifters are toggled on regardless of what the user chose, because they are essential to the sentiment model. Our work for "Topic Modeling" was prepared in an external Python script with fixed processing configurations (lemmatization, SMART stopwords, name and ingredient stopwords, no filtering by document frequency, max vocab 5000, and do not keep valence shifters).

The "full version" (`ice-cream-app/`) is no longer being maintained. The version deployed is `ice-cream-app-lite/`.

## Planned improvements/further ideas
- Add a video demo to README
- Improve UI: there's a lot of room for improvement here
   - Scroll plot into view after loading
   - Make it so the document does not expand vertically after plots render, i.e. put in the **exact** amount of required vertical space before rendering
   - For homepage may want to display an initial loader