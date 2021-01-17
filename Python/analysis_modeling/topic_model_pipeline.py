# topic_model_pipeline.py
# Three different models for clustering customer reviews.
# This file creates two data files:
# 1. cluster_labelings.csv
# - Column names: [doc_id, brand, X1, X2] + [clus_method1, clus_method2, ...]
# - Each row represents 1 review
# - X1, X2 are embeddings of document-term matrix; used for vector space plot
# - An example of a `clus_method` column name is "y_kmeans_5" where 5 is the number of 
# clusters. This column contains the cluster labelings for this method, e.g.
# [5, 4, 1, 5, 0, 0, 3, ...]
# 2. cluster_words.json
# - JSON file where a key-value pair is of the form
#    - key -- brand+clus_method, e.g. "bj_kmeans_5"
#    - value -- dataframe with column names = range(num_clusters) and each column
#    containing the top 10 words for the respective cluster

import numpy as np
import pandas as pd
from gsdmm import MovieGroupProcess # topic modeling for short texts

# NLTK
from nltk import ngrams

# sklearn
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.decomposition import TruncatedSVD
from sklearn.manifold import TSNE
from sklearn.cluster import KMeans

import os
import random
import json

import warnings  
warnings.filterwarnings('ignore')

# PARAMETERS
DATADIR = "../../data"
CLUSTER_DATADIR = "clustering"
NGRAM_LENGTH = 2
TOP_WORDS = 10
LAMBDA = 0.5
SVD_N_COMPONENTS = 50
TSNE_N_COMPONENTS = 2
BRANDS = ["bj", "hd", "breyers", "talenti"]
# Cluster routines are encoded as (clustering method, number of clusters)
# clus_method: 0 = DMM, 1 = Kmeans, 2 = manual
CLUS_ROUTINES = [(0,5), (1,5), (2,5), (0,10), (1,10)] 
CLUS_METHOD_NAMES = ["dmm", "kmeans", "manual"]
DMM_N_ITERS=25 
DMM_ALPHA=0.1
DMM_BETA=0.1
SEED = 42 # random seed

def seed_everything(seed=42):
    random.seed(seed)
    np.random.seed(seed)
    os.environ['PYTHONHASHSEED'] = str(seed)
    
def get_ngrams(df, doc_id = "doc_id", word = "word", n=2):
	df_doc = df.groupby(doc_id)
	doc_ids = df[doc_id].unique() # save doc_ids so we can match with cluster labelings later
	doc_ids = pd.Series(doc_ids, name = doc_id)
	if n == 1:
		docs = df_doc.apply(lambda x: x[word].values.tolist()).values.tolist()
	else:
		ngram_df = pd.DataFrame(df_doc.apply(lambda x: x[word].values)).rename(columns={0:1})
	
		for i in range(2, n+1):
			ngram_df = ngram_df.join(pd.DataFrame(df_doc.apply(
		    lambda x: ["_".join(ng) for ng in (ngrams(x[word], i))] )).rename(columns={0:i}) , on = doc_id)

		ngram_df = ngram_df.apply(lambda x: [w for ng in x for w in ng], axis = 1)

		docs = ngram_df.values.tolist()

	return doc_ids, docs
 
def compute_V(texts):
	V = set()
	for text in texts:
	    for word in text:
	        V.add(word)
	        
	return len(V)

def relevance(docs, y, K, top_words=10, lam=0.5):
    eps = 0.001 # avoid log(0) error
    
    assert len(docs) == len(y), "Number of documents must match length of cluster label vector"

    cluster_words = ["" for i in range(K)]
    for i,label in enumerate(y):
        cluster_words[label] += " ".join(docs[i])
        
    v = CountVectorizer()
    dtm_c = v.fit_transform(cluster_words) # document-term matrix
    
    x = dtm_c.toarray()
    
    phi = x/x.sum(axis=1).reshape(-1,1) # dim: (num_clusters) x (num_words)
    wc = x.sum() # total word count
    p = x.sum(axis=0)/wc # dim: (num_words)
    rel = lam*np.log10(phi+eps) + (1-lam)*np.log10(phi/p+eps)
    
    inds = rel.argsort(axis=1)

    words = np.array(v.get_feature_names())
    data = []
    for i in range(K):
        data.append(words[inds[i,:-(top_words+1):-1]])
    
    important_words_df = pd.DataFrame(np.array(data).T)
    
    return important_words_df

def df_to_json(df):
	return {df.columns[i]:y.tolist() for i,y in enumerate(df.values.T)} 

if __name__ == "__main__":
	seed_everything(seed=SEED)

	if not os.path.isdir(os.path.join(DATADIR, CLUSTER_DATADIR)):
		os.mkdir(os.path.join(DATADIR, CLUSTER_DATADIR))

	labelings = []
	all_cluster_words = {}
	for brand in BRANDS:
		print("Starting brand", brand)

		# there is at least one string which is literally "null", use keep_default_na to avoid
		# interpreting as an NA value 
		parsed_reviews = pd.read_csv(os.path.join(DATADIR, "parsed_reviews_for_topic_model.csv"), encoding="UTF-8",
			keep_default_na=False) 
			
		parsed_reviews = parsed_reviews.loc[parsed_reviews["brand"] == brand]
		parsed_reviews = parsed_reviews[["doc_id", "word"]] # only required columns
 		
 		# form n-grams
		doc_ids, docs = get_ngrams(parsed_reviews, doc_id = "doc_id", word = "word", n=NGRAM_LENGTH)
		
		# compute vocabulary size for GSDMM
		V = compute_V(docs)

		# Initialize vectorizer; used for embeddings & Kmeans
		vectorizer = CountVectorizer()

		# Use all n-grams for vector space
		dtm = vectorizer.fit_transform([" ".join(doc) for doc in docs]) # document-term matrix

		# OR use only 1-grams (words) for vector space
		# docs2 = [[word for word in doc if "_" not in word] for doc in docs] 
		# dtm = vectorizer.fit_transform([" ".join(doc) for doc in docs2]) # document-term matrix

		# SVD & TSNE embedding
		svd = TruncatedSVD(n_components=SVD_N_COMPONENTS)
		dtm_svd = svd.fit_transform(dtm) 
		emb = TSNE(n_components=TSNE_N_COMPONENTS).fit_transform(dtm_svd)

		labelings_this_brand = []
		for clus_method, K in CLUS_ROUTINES:
			print("Starting (clus_method,K) combination: %i, %i" % (clus_method, K))
			if clus_method == 0:
				mgp = MovieGroupProcess(K=K, n_iters=DMM_N_ITERS, alpha=DMM_ALPHA, beta=DMM_BETA)
				y = mgp.fit(docs, V)

			elif clus_method == 1:
				kmeans = KMeans(n_clusters=K).fit(dtm_svd)
				y = kmeans.predict(dtm_svd)

			elif clus_method == 2:
				c1 = ['flavor', 'flavour', 'yummy', 'tasty', 'taste', 'tasting', 
				      'delicious', 'sweet', 'bland', 'flavorless', 'smell', 'enjoy', 'delight'] # flavor

				c2 = ['texture', 'smooth', 'rich', 'creamy', 'creaminess', 'rich', 'chunky', 
				      'chunk', 'chewy', 'chew', 'gritty', 'air'] # texture

				c3 = ['ingredient', 'pick', 'recipe', 'choice', 'chip', 'quality', 'formula', 'piece', 
				      'check', 'bit', 'combination', 'concoction'] # ingredients

				c4 = ['value', 'expensive', 'buy', 'cheap', 'pricey', 'purchase', 'size', 
				      'big', 'large', 'small', 'tiny', 'balance', 'price', 'cost', 'pint',
				     'money'] # value

				c5 = ['change', 'previous', 'down', 'drop', 'switch', 'past', 'back', 'year', 
				      'time', 'increase', 'low', 'original', 'expect', 'expectation', 'disappoint',
				     'disappointed', 'disappointing', 'miss'] # change/expectations

				y = []
				for doc in docs:
				    lens = np.zeros(K)
				    d = set(doc)
				    for i,c in enumerate([c1,c2,c3,c4,c5]):
				        lens[i] += len(set(c).intersection(d))
				    y.append(np.argmax(lens))

			# append to labelings dataframe for this brand
			cn = "y_%s_%i" % (CLUS_METHOD_NAMES[clus_method], K) # column name
			labelings_this_brand.append(pd.DataFrame(data={cn:y}, index = doc_ids))

			# get top words and append to cluster_words dictionary
			df_name = "%s_%s_%i" % (brand, CLUS_METHOD_NAMES[clus_method], K) # dataframe name
			cluster_words = relevance(docs, y, K, top_words=TOP_WORDS, lam=LAMBDA)
			cluster_words = df_to_json(cluster_words)
			all_cluster_words.update({df_name:cluster_words})

		# combine all labelings for this brand into a dataframe
		labelings.append(pd.concat([pd.DataFrame(data={"brand":[brand]*len(doc_ids), "X1":emb[:,0], "X2":emb[:,1]}, index = doc_ids)] + labelings_this_brand, axis = 1))

	# combine all labelings for all brands into a dataframe and save as .csv
	full_label_df = pd.concat(labelings, axis = 0)
	full_label_df.to_csv(os.path.join(DATADIR, CLUSTER_DATADIR + "/cluster_labelings.csv"), index = True)

	# save cluster_words as JSON
	with open(os.path.join(DATADIR, CLUSTER_DATADIR + "/cluster_words.json"), "w") as f:
		f.write(json.dumps(all_cluster_words))