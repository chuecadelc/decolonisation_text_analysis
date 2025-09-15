# required libraries
# pip install pandas numpy matplotlib seaborn nltk scikit-learn sentence-transformers plotly

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from collections import Counter
import plotly.express as px
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score
from sentence_transformers import SentenceTransformer
from sklearn.decomposition import PCA
from sklearn.feature_extraction.text import CountVectorizer # text documents to a matrix of token counts
from sklearn.feature_extraction.text import TfidfVectorizer # extension of above by calculating Term Frequency-Inverse Document Frequency (TF-IDF)
from sklearn.decomposition import LatentDirichletAllocation # Latent Dirichlet Allocation with online variational Bayes algorithm
from sklearn.decomposition import NMF # Non-Negative Matrix Factorization (NMF) to reduce dimensionality of a matrix

# cleaning the data
import nltk
from nltk.corpus import stopwords
from nltk.stem import WordNetLemmatizer
from nltk import pos_tag, word_tokenize
from nltk.corpus import wordnet

# Download stopwords and tokenizer
nltk.download('stopwords')
nltk.download('punkt')
nltk.download('punkt_tab')
nltk.download('averaged_perceptron_tagger_eng')
nltk.download('wordnet') # to remove

# change wk dir
import os
os.chdir("C:/YOUR FOLDER")

# use this if single file
#df = pd.read_csv('my_data.csv', sep=",") 

## import all CSV files, combine and process them

def combine_course_files(folder_path, course_ids, file_extension=".csv"):
    """
    Combine N course files into a single DataFrame with additional course_id column.

    Parameters:
        folder_path (str): Path to the folder containing files.
        course_ids (list): List of course IDs to allow grouping later on
        file_extension (str): File extension to look for (default: ".csv")

    Returns:
        pd.DataFrame: Combined DataFrame with specific columns
    """
    all_data = []

    files = [f for f in os.listdir(folder_path) if f.endswith(file_extension)]

    # failsafe 
    # if len(files) != len(course_ids):
    #     raise ValueError("Number of files does not match number of course_ids")

    for file, course_id in zip(files, course_ids):
        file_path = os.path.join(folder_path, file)
        df = pd.read_csv(file_path, skiprows=3)  # Change to read_excel/read_json if needed
        
        # renaming cause messy titles in my OG df - comment out if not needed
        df.columns.values[0] = 'lec_num'
        df.columns.values[1] = 'focus_lec'
        df.columns.values[2] = 'theories'
        df.columns.values[3] = 'concepts'

        # Select relevant columns
        df = df[['lec_num', 'focus_lec', 'theories', 'concepts']]
        
        # Add course_id column so we can identify them later on
        df['course_id'] = course_id
        
        all_data.append(df)

    # Combine all DataFrames
    combined_df = pd.concat(all_data, ignore_index=True)

    return combined_df

folder_path = "./data inputs"  
course_ids = ["SOCI01","SOCI02","SOCI03","SOCI04","SOCI05","SOCI06","SOCI07","SOCI08","SOCI09","SOCI10","SOCI11"]  

combined_df = combine_course_files(folder_path, course_ids)

# print(combined_df.head())

### Text cleaning and preprocessing ###

# specific list of words to exclude 
custom_exclude = set([
    "key", "preparation", "form", "role", "methods", "concept", "concepts","overview","quick","recap",
    "street", "external", "internal", "issues", "critically", "techniques", "well","recaps","new","case",
    "solutions", "mainstream", "main", "extent", "importance", "exploration", "figure","themes","rise",
    "effective", "really", "scale", "related","introduce", "look", "explore", "cover", "point","fall",
    "discuss", "examine","investigate", "address","understand","group work", "presentation","means",
    "delivery", "back", "discussion", "aim", "several","lecture","lecturer", "introduction", "day",
    "regard","core","module", "presentation", "groupwork", "likely","focus","everyday","use","wider",
    "eg","guest","theme","type","study", "exam", "assessment", "summative", "workshop", "apps", "face",
    "year", "need", "preparation", "commonplace", "way", "vs","issue","skill","first","second","third",
    "wave","decades","massive","recent","graduate","speech","pursuit","proposal","student","students",
    "numbers","subsequent","scholar","scholars","figures","graduate","ways","exploring","half","prevalence",
    "disseration","project","format","style","various","overview","general","formation","reference"])

#set the stop words to the relevant language
stop_words = set(stopwords.words('english'))

# keeps only nous and adjective
def get_wordnet_pos(tag):
    if tag.startswith('J'):
        return wordnet.ADJ
    elif tag.startswith('N'):
        return wordnet.NOUN
    else:
        return None  # We will skip verbs, adverbs, etc.


def clean_text(text):
    
    if not isinstance(text, str):  # just in case to avoid errors down the line
        return ""
    
    tokens = word_tokenize(text.lower())
    
    filtered_tokens = [word for word in tokens if word.isalpha() and word not in custom_exclude]
    pos_tags = pos_tag(filtered_tokens)
    cleaned_words = []

    for word, tag in pos_tags:
        
            if not word.isalpha(): # removes non-text characters
                continue

            if word.endswith('ing'): # additional prior step to remove verbs
                continue
            
            if word.startswith('intro'): # omit words like introduction, introduce etc
                continue
            
            if word in stop_words:
                continue

            pos = get_wordnet_pos(tag)
            
            if word in custom_exclude: 
                continue
            
            if tag.startswith("NN") or tag.startswith("JJ"):
                cleaned_words.append(word)
                
    return " ".join(cleaned_words)

## example
# text = "introduces the module and looks at the historical origins key concepts and research fields of medical sociology"
# # tokens = word_tokenize(text.lower()) ## tells you what is mapped as what , relevant for get_wordnet_pos fnc
# # print("Cleaned output:", clean_focus_text(text))


# applies the cleaning text fnc to only string cols & renames
string_cols = combined_df.columns[1:4]

for col in string_cols:
    combined_df[f"{col}_cleaned"] = combined_df[col].apply(clean_text)

# print(combined_df.head())

####################################
####### Clustering K-means #########
####################################

# Embed using Sentence Transformers - See documentation here: https://huggingface.co/sentence-transformers/all-MiniLM-L6-v2
model = SentenceTransformer('all-MiniLM-L6-v2')

# using focus_lec since it has the largest corpus, compared to theories and concepts
embeddings = model.encode(combined_df["focus_lec_cleaned"], show_progress_bar=True)

## exploring the results
print(embeddings.shape)  # (num_rows, embedding_dim)
print(embeddings[0])

# Detects clusters via K-means
silhouette_scores = []
for k in range(2, 8):
    kmeans = KMeans(n_clusters=k, random_state=42)
    labels = kmeans.fit_predict(embeddings)
    score = silhouette_score(embeddings, labels)
    silhouette_scores.append((k, score))

# Choose best K-means
best_k = max(silhouette_scores, key=lambda x: x[1])[0]
kmeans = KMeans(n_clusters=best_k, random_state=42)
combined_df["cluster_focus_lec"] = kmeans.fit_predict(embeddings)  

# ensures top words are used to label clusters, otherwise just numbers.
# focus on each module
def get_top_n_words_per_cluster(df, cluster_col, text_col, n):
    cluster_labels = {}
    vectorizer = TfidfVectorizer(stop_words='english')

    for cluster_id in df[cluster_col].unique():
        cluster_texts = df[df[cluster_col] == cluster_id][text_col].values
        X = vectorizer.fit_transform(cluster_texts)
        mean_tfidf = np.asarray(X.mean(axis=0)).ravel()
        words = np.array(vectorizer.get_feature_names_out())
        top_indices = mean_tfidf.argsort()[::-1][:n]
        top_words = words[top_indices]
        label = " ".join(top_words)
        cluster_labels[cluster_id] = label

    return cluster_labels

# Generate descriptive labels for each cluster instead of just the numbers
cluster_labels = get_top_n_words_per_cluster(combined_df,'cluster_focus_lec','focus_lec_cleaned',2) 

# Map those labels back to the DataFrame
combined_df['cluster_label_lec_focus'] = combined_df['cluster_focus_lec'].map(lambda x: f"Cluster {x}: {cluster_labels[x]}")

# Themes by course_id
topics_per_course = combined_df.groupby("course_id")["cluster_label_lec_focus"].value_counts().unstack().fillna(0).astype(int)

# Histogram of cluster frequencies
cluster_counts = combined_df["cluster_label_lec_focus"].value_counts().reset_index()
cluster_counts.columns = ['cluster_label_lec_focus', 'count']

# Add line break after 'Cluster n:' for better readability
cluster_counts['cluster_label_lec_focus'] = cluster_counts['cluster_label_lec_focus'].apply(
    lambda label: label.replace(':', ':\n') if ':' in label else label
)

cluster_counts = cluster_counts.sort_values(by='count', ascending=False) 

# Create the horizontal barplot
fig, ax = plt.subplots(figsize=(10, 6))
sns.barplot(
    data=cluster_counts,
    y= 'cluster_label_lec_focus', 
    x='count',
    palette='BuPu_r', # light to dark
    ax=ax
)

# Add titles and labels
ax.set_title("Frequency of Themes (Clusters) across lectures and courses")
ax.set_xlabel("Number of Lectures")
ax.set_ylabel("Theme")
# Optional: fine-tune font or spacing
plt.tight_layout()
plt.savefig("frequency_of_themes_by_.jpeg", dpi=300) # this has to go before plt.show() if you actually want the fig and not just a blank
plt.show()

###################################
######## Topic Modelling ##########
###################################

# runing both LDA and NMF #
# Note you'll need to change the col name to run the models on each.
# I've set it to lec_focus as an example.

# Vectorizes text using CountVectorizer and TfidfVectorizer
tfidf_vectorizer = TfidfVectorizer(max_df=0.95, min_df=1)
tfidf = tfidf_vectorizer.fit_transform(combined_df['lec_focus_recleaned']) ## theories / concepts

count_vectorizer = CountVectorizer(max_df=0.95, min_df=1)
count = count_vectorizer.fit_transform(combined_df['lec_focus_recleaned'])

# LDA - Latent Dirichlet Allocation (see more dets about the procedure: https://www.jmlr.org/papers/volume3/blei03a/blei03a.pdf )
lda_model = LatentDirichletAllocation(n_components=4, random_state=42)
lda_topics = lda_model.fit_transform(count)

# Fit NMF (on TF-IDF) Non-Negative Matrix Factorization - unsupervised clustering method (see more dets about the procedure: https://jaiagrawal.medium.com/whats-eminem-thinking-66d9735a3030)

## check this plot before choosing n_components for nmf_model
# errors = []
# for k in range(2, 20):
#     model = NMF(n_components=k, random_state=42)
#     model.fit(tfidf)
#     errors.append(model.reconstruction_err_)

# plt.plot(range(2, 20), errors)
# plt.xlabel("Number of Topics")
# plt.ylabel("Reconstruction Error")
# plt.title("NMF Elbow Curve")
# plt.show()

nmf_model = NMF(n_components=5, init='nndsvda', random_state=42)
nmf_topics = nmf_model.fit_transform(tfidf)


# Visualising topics per model
def display_topics(model, feature_names, no_top_words=10):
    topic_labels = {}
    for topic_idx, topic in enumerate(model.components_):
        words = [feature_names[i] for i in topic.argsort()[:-no_top_words - 1:-1]]
        label = ", ".join(words)
        topic_labels[topic_idx] = label
    return topic_labels


## Showing the words that come up for each of the topics
lda_topic_words = display_topics(lda_model, count_vectorizer.get_feature_names_out(), 10)
for topic in lda_topic_words:
    print(topic)

nmf_topic_words = display_topics(nmf_model, tfidf_vectorizer.get_feature_names_out(), 10)
for topic in nmf_topic_words:
    print(topic)


# Get topic labels for both models
lda_labels = display_topics(lda_model, count_vectorizer.get_feature_names_out(), 10)
nmf_labels = display_topics(nmf_model, tfidf_vectorizer.get_feature_names_out(), 10)

# Assign Dominant Topic to Each Row 
df['LDA_topic_lec_focus'] = np.argmax(lda_topics, axis=1)
df['NMF_topic_lec_focus'] = np.argmax(nmf_topics, axis=1)

# Map Numeric Topics to Readable Labels - so can be meaningfully ploted in R
df['LDA_topic_label_lec_focus'] = df['LDA_topic_lec_focus'].map(lda_labels)
df['NMF_topic_label_lec_focus'] = df['NMF_topic_lec_focus'].map(nmf_labels)

# Export CSV 
df.to_csv("combi_courses_topic_modelling_results.csv", index=False)



# Note these are some simple visualisations for the LDA/NMF results.
# However, the labels are a list of words so hard to fit on the Y-axis
# Recommend to use the topic-modelling_visualisations.R file for more meaninful viz and interpretation

# Plot top 10 LDA topics by frequency
top_lda_topics = df['LDA_topic_label_lec_focus'].value_counts().nlargest(10)
plt.figure(figsize=(12, 6))
sns.barplot(x=top_lda_topics.values, y=top_lda_topics.index, palette="BuPu_r")
plt.title("Top 10 LDA Topics by Frequency")
plt.xlabel("Number of Lectures")
plt.ylabel("Topic")
plt.tight_layout()
plt.show()

# Plot top 10 NMF topics by frequency
top_nmf_topics = df['NMF_topic_label_lec_focus'].value_counts().nlargest(10)
plt.figure(figsize=(12, 6))
sns.barplot(x=top_nmf_topics.values, y=top_nmf_topics.index, palette="BuPu_r")
plt.title("Top 10 NMF Topics by Frequency")
plt.xlabel("Number of Lectures")
plt.ylabel("Topic")
plt.tight_layout()
plt.show()


# Plot: Number of lectures per topic per course (LDA)
plt.figure(figsize=(14, 6))
sns.countplot(
    data=df, 
    y="LDA_topic_label_lec_focus", 
    hue="course_id", 
    palette = "BuPu_r",
    order=top_lda_topics.index)
plt.title("LDA Topic Distribution by Course")
plt.xlabel("Number of Lectures")
plt.ylabel("Topic")
plt.legend(title="Course ID", bbox_to_anchor=(1.05, 1), loc="upper left")
plt.tight_layout()
plt.show()


## desc() order per course_id
topic_order = (
    df.groupby("LDA_topic_label_lec_focus") 
    .size()
    .sort_values(ascending=False)
    .index
)

plt.figure(figsize=(14, 6))
sns.countplot(
    data=df,
    y="LDA_topic_label_lec_focus",  
    hue="course_id",
    order=topic_order,
    palette="BuPu_r"
)
plt.title("LDA Topic Distribution by Course")
plt.xlabel("Number of Lectures")
plt.ylabel("Topic")
plt.legend(title="Course ID", bbox_to_anchor=(1.05, 1), loc="upper left")
plt.tight_layout()
plt.show()
