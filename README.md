# Sociology Curriculum Quantitative Text Analysis
## Decolonisation Group, Durham University

This repository contains the code for a quantitative text analysis project examining the thematic content of sociology lectures at Durham University, conducted in collaboration with the Decolonisation Group in the Department of 
Sociology. The underlying data was collected by Maryanne Ko and Dr. Stephen Ashe (Durham University). All data processing, analysis and visualisation code was written by Dr. Cristina Chueca Del Cerro.

The analysis examines 11 modules comprising 20 lectures each, extracting themes, theories, concepts and geographic focus to assess the diversity and global reach of the curriculum. Findings were used to inform curriculum development within the department going forward.

> **Note:** The dataset is internal to Durham University and cannot be shared publicly. However, the scripts are written to work with any similarly structured qualitative dataset — see Data Format below for input requirements.

---

## Project Objectives

- Clean and consolidate multiple messy CSV exports from a source spreadsheet, including column renaming, string replacement and custom exclusion of uninformative terms
- Apply sentence transformers (HuggingFace) and KMeans clustering to identify semantically coherent topic clusters across lecture content
- Run Latent Dirichlet Allocation (LDA) and Non-negative Matrix Factorisation (NMF) topic models to extract core themes, theories and concepts across modules
- Quantify the frequency of references to imperialism, colonialism, racism and global focus across modules using regex-based detection
- Produce a suite of visualisations communicating curriculum diversity to non-technical academic audiences including word clouds, treemaps, frequency plots and world maps.

---

## Repository Structure

```
sociology-curriculum-text-analysis/
│
├── topic_modelling.py
│   # Main NLP pipeline:
│   # - Loads and preprocesses consolidated CSV data
│   # - Applies custom_exclude() function to remove uninformative terms
│   # - Generates sentence embeddings using HuggingFace sentence-transformers
│   # - Clusters embeddings using KMeans
│   # - Runs LDA and NMF topic models (sklearn)
│   # - Tokenizes text using NLTK
│   # - Exports topic model results for downstream R visualisations
│
├── frequency_of_mentions.R
│   # Data cleaning and frequency analysis:
│   # - Imports and consolidates multiple CSV files
│   # - Cleans strings, renames columns, removes special characters
│   # - Encodes binary variables for global focus, imperialism, colonialism, and racism using regex pattern matching (e.g. \\bimperi\\w* captures imperialism, imperial etc.)
│   # - Computes mention ratios and frequencies across modules
│
├── topic_modelling_visualisations.R
│   # Visualisation of Python topic modelling outputs:
│   # - Imports LDA/NMF results exported from 01_topic_modelling.py
│   # - Applies bulk text normalisation (e.g. man/mens → men, woman/womens → women) for consistent term aggregation
│   # - Produces treemaps of core themes, theories, and lecture focus
│   # - Produces word clouds of most prominent one- and two-word terms across modules
│
├── country_map_creation.R
│   # Geographic focus analysis and visualisation:
│   # - Imports original CSV files and constructs a reference country list
│   # - Matches country mentions in lecture text against reference list
│   # - Produces a geom_tile heatmap of countries by lecture and number of mentions
│   # - Produces a world map where fill colour represents total number of mentions per country across all modules
│
├── README.md
└── LICENSE
```

---

## Methods

### Data Preparation
Raw data arrived as a poorly structured Excel spreadsheet requiring significant cleaning: multiple CSV exports were consolidated, columns renamed, strings standardised, and special characters removed. A custom `custom_exclude()` function was developed to systematically remove uninformative and stop words specific to this corpus beyond standard stopword libraries.

### Topic Modelling (Python)
- **Sentence Transformers** (HuggingFace) — generates dense semantic embeddings of lecture text, capturing meaning beyond simple word frequency
- **KMeans Clustering** — groups semantically similar content into coherent topic clusters
- **LDA** (sklearn) — probabilistic topic model extracting latent themes across the corpus
- **NMF** (sklearn) — matrix factorisation approach providing an alternative topic decomposition for comparison
- **NLTK** — tokenisation and text preprocessing pipeline

### Frequency Analysis (R)
Regex-based binary encoding of ideologically significant terms across modules, capturing morphological variants:
- `\\bimperi\\w*` → imperialism, imperial, imperialist
- `\\bcolon\\w*` → colonialism, colonial, colonisation
- `\\b(raci|race)\\w*` → racism, racial, racialised, race

### Visualisations (R)
- **Treemaps** — proportional representation of themes, theories, and concepts by frequency across modules
- **Word clouds** — most prominent one- and two-word terms after text normalisation
- **geom_tile heatmap** — country mentions by lecture
- **World map** — global geographic focus of the curriculum by total country mentions

---

## Data Format

The scripts expect input data structured as follows:

- One or more CSV files per module, consolidatable into a single dataframe
- A column containing free text lecture content for NLP processing
- A column indicating global focus (Yes/No or equivalent)
- A column containing reference text for ideology term detection

Adapt column names in the cleaning sections of each script to match your own dataset's structure.

---

## Tools & Dependencies

### Python
```bash
pip install pandas numpy matplotlib seaborn plotly scikit-learn sentence-transformers nltk
```

| Library | Purpose |
|--------|---------|
| `pandas` | Data import and manipulation |
| `numpy` | Numerical operations |
| `matplotlib / seabornStatic` | visualisations |
| `sentence-transformers` | HuggingFace semantic embeddings |
|`sklearn.cluster.KMeansSemantic`| cluster assignment |
|`sklearn.metrics.silhouette_score `| Evaluating optimal cluster number |
|`sklearn.decomposition.PCA `| Dimensionality reduction for visualisation|
|`sklearn.feature_extraction.CountVectorizer `| Token count matrix construction|
|`sklearn.feature_extraction.TfidfVectorizer `| TF-IDF weighted term matrix|
|`sklearn.decomposition.LatentDirichletAllocation `| LDA probabilistic topic modelling |
|`sklearn.decomposition.NMF `| Non-negative matrix factorisation topic modelling |
| `nltk` | Tokenisation and text preprocessing |

Additional requirements for `nltk`
```bash
nltk.download('stopwords')
nltk.download('punkt')
nltk.download('punkt_tab')
nltk.download('averaged_perceptron_tagger_eng')
nltk.download('wordnet')

```
### R
```r
install.packages(c("tidyverse", "purrr", "tidytext", "tidyr", "viridis", "wordcloud2", "plotrix", "ggthemes",
                   "ggrepel", "tm", "RWeka", "mapview", "htmlwidgets", "udpipe"))
```

| Package | Purpose |
|--------|---------|
| `tidyverse` | Data cleaning and manipulation |
| `tidytext` | Text preprocessing and tokenisation |
|`tidyr`| Data reshaping |
|`tm`| Text mining and corpus management |
|`RWeka`| N-gram tokenisation for two-word term extraction |
|`udpipe`| Universal POS tagging and dependency parsing |
|`wordcloud2`| Interactive word cloud visualisations |
|`ggthemes`| Extended ggplot2 themes |
|`viridis`| Colour palettes for accessible visualisations  |
|`ggrepel`| Non-overlapping text labels on plots |
|`mapview`| Interactive geographic map visualisations |
|`htmlwidgets`| Saving interactive HTML visualisations |

---

## Execution Order

1. Run `topic_modelling.py` — generates topic model outputs as CSV
2. Run `frequency_of_mentions.R` — independently of Python outputs
3. Run `wordclouds_and_treemaps.R` — requires outputs from step 1
4. Run `country_map_creation.R` — independently of Python outputs

---

## Author & Contributions

**Data collection:** Maryanne Ko and Dr. Stephen Ashe, Department of Sociology, Durham University

**Data processing, analysis & visualisation:** Dr. Cristina Chueca Del Cerro, find out more in my [Portfolio](https://chuecadelc.github.io/)


---
