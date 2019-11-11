# clustering_wGLRM

When it comes to cluster analysis for retail and e-commerce customer data, more often than not, you will find the dataset messy, high dimensional and with many categorical variables. Although there are many dimensional reduction techniques, most of them do not work well with the dataset with many categorical variables. Traditionally, clustering approaches suffer when features are not clean numeric values. For example, the most popular algorithm KNN can only handle numeric variables.

Generalized low rank models (GLRMs), developed by students at Stanford University (see Udell '16) - propose a new clustering framework to handle all types of data even with mixed datatypes. Using GLRMs, you can transform categorical columns into binary columns. Generating clusters on this type of dataset would be considered too messy for other techniques.
