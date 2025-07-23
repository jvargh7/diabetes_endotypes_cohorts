# the purpose of this script is to validate the classficiation algorithm in the NHANES new diabetes dataset 

import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report, confusion_matrix
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import KMeans


# Load NHANES dataset
nhns_path = '/Users/zhongyuli/Desktop/python/cluster analysis/dataset/knn_clusters.csv'
nhns_data = pd.read_csv(nhns_path)
# rename the columns to match the pooled cohort dataset
nhns_data = nhns_data.rename(columns={
    'dm_age': 'dmagediag',
    'glycohemoglobin': 'hba1c',
    'triglyceride': 'tgl',
    'hdl': 'hdlc',
    'ldl': 'ldlc',
    'respondentid': 'study_id'
})
nhns_data['ratio_th'] = nhns_data['ldlc'] / nhns_data['hdlc']

# Load the pooled cohort dataset
pooled_path = '/Users/zhongyuli/Desktop/python/cluster analysis/dataset/final_dataset_6c_clean_mi_imputed_homa2.csv'
pooled_data = pd.read_csv(pooled_path)


#select variables and keep only the relevant and nonmissing columns in both datasets
selected_variables = ['study_id','bmi', 'hba1c', 'dmagediag','homa2b','homa2ir','tgl','ldlc','ratio_th','sbp','dbp','hdlc']

pooled_data = pooled_data[selected_variables].dropna()
nhns_data = nhns_data[selected_variables].dropna()

# check the data
print(pooled_data.head())
print(nhns_data.head())
# identify five variables for k means clustering
vars_5 = ['bmi', 'hba1c', 'dmagediag', 'homa2b', 'homa2ir']

# Standardize the pooled data
scaler = StandardScaler()
pooled_scaled = scaler.fit_transform(pooled_data[vars_5])
# Convert to DataFrame
pooled_scaled = pd.DataFrame(pooled_scaled, columns=vars_5)

# Save the scaling parameters
pooled_mean = scaler.mean_
pooled_scale = scaler.scale_

# Apply the same transformation to the NHANES data
nhns_scaled = (nhns_data[vars_5] - pooled_mean) / pooled_scale

# run k means clustering on the pooled data to create the TRUE labels
kmeans = KMeans(
    init="random", n_clusters=4, n_init=10, max_iter=300, random_state=57
)
kmeans.fit(pooled_scaled)
pooled_data['cluster'] = kmeans.labels_
# show the mean and sd of each of the five variables in each cluster and sample size 
cluster_counts = pooled_data['cluster'].value_counts()
print("Cluster Counts:\n", cluster_counts)
cluster_means = pooled_data.groupby('cluster')[vars_5].mean()
cluster_sds = pooled_data.groupby('cluster')[vars_5].std()
print("Cluster Means:\n", cluster_means)
print("Cluster Standard Deviations:\n", cluster_sds)    
# rename the cluster labels 
pooled_data['true_label'] = pooled_data['cluster'].map({
    0: 'SIDD',
    1: 'MOD',
    2: 'MARD',
    3: 'SIRD'
})

# save the centroids in sandardized space
centroids = pd.DataFrame(kmeans.cluster_centers_, columns=vars_5)
centroids.to_csv('/Users/zhongyuli/Desktop/python/cluster analysis/dataset/kmeans_centroids.csv', index=False)

from scipy.spatial.distance import cdist

# Compute distances to centroids (all in standardized space)
distances = cdist(nhns_scaled, centroids.values, metric='euclidean')

# Assign each NHANES participant to the closest cluster
nhns_data['true_label'] = np.argmin(distances, axis=1)

# show the mean and sd of each of the five variables in each cluster as well as sample size 
nhns_cluster_counts = nhns_data['true_label'].value_counts()
print("NHANES Cluster Counts:\n", nhns_cluster_counts)
nhns_cluster_means = nhns_data.groupby('true_label')[vars_5].mean()
nhns_cluster_sds = nhns_data.groupby('true_label')[vars_5].std()
print("NHANES Cluster Means:\n", nhns_cluster_means)
print("NHANES Cluster Standard Deviations:\n", nhns_cluster_sds)

# rename the cluster labels in NHANES data
nhns_data['true_label'] = nhns_data['true_label'].map({
    0: 'SIDD',
    1: 'MOD', 
    2: 'MARD',
    3: 'SIRD'
})

# import the beta coefficients from the pooled cohort dataset
betas = pd.read_csv('/Users/zhongyuli/Desktop/python/cluster analysis/dataset/betas.csv', index_col= 0)
# calculate the predicted probabilities for each cluster

cluster_order = ['SIDD', 'MOD', 'MARD']
thresholds = {'SIDD': 0.16,'MOD': 0.38,'MARD': 0.32}

# Prepare variables for prediction
vars_9 = ['bmi', 'hba1c', 'dmagediag', 'tgl', 'ldlc', 'ratio_th', 'sbp', 'dbp', 'hdlc']
X = nhns_data[vars_9].copy()
X['intercept'] = 1
X = X[['intercept'] + vars_9]  # Ensure correct column order

# Calculate logits for each cluster
B = betas.loc[X.columns].values  # Shape: (10 predictors, 3 clusters)
logits = np.dot(X.values, B)  # Shape: (n_samples, 3 clusters)

def sigmoid(x):
    return 1 / (1 + np.exp(-x))

probs = sigmoid(logits)  # shape: (n_samples, 3)


probs_df = pd.DataFrame(probs, index=nhns_data.index, columns=['SIDD', 'MOD', 'MARD'])

# Apply sequential classification based on thresholds
def sequential_classifier(prob_row):
    for cluster in cluster_order:
        if prob_row[cluster] >= thresholds[cluster]:
            return cluster
    return 'Mixed'

nhns_data['predicted_label'] = probs_df.apply(sequential_classifier, axis=1)

# check the predicted labels
print(nhns_data['predicted_label'].value_counts())  

# remove the mixed label and the SIRD label
comparison_df = nhns_data[(nhns_data['predicted_label'] != 'Mixed') & (nhns_data['true_label'] != 'SIRD')]

# Compare predicted vs true labels
print(confusion_matrix(comparison_df['true_label'], comparison_df['predicted_label']))
print(classification_report(comparison_df['true_label'], comparison_df['predicted_label']))

# descripbe the predicted labels and mean and sd of the five variables in each cluster
predicted_counts = comparison_df['predicted_label'].value_counts()
print("Predicted Cluster Counts:\n", predicted_counts)
predicted_means = comparison_df.groupby('predicted_label')[vars_5].mean()
predicted_sds = comparison_df.groupby('predicted_label')[vars_5].std()
print("Predicted Cluster Means:\n", predicted_means)
print("Predicted Cluster Standard Deviations:\n", predicted_sds)    

# Auc and ROC curve
from sklearn.metrics import roc_auc_score
from sklearn.preprocessing import label_binarize

# Keep all individuals with true labels SIDD, MOD, or MARD
comparison_df = nhns_data[nhns_data['true_label'].isin(['SIDD', 'MOD', 'MARD'])]

# Binarize the true labels
y_true_bin = label_binarize(comparison_df['true_label'], classes=['SIDD', 'MOD', 'MARD'])

# Get predicted probabilities for each class (from sigmoid output)
y_probs = probs_df.loc[comparison_df.index][['SIDD', 'MOD', 'MARD']].values

# Compute AUC for each class
from collections import OrderedDict
auc_scores = OrderedDict()
for i, class_name in enumerate(['SIDD', 'MOD', 'MARD']):
    auc = roc_auc_score(y_true_bin[:, i], y_probs[:, i])
    auc_scores[class_name] = auc
    print(f"AUC for {class_name}: {auc:.3f}")



import matplotlib.pyplot as plt
from sklearn.metrics import roc_curve, auc

# Plot ROC curve for each class
plt.figure(figsize=(8, 6))
colors = {'SIDD': 'darkorange', 'MOD': 'blue', 'MARD': 'green'}

for i, class_name in enumerate(['SIDD', 'MOD', 'MARD']):
    fpr, tpr, _ = roc_curve(y_true_bin[:, i], y_probs[:, i])
    roc_auc = auc(fpr, tpr)
    plt.plot(fpr, tpr, color=colors[class_name], lw=2,
             label=f'{class_name} (AUC = {roc_auc:.2f})')

# Plot settings
plt.plot([0, 1], [0, 1], 'k--', lw=1)  # diagonal line
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curves for Subtype Classifiers')
plt.legend(loc="lower right")
plt.grid(True)
plt.tight_layout()
plt.show()