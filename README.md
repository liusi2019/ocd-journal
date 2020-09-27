# Open Category Detection with PAC Guarantees and Sample Size Analysis
Code for replicating the experimental results in ocd journel version.

### Threshold estimate experiments on synthetic datasets:

"Open-Category-Detection-with-PAC-Guaranteesp-Synthetic.ipynb"

### Threshold estimate experiments on benchmark datasets:

"alpha_prime_full.R" (This one is for six UCI datasets and MNIST. Datasets are included in "benchmark_datasets.zip". )

"alpha_prime_tinyimagenet.R" (This one is for Tiny ImageNet. The scores for data points in Tiny ImageNet are included in "benchmark_datasets.zip". The scores are got through the method in Dan Hendrycks and Kevin Gimpel's 2017 paper: A baseline for detecting misclassified and out-of-distribution examples in neural networks.)

### Producing learning curve of nine estimators for alpha on six UCI datasets:

"learning_curve_nine.R"

### Comparing the performance of five etsimators for alpha and threshold estimation on six UCI datasets, based on anomaly scores from iForest:

"iforest_five_alpha.R"

### Comparing the performance of five estimators for alpha and threshold estimation on six UCI datasets, based on predicted probabilities from classifiers:

"int_five_alpha_10_prob.R"

"int_five_alpha_20_prob.R"

"int_five_alpha_40_prob.R"



