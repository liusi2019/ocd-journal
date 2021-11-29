# Open Category Detection with PAC Guarantees and Sample Size Analysis
Code for replicating the experimental results in ocd journel version.

The iForest (Isolation Forest) inplementation used here is provided by Tadesse ZeMicheal and available from https://github.com/tadeze/osu_iforest .

### Threshold estimate experiments on synthetic data sets:

"Open-Category-Detection-with-PAC-Guaranteesp-Synthetic.ipynb"

### Threshold estimate experiments on benchmark data sets:

"alpha_prime_full.R" (This one is for six UCI datasets and MNIST. Datasets are included in "benchmark_datasets.zip". )

"alpha_prime_tinyimagenet.R" (This one is for Tiny ImageNet. The scores for data points in Tiny ImageNet are included in "benchmark_datasets.zip". The scores are got through the method in Dan Hendrycks and Kevin Gimpel's 2017 paper: A baseline for detecting misclassified and out-of-distribution examples in neural networks.)

### Producing learning curve of nine estimators for alpha on six UCI data sets:

"learning_curve_nine.R"

The code file "KM.py" for the first four estimators: alpha_1, alpha_2, alpha_1score and alpha_2score are modified from the code as in link http://web.eecs.umich.edu/~cscott/code.html#kmpe for the paper "Mixture Proportion Estimation via Kernel Embedding of Distributions" by Harish G. Ramaswamy, Clayton Scott and Ambuj Tewari.

The code files for the last four estimators: C-PC, C-ROC, ROC and SPY in folder "R_files_for_sourcing"  are modified from the code as in repo https://github.com/zflin/PU_learning for the paper "A Flexible Procedure for Mixture Proportion Estimation in Positive–Unlabeled Learning" by Zhenfeng Lin and James P. Long.

### Comparing the performance of five etsimators for alpha and threshold estimation on six UCI datasets, based on anomaly scores from iForest:

"iforest_five_alpha.R"

### Comparing the performance of five estimators for alpha and threshold estimation on six UCI datasets, based on predicted probabilities from classifiers:

"int_five_alpha_10_prob.R"

"int_five_alpha_20_prob.R"

"int_five_alpha_40_prob.R"



