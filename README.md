# ADS Project 5: 

Term: Spring 2018

+ Team 2
+ Projec title: Breast Cancer Classification  [Link to Report](./doc/main.Rmd)
+ Team members
	+ Jiaqi Dong
	+ Xiaochen Fan
	+ Wenyuan Gu
	+ Yiyi Zhang
+ Project summary: We have implemented and examined six different classification methods with both full set of features and the reduced set of features in the [Breast Cancer Wisconsin (Diagnostic) Data Set](https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29) to classify whether the breast cancer is malignant or benign based on the features computed from a digitized image of a fine needle aspirate (FNA) of a breast mass. We have compared the prediction accuracy and running time among different methods and have found two best models are SVM with all features (100% prediction accuracy and 0.07s training time) and Logistic Regression with reduced features (98% prediction accuracy and 0.03s training time).  
	
**Contribution statement**: 
+ Jiaqi Dong: Constructed the svm and randomforest model and train based on train data, then predicted based on test data.
+ Xiaochen Fan: Constructed the gbm and logistic model and train based on train data, then predicted based on test data.
+ Wenyuan Gu:
+ Yiyi Zhang: Construced the Rnotebook and performed the data processing, feature selection and evaluation parts. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
