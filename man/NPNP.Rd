\name{NPNP}
\alias{NPNP}
\title{
Disease diagnosing through combining multiple biomarkers.
}
\description{
This function returns false positive rates (FPRs), false negative rates (FNRs), area under ROC curve (AUC), and possibly predicted disease status if test samples are provided. The major inputs of this function include (1) training data information (both biomarker values and disease statuses) and (2) testing data information (only biomarker values) if possible.
}
\usage{
NPNP(training,test_biomarker=NULL,validation_ratio=0.5,n.validation=5,fpr_thre=NULL,transformation='simultaneous')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{training}{A matrix containing the training sample information on both disease status and biomarkers. The first column is for disease statuses and the other columns are for biomarker values.}
  \item{test_biomarker}{A matrix containing the test sample information on biomarkers. The biomarkers in this matrix should correspond to those in `training'.}
  \item{validation_ratio}{Proportion of subjects in the training sample that will be used to for validation, with the default value of 0.5.}
  \item{n.validation}{Number of validation experiments, with the default value of 5.}
  \item{fpr_thre}{False positive rate that determines the threshold for predicting disease status.}
  \item{transformation}{Method for estimating transformation function, which is either `simultaneous' (default) or `sequential'.}
}

\value{
  \item{fpr}{A vector containing false positive rates 0.01, 0.02, ..., 0.99.}
  \item{fnr}{A vector containing false negative rates corresponding to false positive rates.}
  \item{auc}{Area under ROC curve.}
  \item{predict_d}{A vector containing predicted disease statuses, which is provided if `test_biomarker' is available. The threshold used for predicting the disease statuses either is determined by `fpr' (if available) or corresponds to the minimal value of fpr+tpr.}
}

\examples{

data(train);
data(test);

training=as.matrix(train);

test_biomarker=as.matrix(test)[,-1];

res = NPNP(training,test_biomarker=test_biomarker,transformation='simultaneous');

plot(res$fpr,1-res$fnr,xlab='1-Specificity',ylab='Sensitivity',type='l'); # ROC curve

auc = res$auc;  # Area under ROC curve

predict_d = res$predict_d; # Predicted disease statuses



}
