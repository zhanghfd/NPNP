\name{NPNP_diagnosis}
\alias{NPNP_diagnosis}
\title{
Disease diagnosing through combining multiple biomarkers.
}
\description{
This function returns the optimal combination and the corresponding predicted disease status for testing samples. The inputs of this function include (1) training data information (2) testing data information (3) estimated parameters using training sample.
}
\usage{
NPNP_diagnosis(test,test_d,train,train_d,trained_result)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{test}{Testing data (biomarkers values) used for optimal combination.}
  \item{test_d}{Testing data (disease statuses) used for optimal combination.}
  \item{train}{Training data (biomarkers values) used for parameter estimation.}
  \item{train_d}{Training data (disease statuses) used for parameter estimation.}
  \item{trained_result}{A list including trained results.}
}

\value{
  \item{com}{The optimal combination.}
  \item{d}{The predicted disease status according to the optimal combination ``com''.}
}

\examples{

data(train);
train_y=train[,-1];
train_d=train[,1];

trained_result<-NPNP_training(train_y,train_d,transformation='simultaneous')

data(test);
test_y=test[,-1];
test_d=test[,1];
rest_result=NPNP_diagnosis(test_y,test_d,train_y,train_d,trained_result)
optim_com=rest_result$com;
optim_d=rest_result$d;

}
