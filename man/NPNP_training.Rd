\name{NPNP_training}
\alias{NPNP_training}
\title{
Training a model for diagnosis test.
}
\description{
This function returns estimated parameters for a diagnosing model.
The inputs of this function include the disease status and biomarker values.
}
\usage{
NPNP_training(train_y,train_d,transformation='simultaneous')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{train_y}{A matrix recording the values from multiple biomarkers (each column repsents a biomarker).}
  \item{train_d}{A vector recording the true disease status valued either 0 (normal) or 1 (diseased).}
 \item{transformation}{Estimation method for transformation function, either `simultaneous' (default value) or `sequential'.}
}


\examples{

data(train);
train_y=train[,-1];
train_d=train[,1];

trained_result=NPNP_training(train_y,train_d,transformation='simultaneous');

}
