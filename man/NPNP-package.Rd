\name{NPNP-package}
\alias{NPNP-package}
\docType{package}
\title{
An optimal semi-parametric diagnosis test using muitiple biomarkers}
\description{
NPNP is a semi-parametric diagnosis test that combines multiple biomarkers. The biomarker measures are not necessary to be normally distributed, and a data dependent non-linear transformation function is applied to each biomarker using training data. NPNP is based on the Neyman-Pearson lemma so that is statistically efficient.

This package includes two main functions (i.e., NPNP_training and NPNP_diagnosis) and two simulated datasets (i.e., train and test) for illustration. A diagnosis model is trained using NPNP_training, and new subject(s) can be diagnosed using NPNP_diagnosis. Two candidate estimation methods can be used to estimate the unknown transformation function, one is simultaneous and the other is sequential.

}
\details{
\tabular{ll}{
Package: \tab NPNP\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-11-15\cr
License: \tab GPL (>= 2)\cr
}
}
\author{
Ting Lyu and Hong Zhang

Maintainer: Hong Zhang <zhanghfd@fudan.edu.cn>
}
\references{
Lyu T, Ying Z, Zhang H. (2016) An optimal semi-parametric diagnostic test using multiple biomarkers.
}
\keyword{package}
