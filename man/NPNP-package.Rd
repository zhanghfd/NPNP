\name{NPNP-package}
\alias{NPNP-package}
\docType{package}
\title{
An optimal semi-parametric diagnosis test using multiple biomarkers}
\description{
NPNP is a semi-parametric diagnosis test that combines multiple biomarkers. The biomarker measures are not necessary to be normally distributed, and a data dependent non-linear transformation function is applied to each biomarker using training data. NPNP is based on the Neyman-Pearson lemma so that is statistically efficient.

This package includes one main function (i.e., NPNP) and two simulated datasets (i.e., train and test) for illustration. Using NPNP, a diagnosis model is trained using training samples and new subject(s) can be diagnosed. Two candidate estimation methods can be used to estimate the unknown transformation function, one is `simultaneous' and the other is `sequential'.

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
