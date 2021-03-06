% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HSNE.R
\docType{class}
\name{HSNE_parser-class}
\alias{HSNE_parser-class}
\alias{HSNE_parser}
\title{A Reference Class to represent a HSNE_parser.}
\description{
Parses a .hsne file using the method read_HSNE_binary(filename).
}
\section{Methods}{

\describe{
\item{\code{build_subscale(handle, i, numscales, logger)}}{Initialize a subscale with extracted sparse matrices and vectors.}

\item{\code{read_HSNE_binary(filename, verbose = TRUE)}}{Read HSNE binary from file and construct HSNE object with top- and subscales.}

\item{\code{read_scalar_vector(handle)}}{Read float vector from HDI binary file.}

\item{\code{read_unit_vector(handle)}}{Read unsigned int vector from HDI binary file.}
}}

