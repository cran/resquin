% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flag_resp.R
\name{flag_resp}
\alias{flag_resp}
\title{Flag respondents based on response quality indicators}
\usage{
flag_resp(x, ...)
}
\arguments{
\item{x}{A data frame containing response quality indicators. Each column
should be one response quality indicator. Each row should be the
value of the response quality indicator of a respondent.}

\item{...}{Flagging expressions. See details.}
}
\value{
A data frame containing one column per flagging strategy and
the same number of rows as\code{x}. Each column contains \code{T} and \code{F} flags per respondents.
An additional \code{id} column is added as the first column if a column named \code{id}
is present in \code{x}.
}
\description{
Flag respondents with one or more flagging expression.
}
\details{
\code{flag_resp()} works very similar to the popular \code{dplyr::filter()} function. However,
instead of filtering data, \code{flag_resp()} returns a data frame of \code{T} and \code{F} values,
representing which respondents are flagged.

As the first argument, you provide a data frame of response quality indicators,
where each column represents one response quality indicator and each row represents
one respondent.
As the second argument you provide one ore more logical statements to flag respondents.
For example:
\itemize{
\item \code{flag_resp(x,ERS > 0.5)} returns a data frame with one column named \code{ERS > 0.5}. Each
row represents one respondent and shows whether the statement "is the extreme response style
indicator larger than 0.5" is true (\code{T}) or false (\code{F}).
\item \code{flag_resp(x,ERS > 0.5,ii_mean < 3)} returns a data frame with two columns indicating
for which respondents the two flagging expressions are true or false.
}

Note that \code{flag_resp()} is not restricted to functions from the \code{resquin} package.
You can supply any numerical column in the data frame \code{x}. This opens the possibility
to compare flagging strategies based on response quality indicators across
packages and functions.

Use the \code{summary()} function on the results to compare flagging strategies.

For more details see the vignette:
\code{vignette("flagging_respondents", package = "resquin")}
}
\examples{
res_dist_indicators <- resp_distributions(nep) # Create indicator data frame

flagged_respondents <- flag_resp(res_dist_indicators,
                                 ii_mean > 3, # Flagging strategy 1
                                 ii_sd < 2, # Flagging strategy 2
                                 ii_mean > 3 & ii_sd > 2) # Flagging strategy 3
flagged_respondents # A data frame with three columns, each corresponding to one flagging strategy
summary(flagged_respondents) # quickly compare flagging strategies

}
