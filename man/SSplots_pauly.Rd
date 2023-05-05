\name{SSplots_pauly}
\alias{SSplots_pauly}
\title{Stock-Catch-Status Plot or Catch by Stock Status Graph (Pauly et al., 2008)}
\usage{
SSplots_pauly(data,lower.lt,upper.lt, tsplots)
}
\arguments{
\item{data}{dataset}
\item{lower.lt}{lower limit}
\item{upper.lt}{upper limit}
\item{tsplots}{time series plot}
}
\description{
This function provides plots to see the typical transition from undeveloped through fully exploited, to collapsed for each resources.
}
\value{
This function returns two plots one with "number of stocks by status" and the other one is "catch by stock status" apart from the time seriers plots.
}
\details{
 Table1. Criteria used by Pauly et al (2008) to interpret the status of fishery resource
-----------------------------------------------------------------------------------------------------------------
\tabular{lcc}{
Status of fishery   \tab |\tab  Criterion Applied
\cr-------------------------------------------\tab|\tab--------------------------------------------------------------------------------------------\cr

Undeveloped \tab|\tab  Year< max landings AND landings < 10 percent of max value \cr
Developing \tab|\tab  Year < max landings AND landings is between 10-50 percent of max value \cr
Fully exploited \tab|\tab  Landings > 50 percent of max value \cr
Overexploited \tab|\tab Year > max landings AND landings is between 10-50 percent of max value\cr
Collapsed \tab|\tab Year > max landings AND landings< 10 percent of max value\cr
 }
}

\examples{
library (SSplots)
data(SampleData)
SSplots_pauly(data=SampleData,lower.lt=10,upper.lt=50, tsplots=FALSE)
}
\note{Note: tsplots=TRUE for generating the time series plots for each resources. In that case it is advisable to set a working directory and the number of time series plots generated will be equal to the number of resources.}

\keyword{Time Series Plot}
\keyword{Stock Status plots}


\references{1) Grainger RJR and Garcia S (1996) Chronicles of marine fisheries landings (1950-1994): trend analysis and fisheries potential. FAO Fisheries Technical Paper 359, 51 p.

2) Kristin Kleisner and Daniel Pauly (2015) Stock-Status Plots (SSPs), https://www.seaaroundus.org/stock-status-plots-method/ (accessed on 28.02.2023)

3) Pauly D, Alder J, Booth S, Cheung WWL, Christensen V, Close C, Sumaila UR, Swartz W, Tavakolie A, Watson R and Zeller D (2008) Fisheries in Large Marine Ecosystems: Descriptions and Diagnoses. pp. 23-40 In: Sherman K and Hempel G (eds.), The UNEP Large Marine Ecosystem Report: a Perspective on Changing Conditions in LMEs of the World’s Regional Seas. UNEP Regional Seas Reports and Studies No. 182, Nairobi.

4) Sherman K and Hempel G, editors (2008) The UNEP Large Marine Ecosystem report: a Perspective on Changing Conditions in LMEs of the World's Regional Seas. UNEP Regional Seas Reports and Studies No. 182, United Nations Environment Programme, Nairobi. 852 p.
}
