\name{SSplots_kleisner}
\alias{SSplots_kleisner}
\title{Stock-Catch-Status Plot or Catch by Stock Status Graph (Kleisner and Pauly (2011) and Kleisner et al. (2013))}
\usage{
SSplots_kleisner(data,lower.lt,upper.lt, tsplots, MA)
}
\arguments{
\item{data}{dataset}
\item{lower.lt}{lower limit}
\item{upper.lt}{upper limit}
\item{tsplots}{time series plot}
\item{MA}{moving average}}
\description{
This function provides stock status plots to see the typical transition from rebuilding through overexploited to collapsed for each resources.
}
\value{
This function returns two plots one with "number of stocks by status" and the other one is "catch by stock status" apart from the time seriers plots.
}
\details{
Table 2. Criteria used by (Kleisner and Pauly (2011) and Kleisner et al. (2013) to interpret the status of fishery resource.
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
\tabular{lcc}{
Status of fishery  \tab |\tab  Criterion Applied
\cr-----------------------------\tab|\tab--------------------------------------------------------------------------------------------\cr
Rebuilding(Recovering)\tab|\tab  Year of landings > year of post-max-min landings AND post-max-min landings < 10  percent of max landings AND landings is between 10-50 percent of max landings \cr
Developing\tab|\tab  Year of landings < year of max landings AND landings <= 50 percent of max landings OR year of max landings= final year of landings \cr
Exploited\tab|\tab  Landings > 50 percent of max landings \cr
Overexploited\tab|\tab Year of landings > year of max landings AND landings is between 10-50 percent of max landings\cr
Collapsed\tab|\tab Year of landings > year of max landings AND landings< 10 percent of max landings\cr
 }
}
\examples{
library (SSplots)
data(SampleData)
SSplots_kleisner(data=SampleData,lower.lt=10,upper.lt=50, tsplots=FALSE, MA=FALSE)
}
\note{Note1: Here, post-maximum-minimum (post-max-min) indicates the minimum landings occuring after the maximum catch.
\cr\cr
Note2: tsplots=TRUE for generating the time series plots for each resources. In that case it is advisable to set a working directory and number of graphs generated will be equal to the number of resources.
\cr\cr
Note3: MA=TRUE for using the running average of order 3 (a three year running average was used to smooth the curve).
}
\keyword{Time Series Plot}
\keyword{Stock Status plots}
\references{
1) Grainger RJR and Garcia S (1996) Chronicles of marine fisheries landings (1950-1994): trend analysis and fisheries potential. FAO Fisheries Technical Paper 359, 51 p.

2) Kristin Kleisner and Daniel Pauly (2015) Stock-Status Plots (SSPs), https://www.seaaroundus.org/stock-status-plots-method/ (accessed on 28.02.2023)

3) Pauly D, Alder J, Booth S, Cheung WWL, Christensen V, Close C, Sumaila UR, Swartz W, Tavakolie A, Watson R and Zeller D (2008) Fisheries in Large Marine Ecosystems: Descriptions and Diagnoses. pp. 23-40 In: Sherman K and Hempel G (eds.), The UNEP Large Marine Ecosystem Report: a Perspective on Changing Conditions in LMEs of the World’s Regional Seas. UNEP Regional Seas Reports and Studies No. 182, Nairobi.

4) Sherman K and Hempel G, editors (2008) The UNEP Large Marine Ecosystem report: a Perspective on Changing Conditions in LMEs of the World's Regional Seas. UNEP Regional Seas Reports and Studies No. 182, United Nations Environment Programme, Nairobi. 852 p.
}
