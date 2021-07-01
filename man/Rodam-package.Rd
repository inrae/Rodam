\name{Rodam-package}
\alias{Rodam-package}
\alias{Rodam}
\docType{package}
\title{
Wrapper Functions for 'ODAM' (Open Data for Access and Mining) Web Services
}
\description{
'ODAM' (Open Data for Access and Mining) is a framework that implements a simple way to make research data broadly accessible and fully available for reuse, including by a script language such as R. The main purpose is to make a dataset accessible online with a minimal effort from the data provider, and to allow any scientists or bioinformaticians to be able to explore the dataset and then extract a subpart or the totality of the data according to their needs. To install the 'ODAM' software suite  on your own server, this requires a machine (virtual or not) with a recent OS that support Docker. See [1].


The Rodam package has only one class,
\code{\link[Rodam:odamws]{odamws}}
that provides methods to allow you to retrieve online data using 'ODAM' Web Services. This obviously requires that data are implemented according the 'ODAM' approach , namely that the data subsets were deposited in the suitable data repository in the form of TSV files associated with  their metadata also described  in TSV files. See [2] for more details.
}
\author{
Maintainer: Daniel Jacob 
}
\references{
\enumerate{
\item Making experimental data tables in the life sciences more FAIR: a pragmatic approach
  \href{https://academic.oup.com/gigascience/article/9/12/giaa144/6034785}{https://academic.oup.com/gigascience/article/9/12/giaa144/6034785}
\item ODAM: Deployment and User's Guide
  \href{https://inrae.github.io/ODAM/}{https://inrae.github.io/ODAM/}
}
}
\keyword{ package }
\seealso{
The class that actually implements the API layer for the 'ODAM' web services 
\code{\link[Rodam:odamws]{odamws}}
}
