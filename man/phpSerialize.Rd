\name{phpSerialize}
\alias{phpSerialize}
\title{ R to PHP Serialization }
\description{
  Serializes R objects for PHP import into an associative
  array. Main use is for building web pages with R-support.
}
\usage{
phpSerialize(x, file = NULL, append = FALSE, associative = "1D",
   simplifyMono=TRUE, phpTestCode = FALSE)
}
\arguments{
  \item{x}{ an object }
  \item{file}{ A file name or a connection, or NULL to return
   the output as a string. If the connection is not open it
   will be opened and then closed on exit.}
  \item{append}{ Append or overwrite the file? }
  \item{associative}{ a character string of \code{"1D"} (default),
  \code{"2D"} or \code{"no"}. For \code{"1D"}, only scalars
  and vectors are serialized as associative arrays which can
  be retrieved by name, while arrays are exported with numeric indexes.
  For \code{"2D"}, arrays are also exported by name. For \code{"no"},
  objects are serialized with numerical indexes only, and factors are
  serialized as integers. }
  \item{simplifyMono}{ if TRUE (default), unnamed vectors of length 1
  are reduced to scalars, which is easier to understand in PHP. 
  For \code{simplyMono=FALSE}, these vectors are serialized as arrays
  with length 1. We need another level of indexing (\code{[1]}) in PHP, 
  but we are closer to the way R ticks. Named vectors are are always 
  serialized as arrays to preserve the name, even if \code{associative="no"}.
  }
  \item{phpTestCode}{ if TRUE, php test code for direct display of
    the associative PHP-array is added.}
  
}
\details{
Scalars are exported as an array with one element. I am considering of making
this an option in later versions, but I am not sure if this is a good idea. 
Comments are welcome.

See the \code{sample} directory for an extended test case and
the \code{web} directory for an Apache-tested web application.

Matrices with dimensions > 2 and complex numbers are currently not supported.

}
\value{
  Returns a string if file=NULL, else writes the string to the given file. 
  For example, \code{phpSerialize(c("a","b"))} produces the output string
  \code{a:2:\{i:1;s:1:"a";i:2;s:1:"b";\}},
  giving \code{Array([1]=>a,[2]=>b)} after unserializing in PHP.
}
\references{
 Serialize PHP: \url{http://jpspan.sourceforge.net/examples/serialize_php.php}
 Manual PHP: \url{http://www.php.net/manual/en/index.php}
}
\author{ Dieter Menne \email{(dieter.menne@menne-biomed.de)}}
\note{ Code was tested mainly with lm, lme and nlme.  }

\seealso{ \code{\link{capture.output}} }

\examples{
# htest object
wc = wilcox.test(rnorm(10)+2,rnorm(10)+5)
phpSerialize(wc)

#lm summary
group = gl(2,10,20, labels=c("Ctl","Trt"))
weight = c(rnorm(10)+10, rnorm(10)+6)
sumlm = summary(lm(weight ~ group))
phpSerialize(sumlm,phpTestCode=TRUE)

}
\keyword{ interface }
\keyword{ connection }
