\name{g.kg}
\alias{changeUnits}
\alias{cm.m}
\alias{cm2.g.kg.m2}
\alias{cm2.kg.kg.m2}
\alias{cm2.m2}
\alias{g.cm2.kg.m2}
\alias{g.cm3.kg.m3}
\alias{g.g.kg.kg}
\alias{g.kg}
\alias{g.l.kg.m3}
\alias{g.m2.kg.m2}
\alias{kg.l.kg.m3}
\alias{m2.kg.kg.m2}
\alias{mg.kg}
\alias{Mg.kg}
\alias{mm.m}
\alias{mm2.m2}
\alias{mmol.N.kg.kg.kg}
\alias{months.yr}
\alias{per.kg.kg}
\title{Unit conversion functions}
\usage{
  g.kg(x)

  mg.kg(x)

  cm2.m2(x)

  cm.m(x)

  mm.m(x)

  months.yr(x)

  g.cm2.kg.m2(x)

  m2.kg.kg.m2(x)

  cm2.g.kg.m2(x)

  g.m2.kg.m2(x)

  g.cm3.kg.m3(x)

  per.kg.kg(x)

  mm2.m2(x)

  cm2.kg.kg.m2(x)

  mmol.N.kg.kg.kg(x)

  Mg.kg(x)

  g.l.kg.m3(x)

  kg.l.kg.m3(x)

  g.g.kg.kg(x)

  changeUnits(x, unit.from, unit.to)
}
\arguments{
  \item{x}{The measurement in original units}

  \item{unit.from}{Units to convert from (character)}

  \item{unit.to}{Units to convert to (character)}
}
\value{
  The converted measurement (a numeric vector)
}
\description{
  Converts unit before the dot into the unit after it. For
  example, \code{g.kg} converts from grams to kilograms.
  The utility function \code{transform} is provided for
  convenience.
}

