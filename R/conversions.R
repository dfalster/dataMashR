#' Unit conversion functions
#' @description  Converts unit before the dot into the unit after it. For example, \code{g.kg} converts from grams to kilograms. The utility function \code{transform} is provided for convenience.
#' @param x The measurement in original units
#' @param unit.from Units to convert from (character)
#' @param unit.to Units to convert to (character)
#' @return The converted measurement (a numeric vector)
#' @aliases g.kg mg.kg cm2.m2 cm.m mm.m months.yr g.cm2.kg.m2 m2.kg.kg.m2 cm2.g.kg.m2 g.m2.kg.m2
#' g.cm3.kg.m3 per.kg.kg mm2.m2 cm2.kg.kg.m2 mmol.N.kg.kg.kg Mg.kg g.l.kg.m3 kg.l.kg.m3 g.g.kg.kg
g.kg             <-  function(x){x/1000} #from g to kg
mg.kg            <-  function(x){x/1000000} #from mg to kg
cm2.m2           <-  function(x){x/10000} #from cm2 to m2
cm.m             <-  function(x){x/100} #from cm to m
mm.m             <-  function(x){x/1000} #from mm to m
months.yr        <-  function(x){x/12} #from months to yr
g.cm2.kg.m2      <-  function(x){x*10} #from g/cm2 to kg/m2
m2.kg.kg.m2      <-  function(x){1/x} #from m2/kg to kg/m2
cm2.g.kg.m2      <-  function(x){1/x*10} #from cm2/g to kg/m2
g.m2.kg.m2       <-  function(x){x/1000} #from g/m2 to kg/m2
g.cm3.kg.m3      <-  function(x){x*1000} #from g/cm3 to kg/cm3
per.kg.kg        <-  function(x){x/100} #from percentage to decimals
mm2.m2           <-  function(x){x/(10^6)} #from mm2 to m2
cm2.kg.kg.m2     <-  function(x){1000/x} #from cm2/kg to kg/m2
mmol.N.kg.kg.kg  <-  function(x){x*14e-6} #from mmol of nitrogen/kg to kg/kg
Mg.kg            <-  function(x){x/1000} #from megagrams (Mg) to kg
g.l.kg.m3        <-  function(x){x} #from grams/litre to kg/m3
kg.l.kg.m3       <-  function(x){x*1000} #from kilograms/litre to kg/m3
g.g.kg.kg        <-  function(x){x} #stays the same  
transform <- function(x, unit.from, unit.to) {
  if (unit.from != unit.to)
    x <- match.fun(paste(unit.from, unit.to, sep="."))(x)
  x
}