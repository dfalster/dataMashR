library(testthat)

test_that('grepPosition', {

    #testing function grepPosition
    set.seed(1)
    method       <-  c(paste0(letters[1:5], sample(1:5)), 'c4,a2')
    pattern      <-  c('c4', 'd3')
    methodsList  <-  lapply(method, function(x)strsplit(x,',')[[1]])

    #returns a list
	expect_that(class(grepPosition(pattern, methodsList)), is_identical_to('list'))
	
	#list of the same length as vector pattern
	expect_that(length(grepPosition(pattern, methodsList)), is_identical_to(length(pattern)))
    
    #throws error when input is not a character vector
	pattern      <-  matrix(0,1,2)
    expect_that(grepPosition(pattern, methodsList), throws_error())
    pattern      <-  2
    expect_that(grepPosition(pattern, methodsList), throws_error())
    pattern      <-  NA
    expect_that(grepPosition(pattern, methodsList), throws_error())

    #pattern not found returns null list
	pattern      <-  'm5'
    expect_that(is.null(grepPosition(pattern, methodsList)$m5), is_true())

})


test_that('replaceMethod', {
    
    #testing function replaceMethod
    set.seed(1)
    method           <-  c(paste0(letters[1:3], sample(1:3)), 'c4,a2')
    pattern          <-  c('c4', 'b3')
    methodsList      <-  lapply(method, function(x)strsplit(x,',')[[1]])
	replacement      <-  paste0('mx', 1:2)
	replacementList  <-  grepPosition(pattern, methodsList)

    #substitute the right data
    expect_that(replaceMethod(replacementList, method, replacement), is_identical_to(c("a1","mx2","c2","mx1,a2")))
    
    #throws error when replacement and replacementList do not have the same length
	replacement      <-  paste0('mx', 1:3)
    expect_that(replaceMethod(replacementList, method, replacement), throws_error())

    #pattern not found returns original method list
	pattern          <-  'zf4'
    methodsList      <-  lapply(method, function(x)strsplit(x,',')[[1]])
	replacement      <-  'mx1'
	replacementList  <-  grepPosition(pattern, methodsList)

    expect_that(replaceMethod(replacementList, method, replacement), is_identical_to(method))
})
