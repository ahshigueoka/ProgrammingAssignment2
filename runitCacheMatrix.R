library("RUnit")

source("cachematrix.R")

test.suite <- defineTestSuite("CacheMatrix",
	dir = file.path(getwd(), "testdir"),
	testFileRegexp = "Test.+\\.R")

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
