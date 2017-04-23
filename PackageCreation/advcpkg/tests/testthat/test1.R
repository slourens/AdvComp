sS3 <- shapeS3('red')
sS4 <- shapeS4('blue')
triS3 <- triangleS3('red', 'isosceles')

## These tests only assure that there is no output, since we're using cat() 
## which is technically not output according to R
## If printing gave specific elements of shapeObjects, such as those accessed 
## with $, @, then we could check whether it's what we expect via expect_output()... 
expect_silent(sS3)
expect_silent(sS4)
expect_silent(triS3)
