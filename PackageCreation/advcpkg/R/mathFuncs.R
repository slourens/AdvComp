# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Example toy sum function
#'
#' This function calculates the sum of x and y, recycled if necessary. Just a toy
#' function to show creation and documentation of a new function.
#'
#' @param x, y numeric vectors or scalars
#'
#' @return a numeric scalar or vector denoting the sum of x and y
#' @export
#'
#' @examples
#' sum2(2, 3)
#' sum2(1:4, 1:7)
#' \dontrun{
#' sum2("x", "y")
#' }
sum2 <- function(x, y)
{
  return(x + y)
}

#' Calculate the distance of x and y vectors (recycled if necessary)
#'
#' This function calculates the usual Euclidian distance between two vectors. The
#' values will be recycled, if necessary.
#'
#' @param x numeric scalar or vector
#' @param y numeric scalar or vector
#'
#' @return a numeric scalar denoting the Euclidian distance between x and y
#' @export
#'
#' @examples
#' dist2(1:5, 1:4)
#' dist2(rnorm(5, 1, 1), rnorm(5, 0, 1))
dist2 <- function(x, y)
{
  sqrt(sum((x - y) ^ 2))
}

## S3 shapes - shape, triangle, circle, and rectangle

#' Constructor for the shapeS3 class
#'
#' @param color a string variable denoting the color of the shape
#'
#' @return an object of class rectS3
#' @export
#'
#' @examples
#' myShapeS3 <- shapeS3("green")
shapeS3 <- function(color = "blue")
{
  myShapeS3 <- list(color = color)
  class(myShapeS3) <- "shapeS3"
  return(myShapeS3)
}

#' Constructor for the rectS3 class
#'
#' @param width a numeric scalar denoting the width of the rectangle
#' @param height a numeric scalar denoting the height of the rectangle
#' @param color a string variable denoting the color of the rectangle
#'
#' @return an object of class rectS3
#' @export
#'
#' @examples
#' myRect <- rectS3(3, 4)
#' mySquare <- rectS3(3, 3)
rectS3 <- function(width, height, color = "blue")
{
  area <- width * height
  perimeter <- 2 * width + 2 * height
  myRectS3 <- list(width = width, height = height, area = area, perimeter = perimeter, color = color)
  class(myRectS3) <- c("rectS3", "shapeS3")
  return(myRectS3)
}

#' Constructor for the circleS3 class
#'
#' @param radius a numeric scalar denoting the radius of the circle
#' @param color a string denoting the color of the circle
#'
#' @return an object of class circleS3
#' @export
#'
#' @examples
#' myCircle <- circleS3(3, "yellow")
circleS3 <- function(radius, color = "blue")
{
  area <- pi * radius ^ 2
  circumference <- 2 * pi * radius
  myCircleS3 <- list(radius = radius, area = area, circumference = circumference, color = color)
  class(myCircleS3) <- c("circleS3", "shapeS3")
  return(myCircleS3)
}

#' Constructor for S3 triangle class
#'
#' @param width a numeric scalar specifying the width of the triangle
#' @param height a numeric scalar specifying the height of the triangle
#' @param color a string variable denoting the color of the triangle
#' @param type a string variable denoting the type of the triangle, right, isosceles, etc.
#'
#' @return an object of class triangleS3
#' @export
#'
#' @examples
#' myTriangle <- triangleS3("green", "isosceles")
triangleS3 <- function(color = "blue", type = "right")
{
  ## area <- width * height / 2
  myTriangleS3 <- list(type = type, color = "blue")
  class(myTriangleS3) <- c("triangleS3", "shapeS3")
  return(myTriangleS3)
}

print.shapeS3 <- function(shapeS3)
{
  print("I don't have enough information to tell you anything.")
}

print.rectS3 <- function(rectS3)
{
  print("Here are the properties of the rectangle:")
  print(paste("width: ", rectS3$width, sep = ""))
  print(paste("height: ", rectS3$height, sep = ""))
  print(paste("area: ", rectS3$area, sep = ""))
  print(paste("perimeter: ", rectS3$perimeter, sep = ""))
  print(paste("color: ", rectS3$color, sep = ""))
  if (rectS3$width == rectS3$height)
  {
    print(paste("Cool! This is actually a ", rectS3$color, " square!", sep = ""))
  } else {
    print(paste("What a nice ", rectS3$color, " rectangle!", sep = ""))
  }
}

print.circleS3 <- function(circleS3)
{
  print("Here are the properites of the circle:")
  print(paste("radius: ", circleS3$radius, sep = ""))
  print(paste("area: ", circleS3$area, sep = ""))
  print(paste("circumference: ", circleS3$circumference, sep = ""))
  print(paste("color: ", circleS3$color, sep = ""))
  print(paste("That's a cool ", circleS3$color, " circle!", sep = ""))
}

print.triangleS3 <- function(triangleS3)
{
  print("Here are the properties of the triangle:")
  ##print(paste("width: ", triangleS3$width, sep = ""))
  ##print(paste("height: ", triangleS3$height, sep = ""))
  ##print(paste("area: ", triangleS3$area, sep = ""))
  print(paste("type: ", triangleS3$type, sep = ""))
  print(paste("color: ", triangleS3$color, sep = ""))
  print(paste("We have a ", triangleS3$color, ", ", triangleS3$type, " triangle!", sep = ""))
}

## S4 object set-up: shape, triangle, circle, and rectangle

#' An S4 class representing a shape
#'
#' @slot color a character string denoting the color of the shape
#'
#' @return An object of class shapeS4
#' @export
#'
#' @examples
#' new("shapeS4", "red")
setClass("shapeS4", slots = list(color = "character"))

shapeS4 <- function(color = "blue")
{
  myShapeS4 <- new("shapeS4", color = color)
  return(myShapeS4)
}

#' An S4 class representing a triangle
#'
#' @slot color a character string denoting the color of the triangle
#' @slot type a character string denoting the type of triangle, i.e. right, isosceles
#'
#' @return An object of class triangleS4
#' @export
#'
#' @examples
#' new("triangleS4", color = "red", type = "equilateral")
setClass("triangleS4", slots = list(color = "character", type = "character"), contains = c("shapeS4"))


triangleS4 <- function(color = "blue", type = "right")
{
  myTriangleS4 <- new("triangleS4", color = color, type = type)
  return(myTriangleS4)
}


#' An S4 class representing a circle
#'
#' @slot radius A numeric
#' @slot area numeric.
#' @slot circumference numeric.
#' @slot color character.
#'
#' @return An object of class circleS4.
#' @export
#'
#' @examples
#' new("circleS4", radius = 3, area = 1005, circumference = 1.2341, color = "blue")
setClass("circleS4", slots = list(radius = "numeric", area = "numeric",

                                  circumference = "numeric", color = "character"), contains = "shapeS4")

## constructor for circleS4, use this for sensible parameter values
circleS4 <- function(radius, color = "blue")
{
  area <- pi * radius ^ 2
  circumference <- 2 * pi * radius
  myCircleS4 <- new("circleS4", radius = radius, area = area,

                    circumference = circumference, color = color)
  return(myCircleS4)
}

#' Title
#'
#' @slot height numeric scalar representing rectangle height.
#' @slot width numeric scalar representing rectangle width.
#' @slot area numeric scalar representing rectangle area.
#' @slot perimeter numeric scalar representing rectangle perimeter.
#' @slot color character string representing rectangle color.
#'
#' @return an object of class rectangleS4
#' @export
#'
#' @examples
#' new("rectangleS4", height = 3, width = 3, area = 9, perimeter = 10005, color = "blue")
setClass("rectangleS4", slots = list(height = "numeric", width = "numeric", area = "numeric",

                                     perimeter = "numeric", color = "character"))


## Constructor for rectangle - use this to create rectangle with sensible parameters
rectangleS4 <- function(height, width, color = "blue")
{
  area <- height * width
  perimeter <- 2 * height + 2 * width
  myRectangleS4 <- new("rectangleS4", height = height, width = width, area = area,

                       perimeter = perimeter, color = color)
  return(myRectangleS4)
}
