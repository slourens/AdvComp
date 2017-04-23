## S3 shapes - shape, triangle, circle, and rectangle
## Notice you do not need to document methods (generic.class functions)

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
#' @param color a string variable denoting the color of the triangle
#' @param type a string variable denoting the type of the triangle, right, isosceles, etc.
#'
#' @return an object of class triangleS3
#' @export
#'
#' @examples
#' myTriangle <- triangleS3("green", "isosceles")
triangleS3 <- function(type = "right", color = "blue")
{
  ## area <- width * height / 2
  myTriangleS3 <- list(type = type, color = color)
  class(myTriangleS3) <- c("triangleS3", "shapeS3")
  return(myTriangleS3)
}

#' Print method for shapeS3 class
#'
#' @param x An object of class shapeS3
#' @param ... ignored for this method
#'
#' @export
#' @method print shapeS3
#'
#' @return A textual representation of the shape object
#'
#' @examples print(shapeS3("green"))
print.shapeS3 <- function(x, ...)
{
  cat(paste("I don't have enough information to tell you anything, except that the shape is ", x$color, ".\n", sep = ""))
}

#' Print method for rectS3 class
#'
#' @param x An object of class rectS3
#' @param ... ignored for this method
#'
#' @return A textual representation of the rectS3 object
#'
#' @export
#' @method print rectS3
#'
#' @examples print(rectS3(width = 4, height = 3))
print.rectS3 <- function(x, ...)
{
  cat("Here are the properties of the rectangle: \n")
  cat(paste("width: ", x$width, "\n", sep = ""))
  cat(paste("height: ", x$height, "\n", sep = ""))
  cat(paste("area: ", x$area, "\n", sep = ""))
  cat(paste("perimeter: ", x$perimeter, "\n", sep = ""))
  cat(paste("color: ", x$color, "\n",  sep = ""))
  if (x$width == x$height)
  {
    cat(paste("Cool! This is actually a ", x$color, " square!", sep = ""))
  } else {
    cat(paste("What a nice ", x$color, " rectangle!", sep = ""))
  }
}

#' Print method for circleS3 class
#'
#' @param x An object of class circleS3
#' @param ... ignored for this method
#' 
#' @return A textual representation of the circleS3 object
#'
#' @export
#' @method print circleS3
#'
#' @examples print(circleS3(radius = 4))
print.circleS3 <- function(x, ...)
{
  cat("Here are the properites of the circle: \n")
  cat(paste("radius: ", x$radius, "\n", sep = ""))
  cat(paste("area: ", x$area, "\n", sep = ""))
  cat(paste("circumference: ", x$circumference, "\n", sep = ""))
  cat(paste("color: ", x$color, "\n", sep = ""))
  cat(paste("That's a cool ", x$color, " circle! \n", sep = ""))
}

#' Print method for triangleS3 class
#'
#' @param x An object of class triangleS3
#' @param ... ignored for this method
#'
#' @return A textual representation of the triangleS3 object
#'
#' @export
#' @method print triangleS3
#'
#' @examples print(triangleS3())
print.triangleS3 <- function(x, ...)
{
  cat("Here are the properties of the triangle:")
  ##cat(paste("width: ", x$width, "\n", sep = ""))
  ##cat(paste("height: ", x$height, "\n", sep = ""))
  ##cat(paste("area: ", x$area, "\n", sep = ""))
  cat(paste("type: ", x$type, "\n", sep = ""))
  cat(paste("color: ", x$color, "\n", sep = ""))
  cat(paste("We have a ", x$color, ", ", x$type, " triangle! \n", sep = ""))
}
