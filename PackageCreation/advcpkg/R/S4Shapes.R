## S4 object set-up: shape, triangle, circle, and rectangle (I placed this comment here manually for my own sake)

#' An S4 class representing a shape
#'
#' @slot color a character string denoting the color of the shape
#'
#' @return An object of class shapeS4
#' @export
#' @importFrom methods new
#'
#' @examples
#' new("shapeS4", color = "red")
setClass("shapeS4", slots = list(color = "character"))


#' Constructor function for shapeS4 class.
#'
#' @param color a character string denoting the color of the shape.
#'
#' @return an object of class shapeS4.
#' @export
#'
#' @examples
#' shapeS4("purple")
shapeS4 <- function(color = "blue")
{
  myShapeS4 <- new("shapeS4", color = color)
  return(myShapeS4)
}

#' Print method for object of class shapeS4
#'
#' @param x an object of class shapeS4
#' @param ... ignored for this function
#'
#' @return a textual representation of the shapeS4 object
#' @method print shapeS4
#'
#' @export
#'
#' @examples print(shapeS4("red"))
setMethod("print",
          c(x = "shapeS4"),
          function(x, ...) {
            cat(paste("I don't have enough information to tell you anything, except that the shape is ", x@color, ".\n", sep = ""))
          }
)


#' An S4 class representing a triangle
#'
#' @slot color a character string denoting the color of the triangle
#' @slot type a character string denoting the type of triangle, i.e. right, isosceles
#'
#' @return An object of class triangleS4
#' @export
#' @importFrom methods new
#'
#'
#' @examples
#' new("triangleS4", color = "red", type = "equilateral")
setClass("triangleS4", slots = list(color = "character", type = "character"), contains = c("shapeS4"))



#' Constructor function for triangleS4 class.
#'
#' @param color character string denoting the color of the triangle.
#' @param type character string denoting the type of triangle.
#'
#' @return an object of class triangleS4
#' @export
#'
#' @examples
#' triangleS4("red", type = "equilateral")
triangleS4 <- function(color = "blue", type = "right")
{
  myTriangleS4 <- new("triangleS4", color = color, type = type)
  return(myTriangleS4)
}

#' Print method for object of class triangleS4
#'
#' @param x an object of class triangleS4
#' @param ... ignored for this function
#'
#' @return a textual representation of the triangleS4 object
#' @method print triangleS4
#'
#' @export
#'
#' @examples print(triangleS4("red", "right"))
setMethod("print",
          c(x = "triangleS4"),
          function(x, ...) {
            cat("Here are the properties of the triangle: \n")
            cat(paste("type: ", x@type, "\n", sep = ""))
            cat(paste("color: ", x@color, "\n", sep = ""))
            cat(paste("We have a ", x@color, ", ", x@type, " triangle!\n", sep = ""))
          }
)


#' An S4 class representing a circle
#'
#' @slot radius A numeric scalar.
#' @slot area A numeric scalar.
#' @slot circumference A numeric scalar.
#' @slot color A character string.
#'
#' @return An object of class circleS4.
#' @export
#' @importFrom methods new
#'
#' @examples
#' new("circleS4", radius = 3, area = pi * 3 ^ 2, circumference = 10000000, color = "blue")
setClass("circleS4", slots = list(radius = "numeric", area = "numeric",

                                  circumference = "numeric", color = "character"), contains = "shapeS4")

#' Constructor function for circleS4 class.
#'
#' @param radius a numeric scalar denoting the radius of the circle.
#' @param color a character string denoting the color of the circle.
#'
#' @return an object of class circleS4.
#' @export
#'
#' @examples
#' circleS4(10, "lime green")
circleS4 <- function(radius, color = "blue")
{
  area <- pi * radius ^ 2
  circumference <- 2 * pi * radius
  myCircleS4 <- new("circleS4", radius = radius, area = area,

                    circumference = circumference, color = color)
  return(myCircleS4)
}


#' print method for circleS4 objects
#'
#' @param x An object of class circleS4 
#' @param ... ignored for this function
#'
#' @return a textual representation of the circleS4 object
#' @export
#'
#' @examples print(circleS4(3, 'red'))
setMethod("print",
          c(x = "circleS4"),
          function(x, ...) {
            cat("Here are the properites of the circle:\n")
            cat(paste("radius: ", x@radius, "\n", sep = ""))
            cat(paste("area: ", x@area, "\n", sep = ""))
            cat(paste("circumference: ", x@circumference, "\n", sep = ""))
            cat(paste("color: ", x@color, "\n", sep = ""))
            cat(paste("That's a cool ", x@color, " circle! \n", sep = ""))
          }
)

#' Constructor function for rectS4 class.
#'
#' @slot height numeric scalar representing rectangle height.
#' @slot width numeric scalar representing rectangle width.
#' @slot area numeric scalar representing rectangle area.
#' @slot perimeter numeric scalar representing rectangle perimeter.
#' @slot color character string representing rectangle color.
#'
#' @return an object of class rectS4
#' @export
#' @importFrom methods new
#'
#' @examples
#' new("rectS4", height = 3, width = 3, area = 9, perimeter = 10005, color = "blue")
setClass("rectS4", slots = list(height = "numeric", width = "numeric", area = "numeric",

                                     perimeter = "numeric", color = "character"))


#' Constructor function for rectS4 class.
#'
#' @param height a numeric scalar denoting the height of the rectangle
#' @param width a numeric scalar denoting the width of the rectangle
#' @param color a character string denoting the color of the rectangle
#'
#' @return an object of class rectS4
#' @export
#'
#' @examples
#' rectS4(5, 3.2, "orange")
rectS4 <- function(height, width, color = "blue")
{
  area <- height * width
  perimeter <- 2 * height + 2 * width
  myRectangleS4 <- new("rectS4", height = height, width = width, area = area,

                       perimeter = perimeter, color = color)
  return(myRectangleS4)
}

#' print method for objects of class rectS4
#'
#' @param x an object of class rectS4 
#' @param ... ignored for this function
#'
#' @return a textual representation of the rectS4 object
#' @export
#'
#' @examples print(rectS4(4, 4, "blue"))
setMethod("print",
          c(x = "rectS4"),
          function(x, ...) {
            cat("Here are the properties of the rectangle:\n")
            cat(paste("width: ", x@width, "\n", sep = ""))
            cat(paste("height: ", x@height, "\n", sep = ""))
            cat(paste("area: ", x@area, "\n", sep = ""))
            cat(paste("perimeter: ", x@perimeter, "\n", sep = ""))
            cat(paste("color: ", x@color, "\n", sep = ""))
            if (x@width == x@height)
            {
              cat(paste("Cool! This is actually a ", x@color, " square! \n", sep = ""))
            } else {
              cat(paste("What a nice ", x@color, " rectangle! \n", sep = ""))
            }
          }
)
