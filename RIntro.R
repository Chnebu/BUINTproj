#R intro
# elaborated from input for R-bootcamp of Dr. Matteo Tanadini

#prompt >
5 + 3
results [1]

#ESC to terminate/interrupt a statement
#incomplete statement can be continued on the next line

#storing of result into an object
x_1 <- (5 * (3 + 2) - 11) %% 3
#logical operators == != > < 
#case 
X_1 <- c(1:10)
X_1 == x_1

#objects naming conventions (start with a letter; can contain dots or underscores (e.g. a.1, a_1); cannot contain special characters (e.g. #, *, &, ...)

#script stores sequences of instructions
#comments are identified by #

#VECTORS: c() function to create them (combine)
# numeric vectors (sequences can be created with ":" e.g. 10:20)
# character (or string) vectors(elements defined with quotation marks and can include spaces "Luca Mazzola")
# logical vectors (can be created with logical operators)

#Operations are applied element-wise (with recycling for not matching lengths):
v4 <- c(1:8,ncol=2)
t1b <- c(10,10,10) + c(1,2,3)
#alert for not multiple lenghts
T1A <- c(10,20,30) + 1:6

#Accessing elements of a vector can be done via indexing, using square brackets.
T1A[c(2,4,5)] # (can also be T1A[2,4,5])
t1b[2:3] == t1b[-1]

#MATRICES: matrix() function to create them
V2_xx <- matrix(c(1,1,2,1,2,3,2,3,5,6),ncol=3)
V4_xx <- matrix(c(1,1,2,3,1,2,3,5,2,3,5,8),ncol=3)
V4_xx_bis <- matrix(c(1,1,2,1,2,3,2,3,5,3,5,8),ncol=3)

#Extracting element(s) : Matrix.name[row(s), column(s)]
V4_xx[2:4,]/V3_xx[1:3,] # (but not V4_xx[1:4,]/V3_xx ==> Error in V4_xx[1:4, ]/V3_xx : non-conformable arrays)

#extend the matrix with a new column
V3_xx <- cbind(V3_xx, c(9, 9, 9, 9, 8, 5))


#extract 2&3 rows and first column 
V4_xx[2:3,1]
V4_xx[2:3,1] == V4_xx[2:3]



#DATA FRAMES (similar to tables: every column can be a different type of data) created using the data.frame() function or importing external files
#Accessing elements
# - via square brackets (as for vectors and matrices)
# - via the ”$” symbol (can also use ”partial matching”, but is not suggested)
# - negative indexes can be used to ignore elements (they remain in the original matrix)
 
#Modifying existing data frames
# - columns can be added via the ”$” symbol or the squared brackets
# - rows and columns can be added with the rbind() and cbind() functions (less common)
# - data frames can be combined via the merge() function 
 
#Lists (more flexible objects) can store objects of different classes and different dimensions
l.1 <- list(A = "a", num.vec = 10:5, mat1 = V3_xx, V4_xx_bis)
l.1
l.1$num.vec
l.1$mat1 
(l.1$mat1==l.1[[3]])
l.1[[4]][1:2,3]

#a list can contain another list
l.1[["another.list"]] <- list(B="b",C="c",mat.in=V4_xx)

