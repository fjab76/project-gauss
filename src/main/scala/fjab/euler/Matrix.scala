package fjab.euler


object Matrix {

  def columns(arr: Array[Array[Int]]): Array[Array[Int]] =
    for(j <- arr.indices.toArray)
      yield for(i <- arr.indices.toArray) yield arr(i)(j)

  /**
    * j = i
    */
  def mainRightWardsDiagonal(arr: Array[Array[Int]]): Array[Int] =
    for (i <- arr.indices.toArray)
      yield arr(i)(i)

  /**
    * n x n
    * j = n - 1 - i
   */
  def mainLeftWardsDiagonal(arr: Array[Array[Int]]): Array[Int] =
    for (i <- arr.indices.toArray)
      yield arr(i)(arr.length - 1 - i)

  /**
    *  n x n
    *
    *  | 1 2 3 4 |
    *  | 5 6 7 8 |
    *  | 9 0 1 2 |
    *  | 3 4 5 6 |
    *
    *  elements of 1st diagonal: (1,0) (2,1) (3,2) -> i=j+1
    *  elements of 2nd diagonal: (2,0) (3,1)       -> i=j+2
    *  elements of 3rd diagonal: (3,0)             -> i=j+3
    *
    *  k=1..n-1
    *  j=0..n-1-k
    *  i=j+k
    */
  def lowerRightWardsDiagonal(arr: Array[Array[Int]]): Array[Array[Int]] =
    for (k <- (1 to arr.length-1).toArray)
      yield for (j <- (0 to arr.length-1-k).toArray) yield arr(j+k)(j)


  /**
    *  n x n
    *
    *  | 1 2 3 4 |
    *  | 5 6 7 8 |
    *  | 9 0 1 2 |
    *  | 3 4 5 6 |
    *
    *  elements of 1st diagonal: (0,1) (1,2) (2,3) -> i=j-1
    *  elements of 2nd diagonal: (0,2) (1,3)       -> i=j-2
    *  elements of 3rd diagonal: (0,3)             -> i=j-3
    *
    *  k=1..n-1
    *  j=k..n-1
    *  i=j-k
    */
  def upperRightWardsDiagonal(arr: Array[Array[Int]]): Array[Array[Int]] =
    for (k <- (1 to arr.length-1).toArray)
      yield for (j <- (k to arr.length-1).toArray) yield arr(j-k)(j)



  /**
    *  n x n
    *
    *  | 1 2 3 4 |
    *  | 5 6 7 8 |
    *  | 9 0 1 2 |
    *  | 3 4 5 6 |
    *
    *  elements of 1st diagonal: (1,3) (2,2) (3,1) -> j=n-1-i+1
    *  elements of 2nd diagonal: (2,3) (3,2)       -> j=n-1-i+2
    *  elements of 3rd diagonal: (3,3)             -> j=n-1-i+3
    *
    *  k=1..n-1
    *  i=k..n-1
    *  j=n-1-i+k
    */
  def lowerLeftWardsDiagonal(arr: Array[Array[Int]]): Array[Array[Int]] =
    for (k <- (1 to arr.length-1).toArray)
      yield for (i <- (k to arr.length-1).toArray) yield arr(i)(arr.length-1-i+k)


  /**
    *  n x n
    *
    *  | 1 2 3 4 |
    *  | 5 6 7 8 |
    *  | 9 0 1 2 |
    *  | 3 4 5 6 |
    *
    *  elements of 1st diagonal: (0,2) (1,1) (2,0) -> i=n-1-j-1
    *  elements of 2nd diagonal: (0,1) (1,0)       -> i=n-1-j-2
    *  elements of 3rd diagonal: (0,0)             -> i=n-1-j-3
    *
    *  k=1..n-1
    *  j=n-1-k..0
    *  i=n-1-j-k
    */
  def upperLeftWardsDiagonal(arr: Array[Array[Int]]): Array[Array[Int]] =
    for (k <- (1 to arr.length-1).toArray)
      yield for (j <- (arr.length-1-k to 0 by -1).toArray) yield arr(arr.length-1-j-k)(j)



}
