package scratchscala

// ===========================================================================
object LinearAlgebra {
  type Vector = List[Double] // note that scala.Vector may actually be a better choice for performance here 

  /** Adds corresponding elements */
  def add(v: Vector, w: Vector): Vector =
    for { (v_i, w_i) <- v.zipSameSize(w, _ => "vectors must be the same length") }
      yield v_i + w_i         

  /** Subtracts corresponding elements */
  def subtract(v: Vector, w: Vector): Vector =
    for { (v_i, w_i) <- v.zipSameSize(w, _ => "vectors must be the same length") }
      yield v_i - w_i

  /** Sums all corresponding elements */
  def vector_sum(vectors: List[Vector]): Vector = {
    // Check that vectors is not empty
    assert(vectors.nonEmpty, "no vectors provided!")

    // Check the vectors are all the same size
    val num_elements = vectors.head.size
    assert(vectors.forall { _.size == num_elements }, "different sizes!")

    // the i-th element of the result is the sum of every vectorl(i)
    Range(0, num_elements).map { i => vectors.map(_.apply(i)).sum }.toList  
  }

  /** Multiplies every element by c */
  def scalar_multiply(c: Double, v: Vector): Vector =
    v.map(v_i => c * v_i)

  /** Computes the element-wise average */
  def vector_mean(vectors: List[Vector]): Vector = {
    val n = vectors.size
    return scalar_multiply(1.0/n, vector_sum(vectors)) // notice: 1.0 instead of 1 (scala)
  }  

  /** Computes v_1 * w_1 + ... + v_n * w_n */
  def dot(v: Vector, w: Vector): Double =
    (for { (v_i, w_i) <- v.zipSameSize(w, _ => "vectors must be the same length") }
       yield v_i * w_i )
      .sum  

  /** Returns v_1 * v_1 + ... + v_n * v_n */
  def sum_of_squares(v: Vector): Double =
    dot(v, v)

  /** Returns the magnitude (or length) of v */
  def magnitude(v: Vector): Double =
    math.sqrt(sum_of_squares(v))
  
  /** Computes (v_1 - w_1) ** 2 + ... + (v_n - w_n) ** 2 */
  def squared_distance(v: Vector, w: Vector): Double =
    sum_of_squares(subtract(v, w))
  
  /** Computes the distance between v and w */
  def distance0(v: Vector, w: Vector): Double =
    math.sqrt(squared_distance(v, w))
  
  /** Computes the distance between v and w */
  def distance (v: Vector, w: Vector): Double =
    magnitude(subtract(v, w))

  // Another type alias
  type Matrix = List[List[Double]]
  
  /** Returns (# of rows of A, # of columns of A) */
  def shape(A: Matrix): (Int, Int) = {
    val num_rows = A   .size
    val num_cols = A(0).size // number of elements in first row

    (num_rows, num_cols)
  }
  
  /** Returns the i-th row of A (as a Vector) */
  def get_row   (A: Matrix, i: Int): Vector =
    A(i) // A(i) is already the ith row  
    
  /** Returns the j-th column of A (as a Vector) */
  def get_column(A: Matrix, j: Int): Vector =
    A.map(A_i => A_i(j)) // jth element of row A_i for each row A_i

  /** Returns a num_rows x num_cols matrix
      whose (i,j)-th entry is entry_fn(i, j) */
  def make_matrix(num_rows: Int,
                  num_cols: Int,
                  entry_fn: (Int, Int) => Double): Matrix =
    Range(0, num_rows).toList.map { i =>   // create one list for each i
      Range(0, num_rows).toList.map { j => // [entry_fn(i, 0), ... ]
          entry_fn(i, j) } }               // given i, create a list

  /** Returns the n x n identity matrix */
  def identity_matrix(n: Int): Matrix =
      make_matrix(n, n, (i, j) => if (i == j) 1 else 0)

  assert(identity_matrix(5) == List(List(1, 0, 0, 0, 0),
                                    List(0, 1, 0, 0, 0),
                                    List(0, 0, 1, 0, 0),
                                    List(0, 0, 0, 1, 0),
                                    List(0, 0, 0, 0, 1)) )

  val data = List(List(70, 170, 40),
                  List(65, 120, 26),
                  List(77, 250, 19),
                  // ....
                  )

  val friendships = List((0, 1), (0, 2), (1, 2), (1, 3), (2, 3), (3, 4),
                         (4, 5), (5, 6), (5, 7), (6, 8), (7, 8), (8, 9))

  //            user 0  1  2  3  4  5  6  7  8  9
  //
  val friend_matrix = List(List(0, 1, 1, 0, 0, 0, 0, 0, 0, 0),  // user 0
                           List(1, 0, 1, 1, 0, 0, 0, 0, 0, 0),  // user 1
                           List(1, 1, 0, 1, 0, 0, 0, 0, 0, 0),  // user 2
                           List(0, 1, 1, 0, 1, 0, 0, 0, 0, 0),  // user 3
                           List(0, 0, 0, 1, 0, 1, 0, 0, 0, 0),  // user 4
                           List(0, 0, 0, 0, 1, 0, 1, 1, 0, 0),  // user 5
                           List(0, 0, 0, 0, 0, 1, 0, 0, 1, 0),  // user 6
                           List(0, 0, 0, 0, 0, 1, 0, 0, 1, 0),  // user 7
                           List(0, 0, 0, 0, 0, 0, 1, 1, 0, 1),  // user 8
                           List(0, 0, 0, 0, 0, 0, 0, 0, 1, 0))  // user 9

  assert(friend_matrix(0)(2) == 1, "0 and 2 are friends")
  assert(friend_matrix(0)(8) == 0, "0 and 8 are not friends")
  
  // only need to look at one row
  val friends_of_five =
    friend_matrix(5)
      .zipWithIndex
      .flatMap { case (is_friend, i) =>
        if (is_friend == 1) Some(i) else None }

  val height_weight_age = List(70,  // inches,
                               170, // pounds,
                               40 ) // years
  
  val grades = List(95,   // exam1
                    80,   // exam2
                    75,   // exam3
                    62 )  // exam4

  assert(add(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
  assert(subtract(List(5, 7, 9), List(4, 5, 6)) == List(1, 2, 3))  
  assert(vector_sum(List(List(1, 2), List(3, 4), List(5, 6), List(7, 8))) == List(16, 20))  
  assert(scalar_multiply(2, List(1, 2, 3)) == List(2, 4, 6))  
  assert(vector_mean(List(List(1, 2), List(3, 4), List(5, 6))) == List(3, 4))
  assert(dot(List(1, 2, 3), List(4, 5, 6)) == 32) // 1 * 4 + 2 * 5 + 3 * 6
  assert(magnitude(List(3, 4)) == 5)
  assert(sum_of_squares(List(1, 2, 3)) == 14) // 1 * 1 + 2 * 2 + 3 * 3

  val A = List(List(1, 2, 3),  // A has 2 rows and 3 columns
               List(4, 5, 6))

  val B = List(List(1, 2),     // B has 3 rows and 2 columns
               List(3, 4),
               List(5, 6))
     
  assert(shape(List(List(1, 2, 3), List(4, 5, 6))) == (2, 3))  // 2 rows, 3 columns
}

// ===========================================================================
