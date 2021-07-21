package scratchscala

import LinearAlgebra.{Vector, dot, distance, scalar_multiply, add, vector_mean}

// ===========================================================================
object GradientDescent {

  /** Computes the sum of squared elements in v */ 
  def sum_of_squares(v: Vector): Double = dot(v, v)
  
  def difference_quotient(f: Double => Double,
                          x: Double,
                          h: Double): Double =
      (f(x + h) - f(x)) / h
  
  def square(x: Double): Double = x * x
  
  def derivative(x: Double): Double = 2 * x
  
  def estimate_gradient(f: Vector => Double,
                        v: Vector,
                        h: Double = 0.0001) =
      Range(0, v.size).map { i => partial_difference_quotient(f, v, i, h) }
  
  /** Moves `step_size` in the `gradient` direction from `v` */ 
  def gradient_step(v: Vector, gradient: Vector, step_size: Double): Vector = {
      assert(v.size == gradient.size)
      val step = scalar_multiply(step_size, gradient)
      add(v, step)
  }

  def sum_of_squares_gradient(v: Vector): Vector = v.map(2 * _)
  
  // x ranges from -50 to 49, y is always 20 * x + 5
  val inputs = Range(-50, 50).map { x => (x, 20 * x + 5) }.toList
  
  def linear_gradient(x: Double, y: Double, theta: Vector): Vector = {
      val (slope,  intercept) = theta.listToPair
      val predicted = slope * x + intercept    // The prediction of the model.
      val error = (predicted - y)              // error is (predicted - actual)
      val squared_error = math.pow(error, 2)   // We'll minimize squared error
      val grad = (2 * error * x, 2 * error)    // using its gradient.
      grad.pairToList
  }

  /** Generates `batch_size`-sized minibatches from the dataset */
  def minibatches[T](dataset: List[T],
                     batch_size: Int,
                     shuffle: Boolean = true): Iterator[List[T]] = {
    // Start indexes 0, batch_size, 2 * batch_size, ...
    val batch_starts =
      Range(0, dataset.size, batch_size)
        .toList
        .pipe { values =>      
          if (shuffle) random.shuffle(values)  // shuffle the batches
          else         values }

    batch_starts
      .map { start =>
        val end = start + batch_size
        dataset.slice(start, end) }
      .iterator
  }
  
  /** Returns the i-th partial difference quotient of f at v */
  def partial_difference_quotient(f: Vector => Double,
                                  v: Vector,
                                  i: Int,
                                  h: Double): Double = {
      val w = v.zipWithIndex.map { case (v_j, j) => 
        (v_j + (if (j == i) h else 0)) } // add h to just the ith element of v             
  
      (f(w) - f(v)) / h
  }

  def main(args: Array[String]): Unit = {
    val xs = Range(-10, 11)
    val actuals = xs.map(derivative(_))
    val estimates = xs.map(difference_quotient(square, _, h=0.001))

    // plot to show they"re basically the same
    plt.title("Actual Derivatives vs. Estimates")
    plt.plot(xs, actuals, "rx", label="Actual")       // red  x
    plt.plot(xs, estimates, "b+", label="Estimate")   // blue +
    plt.legend(loc=9)
    // plt.show()      
    
    plt.close()

    // ---------------------------------------------------------------------------
    // note: had to move this one out of the main (scala)
    //def partial_difference_quotient(f: Vector => Double, ...

    // ---------------------------------------------------------------------------    
    // "Using the Gradient" example
    
    // pick a random starting point
    var v = List.fill(3) { random.uniform(-10, 10) }
    
    Range(0, 1000).map { epoch =>
        val grad = sum_of_squares_gradient(v)    // compute the gradient at v
        v = gradient_step(v, grad, -0.01)        // take a negative gradient step
        /*println(epoch, v)*/ }      
    assert(distance(v, List(0, 0, 0)) < 0.001)   // v should be close to 0)

    // First "Using Gradient Descent to Fit Models" example

    // Start with random values for slope and intercept.
    var theta = (random.uniform(-1, 1), random.uniform(-1, 1))
    
    val learning_rate = 0.001
    
    Range(0, 5000).foreach { epoch =>
        // Compute the mean of the gradients
        val grad = vector_mean(inputs.map { case (x, y) => linear_gradient(x, y, theta.pairToList) })
        
        // Take a step in that direction
        theta = gradient_step(theta.pairToList, grad, -learning_rate).listToPair
        /*println(epoch, theta)*/ }
    
    var slope     = theta._1
    var intercept = theta._2
    assert(19.9 < slope     && slope     < 20.1, "slope should be about 20")
    assert( 4.9 < intercept && intercept <  5.1, "intercept should be about 5")
    
    
    // Minibatch gradient descent example
    
    theta = (random.uniform(-1, 1), random.uniform(-1, 1))
    
    Range(0, 1000).foreach { epoch =>
        minibatches(inputs, batch_size=20).foreach { batch =>            
            val grad = vector_mean(batch.map { case (x, y) => linear_gradient(x, y, theta.pairToList) })
            theta = gradient_step(theta.pairToList, grad, -learning_rate).listToPair }
        /*println(epoch, theta)*/ }

    slope     = theta._1
    intercept = theta._2
    assert(19.9 < slope     && slope     < 20.1, "slope should be about 20")
    assert( 4.9 < intercept && intercept <  5.1, "intercept should be about 5")

    
    // Stochastic gradient descent example
    
    theta = (random.uniform(-1, 1), random.uniform(-1, 1))
    
    Range(0, 100).foreach { epoch =>
        inputs.map { case (x, y) =>
            val grad = linear_gradient(x, y, theta.pairToList)
            theta = gradient_step(theta.pairToList, grad, -learning_rate).listToPair }
        /*println(epoch, theta)*/ }
    
    slope     = theta._1
    intercept = theta._2
    assert(19.9 < slope     && slope     < 20.1, "slope should be about 20")
    assert( 4.9 < intercept && intercept <  5.1, "intercept should be about 5")
  }

}

// ===========================================================================
