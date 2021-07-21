package scratchscala

// ===========================================================================
object MachineLearning {
  
  /** Split data into fractions [prob, 1 - prob] */
  def split_data[X](data: List[X], prob: Double): (List[X], List[X]) = {       
    val cut = (data.size * prob).toInt // Use prob to find a cutoff    
    random.shuffle(data).splitAt(cut)  // and split the shuffled list there.
  }

  val data = Range(0, 1000).toList
  val (train, test) = split_data(data, 0.75)
  
  // The proportions should be correct
  assert(train.size == 750)
  assert(test .size == 250)
  
  // And the original data should be preserved (in some order)
  assert((train ++ test).sorted == data)
  
  def train_test_split[X, Y](xs: List[X],
                             ys: List[Y],
                             test_pct: Double): (List[X], List[X], List[Y], List[Y]) = {
      // Generate the indices and split them.
      val idxs = Range(0, xs.size).toList
      val (train_idxs, test_idxs) = split_data(idxs, 1 - test_pct)
  
      (train_idxs.map(xs.apply), // x_train
       test_idxs .map(xs.apply), // x_test
       train_idxs.map(ys.apply), // y_train
       test_idxs .map(ys.apply)) // y_test
  }
  
  val xs = Range(0, 1000).toList  // xs are 1 ... 1000
  val ys = xs.map(2 * _)          // each y_i is twice x_i
  val (x_train, x_test, y_train, y_test) = train_test_split(xs, ys, 0.25)
  
  // Check that the proportions are correct
  assert(x_train.size == y_train.size)
  assert(                y_train.size == 750)
  assert(x_test .size == y_test .size)
  assert(                y_test .size == 250)
  
  // Check that the corresponding data points are paired correctly.
  assert(x_train.zip(y_train).forall { case (x, y) => y == 2 * x })
  assert(x_test .zip(y_test) .forall { case (x, y) => y == 2 * x })
  
  def accuracy(tp: Int, fp: Int, fn: Int, tn: Int): Double = {
      val correct = tp + tn
      val total = tp + fp + fn + tn
      correct / total.toDouble // notice the .toDouble (scala)
  }
  
  assert(accuracy(70, 4930, 13930, 981070) == 0.98114)
  
  def precision(tp: Int, fp: Int, fn: Int, tn: Int): Double =
      tp / (tp + fp).toDouble // notice the .toDouble (scala)
  
  assert(precision(70, 4930, 13930, 981070) == 0.014)
  
  def recall(tp: Int, fp: Int, fn: Int, tn: Int): Double =
      tp / (tp + fn).toDouble // notice the .toDouble (scala)
  
  assert(recall(70, 4930, 13930, 981070) == 0.005)
  
  def f1_score(tp: Int, fp: Int, fn: Int, tn: Int): Double = {
      val p = precision(tp, fp, fn, tn)
      val r = recall(tp, fp, fn, tn)
  
      2 * p * r / (p + r)
  }
  
}

// ===========================================================================
