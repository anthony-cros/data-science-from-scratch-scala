package scratchscala

import LinearAlgebra._
import me.shadaj.scalapy.py  

// ===========================================================================
object KNearestNeighbors { // use eg -Xmx8g -Xms8g
  
  def raw_majority_vote(labels: List[String]): String = 
    Counter(labels).most_common(1)(0)._1  

  assert(raw_majority_vote(List("a", "b", "c", "b")) == "b")
  
  /** Assumes that labels are ordered from nearest to farthest. */
  def majority_vote(labels: List[String]): String = {
      val vote_counts = Counter(labels)
      val (winner, winner_count) = vote_counts.most_common(1)(0)
      
      val num_winners = vote_counts.values.filter(_ == winner_count).size
  
      if (num_winners == 1)
          winner                     // unique winner, so it
      else
          majority_vote(labels.init) // try again without the farthest
  }
  
  // Tie, so look at first 4, then "b"
  assert(majority_vote(List("a", "b", "c", "b", "a")) == "b")
  
  case class LabeledPoInt(
      point: Vector,
      label: String)
  
  def knn_classify(k: Int,
                   labeled_points: List[LabeledPoInt],
                   new_point: Vector): String = {
  
      // Order the labeled points from nearest to farthest.
      val by_distance = 
        labeled_points.sortBy { lp =>
          distance(lp.point, new_point) }
  
      // Find the labels for the k closest
      val k_nearest_labels = by_distance.slice(0, k).map(_.label)
  
      // and let them vote.
      majority_vote(k_nearest_labels)
  }
  
  def random_point(dim: Int): Vector =
    List.fill(dim) { random.random() } 
  
  def random_distances(dim: Int, num_pairs: Int): List[Double] =
    List.fill(num_pairs) { 
      distance(random_point(dim), random_point(dim)) }
  
  // ===========================================================================
  var MaxCurseDimensions = 101 // just so we can override to a lower number in tests  
  def main(args: Array[String]): Unit = {
    
    /** sepal_length, sepal_width, petal_length, petal_width, class */
    def parse_iris_row(row: List[String]): LabeledPoInt = {
        val measurements = row.init.map(_.toDouble)
        // class is e.g. "Iris-virginica"; we just want "virginica"
        val label = row.last.split("-").last
    
        LabeledPoInt(measurements, label)
    }  
      
    val iris_data = Utils.readTable("./src/main/resources/iris.data", sep = ',').map(parse_iris_row(_))
        
    // We'll also group just the points by species/label so we can plot them.
    val points_by_species = iris_data.groupBy(_.label).mapValues(_.map(_.point))

    val metrics = List("sepal length", "sepal width", "petal length", "petal width")
    val pairs = for { i <- Range(0, 4); j <- Range(0, 4) if (i < j) } yield (i, j)

    val marks = List("+", ".", "x") // we have 3 classes, so 3 markers

    val tmp = plt.subplots(2, 3)
    val (fig, ax) = tmp.as[(py.Dynamic, List[List[py.Dynamic]])]

    Range(0, 2).foreach { row =>
        Range(0, 3).foreach { col =>
            val (i, j) = pairs(3 * row + col)

            ax(row)(col).set_title(s"${metrics(i)} vs ${metrics(j)}", fontsize=8)
            ax(row)(col).set_xticks(List[Double]())
            ax(row)(col).set_yticks(List[Double]())

            marks
              .zip(points_by_species.toList)
              .foreach { case (mark, (species, points)) =>
                val xs = points.map(_(i))
                val ys = points.map(_(j))
                ax(row)(col).scatter(xs, ys, marker=mark, label=species) } } }

    val size1 = ax.size 
    val size2 = ax(size1 - 1).size    
    ax(size1 - 1)(size2 - 1).legend(loc="lower right", prop = Map("size" -> 6))
    
    // plt.show()
    
    plt.savefig("./output/iris_scatter.png")
    plt.gca().clear()

    random.seed(12)
    val (iris_train, iris_test) = MachineLearning.split_data(iris_data, 0.70)
    assert(iris_train.size == 0.7 * 150)
    assert(iris_test .size == 0.3 * 150)

    // track how many times we see (predicted, actual)
    
    val zipped =
        iris_test.map(_.label)
      .zip  { 
        iris_test.map { iris => knn_classify(5, iris_train, iris.point) } }
    
    val num_correct = zipped.filter(_.same).size

    val confusion_matrix = zipped.groupBy(identity).mapValues(_.size)

    val pct_correct = num_correct / iris_test.size.toDouble

    val dimensions = Range(1, MaxCurseDimensions)

    val avg_distances = MutableList[Double]()
    val min_distances = MutableList[Double]()

    random.seed(0)
    
    dimensions
      .map { dim => // for dim in tqdm.tqdm(dimensions, desc="Curse of Dimensionality"):
        println(s"\tdim=${dim} / 100")
        val distances = random_distances(dim, 10000) // 10,000 random pairs
        avg_distances += distances.sum / 10000       // track the average
        min_distances += distances.min }             // track the minimum    

    val min_avg_ratio = 
      min_distances.zip(avg_distances)
        .map { case (min_dist, avg_dist) =>
          min_dist / avg_dist }
        .toList
    println(min_avg_ratio.mkString("\n"))

    plt.close()
    plt.plot(dimensions, min_avg_ratio) // note: was missing

    // plt.show()
  }
}

// ===========================================================================
