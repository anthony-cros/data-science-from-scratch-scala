package scratchscala

import LinearAlgebra.{Vector, squared_distance, vector_mean, distance}

// ===========================================================================
object Clustering {

  def num_differences(v1: Vector, v2: Vector): Int = {
      assert(v1.size == v2.size)
      v1.zip(v2).count { case (x1, x2) => x1 != x2 }
  }
  assert(num_differences(List(1, 2, 3), List(2, 1, 3)) == 2)
  assert(num_differences(List(1, 2),    List(1, 2))    == 0)

  def cluster_means(k: Int,
                    inputs: List[Vector],
                    assignments: List[Int]): List[Vector] = {
    val grouped = assignments.zip(inputs).groupByKey // or could use: .withDefault(Nil)
    
    Range(0, k).toList
      .map { i =>
        val cluster = grouped.getOrElse(i, Nil)
    
        if (cluster.isEmpty) random.choice(inputs)
        else                 vector_mean(cluster) }
  }

  class KMeans(k: Int /* number of clusters */) {
      var means: List[Vector] = _

      /** return the index of the cluster closest to the input */
      def classify(input: Vector): Int = 
          Range(0, k).minBy { i => 
            squared_distance(input, this.means(i)) }
  
      def train(inputs: List[Vector]): Unit = {
        // Start with random assignments
        var assignments = List.fill(inputs.size) { random.randrange(0, k) }

        var stop       = false
        var iterations = 0
        
        println(s"k = ${k}")
        while (!stop) { // with tqdm.tqdm(itertools.count()) as t: for _ in t: --> note: reproducing this in scala is not the point here
          println(s"\titerations = ${iterations += 1; iterations}")
          
          // Compute means and find new assignments
          this.means = cluster_means(k, inputs, assignments)

          val new_assignments = inputs.map(classify)

          // Check how many assignments changed and if we're done
          val num_changed = num_differences(assignments.map(_.toDouble), new_assignments.map(_.toDouble))

          if (num_changed == 0) { 
            stop = true 
          } else {
            // Otherwise keep the new assignments, and compute new means
            assignments = new_assignments

// extraneous?
//this.means = cluster_means(k, inputs, assignments)
            
            //t.set_description(f"changed: {num_changed} / {len(inputs)}")                    
          }
      }
    }
  }

  case class Leaf(
      value: Vector) extends Cluster  
  
  val leaf1 = Leaf(List(10,  20))
  val leaf2 = Leaf(List(30, -15))
  
  case class Merged(
      children: (Cluster, Cluster),
      order   : Int               ) extends Cluster
  
  val merged = Merged((leaf1, leaf2), order=1)
  
  sealed trait Cluster
  
  def get_values(cluster: Cluster): List[Vector] =
      cluster match {
        case Leaf(value)         => List(value)
        case Merged(children, _) =>
          children
            .pairToList
            .flatMap(get_values) }
  
  assert(get_values(merged) == List(List(10, 20), List(30, -15)))
  
  /** compute all the pairwise distances between cluster1 and cluster2
      and apply the aggregation function _distance_agg_ to the resulting list */ 
  def cluster_distance(cluster1: Cluster,
                       cluster2: Cluster,
                       distance_agg: Vector => Double = _.min): Double =
      distance_agg(
        for {
           v1 <- get_values(cluster1)
           v2 <- get_values(cluster2) }
          yield distance(v1, v2) )
  
  def get_merge_order(cluster: Cluster): Double =
    cluster match {
      case _: Leaf          => Int.MaxValue //  Was never merged
      case Merged(_, order) => order }
  
  def get_children(cluster: Cluster): List[Cluster] =
      cluster match {
        case _: Leaf             => throw new IllegalStateException("Leaf has no children")
        case Merged(children, _) => children.pairToList }
    
  def bottom_up_cluster(inputs: List[Vector],
                        distance_agg: Vector => Double = _.min): Cluster = { // as stated in the book, this is a very inefficient implementation (see p276)
      // Start with all leaves
      var clusters: List[Cluster] = inputs.map(Leaf.apply)
  
      def pair_distance(pair: (Cluster, Cluster)): Double =
          cluster_distance(pair._1, pair._2, distance_agg)
  
      // as long as we have more than one cluster left...
      while (clusters.size > 1) { 
  
        // find the two closest clusters      
        val (c1, c2) = (for {
              (cluster1, i) <- clusters.zipWithIndex
               cluster2     <- clusters.slice(0, i) }
            yield { (cluster1, cluster2) })
          .minBy(pair_distance)
  
          // remove them from the list of clusters
          clusters = clusters.filter { c => c != c1 && c != c2 }
  
          // merge them, using merge_order = # of clusters left
          val merged_cluster = Merged((c1, c2), order = clusters.size)
  
          // and add their merge
          clusters = clusters :+ merged_cluster
      }
  
      // when there"s only one cluster left, it
      clusters.head
  }
  
  def generate_clusters(base_cluster: Cluster,
                        num_clusters: Int): List[Cluster] = {
    // start with a list with just the base cluster
    var clusters = List(base_cluster)
  
    // as long as we don't have enough clusters yet...
    while (clusters.size < num_clusters) {      
        // choose the last-merged of our clusters
        val next_cluster = clusters.minBy(get_merge_order _)
  
        // remove it from the list
        clusters = clusters.filterNot(_ == next_cluster)
  
        // and add its children to the list (i.e., unmerge it)
        clusters = clusters ++ get_children(next_cluster)
    }
  
    // once we have enough clusters...
    clusters
  }

  def main(args: Array[String]): Unit = {  
    val inputs: List[List[Double]] = List((-14,-5),(13,13),(20,23),(-19,-11),(-9,-16),(21,27),(-49,15),(26,13),(-46,5),(-34,-1),(11,15),(-49,0),(-22,-16),(19,28),(-12,-8),(-13,-19),(-41,8),(-11,-6),(-25,-9),(-18,-3))
      .map(_.pairToList.map(_.toDouble))

    random.seed(12) // so you get the same results as me

    var clusterer = new KMeans(k=3)
    clusterer.train(inputs)
    var means = clusterer.means.sortWith(Utils.compareIterables)   // sort for the unit test
    
    assert(means.size == 3)
    
    // Check that the means are close to what we expect.
    assert(squared_distance(means(0), List(-44, 5)) < 1)
    assert(squared_distance(means(1), List(-16, -10)) < 1)
    assert(squared_distance(means(2), List(18, 20)) < 1)
    
    random.seed(0)
    clusterer = new KMeans(k=2)
    clusterer.train(inputs)
    means = clusterer.means.sortWith(Utils.compareIterables)
    
    assert(means.size == 2)
    assert(squared_distance(means(0), List(-26, -5)) < 1)
    assert(squared_distance(means(1), List(18, 20)) < 1)
    
    /** finds the total squared error from k-means clustering the inputs */ 
    def squared_clustering_errors(inputs: List[Vector], k: Int): Double = {
        val clusterer   = new KMeans(k)
        clusterer.train(inputs)
        val means       = clusterer.means
        val assignments = inputs.map(clusterer.classify)

        inputs.zip(assignments)
          .map { case (input, cluster) => 
            squared_distance(input, means(cluster)) }
          .sum
    }

    // now plot from 1 up to inputs.size clusters
    
    val ks = Range(1, inputs.size + 1)
    val errors = ks.map { k => squared_clustering_errors(inputs, k) }
    
    plt.plot(ks, errors)
    plt.xticks(ks)
    plt.xlabel("k")
    plt.ylabel("total squared error")
    plt.title("Total Error vs. # of Clusters")
    // plt.show()    
    
    plt.savefig("./output/total_error_vs_num_clusters")
    plt.gca().clear() 
      
    val image_path = "./src/main/resources/girl_with_book.jpg"

    val img =
      utils.Images
        .colorMatrix(image_path)
        .map { _.toList.map { color => // rescale to between 0 and 1
          utils.Images.rgbValues(color).map { _ / 256.0 } } }
        .toList

    val pixels = img.flatten

    clusterer = new KMeans(5)
    clusterer.train(pixels)   // this might take a while

    def recolor(pixel: Vector): Vector = {
        val cluster = clusterer.classify(pixel) // index of the closest cluster
        clusterer.means(cluster) }              // mean of the closest cluster    
    
    val new_img = img.map { _.map(recolor) }

    plt.close()
    
    import me.shadaj.scalapy.py.SeqConverters
    plt.imshow(new_img.toPythonCopy) // TODO: t210721090607 - not sure why toPythonProxy fails here

    plt.axis("off")    
    // plt.show()
    
    plt.savefig("./output/recolored_girl_with_book.jpg")
    plt.gca().clear()

    // ---------------------------------------------------------------------------    
    val base_cluster = bottom_up_cluster(inputs)
    
    val three_clusters = generate_clusters(base_cluster, 3).map(get_values)
    
    // sort smallest to largest
    val tc = three_clusters.sortBy(_.size)
    assert(tc.size == 3)
    assert(tc.map(_.size) == List(2, 4, 14))
    assert(tc(0).sortWith(Utils.compareIterables) == List(List(11, 15), List(13, 13)))    
    
    plt.close()
    
    for { (i, cluster, marker, color) <- Utils.zip(List(1, 2, 3),
                                                   three_clusters, 
                                                   List("D","o","*"),
                                                   List("r","g","b")) }
      yield {
        val (xs, ys) = cluster.map(_.listToPair).unzip
 
        plt.scatter(xs, ys, color=color, marker=marker)
    
        // put a number at the mean of the cluster
        val (x, y) = vector_mean(cluster).listToPair
        plt.plot(x, y, marker="$" + i + "$", color="black")                 
      }
        
    plt.title("User Locations -- 3 Bottom-Up Clusters, Min")
    plt.xlabel("blocks east of city center")
    plt.ylabel("blocks north of city center")
    // plt.show()
    
    
    
    plt.savefig("./output/bottom_up_clusters_min.png")
    plt.gca().clear()
    plt.close()
    
    
    
    val base_cluster_max = bottom_up_cluster(inputs, _.max)
    val three_clusters_max = generate_clusters(base_cluster_max, 3).map(get_values)
    
    
    for { (i, cluster, marker, color) <- Utils.zip(List(1, 2, 3),
                                                   three_clusters_max, 
                                                   List("D","o","*"),
                                                   List("r","g","b")) }
      yield {
        val (xs, ys) = cluster.map(_.listToPair).unzip

        plt.scatter(xs, ys, color=color, marker=marker)
    
        // put a number at the mean of the cluster
        val (x, y) = vector_mean(cluster).listToPair
        plt.plot(x, y, marker="$" + i + "$", color="black")
      }

    plt.title("User Locations -- 3 Bottom-Up Clusters, Max")
    plt.xlabel("blocks east of city center")
    plt.ylabel("blocks north of city center")
    plt.savefig("./output/bottom_up_clusters_max.png")
    plt.gca().clear()
  }

}

// ===========================================================================
