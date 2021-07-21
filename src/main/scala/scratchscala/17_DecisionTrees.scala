package scratchscala

// ===========================================================================
object DecisionTrees {
  
  /** Given a list of class probabilities, compute the entropy */
  def entropy(class_probabilities: List[Double]): Double =
    class_probabilities
      .filterNot(_ == 0) // ignore zero probabilities
      .map { p => -p * Utils.log2(p) }
      .sum

  assert(entropy(List(1.0))      == 0)
  assert(entropy(List(0.5, 0.5)) == 1)
  assert(entropy(List(0.25, 0.75)).pipe { x => 0.81 < x && x < 0.82 })

  def class_probabilities(labels: List[Any]): List[Double] = {
    val total_count = labels.size.toDouble

    Counter(labels).values.map(_ / total_count)
  }

  def data_entropy(labels: List[Any]): Double =
    entropy(class_probabilities(labels))

  assert(data_entropy(List('a')) == 0)
  assert(data_entropy(List(true, false)) == 1)
  assert(data_entropy(List(3, 4, 4, 4)) == entropy(List(0.25, 0.75)))
  
  /** Returns the entropy from this partition of data into subsets */
  def partition_entropy(subsets: List[List[Any]]): Double = {
		val total_count = subsets.map(_.size).sum
		
		subsets
		  .map { subset => 
		    data_entropy(subset) * subset.size / total_count }
		  .sum
  }
    
  case class Candidate(
      level : String,
      lang  : String,
      tweets: Boolean,
      phd   : Boolean,

      // note: null is poor practice in scala, should use Option
      did_well: Boolean = null.asInstanceOf[Boolean]) // allow unlabeled data      

  // ===========================================================================
  lazy val inputs =
    //              level     lang     tweets phd    did_well
    List(Candidate("Senior", "Java",   false, false, false),
         Candidate("Senior", "Java",   false, true,  false),
         Candidate("Mid",    "Python", false, false, true),
         Candidate("Junior", "Python", false, false, true),
         Candidate("Junior", "R",      true,  false, true),
         Candidate("Junior", "R",      true,  true,  false),
         Candidate("Mid",    "R",      true,  true,  true),
         Candidate("Senior", "Python", false, false, false),
         Candidate("Senior", "R",      true,  false, true),
         Candidate("Junior", "Python", true,  false, true),
         Candidate("Senior", "Python", true,  true,  true),
         Candidate("Mid",    "Python", false, true,  true),
         Candidate("Mid",    "Java",   true,  false, true),
         Candidate("Junior", "Python", false, true,  false) )
      
  /** Partition the inputs into lists based on the specified attribute. */
  def partition_by[T <: Product](inputs: List[T], attribute: String): Map[Any, List[T]] = 
    inputs.groupBy { _.fieldValue(attribute) }

  // ---------------------------------------------------------------------------
  /** Compute the entropy corresponding to the given partition */
  def partition_entropy_by[T <: Product](inputs: List[T],
                                         attribute: String,
                                         label_attribute: String): Double = {

     // partitions consist of our (grouped) inputs
     val partitions = partition_by(inputs, attribute)
		   
     // but partition_entropy needs just the class labels
     val labels =
       partitions
         .values // python dictionary order: unclear here... (https://stackoverflow.com/questions/39980323/are-dictionaries-ordered-in-python-3-6)
         .map {
           _.map(_.fieldValue(label_attribute)) }
         .toList     

     partition_entropy(labels)
  }

  // ---------------------------------------------------------------------------
  List("level","lang","tweets","phd").map { key =>
    println(key -> partition_entropy_by(inputs, key, "did_well")) }

  assert(partition_entropy_by(inputs, "level",  "did_well").pipe { x => 0.69 < x && x < 0.70 })
  assert(partition_entropy_by(inputs, "lang",   "did_well").pipe { x => 0.86 < x && x < 0.87 })
  assert(partition_entropy_by(inputs, "tweets", "did_well").pipe { x => 0.78 < x && x < 0.79 })
  assert(partition_entropy_by(inputs, "phd",    "did_well").pipe { x => 0.89 < x && x < 0.90 })

  lazy val senior_inputs = inputs.filter(_.level == "Senior")

  assert(0.4 == partition_entropy_by(senior_inputs, "lang", "did_well"))
  assert(0.0 == partition_entropy_by(senior_inputs, "tweets", "did_well"))

  assert(partition_entropy_by(senior_inputs, "phd", "did_well").pipe { x => 0.95 < x && x < 0.96 })

  // help readability
  type AttributeName       = String
  type AttributeValue      = Any 
  type LabelAttributeValue = Any  

  case class Leaf(value: LabelAttributeValue) extends DecisionTree

  case class Split(
      attribute    : AttributeName,
      subtrees     : Map[AttributeValue, DecisionTree],
      default_value: AttributeValue = null /* note: not a good practice in Scala... */)
    extends DecisionTree

  sealed trait DecisionTree

  /** classify the input using the given decision tree */
  def classify[T <: Product](tree: DecisionTree, input: T): LabelAttributeValue = {
    tree match {

    	// If this is a leaf node, return its value
      case Leaf(value) => value

		  // Otherwise this tree consists of an attribute to split on
		  // and a dictionary whose keys are values of that attribute
		  // and whose values of are subtrees to consider next
      case split: Split =>
        val subtree_key = input.fieldValue(split.attribute)

        split.subtrees.get(subtree_key) match {
          case None          => split.default_value        // Return the default value if no subtree for key
          case Some(subtree) => classify(subtree, input) } // Choose the appropriate subtree and use it to classify the input.        
    }
  }
  
  // ---------------------------------------------------------------------------
  def build_tree_id3[T <: Product](
                     inputs: List[T],
                     split_attributes: List[String],
                     target_attribute: String)
                   : DecisionTree = {
     // Count target labels    
     val label_counts = 
       inputs
         .map(_.fieldValue(target_attribute))
         .pipe(Counter.apply)

    val most_common_label = label_counts.most_common(1).head._1

    // If there's a unique label, predict it
    if (label_counts.size == 1)        Leaf(most_common_label)

    // If no split attributes left, return the majority label
    else if (split_attributes.isEmpty) Leaf(most_common_label)

    // Otherwise split by the best attribute
    else {

  	  /** Helper function for finding the best attribute */
      def split_entropy(attribute: String): Double = 
        partition_entropy_by(inputs, attribute, target_attribute)

      val best_attribute = split_attributes.minBy(split_entropy)

      val partitions = partition_by(inputs, best_attribute)
      val new_attributes = split_attributes.filterNot(_ == best_attribute)  
     
      // recursively build the subtrees
      val subtrees = 
        partitions
        .map { case (attribute_value, subset) =>
          attribute_value ->             
            build_tree_id3(
                subset,
          		  new_attributes,
          		  target_attribute) }

      Split(best_attribute, subtrees, default_value=most_common_label)        
    }
  }

  // ===========================================================================
  def main(args: Array[String]): Unit = {

    val hiring_tree = Split("level", Map(  // First, consider "level".
        "Junior" -> Split("phd", Map(      // if level is "Junior", next look at "phd"
            false -> Leaf(true),           //   if "phd" is false, predict true
            true  -> Leaf(false)           //   if "phd" is true,  predict false
        )),
        "Mid" -> Leaf(true),               // if level is "Mid", just predict true
        "Senior" ->  Split("tweets", Map(  // if level is "Senior", look at "tweets"
            false -> Leaf(false),          //   if "tweets" is false, predict false
            true  -> Leaf(true)            //   if "tweets" is true,  predict true
        ))
    ))

    // ---------------------------------------------------------------------------
    val tree = build_tree_id3(inputs,
                              List("level", "lang", "tweets", "phd"),
                              "did_well")

    // Should predict true
    assert(classify(tree, Candidate("Junior", "Java", true, false)) == true)

    // Should predict false
    assert(classify(tree, Candidate("Junior", "Java", true, true)) == false)
    
    // Should predict true
    assert(classify(tree, Candidate("Intern", "Java", true, true)) == true)
  }
}

// ===========================================================================
