package scratchscala

// ===========================================================================
object Driver {
  
  // just to exercise each
  def main(args: Array[String]): Unit = {        
    new java.io.File("./output/").mkdirs()
    
    KNearestNeighbors.MaxCurseDimensions = 10 // else will take a long time
    
		/*  4 */ LinearAlgebra
    /*  5 */ Statistics
    /*  8 */ GradientDescent  .main(Array())      
    /* 11 */ MachineLearning       
    /* 12 */ KNearestNeighbors.main(Array())
    /* 17 */ DecisionTrees    .main(Array())
    /* 20 */ Clustering       .main(Array())

    println("done")
  }
  
}

// ===========================================================================
