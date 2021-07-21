package scratchscala.utils

import scratchscala._

// ===========================================================================
// useful python abstraction, worth porting as is
class Counter[T] private (val items: List[(T, Count)]) { //TODO: t210721091359 - as list map rather? depends on size and most common data usage pattern
    def apply(value: T): Count = items.find(_._1 == value).map(_._2).getOrElse(0)
	  def size           : Int = items.size

		def values         : List[Count] = items.map(_._2)

		def most_common(n: Int)               : Array[(T, Count)] = most_common(Some(n))
    def most_common(n: Option[Int] = None): Array[(T, Count)] = items.sortBy(-_._2).toArray
  }

  // ===========================================================================
  object Counter {
    
    def apply[T](values: Seq[T]): Counter[T] =
      values
        .map { _ -> () } // sadly not countBy in the stdlib
        .pipe(groupByKeyWithListMap)
        .toList
        .map  { case (value, units) => value -> units.size }
        .pipe { new Counter(_) }

    // ---------------------------------------------------------------------------
    import scala.collection.immutable
    private def groupByKeyWithListMap[K, V](entries: Seq[(K, V)]): immutable.ListMap[K, Seq[V]] = { // inspired by the stdlib's Seq.groupBy
      var m = immutable.ListMap.empty[K, MutableList[V]]
    
      for (elem <- entries) {
        val key = elem._1
        val bldr =
          m.get(key) match {
            case Some(x) => x
            case None =>
              val x = MutableList[V]()
              m = m + (key -> x)
              x }
        bldr += elem._2
      }
    
      val b = immutable.ListMap.newBuilder[K, Seq[V]]
      for ((k, v) <- m)
        b += ((k, v.toList))
    
      b.result
    }

  }

// ===========================================================================
