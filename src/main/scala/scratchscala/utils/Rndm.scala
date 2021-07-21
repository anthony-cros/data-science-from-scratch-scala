package scratchscala.utils

// ===========================================================================
trait Rndm { // reproduce python interface
    def seed(n: Int): Unit
    def random()    : Double
  
    def shuffle[T](values: List[T]): List[T]
    def choice [T](values: List[T]):      T
    
    def randrange(from: Int, to: Int): Int
  
    def uniform(a: Double, b: Double): Double
  }  

  // ===========================================================================
  object ScalaRndm extends Rndm { // scala version
    import scala.util.Random
    
    def seed(n: Int): Unit                    = { Random.setSeed(n) }
    def random()    : Double                  = Random.nextDouble()
    def shuffle[T](values: List[T]): List[T]  = Random.shuffle(values)
    def choice [T](values: List[T]):      T   = values(math.abs(Random.nextInt) % values.size)    
    def randrange(from: Int, to: Int): Int    = math.abs(Random.nextInt() % (to - from)) + from // OK with step = 1  
    def uniform(a: Double, b: Double): Double = if (a > b) uniform(b, a) else (random() * (b - a)) + a
  }

  // ===========================================================================
  object PythonRndm extends Rndm { // so can get "same randomness" as the python code
	  import me.shadaj.scalapy.py
	  import me.shadaj.scalapy.py.SeqConverters

    private lazy val _random  = me.shadaj.scalapy.py.module("random")
  
    // ---------------------------------------------------------------------------
    def seed(n: Int): Unit   = _random.seed(n)
    def random()    : Double = _random.random().as[Double]
	  
    def choice [T](values: List[T]): T = {
	    val size = values.size
	    val indices = Range(0, size).toPythonProxy // trick to get around genericity
	    val index = _random.choice(indices)
	    values(index.as[Int])
	  }

    def shuffle[T](values: List[T]): List[T] = {
	    val size = values.size
	    val indices = Range(0, size).toPythonCopy // trick to get around genericity   
	    _random.shuffle(indices)
	    indices.as[List[Int]].map(values.apply)
    }

    def randrange(from: Int, to: Int): Int    = _random.randrange(from, to).as[Int]  
    def uniform(a: Double, b: Double): Double = _random.uniform(a, b).as[Double]
  }

// ===========================================================================
