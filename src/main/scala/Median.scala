import scala.collection.mutable
import scala.math._
import util.Random

//https://www.programcreek.com/scala/?code=CrestOfWave%2FSpark-2.3.1%2FSpark-2.3.1-master%2Fcore%2Fsrc%2Fmain%2Fscala%2Forg%2Fapache%2Fspark%2Futil%2Fcollection%2FMedianHeap.scala
object Median extends App {
  private def ascendingOrder(v: Int) = -v
  def generateInts(n: Int): Seq[Int] = Seq.fill(n)(Random.nextInt(1000))

  def median(s: Seq[Int]): Float = {
    val (lower, upper) = s.sortWith(_ < _).splitAt(s.size / 2)
    if (s.size % 2 == 0) (lower.last + upper.head).toFloat / 2 else upper.head
  }

  def median_priority(s: Seq[Int]): Float = {
    val lowerQueue: mutable.PriorityQueue[Int] = mutable.PriorityQueue.empty[Int]
    val higherQueue: mutable.PriorityQueue[Int] = mutable.PriorityQueue.empty[Int](Ordering.by(ascendingOrder))
    s.headOption match {
      case Some(v) => lowerQueue.enqueue(v)
      case None => throw new NoSuchElementException("Median sequence is empty.")
    }

    for (v <- s.tail){
      v match {
          case v if v < lowerQueue.head => lowerQueue.enqueue(v)
          case v if higherQueue.isEmpty || v > higherQueue.head => higherQueue.enqueue(v)
          case v if lowerQueue.size > higherQueue.size => higherQueue.enqueue(v)
          case v => lowerQueue.enqueue(v)
      }


      // balance
      if (lowerQueue.size - higherQueue.size > 1) {
        higherQueue.enqueue(lowerQueue.dequeue)
      }
      else if (higherQueue.size - lowerQueue.size > 1) {
        lowerQueue.enqueue(higherQueue.dequeue)
      }
    }
    if (s.size % 2 == 0) (lowerQueue.head + higherQueue.head).toFloat / 2
    else if (lowerQueue.size > higherQueue.size) lowerQueue.head
    else higherQueue.head
  }

  //comparison: https://gist.github.com/pauca/a7b3264d4102f5d1dec4
  //https://biercoff.com/easily-measuring-code-execution-time-in-scala/

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  //comparison: https://gist.github.com/pauca/a7b3264d4102f5d1dec4
  val randomlyGeneratedSeq = generateInts(20000)
  println("Median sorting result: "+time(median(randomlyGeneratedSeq)))
  println("Median priority result: "+time(median_priority(randomlyGeneratedSeq)))
}
