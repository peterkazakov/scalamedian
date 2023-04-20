import org.scalatest.funsuite.AnyFunSuite
import Median.{generateInts, median, median_priority};

class MedianTest  extends AnyFunSuite {
  test("One elem case") {
    assert(median(Seq(1)) === 1)
    assert(median_priority(Seq(1)) === 1)
  }
  test("Two elems case") {
    assert(median(Seq(7,1)) === 4)
    assert(median_priority(Seq(7,1)) === 4)
  }
  test("Basic odd case") {
    assert(median(Seq(1,3,5,7,3)) === 3)
    assert(median_priority(Seq(1,3,5,7,3)) === 3)
  }
  test("Basic even case") {
    val randomlyGeneratedSeq = Seq(1,3,5,7,3,12,4,5,44,-1)
    assert(median(randomlyGeneratedSeq) === 4.5)
    assert(median_priority(randomlyGeneratedSeq) === 4.5)
  }
  test("Additional odd case") {
    val randomlyGeneratedSeq = Seq(1, 3, 5, 7, 3, 12, 4, 5, 44, -1, 4)
    assert(median(randomlyGeneratedSeq) === 4)
    assert(median_priority(randomlyGeneratedSeq) === 4)
  }
  test("Randomly generated even case") {
    val randomlyGeneratedSeq = generateInts(2000)
    assert(median(randomlyGeneratedSeq) === median_priority(randomlyGeneratedSeq))
  }
  test("Randomly generated odd case") {
    val randomlyGeneratedSeq = generateInts(2001)
    assert(median(randomlyGeneratedSeq) === median_priority(randomlyGeneratedSeq))
  }
}
