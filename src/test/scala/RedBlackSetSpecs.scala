import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import pfds.RedBlackSet
import scala.collection.immutable.Range.Inclusive

class RedBlackSetSpecs extends WordSpec with ShouldMatchers {

  "RedBlackSet" should {

    val rbSet = RedBlackSet(1, 2, 3, 4, 5, 6, 7)

    "contain all the elements" in {
      rbSet.contains(1) should be(true)
      rbSet.contains(2) should be(true)
      rbSet.contains(3) should be(true)
      rbSet.contains(4) should be(true)
      rbSet.contains(5) should be(true)
      rbSet.contains(6) should be(true)
      rbSet.contains(7) should be(true)
    }

    "not contain anything else" in {
      rbSet.contains(0) should be(false)
      rbSet.contains(8) should be(false)
    }
  }

  val bigCount = 1000000

  "Big RedBlackSet with %d elements".format(bigCount) should {
    "work as expected" in {
      val range = 1 to bigCount
      val largeSet = RedBlackSet(range:_*)

      range.forall(largeSet.contains) should be (true)

    }
  }

}
