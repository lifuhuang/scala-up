/**
  * Created by Leo on 2016/10/21.
  */
import org.scalatest.FlatSpec
class ConcSpec extends FlatSpec{
  "A Conc" should "have size 1000 after appending 1000 elements" in {
    var c: Conc[Int] = Conc.empty
    for (i <- 1 to 1000) {
      c = c.append(i)
    }
    assert(c.size == 1000)
  }

}
