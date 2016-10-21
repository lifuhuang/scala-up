/**
  * Created by Leo on 2016/10/20.
  */
object Parallel {
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._
  import scala.concurrent.duration.Duration

  def parallel[T1, T2](left: => T1, right: => T2): (T1, T2) = {
    val (f1, f2) = (Future(left), Future(right))
    val r1 = Await.result(f1, Duration.Inf)
    val r2 = Await.result(f2, Duration.Inf)
    (r1, r2)
  }
}
