package morphism

import java.util.{UUID => JavaUuid}
import org.scalatest._
import scalaz._
import scalaz.std.string._

class MorphismTest extends FunSuite with Matchers {
  val javaUuid = JavaUuid.randomUUID()
  val uuid = Ops.wrap[Uuid, JavaUuid](javaUuid)

  def myEq[A: Equal](x: A, y: A) = assert(implicitly[Equal[A]].equal(x, y))
  test("Verbose syntax") {
    import Ops._
    myEq(wrap[AnyString, String]("s"), AnyString("s"))
    myEq(unwrap[AnyString, String](AnyString("s")), "s")

    myEq(uuid.uuid.toString, javaUuid.toString.toLowerCase)
    myEq(tryWrap[InvalidUuid, Uuid, String](uuid.toString).map(_.uuid.toString), \/-(javaUuid.toString.toLowerCase))
    myEq(unwrap[Uuid, String](uuid), uuid.toString)
  }

  test("Generic") {
    myEq(unwrapToString(uuid), javaUuid.toString)
    myEq(unwrapToString(AnyString("s")), "s")

    myEq(wrapString[AnyString]("s"), AnyString("s"))
    myEq(tryWrapString[InvalidUuid, Uuid](uuid.toString), \/-(uuid))
  }

  def unwrapToString[A](a: A)(implicit unwrap: Unwrap[A, String]): String = unwrap.apply(a)
  def wrapString[A](b: String)(implicit wrap: Wrap[A, String]): A = wrap.apply(b)
  def tryWrapString[E, A](b: String)(implicit tryWrap: TryWrap[E, A, String]): E \/ A = tryWrap.apply(b)
}
