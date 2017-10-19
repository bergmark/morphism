package morphism

import java.util.{UUID => JavaUuid}
import org.scalacheck._
import org.scalatest._
import scalaz._
import scalaz.std.string._
import syntax.equal._

object Generators {
  import Arbitrary.arbitrary

  implicit val anyString: Arbitrary[AnyString] = Arbitrary(
    arbitrary[String].map(AnyString.apply)
  )
  implicit val javaUuid: Arbitrary[JavaUuid] =
    Arbitrary(
      for {
        mostSig <- arbitrary[Long]
        leastSig <- arbitrary[Long]
      } yield new JavaUuid(mostSig, leastSig)
    )

  implicit val uuid: Arbitrary[Uuid] = Arbitrary(
    arbitrary[JavaUuid].map(Ops.wrap[Uuid, JavaUuid])
  )
}

object MorphismSpecification extends Properties("Morphism") {
  import Generators._
  import Ops._
  import Prop.forAll

  property("wrap/unwrap") = forAll((a: AnyString) =>
    unwrap[AnyString, String](a) === a.toString &&
    wrap[AnyString, String](a.toString) === a
  )

  property("tryWrap/unwrap") = forAll((a: Uuid) =>
    unwrap[Uuid, String](a) === a.toString &&
    tryWrap[InvalidUuid, Uuid, String](a.toString) === \/-(a)
  )

}

class MorphismTest extends FunSuite with Matchers {
  val javaUuid = JavaUuid.randomUUID()
  val uuid = Ops.wrap[Uuid, JavaUuid](javaUuid)

  // Make sure to workaround overridden ===
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
