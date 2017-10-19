package morphism

import scalaz._
import java.util.{UUID => JavaUuid}

trait Wrap[A, B] {
  def apply: B => A
}
object Wrap {
  def apply[A, B](f: B => A): Wrap[A, B] =
    new Wrap[A, B] { override def apply = f }
}

trait TryWrap[E, A, B] {
  def apply: B => E \/ A
}
object TryWrap {
  def apply[E, A, B](f: B => E \/ A): TryWrap[E, A, B] =
    new TryWrap[E, A, B] { override def apply = f }
}

trait Unwrap[A, B] {
  def apply: A => B
}
object Unwrap {
  def apply[A, B](f: A => B): Unwrap[A, B] =
    new Unwrap[A, B] { override def apply = f }
}


object Ops {
  def wrap[A, B](b: B)(implicit wrap: Wrap[A, B]): A = wrap.apply(b)
  def unwrap[A, B](a: A)(implicit unwrap: Unwrap[A, B]): B = unwrap.apply(a)
  def tryWrap[E, A, B](b: B)(implicit tryWrap: TryWrap[E, A, B]): E \/ A = tryWrap.apply(b)
}

class AnyString private[AnyString] (override val toString: String) extends AnyVal
object AnyString {
  def apply(s: String): AnyString = new AnyString(s)
  implicit val eq: Equal[AnyString] = Equal.equalA
  implicit val stringWrap : Wrap[AnyString, String] = Wrap[AnyString, String](AnyString.apply)
  implicit val stringUnwrap: Unwrap[AnyString, String] = Unwrap[AnyString, String](_.toString)
}

class Uuid private[Uuid] (val uuid: JavaUuid) extends AnyVal {
  override def toString: String = uuid.toString.toLowerCase
}
case class InvalidUuid(exception: IllegalArgumentException)
object InvalidUuid {
  implicit val eq: Equal[InvalidUuid] = Equal.equalA
}
object Uuid {
  def apply(s: String): InvalidUuid \/ Uuid =
    try {
      \/-(new Uuid(JavaUuid.fromString(s)))
    } catch {
      case e: IllegalArgumentException => -\/(InvalidUuid(e))
    }
  implicit val eq: Equal[Uuid] = Equal.equalA
  implicit val uuidWrap: Wrap[Uuid, JavaUuid] = Wrap[Uuid, JavaUuid](new Uuid(_))
  implicit val uuidUnwrap: Unwrap[Uuid, JavaUuid] = Unwrap[Uuid, JavaUuid](_.uuid)
  implicit val stringTryWrap: TryWrap[InvalidUuid, Uuid, String] = TryWrap[InvalidUuid, Uuid, String](apply)
  implicit val stringUnwrap: Unwrap[Uuid, String] = Unwrap[Uuid, String](_.toString)
}
