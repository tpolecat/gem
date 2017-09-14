// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.composite

import doobie._

trait EitherComposite {

  /**
   * Map (Option[A], Option[B]) to Either[A, B], raising an exception on read if both values are
   * defined, or if neither is defined.
   */
  implicit def EitherComposite[A, B](
    implicit coa: Composite[Option[A]],
             cob: Composite[Option[B]]
  ): Composite[Either[A, B]] =
    Composite[(Option[A], Option[B])].imap {
      case (Some(a), None)    => Left(a)
      case (None,    Some(b)) => Right(b)
      case (Some(_), Some(_)) => sys.error("EitherComposite: left and right are both defined")
      case (None,    None)    => sys.error("EitherComposite: neither left nor right is defined")
    } {
      case Left(a) => (Some(a), None)
      case Right(b) => (None, Some(b))
    }

  def taggedEitherComposite[T, A, B](t1: T, t2: T)(
    implicit ct:  Composite[T],
             coa: Composite[Option[A]],
             cob: Composite[Option[B]]
  ): Composite[Either[A, B]] =
    Composite[(T, Option[A], Option[B])].imap {
      case (`t1`, Some(a), None   ) => Left(a)
      case (`t2`, None,    Some(b)) => Right(b)
      case (_,    Some(_), Some(_)) => sys.error("EitherComposite: left and right are both defined")
      case (_,    None,    None   ) => sys.error("EitherComposite: neither left nor right is defined")
      case (t,    _,       _      ) => sys.error(s"Tag $t does not match returned data.")
    } {
      case Left(a)  => (t1, Some(a), None)
      case Right(b) => (t2, None, Some(b))
    }

  import shapeless._

  def taggedCoproductComposite[T, A, B](t1: T, t2: T)(
    implicit ct:  Composite[T],
             coa: Composite[Option[A]],
             cob: Composite[Option[B]]
  ): Composite[A :+: B :+: CNil] =
    Composite[(T, Option[A] :: Option[B]:: HNil)].imap {
      case (`t1`, Some(a) :: None    :: HNil) => Coproduct[A :+: B :+: CNil](a)
      case (`t2`, None    :: Some(b) :: HNil) => Coproduct[A :+: B :+: CNil](b)
      case (_,    Some(_) :: Some(_) :: HNil) => sys.error("EitherComposite: left and right are both defined")
      case (_,    None    :: None    :: HNil) => sys.error("EitherComposite: neither left nor right is defined")
      case (t,    _                         ) => sys.error(s"Tag $t does not match returned data.")
    } { c =>
      c.select[A].map(a => (t1, Some(a) :: None    :: HNil)) orElse
      c.select[B].map(b => (t2, None    :: Some(b) :: HNil)) getOrElse sys.error("unpossible")
    }

}
object EitherComposite extends EitherComposite
