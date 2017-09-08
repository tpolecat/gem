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

}
object EitherComposite extends EitherComposite
