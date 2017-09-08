// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import doobie._
import doobie.implicits._
import gem.dao.meta._
import gem.enum._
import gem.math._

object TargetDao {
  import EnumeratedMeta._

  object Statements {

    implicit val OptionEphemerisKeyComposite: Composite[Option[EphemerisKey]] =
      Composite[Option[(EphemerisKeyType, String)]].imap(
        _.map(p => EphemerisKey.unsafeFromTypeAndDes(p._1, p._2)))(
        _.map(k => (k.keyType, k.des))
      )

    implicit val EpochMeta: Meta[Epoch] =
      Meta[String].xmap(Epoch.unsafeFromString, _.format)

    // m/sec for now
    implicit val RadialVelocityMeta: Meta[RadialVelocity] =
      Meta[Int].xmap(RadialVelocity.apply, _.toMetersPerSecond)

    // N.B., local only. Not great.
    implicit val AngleMeta: Meta[Angle] =
      Meta[Long].xmap(Angle.fromMicroarcseconds, _.toMicroarcseconds)

    implicit val x: Composite[Option[Coordinates]] = null

    implicit def EitherComposite[A, B](
      implicit coa: Composite[Option[A]],
               cob: Composite[Option[B]]
    ): Composite[Either[A, B]] =
      Composite[(Option[A], Option[B])].imap {
        case (Some(a), _) => Left(a)
        case (_, Some(b)) => Right(b)
        case (None, None) => sys.error("neither value was available")
      } {
        case Left(a) => (Some(a), None)
        case Right(b) => (None, Some(b))
      }

    implicit val TrackComposite: Composite[Track] =
      Composite[Either[ProperMotion, EphemerisKey]].imap[Track] {
        case Left(pm)  => Track.Sidereal(pm)
        case Right(ek) => Track.Nonsidereal.empty(ek)
      } {
        case Track.Sidereal(pm)        => Left(pm)
        case Track.Nonsidereal(ek, _)  => Right(ek)
      }

    def select(id: Int): Query0[Target] =
      sql"""
        SELECT
          name,
          ephemeris_key_type,
          ephemeris_key,
          ra_micro,
          dec_micro,
          offset_p,
          offset_q,
          radial_velocity,
          parallax
        FROM  target
        WHERE id = $id
      """.query[Target]

  }

}
