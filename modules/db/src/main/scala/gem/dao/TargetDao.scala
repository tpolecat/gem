// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import doobie._
import doobie.implicits._
import gem.dao.meta._
import gem.dao.composite._
import gem.math._

object TargetDao {
  import DeclinationMeta._
  import EitherComposite._
  import EnumeratedMeta._
  import EphemerisKeyComposite._
  import EpochMeta._
  import RadialVelocityMeta._
  import RightAscensionMeta._

  object Statements {

    // Locally we will map angles (parallax) to microarcseconds
    private implicit lazy val AngleMeta: Meta[Angle] =
      Meta[Long].xmap(Angle.fromMicroarcseconds, _.toMicroarcseconds)

    // In this case the Track is laid out as ProperMotion and the EphemerisKey
    private implicit lazy val TrackComposite: Composite[Track] =
      Composite[Either[ProperMotion, EphemerisKey]].imap[Track] {
        case Left(pm)  => Track.Sidereal(pm)
        case Right(ek) => Track.Nonsidereal.empty(ek)
      } {
        case Track.Sidereal(pm)        => Left(pm)
        case Track.Nonsidereal(ek, _)  => Right(ek)
      }

    /** Flattened `a` as a VALUES argument (...). */
    def values[A](a: A)(implicit ev: Composite[A]): Fragment =
      Fragment(List.fill(ev.length)("?").mkString("(", ", ", ")"), a)

    def select(id: Int): Query0[Target] =
      sql"""
        SELECT name,
               ra_micro, dec_micro, offset_p, offset_q, radial_velocity, parallax, -- proper motion
               ephemeris_key_type, ephemeris_key                                   -- ephemeris key
          FROM target
         WHERE id = $id
      """.query[Target]

    def insert(t: Target): Update0 =
      (fr"""INSERT INTO target (
              name,
              ra_micro, dec_micro, offset_p, offset_q, radial_velocity, parallax, -- proper motion
              ephemeris_key_type, ephemeris_key                                   -- ephemeris key
           ) VALUES""" ++ values(t)).update

  }

}
