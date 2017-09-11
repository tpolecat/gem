// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import doobie._
import doobie.implicits._
import gem.dao.composite._
import gem.math._

object TargetDao {
  import ProperMotionComposite._
  import EitherComposite._
  import EphemerisKeyComposite._

  def select(id: Int): ConnectionIO[Option[Target]] =
    Statements.select(id).option

  def insert(target: Target): ConnectionIO[Int] =
    Statements.insert(target).withUniqueGeneratedKeys[Int]("id")

  object Statements {

    // Track sum type is laid out as ProperMotion followed by EphemerisKey
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

    def insert(target: Target): Update0 =
      (fr"""INSERT INTO target (
              name,
              ra_micro, dec_micro, offset_p, offset_q, radial_velocity, parallax, -- proper motion
              ephemeris_key_type, ephemeris_key                                   -- ephemeris key
           ) VALUES""" ++ values(target)).update

  }

}
