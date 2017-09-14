// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import doobie._
import doobie.implicits._
import gem.dao.meta._
import gem.dao.composite._
import gem.enum.TrackType
import shapeless._

object TargetDao {
  import EnumeratedMeta._
  import EphemerisKeyComposite._
  import ProperMotionComposite._
  import TaggedCoproduct._
  import Track._

  def select(id: Int): ConnectionIO[Option[Target]] =
    Statements.select(id).option

  // def insert(target: Target): ConnectionIO[Int] =
  //   Statements.insert(target).withUniqueGeneratedKeys[Int]("id")

  object Statements {

    // Track is laid out as a tagged coproduct: (tag, sidereal, nonsidereal).
    private implicit val TaggedTrackComposite: Composite[Track] = {

      // We map only the ephemeris key portion of the nonsidereal target here, and we only need to
      // consider the Option[Nonsidereal] case because this is what the coproduct encoding needs.
      implicit val compositeOptionNonsidereal: Composite[Option[Nonsidereal]] =
        Composite[Option[EphemerisKey]].imap(_.map(Nonsidereal.empty))(_.map(_.ephemerisKey))

      // irritating, widen these
      val sidereal:    TrackType = TrackType.sidereal;
      val nonsidereal: TrackType = TrackType.nonsidereal;

      // Construct an encoder for track constructors, tagged by TrackType.
      val enc = tagged[Sidereal](sidereal) :+: tagged[Nonsidereal](nonsidereal) :+: TNil

      // from enc we get a Composite[Sidereal :+: Nonsidereal :+: CNil], which we map out to Track
      enc.composite.imap(_.unify) {
        case t: Sidereal    => enc.inj(t)
        case t: Nonsidereal => enc.inj(t)
      }

    }

    def select(id: Int): Query0[Target] =
      sql"""
        SELECT name, track_type,
               ra, dec, epoch, pv_ra, pv_dec, rv, px, -- proper motion
               e_key_type, e_key                      -- ephemeris key
          FROM target
         WHERE id = $id
      """.query[Target]

    // def insert(target: Target): Update0 =
    //   (fr"""INSERT INTO target (
    //           name, track_type,
    //           ra_str, dec_str,
    //           ra, dec, pv_ra, pv_dec, rv, px, -- proper motion
    //           e_key_type, e_key,              -- ephemeris key
    //        ) VALUES""" ++ values(target)).update

  }

}
