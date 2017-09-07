// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.enum.Site
import gem.math._
import gem.util.InstantMicros
import java.time.Instant

/**
 * Time/site-parameterized coordinates over a span of time. This generalizes proper motion and
 * ephemerides.
 */
sealed trait Track {
  def at(time: Instant, site: Site): Option[Coordinates]
}

object Track {

  final case class Sidereal(properMotion: ProperMotion) extends Track {
    override def at(time: Instant, site: Site) =
      Some(properMotion.at(time).baseCoordinates)
  }

  final case class Nonsidereal(ephemerisKey: EphemerisKey, ephemerides: Map[Site, Ephemeris]) extends Track {

    override def at(time: Instant, s: Site) =
      ephemeris(s).flatMap(_.get(InstantMicros.truncate(time)))

    def ephemeris(s: Site): Option[Ephemeris] =
      ephemerides.get(s)

  }
  object Nonsidereal {
    def empty(key: EphemerisKey): Nonsidereal =
      Nonsidereal(key, Map.empty)
  }

}
