// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.math.{ Coordinates, Ephemeris }
import gem.util.InstantMicros

import cats.effect.IO
import cats.tests.CatsSuite

import fs2.Stream

import java.io.InputStream

import scala.collection.immutable.TreeMap
import scala.io.Source


/** Not really a spec per se, but rather a way to exercise the ephemeris parser
  * with a few fixed examples and get a sense of whether it works.
  */
@SuppressWarnings(Array("org.wartremover.warts.Equals"))
final class EphemerisParserSpec extends CatsSuite with EphemerisTestSupport {

  import EphemerisParserSpec._

  test("Must parse borrelly") {

    val head = eph(
      "2017-Aug-01 00:00:00.000" -> "14 47 38.2973 -01 50 22.540",
      "2017-Aug-02 20:09:36.000" -> "14 47 51.5561 -01 59 08.132",
      "2017-Aug-04 16:19:12.000" -> "14 48 07.0089 -02 07 58.456"
    )

    val tail = eph(
      "2018-Jan-28 07:40:48.000" -> "16 30 47.6507 -13 13 47.719",
      "2018-Jan-30 03:50:24.000" -> "16 31 39.3844 -13 15 44.639",
      "2018-Feb-01 00:00:00.000" -> "16 32 29.4260 -13 17 34.938"
    )

    checkParse("borrelly", head, tail)
  }

  test("Must parse ceres") {

    val head = eph(
      "2017-Aug-01 00:00:00.000" -> "06 38 09.5151 +24 12 20.292",
      "2017-Aug-02 20:09:36.000" -> "06 41 28.7671 +24 12 58.899",
      "2017-Aug-04 16:19:12.000" -> "06 44 47.7154 +24 13 24.383"
    )

    val tail = eph(
      "2018-Jan-28 07:40:48.000" -> "09 15 02.5300 +29 46 51.372",
      "2018-Jan-30 03:50:24.000" -> "09 13 20.3432 +30 00 13.038",
      "2018-Feb-01 00:00:00.000" -> "09 11 36.4958 +30 13 02.822"
    )

    checkParse("ceres", head, tail)
  }

  test("Must parse enceladus") {

    val head = eph(
      "2017-Aug-01 00:00:00.000" -> "17 22 55.2824 -21 54 44.331",
      "2017-Aug-02 20:09:36.000" -> "17 22 38.2185 -21 54 20.115",
      "2017-Aug-04 16:19:12.000" -> "17 22 25.7048 -21 54 44.439"
    )

    val tail = eph(
      "2018-Jan-28 07:40:48.000" -> "18 18 14.9019 -22 29 18.412",
      "2018-Jan-30 03:50:24.000" -> "18 19 03.0465 -22 29 18.472",
      "2018-Feb-01 00:00:00.000" -> "18 19 49.8023 -22 28 35.777"
    )

    checkParse("enceladus", head, tail)
  }

  test("Must parse mars") {

    val head = eph(
      "2017-Aug-01 00:00:00.000" -> "08 39 26.1113 +19 33 27.498",
      "2017-Aug-02 20:09:36.000" -> "08 44 16.7664 +19 15 25.619",
      "2017-Aug-04 16:19:12.000" -> "08 49 06.2770 +18 56 56.576"
    )

    val tail = eph(
      "2018-Jan-28 07:40:48.000" -> "15 55 24.7095 -19 39 29.390",
      "2018-Jan-30 03:50:24.000" -> "16 00 05.7288 -19 54 14.198",
      "2018-Feb-01 00:00:00.000" -> "16 04 46.9735 -20 08 31.938"
    )

    checkParse("mars", head, tail)
  }

  private def checkParse(name: String,
               head: TreeMap[InstantMicros, Coordinates],
               tail: TreeMap[InstantMicros, Coordinates]): org.scalatest.Assertion = {

    val e = EphemerisParser.parse(load(name)).option.getOrElse(Ephemeris.Empty)

    // This works but the error message isn't helpful when it fails.  There
    // should be a way to combine shouldEqual assertions ...
    assert(
      (e.toMap.size                == 101 ) &&
      (e.toMap.to(head.lastKey)    == head) &&
      (e.toMap.from(tail.firstKey) == tail)
    )
  }

  test("Must stream mars") {
    val head = eph(
      "2017-Aug-01 00:00:00.000" -> "08 39 26.1113 +19 33 27.498",
      "2017-Aug-02 20:09:36.000" -> "08 44 16.7664 +19 15 25.619",
      "2017-Aug-04 16:19:12.000" -> "08 49 06.2770 +18 56 56.576"
    )

    val s = stream("mars").through(EphemerisParser.elements[IO])
    val m = TreeMap(s.take(head.size.toLong).runLog.unsafeRunSync: _*)

    assert(m == head)
  }

  test("Must handle errors") {
    val z = InstantMicros.ofEpochMilli(0L) -> Coordinates.Zero
    val s = stream("mars-error")
             .through(EphemerisParser.elements[IO])
             .onError(_ => Stream(z))
    assert(Vector(Some(z)) == s.last.runLog.unsafeRunSync)
  }

  test("Must stream eitherElements") {
    val head = Vector[Either[String, Ephemeris.Element]](
      Right(time("2017-Aug-01 00:00:00.000") -> coords("08 39 26.1113 +19 33 27.498")),
      Right(time("2017-Aug-02 20:09:36.000") -> coords("08 44 16.7664 +19 15 25.619")),
      Left("Failure reading:solarPresence")
    )

    val s = stream("mars-error").through(EphemerisParser.eitherElements[IO])
    val m = s.take(head.size.toLong).runLog.unsafeRunSync()

    assert(m == head)
  }

  test("Must stream validElements") {
    val head = eph(
      "2017-Aug-01 00:00:00.000" -> "08 39 26.1113 +19 33 27.498", // 0
      "2017-Aug-02 20:09:36.000" -> "08 44 16.7664 +19 15 25.619", // 1
      "2017-Aug-06 12:28:48.000" -> "08 53 54.4942 +18 38 00.694"  // 3 (skipping 2)
    )

    val s = stream("mars-error").through(EphemerisParser.validElements[IO])
    val m = TreeMap(s.take(head.size.toLong).runLog.unsafeRunSync: _*)

    assert(m == head)
  }

}

object EphemerisParserSpec {
  private def inputStream(n: String): InputStream =
    getClass.getResourceAsStream(s"$n.eph")

  private def stream(n: String): Stream[IO, String] =
    fs2.io.readInputStream(IO(inputStream(n)), 128)
          .through(fs2.text.utf8Decode)

  private def load(n: String): String =
    Source.fromInputStream(inputStream(n)).mkString
}