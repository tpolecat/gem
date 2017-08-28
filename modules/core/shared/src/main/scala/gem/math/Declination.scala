// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package math

import cats.{ Order, Show }
import cats.instances.long._
import gem.parser.CoordinateParsers
import gem.syntax.parser._

/**
 * Celestial latitude, measured in angular distance from the celestial equator. Points north of the
 * celestial equator have positive declinations, while those south have negative declinations. This
 * is a newtype wrapper for an `Angle` constrainted to the range [-90°, 90°], or [270 - 360) +
 * [0 - 90] in terms of the underlying `Angle`. Note that the range is *inclusive* of both poles.
 * @see The helpful [[https://en.wikipedia.org/wiki/Declination Wikipedia]] article.
 */
sealed abstract case class Declination private (toAngle: Angle) {

  // Sanity check … should be correct via the companion constructor.
  assert(
    toAngle.toMicroarcseconds >= Angle.Angle270.toMicroarcseconds ||
    toAngle.toMicroarcseconds <= Angle.Angle90.toMicroarcseconds,
    s"Invariant violated. $toAngle is outside the range [270 - 360) + [0 - 90]"
  )

  /**
   * Offset this [[Declination]] by the given angle, returning the result and a carry bit. A carry
   * of `true` indicates that the result lies on the opposite side of the sphere and the
   * associated [[RightAscension]] (if any) must be flipped by around the 90° axis. Exact,
   * invertible by offseting again by `-a` if carry is false, or by `a` if true; new carry will be
   * the same.
   * @group Operations
   */
  def offset(a: Angle): (Declination, Boolean) =
    Declination.fromAngleWithCarry(toAngle + a)

  /**
   * Format this [[Declination]] as a standard human-readable string. Invertable via
   * `Declination.parse`.
   */
  def format: String =
    toAngle.formatSignedDMS

  final override def toString: String =
    s"Dec($format)"

}

object Declination {

  val Min:  Declination = unsafeFromAngle(Angle.Angle270)
  val Max:  Declination = unsafeFromAngle(Angle.Angle90)
  val Zero: Declination = unsafeFromAngle(Angle.Angle0)

  /**
   * Construct a `Declination` from an `Angle` in [270 - 360) + [0 - 90], if possible.
   * @group Constructors
   */
  def fromAngle(a: Angle): Option[Declination] =
    if (
      a.toMicroarcseconds >= Angle.Angle270.toMicroarcseconds ||
      a.toMicroarcseconds <= Angle.Angle90.toMicroarcseconds
    ) Some(new Declination(a) {}) else None

  /**
   * Construct a `Declination` from an `Angle`, mirroring about the 90° axis if out of range and
   * reporting whether or not mirroring was required. This operation is useful when offsetting might
   * cause coordinates to cross the pole, in which case the associated RA will need to be flipped
   * 180°.
   * @group Constructors
   */
  def fromAngleWithCarry(a: Angle): (Declination, Boolean) =
    Declination.fromAngle(a).map((_, false)) getOrElse {
      (Declination.unsafeFromAngle(a mirrorBy Angle.Angle90), true)
    }

  /**
   * Construct a `Declination` from an `Angle` in [270 - 360) + [0 - 90], raising an exception if
   * out of range.
   * @group Constructors
   */
  def unsafeFromAngle(a: Angle): Declination =
    fromAngle(a).getOrElse(sys.error(s"Declination out of range: $a"))

  /** Attempt to parse a `Declination` from a `format`-formatted string. */
  def parse(s: String): Option[Declination] =
    CoordinateParsers.dec.parseExact(s)

  /**
   * Declinations are ordered from south to north.
   * @group Typeclass Instances
   */
  implicit val DeclinationOrder: Order[Declination] =
    Order.by(_.toAngle.toSignedMicroarcseconds)

  implicit val DeclinationShow: Show[Declination] =
    Show.fromToString

}