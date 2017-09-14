// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.composite

// import cats._
// import cats.implicits._
// import gem.Track
// import doobie._
import shapeless._
// import shapeless.ops.hlist._
// import shapeless.ops.record._
// import shapeless.record._
// import shapeless.labelled._

object Coproductx {

  /** Encode A :+: B :+: .. :+: CNil as Option[A] :: Option[B] :: ... :: HNil. */
  trait CoproductEncoder[C <: Coproduct] { outer =>
    type Out <: HList
    def empty: Out
    def encode(c: C): Out
    def decode(h: Out): Option[C]
  }
  object CoproductEncoder {

    def apply[A <: Coproduct](implicit ev: CoproductEncoder[A]): ev.type = ev

    type Aux[C <: Coproduct, O] = CoproductEncoder[C] { type Out = O }

    implicit def base[A]: CoproductEncoder.Aux[A :+: CNil, Option[A] :: HNil] =
      new CoproductEncoder[A :+: CNil] {
        type Out = Option[A] :: HNil
        def empty = None :: HNil
        def encode(c: A :+: CNil) = c.select[A] :: HNil
        def decode(h: Out) =
          h match {
            case Some(a) :: HNil => Some(Inl(a))
            case None    :: HNil => sys.error("corrupt - no value present")
          }
      }

    implicit def inductive[A, B <: Coproduct](
      implicit ceb: CoproductEncoder[B]
    ): CoproductEncoder.Aux[A :+: B, Option[A] :: ceb.Out] =
      new CoproductEncoder[A :+: B] {
        type Out = Option[A] :: ceb.Out
        def empty = None :: ceb.empty
        def encode(c: A :+: B) =
          c match {
            case Inl(a) => Some(a) :: ceb.empty
            case Inr(b) => None    :: ceb.encode(b)
          }
        def decode(h: Out) = {
          val cebEmpty = ceb.empty // need a stable identifier
          h match {
            case Some(a) :: `cebEmpty` => Some(Inl(a))
            case None    :: tail       => ceb.decode(tail).map(b => Inr(b) : A :+: B)
            case _                     => sys.error("corrupt - more than one value is present")
          }
        }
      }

  }


////////////////////



}
