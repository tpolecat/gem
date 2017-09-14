// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.composite

import cats._
import cats.implicits._
// import gem.Track
import doobie._
import shapeless._
// import shapeless.ops.hlist._
// import shapeless.ops.record._
// import shapeless.record._
// import shapeless.labelled._
import shapeless.ops.coproduct.Inject

object TaggedCoproduct {

  trait TaggedCoproductEncoder[T, C <: Coproduct] { outer =>
    type Out <: HList
    def empty: Out
    def encode(c: C): (T, Out)
    def decode(t: T, h: Out)(implicit ev: Eq[T]): Option[C]
    def apply[A: Inject[C, ?]](a: A): (T, Out) = encode(inj(a))
    def :+:[A](t: Tag[T, A]): TaggedCoproductEncoder.Aux[T, A :+: C, Option[A] :: Out] =
      TaggedCoproductEncoder.inductive(t.tag)(this)
    def unsafeDecode(t: T, h: Out)(implicit ev: Eq[T]): C =
      decode(t, h).getOrElse(sys.error(s"invalid tagged coproduct encoding ($t, $h)"))
    def composite(implicit ev: Composite[(T, Out)], eq: Eq[T]): Composite[C] =
      ev.imap((unsafeDecode _).tupled)(encode)

    def inj[A: Inject[C, ?]](a: A): C = Coproduct(a)

  }
  object TaggedCoproductEncoder {

    type Aux[T, C <: Coproduct, O] = TaggedCoproductEncoder[T, C] { type Out = O }

    def base[T, A](tag: T): TaggedCoproductEncoder.Aux[T, A :+: CNil, Option[A] :: HNil] =
      new TaggedCoproductEncoder[T, A :+: CNil] {
        type Out = Option[A] :: HNil
        def empty = None :: HNil
        def encode(c: A :+: CNil) = (tag, c.select[A] :: HNil)
        def decode(t: T, h: Out)(implicit ev: Eq[T]) =
          if (t =!= tag) None
          else h match {
            case Some(a) :: HNil => Some(Inl(a))
            case None    :: HNil => None
          }
      }

    def inductive[T, A, B <: Coproduct](tag: T)(
      ceb: TaggedCoproductEncoder[T, B]
    ): TaggedCoproductEncoder.Aux[T, A :+: B, Option[A] :: ceb.Out] =
      new TaggedCoproductEncoder[T, A :+: B] {
        type Out = Option[A] :: ceb.Out
        def empty = None :: ceb.empty
        def encode(c: A :+: B) =
          c match {
            case Inl(a) => (tag, Some(a) :: ceb.empty)
            case Inr(b) => ceb.encode(b).map(None :: _)
          }
        def decode(t: T, h: Out)(implicit ev: Eq[T]) = {
          val Empty = ceb.empty // need a stable identifier
          h match {
            case Some(a) :: Empty => if (t === tag) Some(Inl(a)) else None
            case None    :: tail  => if (t =!= tag) ceb.decode(t, tail).map(b => Inr(b) : A :+: B) else None
            case _                => None
          }
        }
      }

  }


  // a little dsl for constructing instances: tagged[Int]("foo") :+: tagged[String]("bar") :+: TNil
  final case class Tag[T, A](tag: T)
  object tagged {
    def apply[A]: TaggedPartial[A] = new TaggedPartial[A]
    final class TaggedPartial[A] {
      def apply[T](t: T): Tag[T, A] = Tag[T, A](t)
    }
  }
  object TNil {
    def :+:[T, A](t: Tag[T, A]): TaggedCoproductEncoder.Aux[T, A :+: CNil, Option[A] :: HNil] =
      TaggedCoproductEncoder.base[T, A](t.tag)
  }

}
