// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import cats.effect.IO
import doobie._, doobie.implicits._
import gem.Program
import gem.syntax.prism._

import scala.collection.immutable.TreeMap

/** Base trait for DAO test cases.
  */
trait DaoTest extends gem.Arbitraries {
  val pid = Program.Id.fromString.unsafeGet("GS-1234A-Q-1")

  protected val xa: Transactor[IO] =
    Transactor.after.set(DatabaseConfiguration.forTesting.transactor[IO], HC.rollback)

  def withProgram[A](test: ConnectionIO[A]): A =
    (for {
      _ <- ProgramDao.insertFlat(Program(pid, "Test Prog", TreeMap.empty))
      a <- test
    } yield a).transact(xa).unsafeRunSync()

}
