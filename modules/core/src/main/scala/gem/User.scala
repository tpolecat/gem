// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.enum.ProgramRole

import scala.language.implicitConversions

import cats._, cats.data._, cats.implicits._

final case class User[A](
  id: User.Id,
  firstName: String,
  lastName:  String,
  email: String,
  isStaff: Boolean,
  allProgramRoles: Map[Program.Id, Set[A]]
)

object User {
  type Id = String // TODO
}

class UserProgramRoleOps(self: User[ProgramRole]) {

  def programRoles(pid: Program.Id): Set[ProgramRole] =
    self.allProgramRoles.get(pid).orEmpty

  def hasProgramRole(pid: Program.Id, role: ProgramRole): Boolean =
    programRoles(pid).contains(role)

}

trait ToUserProgramRoleOps {
  implicit def toUserProgramRoleOps(self: User[ProgramRole]): UserProgramRoleOps =
    new UserProgramRoleOps(self)
}
