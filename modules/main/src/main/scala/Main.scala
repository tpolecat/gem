// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.main

import cats.data.StateT
import cats.effect.IO
import gem.dao.{ DatabaseConfiguration => DBC }
import fs2.{ Stream, StreamApp }

/** Main entry point for gem. Starts up web and telnet servers. */
object Main extends StreamApp[IO] {
  import StreamApp.ExitCode

  // When we start the app with docker we pass a few config values as environment variables, which
  // we substitute over the default properties. We will eventually plug in a real config layer here.
  def config: IO[MainConfiguration] = {

    // The env variables we know about.
    val ENV_GEM_DB_URL : String = "GEM_DB_URL"
    val ENV_GEM_DB_USER: String = "GEM_DB_USER"
    val ENV_GEM_DB_PASS: String = "GEM_DB_PASS"

    // Update a DatabaseConfiguration from an env variable if it's defined.
    def update(env: String, f: (DBC, String) => DBC): StateT[IO, DBC, Unit] =
      StateT(a => IO(sys.env.get(env).fold((a, ()))(s => (f(a, s), ()))))

    // A program to update all configurable properties, where defined.
    val updateAll: StateT[IO, DBC, Unit] =
      for {
        _ <- update(ENV_GEM_DB_URL,  (a, b) => a.copy(connectUrl = b))
        _ <- update(ENV_GEM_DB_USER, (a, b) => a.copy(userName = b))
        _ <- update(ENV_GEM_DB_PASS, (a, b) => a.copy(password = b))
      } yield ()

    // Test configuration with its database hunk updated, perhaps.
    val cfg = MainConfiguration.forTesting
    updateAll.runS(cfg.database).map(a => cfg.copy(database = a))

  }

  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] =
    for {
      cfg <- Stream.eval(config)
      _   <- MainServer.stream(cfg)
      _   <- Stream.eval(IO(Console.println("Press a key to exit."))) // scalastyle:off
      _   <- Stream.eval(IO(scala.io.StdIn.readLine()))
      _   <- Stream.eval(requestShutdown)
    } yield ExitCode(0)

}