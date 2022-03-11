package simulation

import outwatch._
import cats.effect.IO

object Main {
  LoadCss()

  def main(args: Array[String]) =
    OutWatch.renderReplace[IO]("#app", App.app).unsafeRunSync()
}
