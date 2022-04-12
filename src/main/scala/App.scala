package simulation

import outwatch._
import outwatch.dsl._
import colibri._
import cats.effect.SyncIO

object App {
  val app = {
    val tickTime     = Subject.behavior(1000)
    val speed        = Subject.behavior(0.0)
    val resetTrigger = Subject.behavior(())

    val refreshMs = 500
    val subSteps  = 600
    var lastStep  = Simulation.nowSeconds

    val tick = resetTrigger
      .combineLatest(tickTime)
      .switchMap { case (_, tickTime) =>
        Observable
          .intervalMillis(tickTime)
          .map(_ => if (tickTime == 0) subSteps else 1)
      }
      .publish

    tick.value.foreach { substeps =>
      flatland.loop(substeps) { _ =>
        Simulation.nextStep()
      }
    }

    div(
      tick.value.scan(0L)((sum, substeps) => sum + substeps).sampleMillis(refreshMs).map(timeSpanFromSeconds),
      div("speed: ", speed, "x"),
      managed(SyncIO(tick.connect())),
      SpeedSlider(tickTime),

      // visualization runs independently of simulation
      div(
        cls := "flex items-start",
        tick.value.sampleMillis(refreshMs).map { _ =>
          speed.onNext((Simulation.nowSeconds - lastStep).toDouble * 1000.0 / refreshMs)
          lastStep = Simulation.nowSeconds

          VDomModifier(
            showPage(
              "New",
              (Simulation.submissions.id.length - 1) to Math.max(Simulation.submissions.id.length - 30, 0) by -1,
            ),
            showPage(
              "Frontpage",
              Simulation.submissions.frontPageIndices.take(30),
            ),
          )
        },
        Plot.upvoteQualityPlot(tick.value.sampleMillis(refreshMs).map(_ => 0 until Simulation.submissions.id.length))(
          attr("width")  := s"300",
          attr("height") := s"300",
        ),
        Plot.voteOverRankDistribution(
          tick.value
            .sampleMillis(refreshMs)
            .map(_ => (Simulation.stats.frontpageUpvotesOnRanks, Simulation.stats.frontpageUpvotesOnRanksTimestamps)),
        )(
          attr("width")  := s"300",
          attr("height") := s"400",
        ),
      ),
    )
  }

  def showPage(title: String, submissionIndices: Iterable[Int]) =
    div(
      cls := "p-5 w-80",
      div(title),
      submissionIndices.map(showSubmission).toSeq,
    )

  def showSubmission(submissionIndex: Int): HtmlVNode = {
    import Simulation.submissions
    val title    = s"Story ${submissions.id(submissionIndex)}"
    val subtitle =
      s"${submissions.upvotes(submissionIndex)} points, ${timeSpanFromSeconds(Simulation.age(submissions.submissionTimeSeconds(submissionIndex)))} ago"

    val qualityAlpha = Math.min(submissions.quality(submissionIndex) / Data.qualityDistribution.ev * 0.5, 1.0)
    div(
      cls := "mt-2",
      div(
        div(
          cls     := "bg-blue-400 inline-block mr-1 rounded-sm",
          opacity := qualityAlpha,
          width   := "10px",
          height  := "10px",
        ),
        title,
      ),
      div(subtitle, opacity := 0.5),
    )
  }

  def timeSpanFromSeconds(seconds: Long): String = {
    val ageHours = seconds / 3600
    val ageMin   = (seconds % 3600) / 60
    s"${if (ageHours == 0) s"${ageMin}min" else s"${ageHours}h"}"
  }
}
