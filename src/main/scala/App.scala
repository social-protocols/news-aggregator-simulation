package simulation

import outwatch._
import outwatch.dsl._
import colibri._
import org.scalajs.dom
import cats.effect.SyncIO

object App {
  val app = {
    val liveNewPage       = Subject.behavior[Vector[Submission]](Vector.empty)
    val liveTopPage       = Subject.behavior[Vector[Submission]](Vector.empty)
    val recentSubmissions = Subject.behavior[Vector[Submission]](Vector.empty)
    val tickTime          = Subject.behavior(1000)
    val resetTrigger      = Subject.behavior(())

    val subSteps = 600

    val tick = resetTrigger
      .combineLatest(tickTime)
      .switchMap { case (_, tickTime) =>
        Observable
          .intervalMillis(tickTime)
          .map(_ => if (tickTime == 0) subSteps else 1)
      }
      .publish

    tick.value.foreach { substeps =>
      for (_ <- 0 until substeps)
        Simulation.nextStep()
    }

    // visualization runs independently of simulation
    tick.value.sampleMillis(1000).foreach { _ =>
      liveNewPage.onNext(Simulation.newpage(Simulation.submissions).toVector)
      liveTopPage.onNext(Simulation.frontpage(Simulation.submissions).toVector)
      recentSubmissions.onNext(Simulation.recentSubmissions(Simulation.submissions).toVector)
    }

    div(
      tick.value.scan(0L)((sum, substeps) => sum + substeps).map(timeSpanFromSeconds),
      managed(SyncIO(tick.connect())),
      SpeedSlider(tickTime),
      div(
        cls := "flex items-start",
        liveNewPage.map(x => showPage("New", x)),
        liveTopPage.map(x => showPage("Frontpage", x)),
        showUpvoteQualityPlot(recentSubmissions)(
          attr("width")  := s"400",
          attr("height") := s"600",
        ),
      ),
    )
  }

  def showUpvoteQualityPlot(submissions: Observable[Vector[Submission]]) =
    canvas(
      cls := "border border-gray-400",
      managedElement { elem =>
        val canvasElement = elem.asInstanceOf[dom.HTMLCanvasElement]
        val width         = canvasElement.width.toDouble;
        val height        = canvasElement.height.toDouble;

        val context =
          elem
            .asInstanceOf[dom.HTMLCanvasElement]
            .getContext("2d")
            .asInstanceOf[dom.CanvasRenderingContext2D];

        // bottom left is (0,0)
        context.translate(0, height);
        context.scale(1, -1);

        context.fillStyle = "rgb(59, 130, 246, 0.3)"

        submissions.foreach { submissions =>
          context.clearRect(0, 0, width, height)

          submissions.foreach { submission =>
            val x = submission.quality * width
            val y = submission.score.toDouble
            context.beginPath()
            context.arc(x, y, 3, 0, Math.PI * 2)
            context.fill()
          }
        }
        Cancelable(() => ())
      },
    )

  def showPage(title: String, submissions: Vector[Submission]) =
    div(
      cls := "p-5 w-80",
      div(title),
      submissions.take(30).map(showSubmission),
    )

  def showSubmission(submission: Submission): HtmlVNode = {
    val title    = s"Story ${submission.id}"
    val subtitle =
      s"${submission.score} points, ${timeSpanFromSeconds(Simulation.timeSeconds - submission.timeSeconds)} ago"

    val qualityAlpha = Math.min(submission.quality, 1.0)
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
