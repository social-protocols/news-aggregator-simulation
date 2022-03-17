package simulation

import outwatch._
import outwatch.dsl._
import colibri._
import org.scalajs.dom

object Plot {

  def upvoteQualityPlot(submissionIndices: Observable[Iterable[Int]]) =
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

        submissionIndices.foreach { submissionIndices =>
          context.clearRect(0, 0, width, height)

          var minX = Double.PositiveInfinity
          var maxX = 0.0
          Simulation.submissions.quality.foreachElement { x =>
            if (x > maxX) maxX = x
            if (x < minX) minX = x
          }

          var minY = Double.PositiveInfinity
          var maxY = 0.0
          Simulation.submissions.upvotes.foreachElement { y =>
            if (y > maxY) maxY = y.toDouble
            if (y < minY) minY = y.toDouble
          }

          @inline def transform(x: Double, min: Double, max: Double, size: Double) = (x - min) / (max - min) * size
          @inline def transformX(x: Double)                                        = transform(x, minX, maxX, width)
          @inline def transformY(y: Double)                                        = transform(y, minY, maxY, height)

          submissionIndices.foreach { submissionIndex =>
            val x = transformX(Simulation.submissions.quality(submissionIndex))
            val y = transformY(Simulation.submissions.upvotes(submissionIndex).toDouble)
            context.beginPath()
            context.arc(x, y, 3, 0, Math.PI * 2)
            context.fill()
          }
        }
        Cancelable(() => ())
      },
    )

}
