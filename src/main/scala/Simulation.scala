package simulation

import flatland._

object Simulation {
  var nowSeconds     = 0L
  var nextSubmission = Data.nextSubmissionArrivalDelay.get

  def age(submissionTimeSeconds: Long) = nowSeconds - submissionTimeSeconds

  object submissions {
    val id                    = flatland.ArrayQueueLong.create(Data.updateSize)
    val quality               = flatland.ArrayQueueDouble.create(Data.updateSize)
    val upvotes               = flatland.ArrayQueueInt.create(Data.updateSize)
    val submissionTimeSeconds = flatland.ArrayQueueLong.create(Data.updateSize)
    val rankingFormulaValue   = flatland.ArrayQueueDouble.create(Data.updateSize)

    // val cumulativeVoteOnRankProbability = new Array[Double](Data.updateSize)

    private val frontpageIndicesArray = new Array[Int](Data.updateSize)
    val frontPageIndices              = flatland.ArraySliceInt(frontpageIndicesArray, 0, 0)

    def add(
      id: Long = nextId,
      quality: Double = Data.qualityDistribution.get,
      upvotes: Int = 1,
      submissionTimeSeconds: Long = nowSeconds,
      rankingFormulaValue: Double = 0,
    ) = {
      // println(submissionTimeSeconds)
      // circular buffer overwrites last if full
      this.id.add(id)
      this.quality.add(quality)
      this.upvotes.add(upvotes)
      this.submissionTimeSeconds.add(submissionTimeSeconds)
      this.rankingFormulaValue.add(rankingFormulaValue)

      recalculateFrontpage()
    }

    def recalculateFrontpage() = {
      // update raking formula values
      this.submissionTimeSeconds.foreachIndexAndElement { (i, submissionTimeSeconds) =>
        this.rankingFormulaValue(i) = rankingFormula(this.upvotes(i), age(submissionTimeSeconds))
      }
      // extract indices of frontpage candidates
      var i = 0
      this.upvotes.foreachIndexAndElement { (queueIndex, upvotes) =>
        if (upvotes >= Data.minScoreToAppearOnFrontpage) {
          frontpageIndicesArray(i) = queueIndex
          i += 1
        }
      }

      scala.util.Sorting.stableSort(
        this.frontpageIndicesArray,
        (a: Int, b: Int) => this.rankingFormulaValue(a) > this.rankingFormulaValue(b),
        0,
        i,
      )

      frontPageIndices.length = i
    }

    private var lastId: Long = 0
    def nextId: Long         = {
      val next = lastId + 1
      lastId = next
      next
    }
  }

  object stats {
    val frontpageUpvotesOnRanks =
      flatland.ArrayQueueInt.create(20000) // should cover more than a day (~16000 upvotes / day)
  }

  def nextStep() = {
    // println(s"nextStep ($nowSeconds), next submission: $nextSubmission")
    val submissionArrives = nowSeconds >= nextSubmission
    if (submissionArrives) {
      // println("submit")
      submissions.add()
      val delta = Data.nextSubmissionArrivalDelay.get
      nextSubmission += delta
    }

    if (nowSeconds % Data.updateIntervalSeconds == 0) {
      submissions.recalculateFrontpage()
    }

    castVotes()

    nowSeconds += 1
  }

  def rankingFormula(upvotes: Int, ageSeconds: Long): Double = {
    // http://www.righto.com/2013/11/how-hacker-news-ranking-really-works.html
    val ageHours = ageSeconds / 3600.0
    Math.pow(upvotes - 1.0, 0.8) / Math.pow(ageHours + 2, 1.8)
  }

  def castVotes() = {
      // frontpage
    loop(Math.min(Data.voteGainOnTopRankPerSecond.length, submissions.frontPageIndices.length)) { rank =>
      val storyIndex          = submissions.frontPageIndices(rank)
      val quality             = submissions.quality(storyIndex)
      val votesPerRankPerTick = Data.voteGainOnTopRankPerSecond(rank)
      val lambda              = quality * votesPerRankPerTick
      val distribution        = Data.voteDistribution(lambda)
      val numberOfUpvotes     = distribution.get
      submissions.upvotes(storyIndex) += numberOfUpvotes

      // update stats
      loop(numberOfUpvotes) { _ =>
        stats.frontpageUpvotesOnRanks.add(rank)
          }
        }

      // newpage
    loop(Math.min(Data.voteGainOnNewRankPerSecond.length, submissions.quality.length)) { rank =>
      val storyIndex          = submissions.quality.length - rank - 1
      val votesPerRankPerTick = Data.voteGainOnNewRankPerSecond(rank)
      val quality             = submissions.quality(storyIndex)
      val lambda              = quality * votesPerRankPerTick
      val distribution        = Data.voteDistribution(lambda)
      val numberOfUpvotes     = distribution.get
      submissions.upvotes(storyIndex) += numberOfUpvotes
            }
          }

}
