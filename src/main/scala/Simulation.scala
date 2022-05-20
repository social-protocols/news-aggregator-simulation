package simulation

import flatland._
import probability_monad._

object Simulation {
  var nowSeconds     = 0L
  var nextSubmission = Data.nextSubmissionArrivalDelay.get

  def age(submissionTimeSeconds: Long) = nowSeconds - submissionTimeSeconds

  object submissions {
    val id                    = flatland.ArrayQueueLong.create(Data.updateSize)
    val quality               = flatland.ArrayQueueDouble.create(Data.updateSize)
    val upvotes               = flatland.ArrayQueueDouble.create(Data.updateSize)
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
    val size                              = 1000000 // should cover more than a day (~16000 upvotes / day)
    val frontpageUpvotesOnRanks           = flatland.ArrayQueueInt.create(size)
    val frontpageUpvotesOnRanksTimestamps = flatland.ArrayQueueLong.create(size)
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

  def rankingFormula(upvotes: Double, ageSeconds: Long): Double = {
    // http://www.righto.com/2013/11/how-hacker-news-ranking-really-works.html
    val ageHours = ageSeconds / 3600.0
    Math.pow(upvotes - 1.0, 0.8) / Math.pow(ageHours + 2, 1.8)
    // (upvotes - 1.0) / Math.pow(ageHours + 2, 1.8)
    // (upvotes - 1.0) / (Data.voteGainOnTopRankPerSecond(rank)) * (ageHours + 2)
  }

  def castVotes() = {
    // frontpage
    loop(Math.min(Data.voteGainOnTopRankPerSecond.length, submissions.frontPageIndices.length)) { rank =>
      val storyIndex          = submissions.frontPageIndices(rank)
      val quality             = submissions.quality(storyIndex)

      // val coefficient = 0.4
      // val rank1AverageUpvotesPerTick = Data.baselineVotesOnTopRankPerSecond(0)
      // val votesPerRankPerSecond = rank1AverageUpvotesPerTick / Math.pow(1.0 + rank, coefficient)
      val votesPerRankPerTick = Data.baselineVotesOnTopRankPerSecond(rank)
      val lambda              = quality * votesPerRankPerTick
      val distribution        = Data.voteDistribution(lambda)
      val numberOfUpvotes     = distribution.get

      /* Simplified Model Probability of upvote = votesPerRankPerTick */
      // val numberOfUpvotes     = if (Distribution.uniform.get < votesPerRankPerTick) 1 else 0
      // var numberOfUpvotes = Math.round(lambda).toInt
      // val remainder = if (Distribution.uniform.get < (lambda - numberOfUpvotes)) 1 else 0
      // numberOfUpvotes = numberOfUpvotes + remainder

      assert(numberOfUpvotes >= 0, s"numberOfUpvotes < 0")

      submissions.upvotes(storyIndex) += numberOfUpvotes

      // update stats
      loop(numberOfUpvotes) { _ =>
        stats.frontpageUpvotesOnRanks.add(rank)
        stats.frontpageUpvotesOnRanksTimestamps.add(nowSeconds)
      }
    }

    // newpage
    loop(Math.min(Data.baselineVotesOnNewRankPerSecond.length, submissions.quality.length)) { rank =>
      val storyIndex          = submissions.quality.length - rank - 1
      val votesPerRankPerTick = Data.baselineVotesOnNewRankPerSecond(rank)
      val quality             = submissions.quality(storyIndex)
      val lambda              = quality * votesPerRankPerTick
      val distribution        = Data.voteDistribution(lambda)
      val numberOfUpvotes     = distribution.get
      submissions.upvotes(storyIndex) += numberOfUpvotes
    }

  }

}
