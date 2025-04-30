// SPDX-License-Identifier: Apache-2.0

package chisel3
package simulator

import chisel3.experimental.SourceInfo
import chisel3.simulator.PeekPokeAPI
import chisel3.util._
import svsim.Simulation

trait SimFailureException extends Exception {
  def messages:   Seq[String]
  def sourceInfo: SourceInfo

  override def getMessage: String =
    (messages :+ sourceInfo.makeMessage(identity)).mkString(" ")
}

case class TimedOutWaiting[T <: Serializable](
  cycles:       Int,
  condition:    T,
  extraMessage: Option[String] = None
)(implicit val sourceInfo: SourceInfo)
    extends SimFailureException {

  val messages = Seq(s"Timed out after $cycles cycles waiting for $condition.") ++ extraMessage
}

trait ThreadedChiselSim extends ChiselSim {

  sealed class ForkBuilder(scheduler: Synchronizer, tasks: Seq[Task] = Seq.empty) {

    def fork(runnable: => Unit): ForkBuilder =
      new ForkBuilder(scheduler, tasks :+ Task(() => runnable, tasks.length + 1))

    def join(): Unit =
      scheduler.run(tasks)

    def joinAndStep(): Unit = {
      join()
      scheduler.stepClock()
    }
  }

  object fork {
    val currentModule = AnySimulatedModule.current
    val clock = DutContext.current.clock.get
    val clockPort = currentModule.port(clock)

    private def tickClock(): Unit = {
      clockPort.tick(
        timestepsPerPhase = 1,
        cycles = 1,
        inPhaseValue = 0,
        outOfPhaseValue = 1
      )
    }

    def apply(runnable: => Unit): ForkBuilder =
      new ForkBuilder(new Synchronizer(() => tickClock())).fork(runnable)
  }

  implicit class threadedTestableClock(clock: Clock)(implicit val sourceInfo: SourceInfo)
      extends PeekPokeAPI.TestableClock(clock) {
    override def step(cycles: Int = 1): Unit = {
      StepBarrier.currentOption match {
        case Some(barrier) =>
          for (_ <- 0 until cycles) {
            barrier.step()
          }
        case None =>
          super.step(cycles)
      }
    }

    override def stepUntil(sentinelPort: Data, sentinelValue: BigInt, maxCycles: Int): Unit = {
      StepBarrier.currentOption match {
        case Some(barrier) =>
          for (_ <- 0 until maxCycles) {
            if (toTestableData(sentinelPort).peek().litValue == sentinelValue) {
              return
            }
            barrier.step()
          }
        case None =>
          super.stepUntil(sentinelPort, sentinelValue, maxCycles)
      }
    }
  }

  protected def currentClock: Option[PeekPokeAPI.TestableClock] =
    DutContext.current.clock.map(threadedTestableClock(_))

  sealed trait ClockedInterface {
    protected val maxWaitCycles: Int = DutContext.current.maxWaitCycles
    protected val clock:         PeekPokeAPI.TestableClock = currentClock.get
    protected def stepClock():   Unit = clock.step()

    protected def waitForSignal[D <: Data](
      signal:        D,
      expectedValue: BigInt = 1,
      maxCycles:     Option[Int] = None
    )(
      implicit sourceInfo: SourceInfo
    ) = {
      val maximumWaitCycles = maxCycles.getOrElse(maxWaitCycles)
      val isSigned = signal.isInstanceOf[SInt]
      val module = AnySimulatedModule.current
      val simulationPort = module.port(signal)

      module.willPeek()
      def getValue = {
        println(s"--- Waiting for signal ${signal.pathName} to be $expectedValue")
        simulationPort.get(isSigned = isSigned).asBigInt
      }

      // clock.stepUntil(signal, expectedValue, maximumWaitCycles)
      // if (signal.peekValue().asBigInt != expectedValue)
      //   throw TimedOutWaiting(maximumWaitCycles, signal.pathName)
      var cycles = 0
      while (getValue != expectedValue) {
        println(s"---    signal ${signal.pathName} is $getValue")
        if (cycles == maximumWaitCycles)
          throw TimedOutWaiting(cycles, signal.pathName)
        stepClock()
        cycles += 1
      }
      println(s"---    signal ${signal.pathName} is $expectedValue")

    }

  }

  implicit final class testableValidIO[T <: Data](sig: Valid[T])(implicit sourceInfo: SourceInfo)
      extends ClockedInterface {

    private def valid = sig.valid

    def enqueue(
      data: T
    )(
      implicit sourceInfo: SourceInfo
    ) = {
      require(data.isLit, "enqueued data must be literal!")
      toTestableData(sig.bits).poke(data)
      valid.poke(1)
      stepClock()
      valid.poke(0)
    }

    def enqueueSeq(
      dataSeq: Seq[T]
    )(
      implicit sourceInfo: SourceInfo
    ) = {
      for (data <- dataSeq) {
        enqueue(data)
      }
    }

    def waitForValid(
    )(
      implicit sourceInfo: SourceInfo
    ) = waitForSignal(valid)

    def dequeue(
    )(
      implicit sourceInfo: SourceInfo
    ): T = { // FIXME TODO: ??? step .. then peek?
      waitForValid()
      val value = sig.bits.peek()
      stepClock()
      value
    }

    def expectDequeue(
      expected: T,
      message:  String
    )(
      implicit sourceInfo: SourceInfo
    ): Unit =
      expectDequeue(expected, Some(message))

    def expectDequeue(
      expected: T,
      message:  Option[String] = None
    )(
      implicit sourceInfo: SourceInfo
    ): Unit = {
      require(expected.isLit, "expected value must be a literal!")
      waitForValid()
      sig.bits.expect(expected, message.getOrElse(""))
      stepClock()
    }

    def expectDequeueSeq(
      dataSeq: Seq[T],
      message: String
    )(
      implicit sourceInfo: SourceInfo
    ): Unit = {
      for ((data, i) <- dataSeq.zipWithIndex) {
        expectDequeue(data, message)
      }
    }

    def expectDequeueSeq(
      dataSeq: Seq[T]
    )(
      implicit sourceInfo: SourceInfo
    ): Unit = {
      for ((exp, i) <- dataSeq.zipWithIndex) {
        expectDequeue(exp, s"Element $i was different from expected value: $exp!")
      }
    }

  }

  implicit final class testableDecoupledIO[T <: Data](sig: DecoupledIO[T])(implicit sourceInfo: SourceInfo)
      extends ClockedInterface {
    private def valid = sig.valid
    private def ready = sig.ready

    def enqueue(
      data: T
    )(
      implicit sourceInfo: SourceInfo
    ) = {
      require(data.isLit, "enqueued data must be literal!")
      println(s">>> [enqueue] ${data.litValue}")
      sig.bits.poke(data)
      println(s">>> [enqueue] valid.poke(1)")
      valid.poke(1)
      println(s">>> [enqueue] waitForReady")
      waitForReady()
      println(s">>> [enqueue] step")
      stepClock()
      println(s">>> [enqueue] valid.poke(0)")
      valid.poke(0)
    }

    def enqueueSeq(
      dataSeq: Seq[T]
    )(
      implicit sourceInfo: SourceInfo
    ) = {
      for (data <- dataSeq) {
        enqueue(data)
      }
    }

    def waitForValid(
    )(
      implicit sourceInfo: SourceInfo
    ) = waitForSignal(valid)

    def waitForReady(
    )(
      implicit sourceInfo: SourceInfo
    ) = waitForSignal(ready)

    def dequeue(
    )(
      implicit sourceInfo: SourceInfo
    ): T = {
      ready.poke(1)
      waitForValid()
      val value = sig.bits.peek()
      stepClock()
      ready.poke(0)
      value
    }

    def expectDequeue(
      expected: T,
      message:  String
    )(
      implicit sourceInfo: SourceInfo
    ): Unit =
      expectDequeue(expected, Some(message))

    def expectDequeue(
      expected: T,
      message:  Option[String] = None
    )(
      implicit sourceInfo: SourceInfo
    ): Unit = {
      require(expected.isLit, "expected value must be a literal!")
      ready.poke(1)
      waitForValid()
      sig.bits.expect(expected, message.getOrElse(""))
      stepClock()
      ready.poke(0)
    }

    def expectDequeueSeq(
      dataSeq: Seq[T],
      message: String
    )(
      implicit sourceInfo: SourceInfo
    ): Unit = {
      for ((data, i) <- dataSeq.zipWithIndex) {
        expectDequeue(data, message)
      }
    }

    def expectDequeueSeq(
      dataSeq: Seq[T]
    )(
      implicit sourceInfo: SourceInfo
    ): Unit = {
      for ((exp, i) <- dataSeq.zipWithIndex) {
        expectDequeue(exp, s"Element $i was different from expected value: $exp!")
      }
    }
  }
}
