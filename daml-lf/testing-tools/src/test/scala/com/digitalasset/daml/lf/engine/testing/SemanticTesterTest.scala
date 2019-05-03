package com.digitalasset.daml.lf.engine.testing

import java.io.File

import com.digitalasset.daml.lf.UniversalArchiveReader
import com.digitalasset.daml.lf.command.Commands
import com.digitalasset.daml.lf.data.Ref.{QualifiedName, SimpleString}
import com.digitalasset.daml.lf.data.{ImmArray, Time}
import com.digitalasset.daml.lf.engine.Event.Events
import com.digitalasset.daml.lf.engine.testing.SemanticTester.GenericLedger
import com.digitalasset.daml.lf.lfpackage.Decode
import com.digitalasset.daml.lf.value.Value.AbsoluteContractId
import com.digitalasset.daml.lf.transaction.{Transaction => Tx}
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import scala.concurrent.duration._

import scala.concurrent.{Await, Future, Promise}

class SemanticTesterTest
  extends AsyncWordSpec
    with Matchers
    with ScalaFutures
    //    with IntegrationPatience
{

  class FakeLedger(submitCommands: Promise[Commands]) extends GenericLedger {
    var ct = Time.Timestamp.Epoch

    override def currentTime: Future[Time.Timestamp] = Future {
      println(s"currentTime: $ct")
      ct
    }

    override def passTime(dtMicros: Long): Future[Unit] = Future {
      println(s"passTime: $dtMicros")
      ct = ct.addMicros(dtMicros)
      ()
    }

    override def submit(submitterName: SimpleString, cmds: Commands)
    : Future[Events[EventNodeId, AbsoluteContractId, Tx.Value[AbsoluteContractId]]] = {
      println(s"submit by $submitterName: $cmds")
      submitCommands.success(cmds)
      Future {
        Events(ImmArray(Seq()), Map.empty)
      }
    }
  }

  private val darFile = new File("daml-lf/tests/BasicTests.dar")

  lazy val (mainPkgId, packages) = {
    val dar = UniversalArchiveReader().readFile(darFile).get
    val packages = Map(dar.all.map {
      case (pkgId, archive) => Decode.readArchivePayloadAndVersion(pkgId, archive)._1
    }: _*)
    (dar.main._1, packages)
  }

  "SemanticTester" can {
    "mangle contract ids correctly" in {
      val p = Promise[Commands]
      val st = new SemanticTester(
        _ => new FakeLedger(p),
        mainPkgId,
        packages)
      val rf = st.testScenario(QualifiedName.assertFromString("BasicTests:test_screateAndExercise"))
      Await.ready(rf, 10.seconds)
      println(s"test scenario ran")
      whenReady(p.future) { cmds =>
        cmds.commandsReference shouldBe "xxxx-111"
      }
    }
  }

}
