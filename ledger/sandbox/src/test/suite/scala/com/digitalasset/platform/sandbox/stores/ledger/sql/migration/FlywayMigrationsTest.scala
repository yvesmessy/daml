package com.digitalasset.platform.sandbox.stores.ledger.sql.migration

import java.io.File

import com.digitalasset.platform.sandbox.SandboxApplication
import com.digitalasset.platform.sandbox.config.{DamlPackageContainer, LedgerIdMode, SandboxConfig}
import com.digitalasset.platform.sandbox.persistence.PostgresAroundAll
import com.digitalasset.platform.sandbox.services.{SandboxFixture, SandboxServerResource}
import com.digitalasset.platform.services.time.{TimeModel, TimeProviderType}
import org.scalatest.{AsyncWordSpec, Matchers}

class FlywayMigrationsTest
    extends AsyncWordSpec
    with Matchers
    with SandboxFixture
    with PostgresAroundAll {

  private val LedgerId = "MyLedger"
  private val DarFile = new File("ledger/sandbox/Test.dar")

  private def startSandbox(targetVersion: Int) = {
    val config =
      SandboxConfig.default
        .copy(
          port = 0, //dynamic port allocation
          damlPackageContainer = DamlPackageContainer(files = List(DarFile)),
          timeProviderType = TimeProviderType.Static,
          timeModel = TimeModel.reasonableDefault,
          ledgerIdMode = LedgerIdMode.Predefined(LedgerId),
          jdbcUrl = Some(postgresFixture.jdbcUrl)
        )

    //TODO: set the target somehow!
    val res = new SandboxServerResource(SandboxApplication(config))
    res.setup()
    res
  }

  "Flyway migration" should {

    "work incrementally using all versions" in {

      val availableTargets = 10 //TODO: get all possible targets available

      (1 to availableTargets).map { targetVersion =>
        val sandbox = startSandbox(targetVersion)

        //TODO: do something and assert on them

        sandbox.close()

      }

      succeed
    }
  }
}
