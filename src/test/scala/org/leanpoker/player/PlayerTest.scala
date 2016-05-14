package org.leanpoker.player

import com.google.gson.JsonParser
import org.scalatest.{FunSpec, MustMatchers}
import org.scalatest._

class PlayerTest extends FunSpec with Matchers {

  it("check basic bet amounts") {
    Player.computeBet(2, 100, 100, 1000) should be (100)
  }

  it("check fold") {
    Player.computeBet(0, 0, 200, 1000) should be (0)
  }

  describe("betPosition") {
    it("gives right position") {
      Player.getBetPosition(0, 5, 1) shouldBe Player.Early
      Player.getBetPosition(0, 5, 2) shouldBe Player.Mid
      Player.getBetPosition(0, 5, 3) shouldBe Player.Mid
      Player.getBetPosition(0, 5, 4) shouldBe Player.Late
      
      Player.getBetPosition(3, 5, 4) shouldBe Player.Early
      Player.getBetPosition(3, 5, 0) shouldBe Player.Mid
      
      Player.getBetPosition(3, 5, 2) shouldBe Player.Late
      Player.getBetPosition(3, 5, 3) shouldBe Player.Late
      Player.getBetPosition(3, 5, 0) shouldBe Player.Mid
    }
  }
}