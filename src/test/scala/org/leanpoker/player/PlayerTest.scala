package org.leanpoker.player

import com.google.gson.JsonParser
import org.scalatest.{FunSpec, MustMatchers}
import org.scalatest._

class PlayerTest extends FunSpec with Matchers {

  it("test bet request") {
    val jsonElement = play.api.libs.json.Json.parse("{\"key1\": \"value1\", \"key2\": \"value2\"}")
    Player.betRequest(jsonElement) should be (100)
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