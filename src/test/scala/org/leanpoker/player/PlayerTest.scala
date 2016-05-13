package org.leanpoker.player

import com.google.gson.JsonParser
import org.scalatest.{FunSpec, MustMatchers}

class PlayerTest extends FunSpec with MustMatchers {

  it("test bet request") {
    val jsonElement = play.api.libs.json.Json.parse("{\"key1\": \"value1\", \"key2\": \"value2\"}")
    Player.betRequest(jsonElement) must be (100)
  }

}