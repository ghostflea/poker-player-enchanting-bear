package org.leanpoker.player

import com.google.gson.JsonParser
import org.scalatest.{FunSpec, MustMatchers}

class PlayerTest extends FunSpec with MustMatchers {

  it("check basic bet amounts") {
    Player.computeBet(2, 100, 100, 1000) must be (100)
  }

  it("check fold") {
    Player.computeBet(0, 0, 200, 1000) must be (0)
  }

}