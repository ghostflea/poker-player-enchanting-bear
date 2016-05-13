package org.leanpoker.player

import play.api.libs.json._
import play.api.libs.json.Json._

object Player {

  val VERSION = "Default Scala folding player"

  def betRequest(request: JsValue) = {
    //request.getAsJsonObject.get
    val currentBuyIn =  (request \ "current_buy_in").as[JsNumber].value.toInt
    val inAction = (request \ "in_action").as[JsNumber].value.toInt
    
    val players = (request \ "players").as[JsArray]
    val activePlayerBet = (players(inAction) \ "bet").as[JsNumber].value.toInt
    
    val callAmount = currentBuyIn - activePlayerBet
    val minimumRaise = (request \ "minimum_raise").as[JsNumber].value.toInt
    
   callAmount
  }

  def showdown(game: JsValue) {

  }
}
