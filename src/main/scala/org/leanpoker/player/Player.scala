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
  def main(args: Array[String]): Unit = {
    val gameState = """{
  "players":[
    {
      "name":"Player 1",
      "stack":1000,
      "status":"active",
      "bet":0,
      "hole_cards":[],
      "version":"Version name 1",
      "id":0
    },
    {
      "name":"Player 2",
      "stack":1000,
      "status":"active",
      "bet":0,
      "hole_cards":[],
      "version":"Version name 2",
      "id":1
    }
  ],
  "tournament_id":"550d1d68cd7bd10003000003",
  "game_id":"550da1cb2d909006e90004b1",
  "round":0,
  "bet_index":0,
  "small_blind":10,
  "orbits":0,
  "dealer":0,
  "community_cards":[],
  "current_buy_in":0,
  "pot":0
}"""
    val parsed = play.api.libs.json.Json.parse(gameState)
    println(parsed)
  }
}
