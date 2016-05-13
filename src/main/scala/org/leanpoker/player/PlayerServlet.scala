package org.leanpoker.player

import javax.servlet.annotation.WebServlet

import com.google.gson.JsonParser
import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

@WebServlet(Array("/"))
class PlayerServlet extends HttpServlet {

    override def doGet(req:HttpServletRequest, resp:HttpServletResponse) {
        resp.getWriter().print("Scala player is running")
    }

    @Override
    override def doPost(req:HttpServletRequest, resp:HttpServletResponse) {
        req.getParameter("action") match {
          case "bet_request" =>
            val gameState = req.getParameter("game_state")
            val gameStateJson = play.api.libs.json.Json.parse(gameState)
            
            resp.getWriter().print(Player.betRequest(gameStateJson))
          case "showdown" =>
            val gameState = req.getParameter("game_state")
            val gameStateJson = play.api.libs.json.Json.parse(gameState)
            
            Player.showdown(gameStateJson)
          case _ =>
            resp.getWriter().print(Player.VERSION)
        }
    }
}
