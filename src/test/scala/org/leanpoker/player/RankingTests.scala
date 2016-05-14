package org.leanpoker.player

import com.google.gson.JsonParser
import org.scalatest.{FunSpec, MustMatchers}
import scalaj.http.Http

class RankingApiTests extends FunSpec with MustMatchers {

  /**
 * Returns the text (content) from a URL as a String.
 * Warning: This method does not time out when the service is non-responsive.
 */
def get(url: String) = scala.io.Source.fromURL(url).mkString
  
  it("test ranking request") {
    /*val jsonElement = play.api.libs.json.Json.parse("{\"key1\": \"value1\", \"key2\": \"value2\"}")
    Player.betRequest(jsonElement) must be (100)*/
    
    val rankRequestBody = """cards=[
    {"rank":"5","suit":"diamonds"},
    {"rank":"6","suit":"diamonds"},
    {"rank":"7","suit":"diamonds"},
    {"rank":"7","suit":"spades"},
    {"rank":"8","suit":"diamonds"},
    {"rank":"9","suit":"diamonds"}
]"""
    
    import scalaj.http.Http

    val response = Http("http://rainman.leanpoker.org/rank").postData(rankRequestBody)
    
    println(response.asString)
  }

}