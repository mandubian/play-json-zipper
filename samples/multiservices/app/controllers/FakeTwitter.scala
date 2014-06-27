package controllers

import scala.concurrent._

import play.api._
import play.api.mvc._

import play.api.libs.functional._
import play.api.libs.iteratee._
import play.api.libs.ws._
import play.api.libs.oauth._

import play.api.libs.json._
import play.api.libs.json.monad.syntax._
import play.api.libs.json.extensions._


import scala.concurrent.ExecutionContext.Implicits.global
import play.api.Play.current

object FakeTwitter extends Controller {

  def userTimeline(user_id: String) = Action { Ok(
    Json.parse(s"""[ 
      {
        "coordinates": null,
        "favorited": false,
        "truncated": false,
        "created_at": "Wed Aug 29 17:12:58 +0000 2012",
        "id_str": "240859602684612608",
        "entities": {
          "urls": [
            {
              "expanded_url": "https://dev.twitter.com/blog/twitter-certified-products",
              "url": "https://t.co/MjJ8xAnT",
              "indices": [
                52,
                73
              ],
              "display_url": "dev.twitter.com/blog/twitter-c\u2026"
            }
          ],
          "hashtags": [
          ],
          "user_mentions": [
          ]
        },
        "in_reply_to_user_id_str": null,
        "contributors": null,
        "text": "Introducing the Twitter Certified Products Program: https://t.co/MjJ8xAnT",
        "retweet_count": 121,
        "in_reply_to_status_id_str": null,
        "id": 240859602684612608,
        "geo": null,
        "retweeted": false,
        "possibly_sensitive": false,
        "in_reply_to_user_id": null,
        "place": null,
        "user": {
        }
      },
      {
        "coordinates": null,
        "favorited": false,
        "truncated": false,
        "created_at": "Wed Aug 29 17:12:58 +0000 2012",
        "id_str": "240859602684612608",
        "entities": {
          "urls": [
            {
              "expanded_url": "https://dev.twitter.com/blog/twitter-certified-products",
              "url": "https://t.co/MjJ8xAnT",
              "indices": [
                52,
                73
              ],
              "display_url": "dev.twitter.com/blog/twitter-c\u2026"
            }
          ],
          "hashtags": [
          ],
          "user_mentions": [
          ]
        },
        "in_reply_to_user_id_str": null,
        "contributors": null,
        "text": "Introducing the Twitter Certified Products Program: https://t.co/MjJ8xAnT",
        "retweet_count": 121,
        "in_reply_to_status_id_str": null,
        "id": 240859602684612608,
        "geo": null,
        "retweeted": false,
        "possibly_sensitive": false,
        "in_reply_to_user_id": null,
        "place": null,
        "user": {
          "id": "$user_id"
        }
      }
      ]"""
    )
  ) }
}