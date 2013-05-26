package controllers

import scala.concurrent._

import play.api._
import play.api.mvc._

import play.api.libs.functional._
import play.api.libs.iteratee._
import play.api.libs.ws._
import play.api.libs.oauth._

import play.api.libs.json._
import play.api.libs.json.syntax._
import play.api.libs.json.extensions._


import scala.concurrent.ExecutionContext.Implicits.global
import play.api.Play.current

object FakeGithub extends Controller {

  def user(user_id: String) = Action { Ok( 
    Json.parse(s"""{
      "login": "$user_id",
      "id": 1,
      "avatar_url": "https://github.com/images/error/octocat_happy.gif",
      "gravatar_id": "somehexcode",
      "url": "https://api.github.com/users/octocat",
      "name": "monalisa octocat",
      "company": "GitHub",
      "blog": "https://github.com/blog",
      "location": "San Francisco",
      "email": "octocat@github.com",
      "hireable": false,
      "bio": "There once was...",
      "public_repos": 2,
      "public_gists": 1,
      "followers": 20,
      "following": 0,
      "html_url": "https://github.com/octocat",
      "created_at": "2008-01-14T04:33:35Z",
      "type": "User"
    }""")
  ) }
}