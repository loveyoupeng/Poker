package com.loveyoupeng.poker.controllers

import play.api._
import play.api.mvc._

object Application extends Controller {
  
  def index = Action {
    Ok(com.loveyoupeng.poker.views.html.main.index("Your new application is ready."))
  }
  
}