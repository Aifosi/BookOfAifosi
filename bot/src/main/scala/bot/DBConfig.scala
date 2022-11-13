package bot

import cats.effect.IO
import doobie.util.transactor.Transactor

trait DBConfig:
  def driver: String
  def user: String
  def password: String
  def url: String
  lazy val transactor: Transactor[IO] = Transactor.fromDriverManager[IO](
    driver, url, user, password
  )