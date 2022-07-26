package bookofaifosi.tasks

import bookofaifosi.Bot
import bookofaifosi.chaster.Client.*
import bookofaifosi.chaster.Client.given
import bookofaifosi.chaster.LockStatus
import bookofaifosi.db.Filters.*
import bookofaifosi.db.{RegisteredUserRepository, given}
import bookofaifosi.model.event.{AutoCompleteEvent, SlashCommandEvent}
import bookofaifosi.model.{ChasterID, Discord, Guild, RegisteredUser, Role, toLong}
import bookofaifosi.syntax.io.*
import bookofaifosi.syntax.stream.*
import bookofaifosi.tasks.RepeatedStreams
import cats.effect.IO
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import cats.syntax.applicative.*
import doobie.postgres.implicits.*
import doobie.syntax.string.*
import fs2.Stream
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.FiniteDuration

object UpdateUsers extends RepeatedStreams:
  private def updateUser(user: RegisteredUser, keyholderIDs: List[ChasterID], isLocked: Boolean): IO[RegisteredUser] =
    if user.keyholderIDs != keyholderIDs || user.isLocked != isLocked then
      RegisteredUserRepository.update(user.id, keyholderIDs, isLocked, user.isWearer, user.isKeyholder, user.token.id)
    else
      user.pure

  private def updateWearer(user: RegisteredUser, guild: Guild, lockedRole: Role)(using Logger[IO]): IO[RegisteredUser] =
    if !user.isWearer then return user.pure
    for
      locks <- user.locks
      lockedLocks = locks.filter(_.status == LockStatus.Locked)
      keyholders = lockedLocks.flatMap(_.keyholder)
      keyholderNames = keyholders.map(_.username)
      user <- updateUser(user, keyholders.map(_._id), lockedLocks.nonEmpty)
      registeredKeyholders <- RegisteredUserRepository.list(fr"chaster_name in $keyholderNames".some, fr"user_type = 'keyholder'".some)
      _ <- if registeredKeyholders.nonEmpty then
        user.addRole(guild, lockedRole)
      else
        user.removeRole(guild, lockedRole)
    yield user

  private def updateKeyholder(user: RegisteredUser, guild: Guild, keyholderRole: Role)(using Logger[IO]): IO[Unit] =
    if !user.isKeyholder then return IO.unit
    for
      profile <- user.profile
      registeredWearers <- RegisteredUserRepository.list(fr"chaster_name = ${profile.username}".some, fr"user_type = 'wearer'".some)
      _ <- if registeredWearers.nonEmpty then
        user.addRole(guild, keyholderRole)
      else
        user.removeRole(guild, keyholderRole)
    yield ()

  private def updateVisitor(user: RegisteredUser, guild: Guild, visitorRole: Role, lockedRole: Role, keyholderRole: Role)(using Logger[IO]): IO[Unit] =
    for
      locked <- user.hasRole(guild, lockedRole)
      keyholder <- user.hasRole(guild, keyholderRole)
      _ <- if !locked && !keyholder then user.addRole(guild, visitorRole) else user.removeRole(guild, visitorRole)
    yield ()

  override def repeatedStream(delay: FiniteDuration)(using Logger[IO]): Stream[IO, Unit] =
    for
      given Logger[IO] <- Bot.logger.get.streamed
      discord <- Bot.discord.get.streamed
      visitorRole <- discord.roleByID(Bot.config.roles.visitor).streamed
      lockedRole <- discord.roleByID(Bot.config.roles.locked).streamed
      keyholderRole <- discord.roleByID(Bot.config.roles.keyholder).streamed
      _ <- Stream.awakeEvery[IO](delay)
      user <- Stream.evalSeq(RegisteredUserRepository.list())
      profile <- user.publicProfileByName(user.chasterName).streamed
      _ <- Stream.filter(profile.exists(!_.isDisabled)) //TODO Log deleted user
      guild <- Stream.emits(discord.guilds)
      user <- updateWearer(user, guild, lockedRole).streamed
      _ <- updateKeyholder(user, guild, keyholderRole).streamed
      _ <- updateVisitor(user, guild, visitorRole, lockedRole, keyholderRole).streamed
    yield ()
