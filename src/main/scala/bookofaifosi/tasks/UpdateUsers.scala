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

  private def updateRole(addCondition: Boolean)(user: RegisteredUser, guild: Guild, role: Role)(using Logger[IO]): IO[Unit] =
    val roleModifier = if addCondition then
      user.addRole(guild, role)
    else
      user.removeRole(guild, role)
    roleModifier.attempt.flatMap(_.fold(
      error => for
        logChannel <- Bot.config.logChannel
         message = s"Failed to add or remove ${role.mention} to ${user.mention}, error: ${error.getMessage}"
        _ <- Logger[IO].error(message)
        _ <- logChannel.fold(IO.unit)(_.sendMessage(message))
      yield (),
      _.pure
    ))

  private def updateWearer(user: RegisteredUser, guild: Guild, lockedRole: Role)(using Logger[IO]): IO[RegisteredUser] =
    if !user.isWearer then return user.pure
    for
      locks <- user.locks
      lockedLocks = locks.filter(_.status == LockStatus.Locked)
      keyholders = lockedLocks.flatMap(_.keyholder)
      keyholderNames = keyholders.map(_.username)
      user <- updateUser(user, keyholders.map(_._id), lockedLocks.nonEmpty)
      registeredKeyholders <- RegisteredUserRepository.list(fr"chaster_name = ANY ($keyholderNames)".some, fr"is_keyholder = true".some)
      _ <- updateRole(registeredKeyholders.nonEmpty)(user, guild, lockedRole)
    yield user

  private def updateKeyholder(user: RegisteredUser, guild: Guild, keyholderRole: Role)(using Logger[IO]): IO[Unit] =
    if !user.isKeyholder then return IO.unit
    for
      profile <- user.profile
      registeredWearers <- RegisteredUserRepository.list(fr"chaster_name = ${profile.username}".some, fr"is_wearer = true".some)
      _ <- updateRole(registeredWearers.nonEmpty)(user, guild, keyholderRole)
    yield ()

  private def updateVisitor(user: RegisteredUser, guild: Guild, visitorRole: Role, lockedRole: Role, keyholderRole: Role)(using Logger[IO]): IO[Unit] =
    for
      locked <- user.hasRole(guild, lockedRole)
      keyholder <- user.hasRole(guild, keyholderRole)
      _ <- updateRole(!locked && !keyholder)(user, guild, visitorRole)
    yield ()

  private def checkUserDeleted(user: RegisteredUser)(using Logger[IO]): Stream[IO, Unit] =
    for
      profile <- user.publicProfileByName(user.chasterName).streamed
      logChannel <- Bot.config.logChannel.streamed
      _ <- if profile.exists(!_.isDisabled) then
        Stream.unit
      else
        lazy val message = s"Profile for ${user.mention} chaster user ${user.chasterName} not found, was it deleted?"
        (Logger[IO].info(message) *> logChannel.fold(IO.unit)(_.sendMessage(message))).streamed >> Stream.empty
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
      _ <- checkUserDeleted(user)
      guild <- Stream.emits(discord.guilds)
      user <- updateWearer(user, guild, lockedRole).streamed
      _ <- updateKeyholder(user, guild, keyholderRole).streamed
      _ <- updateVisitor(user, guild, visitorRole, lockedRole, keyholderRole).streamed
    yield ()
