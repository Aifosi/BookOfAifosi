package bookofaifosi.tasks

import bookofaifosi.Bot
import bookofaifosi.chaster.Client.*
import bookofaifosi.chaster.Client.given
import bookofaifosi.chaster.{LockStatus, PublicUser}
import bookofaifosi.db.Filters.*
import bookofaifosi.db.{RegisteredUserRepository, given}
import bookofaifosi.model.event.{AutoCompleteEvent, SlashCommandEvent}
import bookofaifosi.model.{Channel, ChasterID, Discord, Guild, RegisteredUser, Role, toLong}
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
  private def log(message: => String)(using Logger[IO]): Stream[IO, Nothing] =
    for
      logChannel <- Bot.config.logChannel.streamed
      _ <- Logger[IO].info(message).streamed
      _ <- logChannel.fold(IO.unit)(_.sendMessage(message)).streamed
      n <- Stream.empty
    yield n

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
      registeredKeyholders <- RegisteredUserRepository.list(fr"chaster_name = ANY ($keyholderNames)".some, isKeyholder)
      _ <- Logger[IO].debug(s"registeredKeyholders = $registeredKeyholders of $user")
      _ <- updateRole(registeredKeyholders.nonEmpty)(user, guild, lockedRole)
    yield user

  private def updateKeyholder(user: RegisteredUser, guild: Guild, profile: PublicUser, keyholderRole: Role)(using Logger[IO]): IO[Unit] =
    if !user.isKeyholder then return IO.unit
    for
      registeredWearers <- RegisteredUserRepository.list(fr"${profile._id} = ANY (keyholder_ids)".some, isWearer)
      _ <- Logger[IO].debug(s"registeredWearers = $registeredWearers of $user")
      _ <- updateRole(registeredWearers.nonEmpty)(user, guild, keyholderRole)
    yield ()

  private def updateVisitor(user: RegisteredUser, guild: Guild, visitorRole: Role, lockedRole: Role, keyholderRole: Role)(using Logger[IO]): IO[Unit] =
    for
      locked <- user.hasRole(guild, lockedRole)
      keyholder <- user.hasRole(guild, keyholderRole)
      _ <- updateRole(!locked && !keyholder)(user, guild, visitorRole)
    yield ()

  private def checkChasterUserDeleted(user: RegisteredUser)(using Logger[IO]): Stream[IO, PublicUser] =
    for
      profile <- user.publicProfileByName(user.chasterName).streamed
      profile <- profile.filter(!_.isDisabled).fold {
        log(s"Profile for ${user.mention} chaster user ${user.chasterName} not found, was it deleted?")
      }(Stream.emit)
    yield profile

  private def checkDiscordUserDeleted(user: RegisteredUser, guild: Guild)(using Logger[IO]): Stream[IO, Unit] =
    for
      member <- user.member(guild).attempt.streamed
      _ <- if member.isRight then
        Stream.unit
      else
        log(s"Could not get discord user ${user.mention} did the user delete his discord account or leave the server?")
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
      profile <- checkChasterUserDeleted(user)
      guild <- Stream.emits(discord.guilds)
      _ <- checkDiscordUserDeleted(user, guild)
      user <- updateWearer(user, guild, lockedRole).streamed
      _ <- updateKeyholder(user, guild, profile, keyholderRole).streamed
      _ <- updateVisitor(user, guild, visitorRole, lockedRole, keyholderRole).streamed
    yield ()
