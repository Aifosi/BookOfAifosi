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
import cats.effect.kernel.Ref
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import cats.syntax.applicative.*
import cats.syntax.parallel.*
import doobie.postgres.implicits.*
import doobie.syntax.string.*
import fs2.Stream
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.*

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
      RegisteredUserRepository.update(user.id, keyholderIDs = keyholderIDs.some, isLocked = isLocked.some, lastLocked = Option.when(isLocked)(Instant.now.some))
    else
      user.pure

  private def modifyRole(user: RegisteredUser, guild: Guild, role: Role)(modifier: IO[Unit])(using Logger[IO]): IO[Unit] =
    modifier.attempt.flatMap(_.fold(
      error => for
        logChannel <- Bot.config.logChannel
         message = s"Failed to add or remove ${role.mention} to ${user.mention}, error: ${error.getMessage}"
        _ <- Logger[IO].error(message)
        _ <- logChannel.fold(IO.unit)(_.sendMessage(message))
      yield (),
      _.pure
    ))

  val notified: Ref[IO, Set[UUID]] = Ref.unsafe(Set.empty)

  def notify(user: RegisteredUser)(using Logger[IO]): IO[Unit] =
    notified.get.flatMap {
      case needReRegister if needReRegister.contains(user.id) => IO.unit
      case _ =>
        for
          _ <- (IO.sleep(1.day) *> notified.update(_ - user.id)).start.void
          _ <- notified.update(_ + user.id)
          _ <- user.sendMessage("You are missing permissions from chaster, please use `/register` to update them.")
          _ <- log(s"User ${user.mention} chaster id ${user.chasterID} lacks \"locks\" scope and needs to reregister").compile.drain
        yield ()
    }

  private def shouldAddLocked(user: RegisteredUser, guild: Guild)(using Logger[IO]): IO[Boolean] =
    if !user.token.scope.split(" ").contains("locks") then return notify(user).as(false)
    for
      locks <- user.locks
      lockedLocks = locks.filter(_.status == LockStatus.Locked)
      keyholders = lockedLocks.flatMap(_.keyholder).map(_._id)
      user <- updateUser(user, keyholders, lockedLocks.nonEmpty)
      registeredKeyholders <- RegisteredUserRepository.list(fr"chaster_id = ANY ($keyholders)".some)
      shouldAddLocked = user.lastLocked.exists(_.isAfter(Bot.config.roles.lastLockedCutoff)) || registeredKeyholders.nonEmpty
      _ <- if shouldAddLocked then Logger[IO].debug(s"$user is locked by $registeredKeyholders") else IO.unit
    yield shouldAddLocked

  private def shouldAddKeyholder(user: RegisteredUser, guild: Guild, profile: PublicUser)(using Logger[IO]): IO[Boolean] =
    if !user.token.scope.split(" ").contains("keyholder") then return notify(user).as(false)
    for
      registeredWearers <- RegisteredUserRepository.list(fr"${profile._id} = ANY (keyholder_ids)".some)
      _ <- if registeredWearers.nonEmpty then RegisteredUserRepository.update(user.id, lastKeyheld = Instant.now.some.some) else IO.unit
      shouldAddKeyholder = user.lastKeyheld.exists(_.isAfter(Bot.config.roles.lastKeyheldCutoff)) || registeredWearers.nonEmpty
      _ <- if shouldAddKeyholder then Logger[IO].debug(s"$user is keyholder of $registeredWearers") else IO.unit
    yield shouldAddKeyholder

  private def checkChasterUserDeleted(user: RegisteredUser)(using Logger[IO]): Stream[IO, PublicUser] =
    for
      profile <- user.publicProfileByID(user.chasterID).streamed
      profile <- if profile.isDisabled then
        log(s"Profile for ${user.mention} chaster id ${user.chasterID} not found, was it deleted?")
      else
        Stream.emit(profile)
    yield profile

  private def checkDiscordUserDeleted(user: RegisteredUser, guild: Guild)(using Logger[IO]): Stream[IO, Unit] =
    for
      member <- user.member(guild).attempt.streamed
      _ <- if member.isRight then
        Stream.unit
      else
        log(s"Could not get discord user ${user.mention} did the user delete his discord account or leave the server?")
    yield ()

  override lazy val delay: FiniteDuration = Bot.config.checkFrequency

  override lazy val repeatedStream: Stream[IO, Unit] =
    for
      given Logger[IO] <- Slf4jLogger.create[IO].streamed
      discord <- Bot.discord.get.streamed
      visitorRole <- discord.roleByID(Bot.config.roles.visitor).streamed
      lockedRole <- discord.roleByID(Bot.config.roles.locked).streamed
      switchRole <- discord.roleByID(Bot.config.roles.switch).streamed
      keyholderRole <- discord.roleByID(Bot.config.roles.keyholder).streamed
      user <- Stream.evalSeq(RegisteredUserRepository.list())
      profile <- checkChasterUserDeleted(user)
      guild <- Stream.emits(discord.guilds)
      _ <- checkDiscordUserDeleted(user, guild)
      addLocked <- shouldAddLocked(user, guild).streamed
      addKeyholder <- shouldAddKeyholder(user, guild, profile).streamed
      userRoles <- List(visitorRole, lockedRole, switchRole, keyholderRole)
        .flatTraverse(role => user.hasRole(guild, role).map(Option.when(_)(role).toList)).streamed
      addRole = (role: Role) => if !userRoles.contains(role) then modifyRole(user, guild, role)(user.addRole(guild, role)) else IO.unit
      removeRole = (role: Role) => if userRoles.contains(role) then modifyRole(user, guild, role)(user.removeRole(guild, role)) else IO.unit
      addRoleRemoveOthers = (role: Role) => (addRole(role) *> userRoles.filter(_ != role).parTraverse(removeRole)).streamed
      _ <- (addLocked, addKeyholder) match
        case (true, true) => addRoleRemoveOthers(switchRole)
        case (false, true) => addRoleRemoveOthers(keyholderRole)
        case (true, false) => addRoleRemoveOthers(lockedRole)
        case (false, false) => addRoleRemoveOthers(visitorRole)
    yield ()
