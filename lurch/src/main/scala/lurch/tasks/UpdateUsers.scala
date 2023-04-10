package lurch.tasks

import bot.{Bot, DiscordLogger}
import bot.chaster.{ChasterClient, LockStatus, PublicUser}
import bot.db.Filters.*
import bot.db.{RegisteredUserRepository, given}
import bot.model.event.{AutoCompleteEvent, SlashCommandEvent}
import bot.model.{*, given}
import bot.syntax.io.*
import bot.syntax.stream.*
import bot.tasks.RepeatedStreams
import bot.utils.*
import cats.effect.{Deferred, IO}
import cats.effect.kernel.Ref
import cats.instances.list.*
import cats.syntax.applicative.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import doobie.postgres.implicits.*
import doobie.syntax.string.*
import fs2.Stream
import lurch.Configuration
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.*

class UpdateUsers(
  discord: Deferred[IO, Discord],
  chasterClient: ChasterClient,
  registeredUserRepository: RegisteredUserRepository,
  config: Configuration,
)(using discordLogger: DiscordLogger) extends RepeatedStreams:
  private def updateUser(user: RegisteredUser, keyholderIDs: List[ChasterID], isLocked: Boolean): IO[RegisteredUser] =
    if user.keyholderIDs != keyholderIDs || user.isLocked != isLocked then
      registeredUserRepository.update(user.id, keyholderIDs = keyholderIDs.some, isLocked = isLocked.some, lastLocked = Option.when(isLocked)(Instant.now.some))
    else
      user.pure

  private def modifyRole(member: Member, role: Role)(modifier: IO[Unit])(using Logger[IO]): IO[Unit] =
    modifier.attempt.flatMap(_.fold(
      error =>
        val message = s"Failed to add or remove ${role.mention} to ${member.mention}, error: ${error.getMessage}"
        for
          _ <- Logger[IO].error(message)
          _ <- discordLogger.logToChannel(message)
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
          _ <- Logger[IO].info(s"User ${user.mention} chaster id ${user.chasterID} lacks \"locks\" scope and needs to reregister")
          _ <- discordLogger.logToChannel(s"User ${user.mention} chaster id ${user.chasterID} lacks \"locks\" scope and needs to reregister")
        yield ()
    }

  private def shouldAddLocked(user: RegisteredUser, guild: Guild)(using Logger[IO]): IO[Boolean] =
    if !user.token.scope.split(" ").contains("locks") then return notify(user).as(false)
    for
      locks <- chasterClient.authenticatedEndpoints(user.token).locks
      lockedLocks = locks.filter(lock => lock.status == LockStatus.Locked && !lock.isTestLock)
      keyholders = lockedLocks.flatMap(_.keyholder).map(_._id)
      user <- updateUser(user, keyholders, lockedLocks.nonEmpty)
      registeredKeyholders <- registeredUserRepository.thoroughList(user.keyholderIDs.anyKeyholder)
      shouldAddLocked = user.lastLocked.exists(_.isAfter(config.roles.lastLockedCutoff)) || registeredKeyholders.nonEmpty
      _ <- if shouldAddLocked then Logger[IO].debug(s"$user is locked by $registeredKeyholders") else IO.unit
    yield shouldAddLocked

  private def shouldAddKeyholder(user: RegisteredUser, guild: Guild, profile: PublicUser)(using Logger[IO]): IO[Boolean] =
    if !user.token.scope.split(" ").contains("keyholder") then return notify(user).as(false)
    for
      registeredWearers <- registeredUserRepository.thoroughList(fr"${profile._id} = ANY (keyholder_ids)".some)
      _ <- if registeredWearers.nonEmpty then registeredUserRepository.update(user.id, lastKeyheld = Instant.now.some.some) else IO.unit
      shouldAddKeyholder = user.lastKeyheld.exists(_.isAfter(config.roles.lastKeyheldCutoff)) || registeredWearers.nonEmpty
      _ <- if shouldAddKeyholder then Logger[IO].debug(s"$user is keyholder of $registeredWearers") else IO.unit
    yield shouldAddKeyholder

  private def checkChasterUserDeleted(user: RegisteredUser)(using Logger[IO]): Stream[IO, PublicUser] =
    for
      profile <- chasterClient.publicProfileByID(user.chasterID).streamed
      profile <- if profile.isDisabled then
        discordLogger.logWithoutSpam(s"Profile for ${user.mention} chaster id ${user.chasterID} not found, was it deleted?")
      else
        Stream.emit(profile)
    yield profile

  private def checkDiscordUserDeleted(user: RegisteredUser, guild: Guild)(using Logger[IO]): Stream[IO, Unit] =
    for
      member <- user.member(guild).value.streamed
      _ <- member.fold(discordLogger.logWithoutSpam(s"Could not get discord user ${user.mention} did the user delete his discord account or leave the server?"))(_ => Stream.unit)
    yield ()

  override lazy val delay: FiniteDuration = config.checkFrequency

  def handleRegisteredUser(
    guestRole: Role,
    lockedRole: Role,
    switchRole: Role,
    keyholderRole: Role,
    addRoleRemoveOthers: Role => Stream[IO, Unit]
  )(
    guild: Guild,
    user: RegisteredUser
  )(
    using Logger[IO]
  ): Stream[IO, Unit] =
    for
      profile <- checkChasterUserDeleted(user)
      _ <- checkDiscordUserDeleted(user, guild)
      addLocked <- shouldAddLocked(user, guild).streamed
      addKeyholder <- shouldAddKeyholder(user, guild, profile).streamed
      _ <- (addLocked, addKeyholder) match
        case (true, true) => addRoleRemoveOthers(switchRole)
        case (false, true) => addRoleRemoveOthers(keyholderRole)
        case (true, false) => addRoleRemoveOthers(lockedRole)
        case (false, false) => addRoleRemoveOthers(guestRole)
    yield ()

  override lazy val repeatedStream: Stream[IO, Unit] =
    for
      given Logger[IO] <- Slf4jLogger.create[IO].streamed
      discord <- discord.get.streamed
      visitorRole <- discord.unsafeRoleByID(config.roles.visitor).streamed
      guestRole <- discord.unsafeRoleByID(config.roles.guest).streamed
      lockedRole <- discord.unsafeRoleByID(config.roles.locked).streamed
      switchRole <- discord.unsafeRoleByID(config.roles.switch).streamed
      keyholderRole <- discord.unsafeRoleByID(config.roles.keyholder).streamed
      registeredUsersOrLeft <- registeredUserRepository.thoroughList().streamed
      guild <- Stream.emits(discord.guilds)
      member <- guild.members
      memberRoles <- List(visitorRole, guestRole, lockedRole, switchRole, keyholderRole)
        .flatTraverse(role => member.hasRole(guild, role).map(Option.when(_)(role).toList))
        .streamed
      addRole = (role: Role) => if !memberRoles.contains(role) then modifyRole(member, role)(member.addRole(guild, role)) else IO.unit
      removeRole = (role: Role) => if memberRoles.contains(role) then modifyRole(member, role)(member.removeRole(guild, role)) else IO.unit
      addRoleRemoveOthers = (role: Role) => (addRole(role) *> memberRoles.filter(_ != role).parTraverse(removeRole).void).streamed
      (left, registeredUsers) = registeredUsersOrLeft.partitionMap(identity)
      _ <- if left.isEmpty then Stream.unit else Stream.emits(left).flatMap(left => discordLogger.logWithoutSpam(s"Skipping update for user that left server: $left"))
      _ <- registeredUsers.find(_.discordID == member.discordID).fold(addRoleRemoveOthers(visitorRole)) { registeredUser =>
        handleRegisteredUser(guestRole, lockedRole, switchRole, keyholderRole, addRoleRemoveOthers)(guild, registeredUser)
      }.compile.drain.logErrorOption.streamed
    yield ()
