package bookofaifosi.tasks

import bookofaifosi.Bot
import bookofaifosi.chaster.Client.*
import bookofaifosi.chaster.LockStatus
import bookofaifosi.db.Filters.*
import bookofaifosi.db.{RegisteredUserRepository, given}
import bookofaifosi.model.event.{AutoCompleteEvent, SlashCommandEvent}
import bookofaifosi.model.{Guild, RegisteredUser, Role, toLong}
import bookofaifosi.syntax.io.*
import bookofaifosi.syntax.stream.*
import bookofaifosi.tasks.RepeatedStreams
import cats.effect.IO
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import doobie.syntax.string.*
import fs2.Stream
import org.typelevel.log4cats.Logger

object RoleSetLocked extends RepeatedStreams:

  def addLockedRole(user: RegisteredUser, guild: Guild)(using Logger[IO]): IO[Unit] =
    if !user.isWearer then return IO.unit
    for
      locks <- user.locks
      lockedLocks = locks.filter(_.status == LockStatus.Locked)
      keyholderNames = lockedLocks.flatMap(_.keyholder).map(_.username)
      registeredKeyholders <- RegisteredUserRepository.list(fr"chaster_name in $keyholderNames".some, fr"user_type = 'keyholder'".some)
      discord <- Bot.discord.get
      visitorRole <- discord.roleByID(Bot.config.roles.visitor)
      lockedRole <- discord.roleByID(Bot.config.roles.locked)
      keyholderRole <- discord.roleByID(Bot.config.roles.keyholder)
      hasKeyholderRole <- user.hasRole(guild, keyholderRole)
      _ <- if registeredKeyholders.nonEmpty then
        user.removeRole(guild, visitorRole) *> user.addRole(guild, lockedRole)
      else
        user.removeRole(guild, lockedRole) *> (if hasKeyholderRole then IO.unit else user.addRole(guild, visitorRole))
    yield ()

  lazy val addWearerRole: Stream[IO, Unit] =
    for
      given Logger[IO] <- Bot.logger.get.streamed
      _ <- Stream.awakeEvery[IO](Bot.config.checkFrequency)
      wearer <- Stream.evalSeq(RegisteredUserRepository.list(isWearer))
      discord <- Bot.discord.get.streamed
      guild <- Stream.emits(discord.guilds)
      _ <- addLockedRole(wearer, guild).streamed
    yield ()

  def addKeyholderRole(user: RegisteredUser, guild: Guild)(using Logger[IO]): IO[Unit] =
    if !user.isKeyholder then return IO.unit
    for
      locks <- user.locks
      lockedLocks = locks.filter(_.status == LockStatus.Locked)
      keyholderNames = lockedLocks.flatMap(_.keyholder).map(_.username)
      registeredKeyholders <- RegisteredUserRepository.list(fr"chaster_name in $keyholderNames".some, fr"user_type = 'keyholder'".some)
      discord <- Bot.discord.get
      visitorRole <- discord.roleByID(Bot.config.roles.visitor)
      lockedRole <- discord.roleByID(Bot.config.roles.locked)
      keyholderRole <- discord.roleByID(Bot.config.roles.keyholder)
      hasKeyholderRole <- user.hasRole(guild, keyholderRole)
      _ <- if registeredKeyholders.nonEmpty then
        user.removeRole(guild, visitorRole) *> user.addRole(guild, lockedRole)
      else
        user.removeRole(guild, lockedRole) *> (if hasKeyholderRole then IO.unit else user.addRole(guild, visitorRole))
    yield ()
