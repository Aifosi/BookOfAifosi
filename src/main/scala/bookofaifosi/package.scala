import bookofaifosi.commands.Command
import bookofaifosi.wrappers.event.Event

package object bookofaifosi:
  type AnyCommand = Command[?, ? <: Event]
