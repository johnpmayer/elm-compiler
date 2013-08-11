
module CLI where

import JavaScript (JSString)
import Native.CLI
import Signal (Signal)

stdin : Signal JSString
stdin = Native.CLI.stdin

stdouteffect : Signal JSString -> Signal JSString
stdouteffect = Native.CLI.stdouteffect
