module IlievUtils.Logging (
                           LogPrio(..),

                           initLogging,

                           logmsg,

                           trace,

                           logProgress,
                           logDebug,
                           logDump,
)
where

import qualified Debug.Trace                    as Trace
import qualified Data.IORef                     as IORef
import qualified System.Log.Logger              as Logger
import qualified System.IO.Unsafe               as Unsafe
import Maybe                                    (fromMaybe)


cDEFAULT_TRACE_LEVEL = Logger.INFO

traceIO msg = do
  root <- Logger.getRootLogger
  let level = fromMaybe cDEFAULT_TRACE_LEVEL (Logger.getLevel root)
  Logger.logL root level msg

{-# NOINLINE trace #-}
trace val msg = Unsafe.unsafePerformIO $ do
                  traceIO msg
                  return val

data LogPrio = DUMP | DEBUG | INFO | PROGRESS | WARNING | ERROR
   deriving (Show,Eq,Ord)

initLogging :: LogPrio -> IO ()
initLogging level = do
  root <- Logger.getRootLogger
  let r2 = Logger.setLevel (xlate level) root
  Logger.saveGlobalLogger r2


xlate DUMP = Logger.DEBUG
xlate DEBUG = Logger.DEBUG
xlate INFO = Logger.INFO
xlate PROGRESS = Logger.NOTICE
xlate WARNING = Logger.WARNING
xlate ERROR = Logger.ERROR


logmsg prio x msg =
    Unsafe.unsafePerformIO $ do
      root <- Logger.getRootLogger
      Logger.logL root (xlate prio) msg
      return x

logProgress = logmsg PROGRESS
logDebug    = logmsg DEBUG
logDump     = logmsg DUMP

infix 0 `trace`
