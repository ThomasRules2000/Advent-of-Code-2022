module Util.Debug where
import           Debug.Trace (trace, traceShow)

traceWith :: (a -> String) -> a -> a
traceWith f x = trace (f x) x

traceShowWith :: Show b => (a -> b) -> a -> a
traceShowWith f x = traceShow (f x) x

traceTag :: Show a => String -> a -> a
traceTag s x = trace (s <> ": " <> show x) x

traceShowTag :: (Show a, Show b) => b -> a -> a
traceShowTag s x = trace (show s <> ": " <> show x) x