import           Control.Applicative
import           Control.Monad

import           System.Directory
import           System.Environment
import           System.IO
import           System.Process

-- Smoketests have extra dependencies, so we don't want them to be run as
-- part of the standard testsuite by default; use a flag to turn them on.
smoke :: IO ()
smoke =
  let td       = "test/smoke/"
      ignore p = ".." == p || "." == p || "core" == p
      exec t   = callProcess (td ++ t ++ "/run") []
   in sanity >> filter (not . ignore) <$> getDirectoryContents td >>= mapM_ exec
 where
  sanity = hSetBuffering stdout LineBuffering >> hSetBuffering stderr LineBuffering

main :: IO ()
main = do
  magic <- (maybe False (const True)) <$> (lookupEnv "SMOKETESTS")
  if magic then smoke else (pure ())
