import           Disorder.Core.Main

import qualified Test.Ambiata.Cli.Data
import qualified Test.Ambiata.Cli.Incoming
import qualified Test.Ambiata.Cli.Json


main :: IO ()
main =
  disorderMain [
       Test.Ambiata.Cli.Data.tests,
       Test.Ambiata.Cli.Incoming.tests,
       Test.Ambiata.Cli.Json.tests
    ]
