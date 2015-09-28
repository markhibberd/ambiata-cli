import           Disorder.Core.Main


import qualified Test.IO.Ambiata.Cli.Credentials
import qualified Test.IO.Ambiata.Cli.Downloads
import qualified Test.IO.Ambiata.Cli.Incoming
import qualified Test.IO.Ambiata.Cli.Processing

main :: IO ()
main =
  disorderMain [
    Test.IO.Ambiata.Cli.Credentials.tests,
    Test.IO.Ambiata.Cli.Incoming.tests,
    Test.IO.Ambiata.Cli.Processing.tests,
    Test.IO.Ambiata.Cli.Downloads.tests
    ]
