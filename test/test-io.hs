import           Disorder.Core.Main


import qualified Test.IO.Ambiata.Cli.Api.Upload
import qualified Test.IO.Ambiata.Cli.Downloads
import qualified Test.IO.Ambiata.Cli.Http
import qualified Test.IO.Ambiata.Cli.Incoming
import qualified Test.IO.Ambiata.Cli.Processing
import qualified Test.IO.Ambiata.Cli.Standalone

main :: IO ()
main =
  disorderMain [
      Test.IO.Ambiata.Cli.Api.Upload.tests
    , Test.IO.Ambiata.Cli.Incoming.tests
    , Test.IO.Ambiata.Cli.Processing.tests
    , Test.IO.Ambiata.Cli.Downloads.tests
    , Test.IO.Ambiata.Cli.Http.tests
    , Test.IO.Ambiata.Cli.Standalone.tests
    ]
