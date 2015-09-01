import           Disorder.Core.Main


import qualified Test.IO.TatooineCli.Credentials
import qualified Test.IO.TatooineCli.Downloads
import qualified Test.IO.TatooineCli.Incoming
import qualified Test.IO.TatooineCli.Processing

main :: IO ()
main =
  disorderMain [
    Test.IO.TatooineCli.Credentials.tests,
    Test.IO.TatooineCli.Incoming.tests,
    Test.IO.TatooineCli.Processing.tests,
    Test.IO.TatooineCli.Downloads.tests
    ]
