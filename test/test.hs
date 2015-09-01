import           Disorder.Core.Main

import qualified Test.TatooineCli.Data
import qualified Test.TatooineCli.Incoming
import qualified Test.TatooineCli.Json


main :: IO ()
main =
  disorderMain [
       Test.TatooineCli.Data.tests,
       Test.TatooineCli.Incoming.tests,
       Test.TatooineCli.Json.tests
    ]
