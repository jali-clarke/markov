{-# LANGUAGE
    OverloadedStrings
#-}

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Servant.JS as JS
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))

import Api.Types

apiFunctionsText :: Text.Text
apiFunctionsText =
    let generator = JS.axios JS.defAxiosOptions
    in Text.replace "\nvar " "\nconst " $ JS.jsForAPI api generator

addPreamble :: Text.Text -> Text.Text
addPreamble apiFunctions =
    "const axios = require(\"axios\");\n"
    <> "axios.defaults.baseURL = process.env.API_URL || \"http://localhost\";\n"
    <> apiFunctions

writeApiToDirectory :: FilePath -> IO ()
writeApiToDirectory dir =
    let filePath = dir </> "api.js"
    in Text.writeFile filePath (addPreamble apiFunctionsText)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [directory] -> createDirectoryIfMissing True directory *> writeApiToDirectory directory
        _ -> die "usage: api-generator <dir>"
        