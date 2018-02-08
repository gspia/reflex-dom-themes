
module Main where

------------------------------------------------------------------------------

import Data.Function                          ((&))
import Data.Monoid                            ((<>))

import Language.Javascript.JSaddle            (JSM, syncPoint)
import Language.Javascript.JSaddle.Warp       (run)
import Language.Javascript.JSaddle.WebSockets (debugWrapper, jsaddleWithAppOr)
import Network.Wai                            (Application)
import Network.Wai.Handler.Warp               (defaultSettings, runSettings
                                              , setPort, setTimeout)
import Network.WebSockets                     (defaultConnectionOptions)
import Network.Wai.Application.Static         (ssMaxAge, staticApp
                                              , defaultFileServerSettings)
import WaiAppStatic.Types                     (MaxAge(MaxAgeSeconds))

------------------------------------------------------------------------------

import MainBs (mainW)

------------------------------------------------------------------------------

-- | Note that in the dev-server.sh-script (to use ghcid), we refer to the
-- mainW and in the .ghci file we select, which of the examples is run
-- in the ghcid.
main :: IO ()
main = run 8000 mainW

------------------------------------------------------------------------------

-- | A @main@ for doing development.
devMain :: Application -> JSM () -> Int -> IO ()
devMain backend frontend port = do
  putStrLn $ "Running dev server on localhost:" <> show port
  app <- jsaddleWithAppOr
    defaultConnectionOptions
    (frontend >> syncPoint)
    backend
  runSettings (defaultSettings & setTimeout 3600 & setPort port) app


-- | A version of @devMain@ that can be used with @ghcid --test@
-- to get an auto-reloading server.
devMainAutoReload :: Application -> JSM () -> Int -> IO ()
devMainAutoReload backend frontend port =
  debugWrapper $ \refreshMiddleware registerContext ->
    devMain (refreshMiddleware backend) (registerContext >> frontend) port


-- | Serve content from the ./static -directory.
staticServer :: Application
staticServer = staticApp ((defaultFileServerSettings "./static") & noCache)
  where noCache s = s { ssMaxAge = MaxAgeSeconds 0 }


