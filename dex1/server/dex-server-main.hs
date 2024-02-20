import           Control.Exception           (try)
import           Control.Monad.Trans.Except
import qualified Network.HTTP.Types          as HttpTypes
import           Network.Wai.Handler.Warp    (run, Port)
import           Network.Wai.Middleware.Cors
import           Servant
import           System.Environment          (getArgs)

import           GeniusYield.GYConfig

import           Dex.Api.Api
import           Dex.Api.Context
import           Data.Aeson.Encode.Pretty    (encodePretty)
import qualified Data.ByteString.Lazy.Char8  as BL8
import           Prelude                     

-- | Getting path for our core configuration.
parseArgs :: IO (FilePath, Int)
parseArgs = do
  args <- getArgs
  putStrLn $ "args: " <> show args
  case args of
    coreCfg: port: _ -> return (coreCfg, read port :: Int)
    coreCfg: _ -> return (coreCfg, 8081)
    --[coreCfg: port]: _ -> return (coreCfg, port)
    _invalidArgument -> fail "Error: wrong arguments, needed a path to the CoreConfig JSON configuration file\n"

main :: IO ()
main = do
  putStrLn "Writing Swagger file ..."
  BL8.writeFile "swagger-api.json" (encodePretty apiSwagger)

  putStrLn "parsing Config ..."
  (coreCfgPath, port) <- parseArgs
  coreCfg <- coreConfigIO coreCfgPath  -- Parsing our core configuration.

  putStrLn "Loading Providers ..."
  withCfgProviders coreCfg "api-server"  $ \providers -> do
    let -- port = portS
        ctx = Ctx coreCfg providers
    putStrLn $ "Starting server at \n " <> "http://localhost:" <> show port
    run port $ app ctx


app :: Ctx -> Application
app ctx = cors (const $ Just simpleCorsResourcePolicy { corsRequestHeaders = [HttpTypes.hContentType] }) $ serve appApi $ hoistServer appApi (Handler . ExceptT . try)  $ apiServer ctx
