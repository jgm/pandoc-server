{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Text.Pandoc
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Control.Monad.IO.Class (liftIO)

data Params = Params
  { from :: Text
  , to   :: Text
  , text :: Text
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Params)

type API = "convert"
           :> ReqBody '[JSON] Params
           :> Get '[PlainText, JSON] Text

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = convert
 where
  convert params = handleErr =<< (liftIO . runIO $ do
      (reader, readerExts) <- getReader (from params)
      (writer, writerExts) <- getWriter (to params)
      case (reader, writer) of
        (TextReader r , TextWriter w) ->
          r def{ readerExtensions = readerExts } (text params) >>=
             w def{ writerExtensions = writerExts }
        (TextReader _, _) -> throwError $ PandocAppError $
                              (to params) <> " is not a text writer"
        (_, _) -> throwError $ PandocAppError $
                              (from params) <> " is not a text reader")
  handleErr (Right t) = return t
  handleErr (Left err) =
    throwError $ err500 { errBody = TLE.encodeUtf8 $ TL.fromStrict $
                                   renderError err }
