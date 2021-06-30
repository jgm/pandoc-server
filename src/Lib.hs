{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( app
    , Params(..)
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Servant
import Text.Pandoc
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Maybe (fromMaybe)

-- This is the data to be supplied by the JSON payload
-- of requests.  Maybe values may be omitted and will be
-- given default values.
data Params = Params
  { text           :: Text
  , from           :: Maybe Text
  , to             :: Maybe Text
  , wrapText       :: Maybe WrapOption
  , columns        :: Maybe Int
  } deriving (Show)

-- Automatically derive code to convert to/from JSON.
$(deriveJSON defaultOptions ''Params)

-- This is the API.  The "/convert" endpoint takes a request body
-- consisting of a JSON-encoded Params structure and responds to
-- Get requests with either plain text or JSON, depending on the
-- Accept header.
type API = "convert" :> ReqBody '[JSON] Params :> Post '[PlainText, JSON] Text

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = convert
 where
  -- We use runPure for the pandoc conversions, which ensures that
  -- they will do no IO.  This makes the server safe to use.  However,
  -- it will mean that features requiring IO, like RST includes, will not work.
  -- Changing this to
  --    handleErr =<< liftIO (runIO (convert' params))
  -- will allow the IO operations.
  convert params = handleErr $ runPure (convert' params)

  convert' :: PandocMonad m => Params -> m Text
  convert' params = do
    let readerFormat = fromMaybe "markdown" $ from params
    let writerFormat = fromMaybe "html" $ to params
    (readerSpec, readerExts) <- getReader readerFormat
    (writerSpec, writerExts) <- getWriter writerFormat
    -- We don't yet handle binary formats:
    reader <- case readerSpec of
                TextReader r -> return r
                _ -> throwError $ PandocAppError $
                       readerFormat <> " is not a text reader"
    writer <- case writerSpec of
                TextWriter w -> return w
                _ -> throwError $ PandocAppError $
                       readerFormat <> " is not a text reader"
    reader def{ readerExtensions = readerExts } (text params) >>=
      writer def{ writerExtensions = writerExts
                , writerWrapText = fromMaybe WrapAuto (wrapText params)
                , writerColumns = fromMaybe 72 (columns params) }

  handleErr (Right t) = return t
  handleErr (Left err) = throwError $
    err500 { errBody = TLE.encodeUtf8 $ TL.fromStrict $ renderError err }
