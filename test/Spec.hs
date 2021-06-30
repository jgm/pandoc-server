{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai.Test (SResponse)
import Network.HTTP.Types
import Data.ByteString as BS
import Data.ByteString.Lazy as LBS

main :: IO ()
main = hspec spec

postJSON :: BS.ByteString -> LBS.ByteString -> WaiSession st SResponse
postJSON url req = request methodPost url headers req
  where
   headers = [(hContentType, "application/json")]

spec :: Spec
spec = with (return app) $ do
    describe "POST /convert" $ do
        it "responds with converted text" $ do
            postJSON "/convert" [json|{text: "*hi*", to: "latex"}|]
              `shouldRespondWith` "\\emph{hi}"
