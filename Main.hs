{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Main where

import System.Environment
import System.FilePath

import Data.ByteString

import Control.Monad.Trans.Resource
import Data.Void
import Data.Conduit
import Data.Monoid ((<>))
import qualified Data.Conduit.List as CL

import Network.SSH.Client.LibSSH2
import Network.SSH.Client.LibSSH2.Foreign
import Network.SSH.Client.LibSSH2.Conduit

import Data.MessagePack (Packable)
import Control.Concurrent (threadDelay)
import Network.Fluent.Logger
import Network.Fluent.Conduit


settings :: FluentSettings
settings = defaultFluentSettings { fluentSettingsTag = "debug"
                                 , fluentSettingsHost = "127.0.0.1"
                                 }

ssh :: forall t. String -> String -> Int -> (Session -> IO t) -> IO ()
ssh login host port actions = do
  initialize True
  home <- getEnv "HOME"
  let known_hosts = home </> ".ssh" </> "known_hosts"
      public = home </> ".vagrant.d" </> "insecure_public_key"
      private = home </> ".vagrant.d" </> "insecure_private_key"
  _ <- withSSH2 known_hosts public private "" login host port $ actions
  exit

conduit :: ConduitM ByteString (ByteString, ByteString) (ResourceT IO) ()
conduit = CL.map (\b -> ("vagrant", b))

sinkFluentDelay :: (MonadResource m, Packable a) => FluentSettings -> ConduitM (ByteString, a) Void m ()
sinkFluentDelay set = bracketP
              (newFluentLogger set)
              (\l -> threadDelay 1000 >> closeFluentLogger l) -- ちょっとだけディレイ
              sinkFluentWithLogger

mainCore :: IO ()
mainCore = src "vagrant" "127.0.0.1" 2222
  where
    src :: String -> String -> Int -> IO ()
    src login host port = do
      ssh login host port $ \s -> do
        (Just _, s1) <- execCommand True s "uname"
        (Just _, s2) <- execCommand True s "hostname"
        (Just _, s3) <- execCommand True s "ls -la"
        runResourceT $ (s1 <> s2 <> s3) $= conduit $$ sinkFluentDelay settings

main :: IO ()
main = mainCore
