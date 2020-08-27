{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           Data.Aeson
import           Database.Beam            as B
import           Database.Beam.Postgres      as B
--import           Database.Postgres.Base      as B
import           GHC.Generics
import           Lib
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

-- * api

type ItemApi =
  "item" :> ReqBody '[JSON] Item :> Post '[JSON] String :<|>
  "item" :> Capture "itemId" Int :> Get '[JSON] (Maybe Item)

itemApi :: Proxy ItemApi
itemApi = Proxy

-- * app

main :: IO ()
main = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve itemApi server

server :: Server ItemApi
server =
  insertItem :<|>
  getItemById

data ItemT f
  = Item {
    itemId   :: B.C f Int
  }

deriving instance Generic (ItemT f)
deriving instance Beamable ItemT

type Item = ItemT Identity

deriving instance Show Item
deriving instance Eq Item
deriving instance Ord Item

instance ToJSON Item
instance FromJSON Item

type ItemPrimaryKey = B.PrimaryKey ItemT Identity

instance B.Table ItemT where
  data PrimaryKey ItemT f = ItemPrimaryKey (B.C f Int) deriving (Generic, B.Beamable)
  primaryKey = ItemPrimaryKey . itemId

data TestDb f = TestDb
                      { _item :: f (TableEntity ItemT) }
                        deriving (Generic, Database be)

testDb :: DatabaseSettings be TestDb
testDb = defaultDbSettings

mysqlConfig = B.ConnectInfo
  { connectHost = ""
  , connectPort = 12401
  , connectUser = ""
  , connectPassword  = ""
  , connectDatabase  = ""
  --, connectOptions = []
  --, connectPath  = ""
  --, connectSSL = Nothing
  }

getItemById :: Int -> Handler (Maybe Item)
getItemById input = do
  conn <- liftIO $ connect mysqlConfig
  liftIO $ print "connection establised"
  let predicate = \item -> itemId item ==. B.val_ input
  liftIO $
    B.runBeamPostgresDebug print conn $
      B.runSelectReturningOne $ B.select $ B.filter_ predicate $ B.all_ (_item testDb)

insertItem :: Item -> Handler String
insertItem item = do
  conn <- liftIO $ connect mysqlConfig
  liftIO $ print "connection establised"
  liftIO $
    B.runBeamPostgresDebug print conn $
      B.runInsert $ B.insert (_item testDb) (B.insertExpressions (toRowExpression <$> [item]))
  return "Done"

toRowExpression Item {..} = Item (B.val_ itemId)
