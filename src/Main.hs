{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE QuasiQuotes                #-}
import           Control.Applicative          ((<$>), (<*>))
import           Control.Monad                (mzero)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger         (runNoLoggingT, NoLoggingT)
import           Control.Monad.Trans.Resource (runResourceT, ResourceT)
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as Text
import           Data.Proxy
import qualified Database.Persist.Class       as DB
import qualified Database.Persist.Sqlite      as Sqlite
import           Database.Persist.TH
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp     (run)
import           Network.Wai.Middleware.AddHeaders
import           Servant
import           System.Environment
import           Web.PathPieces

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Todo
    title String
    completed Bool
    order Int
    deriving Show
|]

runDb :: Sqlite.SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb = runNoLoggingT . runResourceT . Sqlite.withSqliteConn "dev.sqlite3" . Sqlite.runSqlConn

instance ToJSON (Sqlite.Entity Todo) where
  toJSON entity = object
      [ "id"        .= key
      , "url"       .= ("http://127.0.0.1:3000/todos/" ++ keyText)
      , "title"     .= todoTitle val
      , "completed" .= todoCompleted val
      , "order"     .= todoOrder val
      ]
    where
      key     = Sqlite.entityKey entity
      val     = Sqlite.entityVal entity
      keyText = Text.unpack $ toPathPiece key


data TodoAction = TodoAction
  { actTitle     :: Maybe String
  , actCompleted :: Maybe Bool
  , actOrder     :: Maybe Int
  } deriving Show

instance FromJSON TodoAction where
  parseJSON (Object o) = TodoAction
    <$> o .:? "title"
    <*> o .:? "completed"
    <*> o .:? "order"
  parseJSON _ = mzero

instance ToJSON TodoAction where
  toJSON (TodoAction mTitle mCompl mOrder) = noNullsObject
      [ "title"     .= mTitle
      , "completed" .= mCompl
      , "order"     .= mOrder
      ]
    where
      noNullsObject = object . filter notNull
      notNull (_, Null) = False
      notNull _         = True

actionToTodo :: TodoAction -> Todo
actionToTodo (TodoAction mTitle mCompleted mOrder) = Todo title completed order
  where
    title     = fromMaybe "" mTitle
    completed = fromMaybe False mCompleted
    order     = fromMaybe 0 mOrder

actionToUpdates :: TodoAction -> [Sqlite.Update Todo]
actionToUpdates act =  updateTitle
                    ++ updateCompl
                    ++ updateOrd
  where
    updateTitle = maybe [] (\title -> [TodoTitle Sqlite.=. title])
                  (actTitle act)
    updateCompl = maybe [] (\compl -> [TodoCompleted Sqlite.=. compl])
                  (actCompleted act)
    updateOrd   = maybe [] (\ord -> [TodoOrder Sqlite.=. ord])
                  (actOrder act)

allowCors :: Middleware
allowCors = addHeaders [
    ("Access-Control-Allow-Origin", "*"),
    ("Access-Control-Allow-Headers", "Accept, Content-Type"),
    ("Access-Control-Allow-Methods", "GET, HEAD, POST, DELETE, OPTIONS, PUT, PATCH")
  ]

allowOptions :: Middleware
allowOptions app req resp = case requestMethod req of
  "OPTIONS" -> resp $ responseLBS status200 [] "Ok"
  _         -> app req resp

type TodoApi = "todos" :> Get '[JSON] [Sqlite.Entity Todo]
            :<|> "todos" :> Delete '[JSON] ()
            :<|> "todos" :> ReqBody '[JSON] TodoAction :> Post '[JSON] (Sqlite.Entity Todo)
            :<|> "todos" :> Capture "todoid" Integer :> Get '[JSON] (Sqlite.Entity Todo)
            :<|> "todos" :> Capture "todoid" Integer :> Delete '[JSON] ()
            :<|> "todos" :> Capture "todoid" Integer :> ReqBody '[JSON] TodoAction :> Patch '[JSON] (Sqlite.Entity Todo)

todoApi :: Proxy TodoApi
todoApi = Proxy

getTodos :: EitherT ServantErr IO [Sqlite.Entity Todo]
getTodos = liftIO $ runDb $ DB.selectList [] ([] :: [Sqlite.SelectOpt Todo])

deleteTodos :: EitherT ServantErr IO ()
deleteTodos =  liftIO $ runDb $ DB.deleteWhere ([] :: [Sqlite.Filter Todo])

getTodo :: Integer -> EitherT ServantErr IO (Sqlite.Entity Todo)
getTodo tid = do
  let tKey = Sqlite.toSqlKey (fromIntegral tid)
  Just todo <- liftIO $ runDb $ DB.get tKey
  return $ Sqlite.Entity tKey todo

deleteTodo :: Integer -> EitherT ServantErr IO ()
deleteTodo tid = do
  let tKey = Sqlite.toSqlKey (fromIntegral tid)
  liftIO $ runDb $ DB.delete (tKey :: Sqlite.Key Todo)

postTodo :: TodoAction -> EitherT ServantErr IO (Sqlite.Entity Todo)
postTodo todoAct = do
  let todo = actionToTodo todoAct
  tid <- liftIO $ runDb $ DB.insert todo
  return $ Sqlite.Entity tid todo

patchTodo :: Integer -> TodoAction -> EitherT ServantErr IO (Sqlite.Entity Todo)
patchTodo tid todoAct = do
  let tKey = Sqlite.toSqlKey (fromIntegral tid)
      updates = actionToUpdates todoAct
  todo <- liftIO $ runDb $ DB.updateGet tKey updates
  return $ Sqlite.Entity tKey todo

server :: Server TodoApi
server =      getTodos
         :<|> deleteTodos
         :<|> postTodo
         :<|> getTodo
         :<|> deleteTodo
         :<|> patchTodo

waiApp :: Application
waiApp = allowCors $ allowOptions $ serve todoApi server

main :: IO ()
main = do
  runDb $ Sqlite.runMigration migrateAll
  port <- read <$> getEnv "PORT"
  run port waiApp
