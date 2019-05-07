#!/usr/bin/env stack

{-# LANGUAGE NoImplicitPrelude #-}

import RIO
import System.IO (hPutStrLn, stderr, stdout)

data App = App
  { appName :: !String
  , appHandle :: !Handle
  }

class HasHandle env where
  handleL :: Lens' env Handle
instance HasHandle App where
  handleL = lens appHandle (\x y -> x { appHandle = y })

class HasName env where
  nameL :: Lens' env String
instance HasName App where
  nameL = lens appName (\x y -> x { appName = y })

main :: IO ()
main = do
  let app = App
        { appName = "Alice"
        , appHandle = stderr
        }
  runRIO app $ addLastName sayHello

addLastName :: HasName env => RIO env a -> RIO env a
addLastName inner = do
  name <- view nameL
  local (over nameL (<> " Smith")) inner

say :: HasHandle env => String -> RIO env ()
say msg = do
  h <- view handleL
  liftIO $ hPutStrLn h msg

sayHello :: RIO App ()
sayHello = do
  App name _h <- ask
  say $ "Hello, " ++ name
