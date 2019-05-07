{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import RIO

data App = App
  { appLogFunc :: !LogFunc
  , appName :: !Utf8Builder
  }

instance HasLogFunc App where

  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

main :: IO ()
main = runApp sayHello

runApp :: RIO App a -> IO a
runApp inner = do
  logOptions' <- logOptionsHandle stderr False
  let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
  withLogFunc logOptions $ \logFunc -> do
    let app = App
          {
            appLogFunc = logFunc
          , appName = "Alice"
          }
    runRIO app inner

sayHello :: RIO App ()
sayHello = do
  name <- view $ to appName
  logInfo $ "Hello, " <> name
