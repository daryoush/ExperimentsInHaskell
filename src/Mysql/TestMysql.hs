
module Mysql.TestMysql where

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL

main = do
  rows <- withRTSSignalsBlocked $ do
    conn <- connectMySQL defaultMySQLConnectInfo {
              mysqlHost     = "127.0.0.1",
              mysqlUser     = "daryoush",
              mysqlPassword = "daryoush",
              mysqlDatabase = "kitchensink"
            }
    quickQuery' conn "SELECT * from test" []
  forM_ rows $ \row -> putStrLn $ show row

