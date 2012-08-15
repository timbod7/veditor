module Examples.Example5 where

import System.FilePath
import System.Directory
import System.Posix
import Control.Monad

import VE
import ErrVal
import Examples.Utils(testE)

type UserCode = Int
type RoleCode = Int

data EnvStruct = EnvStruct {
    user :: UserCode,
    role :: RoleCode
}

userCodeVE, roleCodeVE :: VE (IOE FilePath) Int
userCodeVE = EnumVE (IOE (dirContents "users"))
roleCodeVE = EnumVE (IOE (dirContents "roles"))

envStructVE :: VE (IOE FilePath) EnvStruct
envStructVE = mapVE toStruct fromStruct
    (  label "user" userCodeVE
    .*. label "role" roleCodeVE
    )
      where
        toStruct (a,b) = eVal (EnvStruct a b)
        fromStruct (EnvStruct a b) = (a,b)

dirContents :: String -> FilePath -> IO [String]
dirContents subdir root = do
       putStrLn ("Reading enums from " ++ dir)
       dirExists <- fileExist dir
       if dirExists then filterM isRegFile =<< getDirectoryContents dir
                    else return []
   where
     isRegFile fp = fmap isRegularFile (getFileStatus (combine dir fp))
     dir = combine root subdir

test = testE envStructVE "/tmp"