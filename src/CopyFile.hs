module CopyFile where
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B ( readFile, hPutStr )
import Control.Exception.Base (bracketOnError)
import System.IO (openTempFile, hClose)
import System.Directory (renameFile, removeFile)

copyFile :: IO ()
copyFile = do
    (fileName1:fileName2:_) <- getArgs
    copy fileName1 fileName2

copy :: FilePath -> FilePath -> IO ()
copy source dest = do
    contents <- B.readFile source
    bracketOnError
        (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            B.hPutStr tempHandle contents
            hClose tempHandle
            renameFile tempName dest)
