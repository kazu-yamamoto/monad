module A where

import System.Environment (getArgs)
import Control.Monad.Reader

type RIO r = ReaderT r IO
type Args = [String]
    
runRIO :: RIO r a -> r -> IO a
runRIO = {- runIO -} runReaderT 

main :: IO ()
main = do
    xs <- getArgs
    runRIO foo xs

foo :: RIO Args ()
foo = do
    args <- ask
    if "-f" `elem` args
       then liftIO $ putStrLn "-f option specified"
       else liftIO $ putStrLn "-f option not specified"
