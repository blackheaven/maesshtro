{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Network.SSH.Maesshtro
    ( Partition
    , Sender
    , runSession
    , dummyServer
    , run
    -- , runAndProcess
    ) where

import Control.Monad.Free

data PartitionF next = Command String (Int -> String -> next)

type Partition = Free PartitionF

instance Functor PartitionF where
    fmap f (Command c p) = Command c (\s o -> f (p s o))

run :: String -> Partition String
run c = runAndProcess c $ flip const

runAndProcess :: String -> (Int -> String -> a) -> Partition a
runAndProcess c p = liftF $ Command c p

runSession :: Monad m => Sender m -> Partition a -> m a
runSession s = foldFree $ \x -> case x of
                                  Command c p -> uncurry p <$> s c

type Sender m = String -> m (Int, String)

dummyServer :: Sender []
dummyServer c = if not (null parts) && cmd == "echo" then [(0, args)] else [(1, "Unknown command " ++ cmd)]
  where parts = words c
        cmd = head parts
        args = unwords (tail parts)
