{-# LANGUAGE BangPatterns, CPP #-}

module PipeLine (pipeline) where

import Control.DeepSeq
import Control.Monad.Par.Scheds.Trace
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Char8 (ByteString)
import RSA hiding (encrypt, decrypt)

-- Explicit representation of a Stream

data IList a = Nil | Cons a (IVar (IList a))

  
type Stream a = IVar (IList a)

instance NFData a => NFData (IList a) where
  rnf Nil = ()
  rnf (Cons a b) = rnf a `seq` rnf b


-- A generic producer that turns a lazy list into a Stream

streamFromList ::NFData a => [a] -> Par (Stream a)
streamFromList xs = do
  var <- new
  fork $ loop xs var -- forks the loop that will create the Stream contents
  return var -- Returns the Stream to the caller. The Stream is now
             -- created in parallel.
  where
    loop :: NFData a => [a] -> IVar (IList a) -> Par () 
    loop [] var = put var Nil
    loop (x:xs) var = do
      tail <- new
      put var (Cons x tail)
      loop xs tail


-- A consumer of Streams

streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold fn !acc instrm = do
  ilst <- get instrm
  case ilst of
    Nil -> return acc
    Cons h t -> streamFold fn (fn acc h) t


-- A mapper over Streams. This is both a produce and a consumer:

streamMap :: NFData b => (a -> b) -> Stream a -> Par (Stream b)
streamMap fn instrm = do
  outstrm <- new
  fork $ loop instrm outstrm
  return outstrm
  where
    loop instrm outstrm = do
      ilst <- get instrm
      case ilst of
        Nil -> put outstrm Nil
        Cons h t -> do
          newtl <- new
          put outstrm (Cons (fn h) newtl)
          loop t newtl


-- Now let's use these tools to pipeline our RSA algorithm:


encrypt :: Integer
        -> Integer
        -> Stream ByteString
        -> Par (Stream ByteString)
encrypt n e s = streamMap (B.pack . show . power e n . code) s


decrypt :: Integer
        -> Integer
        -> Stream ByteString
        -> Par (Stream ByteString)
decrypt n d s = streamMap (B.pack . decode . power d n . integer) s
  where
    integer :: ByteString -> Integer
    integer b | Just (i,_) <- B.readInteger b = i


pipeline :: Integer -> Integer -> Integer -> ByteString -> ByteString
pipeline n e d b = runPar $ do
  s0 <- streamFromList (chunk (size n) b)
  s1 <- encrypt n e s0
  s2 <- decrypt n d s1
  xs <- streamFold (\x y -> (y : x)) [] s2
  return $ B.unlines (reverse xs)

