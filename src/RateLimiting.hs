{-# LANGUAGE BangPatterns, CPP #-}

module RateLimiting (ratedPipeline) where

import Control.DeepSeq
import Control.Monad.Par.Scheds.Trace
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Char8 (ByteString)
import RSA hiding (encrypt, decrypt)



-- Stream with Rate-Limiting Facilities

data IList a = Nil
             | Cons a (IVar (IList a))
             | Fork (Par ()) (IList a)
  
type Stream a = IVar (IList a)

instance NFData a => NFData (IList a) where
  rnf Nil = ()
  rnf (Fork a b) = rnf b
  rnf (Cons a b) = rnf a `seq` rnf b



-- Producer with Rate-Limiting Facility.

type ChunkSize = Int
type ForkDistance = Int

streamFromList ::NFData a => ChunkSize
                          -> ForkDistance
                          -> [a]
                          -> Par (Stream a)
streamFromList cSize fDist xs = do
  var <- new
  fork $ loop cSize fDist xs var 
  return var
  where
    loop :: NFData a => Int -> Int -> [a] -> IVar (IList a) -> Par () 
    loop _ _ [] var = put var Nil
    loop cs fd (x:xs) var
      | fd == 0 = do
          tail <- new
          put var (Fork (loop cs cs xs tail) (Cons x tail))
      | otherwise = do
          tail <- new
          put var (Cons x tail)
          loop cs (fd - 1) xs tail




-- A consumer of Rate-Limited Streams

streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold fn !acc instrm = do
  ilst <- get instrm
  case ilst of
    Cons h t -> streamFold fn (fn acc h) t
    Fork op (Cons h t) -> fork op >> streamFold fn (fn acc h) t
    _ -> return acc


-- A mapper over Streams with Rate-Limiting.

streamMap :: NFData b => ChunkSize
                      -> ForkDistance
                      -> (a -> b)
                      -> Stream a
                      -> Par (Stream b)
streamMap cSize fDist fn instrm = do
  outstrm <- new
  fork $ loop cSize fDist instrm outstrm
  return outstrm
  where
    loop cs fd instrm outstrm = do
      ilst <- get instrm
      case ilst of
        Fork op (Cons h t) -> do
          fork op
          newtl <- new
          loop cs fd t newtl
          put outstrm (Cons (fn h) newtl)
        Cons h t -> do
          newtl <- new
          if fd == 0
            then put outstrm (Fork (loop cs cs t newtl) (Cons (fn h) newtl))
            else put outstrm (Cons (fn h) newtl) >> loop cs (fd - 1) t newtl
        _ -> put outstrm Nil



-- Now let's use our improved tools to pipeline our RSA algorithm:


encrypt :: Integer
        -> Integer
        -> Stream ByteString
        -> Par (Stream ByteString)
encrypt n e s = streamMap 100 200 (B.pack . show . power e n . code) s


decrypt :: Integer
        -> Integer
        -> Stream ByteString
        -> Par (Stream ByteString)
decrypt n d s = streamMap 50 50 (B.pack . decode . power d n . integer) s
  where
    integer :: ByteString -> Integer
    integer b | Just (i,_) <- B.readInteger b = i


ratedPipeline :: Integer -> Integer -> Integer -> ByteString -> ByteString
ratedPipeline n e d b = runPar $ do
  s0 <- streamFromList 50 50(chunk (size n) b)
  s1 <- encrypt n e s0
  s2 <- decrypt n d s1
  xs <- streamFold (\x y -> (y : x)) [] s2
  return $ B.unlines (reverse xs)



