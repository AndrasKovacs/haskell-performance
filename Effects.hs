{-# language
  GADTs, BangPatterns, TypeFamilies, MagicHash, KindSignatures,
  PatternSynonyms, ViewPatterns, RoleAnnotations, LambdaCase,
  TemplateHaskell,
  UnboxedTuples, FlexibleInstances, MultiParamTypeClasses #-}

module Main where

import GHC.Prim
import Control.Monad
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Criterion.Main
import Control.Lens

-- coe :: a -> b
-- coe = unsafeCoerce#
-- {-# inline coe #-}

-- type role RSE representational representational representational representational
-- newtype RSE r s (e :: *) (a :: *) = RSE {unRSE :: r -> s -> (# Int#, Any, Any #)}

-- -- Ok  : (# 0#, a, s #)
-- -- Err : (# 1#, e, e #) -- "e" twice for locality

-- instance Functor (RSE r s e) where
--   fmap f (RSE g) = RSE $ \r s -> case g r s of
--     (# 0#, coe -> a, s #) -> (# 0#, coe (f a), s #)
--     err                   -> err
--   {-# inline fmap #-}

-- instance Applicative (RSE r s e) where
--   pure  = return
--   {-# inline pure #-}
--   RSE mf <*> RSE ma = RSE $ \r s -> case mf r s of
--     (# 0#, coe -> f, coe -> s' #) -> case ma r s' of
--       (# 0#, coe -> a, coe -> s'' #) -> (# 0#, coe (f a), s'' #)
--       err -> err
--     err -> err
--   {-# inline (<*>) #-}

-- instance Monad (RSE r s e) where
--   return a = RSE $ \r s -> (# 0#, coe a, coe s #)
--   {-# inline return #-}
--   RSE ma >>= f = RSE $ \r s -> case ma r s of
--     (# 0#, coe -> a, coe -> s' #) -> unRSE (f a) r s'
--     err -> err
--   {-# inline (>>=) #-}

-- instance MonadState s (RSE r s e) where
--   get = RSE $ \r s -> (# 0#, coe s, coe s #)
--   {-# inline get #-}
--   put s = RSE $ \r _ -> (# 0#, coe (), coe s #)
--   {-# inline put #-}
--   state f = RSE $ \r s -> case f s of (a, s') -> (# 0#, coe a, coe s' #)
--   {-# inline state #-}

-- instance MonadReader r (RSE r s e) where
--   ask = RSE $ \r s -> (# 0#, coe r, coe s #)
--   {-# inline ask #-}
--   local f (RSE ma) = RSE $ \r s -> ma (f r) s
--   {-# inline local #-}
--   reader f = RSE $ \r s -> (# 0#, coe (f r), coe s #)
--   {-# inline reader #-}

-- instance MonadError e (RSE r s e) where
--   throwError e = RSE $ \r s -> (# 1#, coe e, coe e #)
--   {-# inline throwError #-}
--   catchError (RSE ma) h = RSE $ \r s -> case ma r s of
--     (# 1#, coe -> e, _ #) -> unRSE (h e) r s
--     ok                    -> ok
--   {-# inline catchError #-}

-- runRSE :: r -> s -> RSE r s e a -> Either e (a, s)
-- runRSE r s (RSE ma) = case ma r s of
--   (# 0#, coe -> a, coe -> s #) -> Right (a, s)
--   (# _ , coe -> e, _        #) -> Left e
-- {-# inline runRSE #-}

type RSE' r s e a = ReaderT r (StateT s (Either e)) a

runRSE' :: r -> s -> RSE' r s e a -> Either e (a, s)
runRSE' r s ma = runStateT (runReaderT ma r) s


-- countDown :: Int -> RSE () Int () ()
-- countDown 0 = return ()
-- countDown n = do
--   put . subtract 1 =<< get
--   countDown (n - 1)

-- countDown' :: Int -> RSE' () Int () ()
-- countDown' 0 = return ()
-- countDown' n = do
--   put . subtract 1 =<< get
--   countDown' (n - 1)

data Dummy = Dummy {
  _a, _b, _c, _d, _e :: Int }

makeLenses ''Dummy

act :: RSE' () Dummy () ()
act = a += 1

replicateM_' :: Monad m => Int -> m a -> m ()
replicateM_' n ma = go n where
  go n | n <= 0 = return ()
  go n = ma >> go (n - 1)

main = do
  -- print $ runRSE () 0 (countDown 10000000)
  -- print $ runRSE' () 0 (countDown' 100000000000)

  print $ _a <$> snd <$> runRSE' () (Dummy 0 0 0 0 0) (replicateM_' 100000 act)
  -- print $ runRSE () 0 (replicateM_' 1000000 (put . (+1) =<< get :: RSE () Int () ()))



