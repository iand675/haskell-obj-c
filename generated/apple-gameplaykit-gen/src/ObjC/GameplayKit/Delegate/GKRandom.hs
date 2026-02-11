{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GKRandom@.
--
-- Usage:
--
-- @
-- delegate <- newGKRandom defaultGKRandomOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameplayKit.Delegate.GKRandom
  ( GKRandomOverrides(..)
  , defaultGKRandomOverrides
  , newGKRandom
  ) where

import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.C.Types
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (withCString)
import Foreign.LibFFI (retCULong, argPtr)

import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass, class_createInstance)
import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.MsgSend (sendSuperMsg)
import ObjC.Runtime.StableIvar

-- | Overrides record for @\@protocol GKRandom@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GKRandomOverrides = GKRandomOverrides
  { _nextInt :: !(Maybe (IO Int))
  , _nextIntWithUpperBound :: !(Maybe (Int -> IO Int))
  , _nextUniform :: !(Maybe (IO Float))
  , _nextBool :: !(Maybe (IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultGKRandomOverrides :: GKRandomOverrides
defaultGKRandomOverrides = GKRandomOverrides
  { _nextInt = Nothing
  , _nextIntWithUpperBound = Nothing
  , _nextUniform = Nothing
  , _nextBool = Nothing
  }

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_f
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CFloat)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CFloat))

foreign import ccall "wrapper"
  wrap_Q_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO CULong))

foreign import ccall "wrapper"
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE gkRandomDelegateClass #-}
gkRandomDelegateClass :: Class
gkRandomDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGKRandom" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_nextInt = unSelector (mkSelector "nextInt")
      sel_nextIntWithUpperBound = unSelector (mkSelector "nextIntWithUpperBound:")
      sel_nextUniform = unSelector (mkSelector "nextUniform")
      sel_nextBool = unSelector (mkSelector "nextBool")
  -- nextInt
  stub_0 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKRandomOverrides
    case _nextInt rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "nextInt" "q@:" stub_0

  -- nextIntWithUpperBound:
  stub_1 <- wrap_Q_Q $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKRandomOverrides
    case _nextIntWithUpperBound rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (fromIntegral arg0)
        pure (fromIntegral r)
  addObjCMethod cls "nextIntWithUpperBound:" "Q@:Q" stub_1

  -- nextUniform
  stub_2 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKRandomOverrides
    case _nextUniform rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "nextUniform" "f@:" stub_2

  -- nextBool
  stub_3 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKRandomOverrides
    case _nextBool rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "nextBool" "B@:" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKRandomOverrides
    if queriedSel == sel_nextInt then pure (maybe 0 (const 1) (_nextInt rec_))
    else if queriedSel == sel_nextIntWithUpperBound then pure (maybe 0 (const 1) (_nextIntWithUpperBound rec_))
    else if queriedSel == sel_nextUniform then pure (maybe 0 (const 1) (_nextUniform rec_))
    else if queriedSel == sel_nextBool then pure (maybe 0 (const 1) (_nextBool rec_))
    else do
      let super_ = ObjCSuper (RawId self) superCls
      sendSuperMsg super_ (mkSelector "respondsToSelector:") retCULong
        [argPtr (castPtr queriedSel :: Ptr ())]
  addObjCMethod cls "respondsToSelector:" "B@::" rtsStub

  addStablePtrDeallocHandler cls
  objc_registerClassPair cls
  pure cls

-- | Create a new delegate implementing this protocol.
--
-- The returned 'RawId' can be used as a delegate or data source.
newGKRandom :: GKRandomOverrides -> IO RawId
newGKRandom overrides = do
  inst <- class_createInstance gkRandomDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
