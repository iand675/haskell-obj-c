{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MLBatchProvider@.
--
-- Usage:
--
-- @
-- delegate <- newMLBatchProvider defaultMLBatchProviderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreML.Delegate.MLBatchProvider
  ( MLBatchProviderOverrides(..)
  , defaultMLBatchProviderOverrides
  , newMLBatchProvider
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

-- | Overrides record for @\@protocol MLBatchProvider@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MLBatchProviderOverrides = MLBatchProviderOverrides
  { _featuresAtIndex :: !(Maybe (Int -> IO RawId))
  , _count :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultMLBatchProviderOverrides :: MLBatchProviderOverrides
defaultMLBatchProviderOverrides = MLBatchProviderOverrides
  { _featuresAtIndex = Nothing
  , _count = Nothing
  }

foreign import ccall "wrapper"
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))

foreign import ccall "wrapper"
  wrap_q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mlBatchProviderDelegateClass #-}
mlBatchProviderDelegateClass :: Class
mlBatchProviderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMLBatchProvider" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_featuresAtIndex = unSelector (mkSelector "featuresAtIndex:")
      sel_count = unSelector (mkSelector "count")
  -- featuresAtIndex:
  stub_0 <- wrap_q_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MLBatchProviderOverrides
    case _featuresAtIndex rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (fromIntegral arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "featuresAtIndex:" "@@:q" stub_0

  -- count
  stub_1 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MLBatchProviderOverrides
    case _count rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "count" "q@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MLBatchProviderOverrides
    if queriedSel == sel_featuresAtIndex then pure (maybe 0 (const 1) (_featuresAtIndex rec_))
    else if queriedSel == sel_count then pure (maybe 0 (const 1) (_count rec_))
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
newMLBatchProvider :: MLBatchProviderOverrides -> IO RawId
newMLBatchProvider overrides = do
  inst <- class_createInstance mlBatchProviderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
