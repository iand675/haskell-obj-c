{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GCRelativeInput@.
--
-- Usage:
--
-- @
-- delegate <- newGCRelativeInput defaultGCRelativeInputOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameController.Delegate.GCRelativeInput
  ( GCRelativeInputOverrides(..)
  , defaultGCRelativeInputOverrides
  , newGCRelativeInput
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

-- | Overrides record for @\@protocol GCRelativeInput@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GCRelativeInputOverrides = GCRelativeInputOverrides
  { _deltaDidChangeHandler :: !(Maybe (IO RawId))
  , _setDeltaDidChangeHandler :: !(Maybe (RawId -> IO ()))
  , _delta :: !(Maybe (IO Float))
  , _analog :: !(Maybe (IO Bool))
  , _lastDeltaTimestamp :: !(Maybe (IO Double))
  , _lastDeltaLatency :: !(Maybe (IO Double))
  , _sources :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultGCRelativeInputOverrides :: GCRelativeInputOverrides
defaultGCRelativeInputOverrides = GCRelativeInputOverrides
  { _deltaDidChangeHandler = Nothing
  , _setDeltaDidChangeHandler = Nothing
  , _delta = Nothing
  , _analog = Nothing
  , _lastDeltaTimestamp = Nothing
  , _lastDeltaLatency = Nothing
  , _sources = Nothing
  }

foreign import ccall "wrapper"
  wrap_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_f
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CFloat)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CFloat))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE gcRelativeInputDelegateClass #-}
gcRelativeInputDelegateClass :: Class
gcRelativeInputDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGCRelativeInput" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_deltaDidChangeHandler = unSelector (mkSelector "deltaDidChangeHandler")
      sel_setDeltaDidChangeHandler = unSelector (mkSelector "setDeltaDidChangeHandler:")
      sel_delta = unSelector (mkSelector "delta")
      sel_analog = unSelector (mkSelector "analog")
      sel_lastDeltaTimestamp = unSelector (mkSelector "lastDeltaTimestamp")
      sel_lastDeltaLatency = unSelector (mkSelector "lastDeltaLatency")
      sel_sources = unSelector (mkSelector "sources")
  -- deltaDidChangeHandler
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCRelativeInputOverrides
    case _deltaDidChangeHandler rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "deltaDidChangeHandler" "@@:" stub_0

  -- setDeltaDidChangeHandler:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCRelativeInputOverrides
    case _setDeltaDidChangeHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setDeltaDidChangeHandler:" "v@:@" stub_1

  -- delta
  stub_2 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCRelativeInputOverrides
    case _delta rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "delta" "f@:" stub_2

  -- analog
  stub_3 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCRelativeInputOverrides
    case _analog rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "analog" "B@:" stub_3

  -- lastDeltaTimestamp
  stub_4 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCRelativeInputOverrides
    case _lastDeltaTimestamp rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "lastDeltaTimestamp" "d@:" stub_4

  -- lastDeltaLatency
  stub_5 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCRelativeInputOverrides
    case _lastDeltaLatency rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "lastDeltaLatency" "d@:" stub_5

  -- sources
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCRelativeInputOverrides
    case _sources rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "sources" "@@:" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCRelativeInputOverrides
    if queriedSel == sel_deltaDidChangeHandler then pure (maybe 0 (const 1) (_deltaDidChangeHandler rec_))
    else if queriedSel == sel_setDeltaDidChangeHandler then pure (maybe 0 (const 1) (_setDeltaDidChangeHandler rec_))
    else if queriedSel == sel_delta then pure (maybe 0 (const 1) (_delta rec_))
    else if queriedSel == sel_analog then pure (maybe 0 (const 1) (_analog rec_))
    else if queriedSel == sel_lastDeltaTimestamp then pure (maybe 0 (const 1) (_lastDeltaTimestamp rec_))
    else if queriedSel == sel_lastDeltaLatency then pure (maybe 0 (const 1) (_lastDeltaLatency rec_))
    else if queriedSel == sel_sources then pure (maybe 0 (const 1) (_sources rec_))
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
newGCRelativeInput :: GCRelativeInputOverrides -> IO RawId
newGCRelativeInput overrides = do
  inst <- class_createInstance gcRelativeInputDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
