{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GCTouchedStateInput@.
--
-- Usage:
--
-- @
-- delegate <- newGCTouchedStateInput defaultGCTouchedStateInputOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameController.Delegate.GCTouchedStateInput
  ( GCTouchedStateInputOverrides(..)
  , defaultGCTouchedStateInputOverrides
  , newGCTouchedStateInput
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

-- | Overrides record for @\@protocol GCTouchedStateInput@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GCTouchedStateInputOverrides = GCTouchedStateInputOverrides
  { _touchedDidChangeHandler :: !(Maybe (IO RawId))
  , _setTouchedDidChangeHandler :: !(Maybe (RawId -> IO ()))
  , _touched :: !(Maybe (IO Bool))
  , _lastTouchedStateTimestamp :: !(Maybe (IO Double))
  , _lastTouchedStateLatency :: !(Maybe (IO Double))
  , _sources :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultGCTouchedStateInputOverrides :: GCTouchedStateInputOverrides
defaultGCTouchedStateInputOverrides = GCTouchedStateInputOverrides
  { _touchedDidChangeHandler = Nothing
  , _setTouchedDidChangeHandler = Nothing
  , _touched = Nothing
  , _lastTouchedStateTimestamp = Nothing
  , _lastTouchedStateLatency = Nothing
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
{-# NOINLINE gcTouchedStateInputDelegateClass #-}
gcTouchedStateInputDelegateClass :: Class
gcTouchedStateInputDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGCTouchedStateInput" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_touchedDidChangeHandler = unSelector (mkSelector "touchedDidChangeHandler")
      sel_setTouchedDidChangeHandler = unSelector (mkSelector "setTouchedDidChangeHandler:")
      sel_touched = unSelector (mkSelector "touched")
      sel_lastTouchedStateTimestamp = unSelector (mkSelector "lastTouchedStateTimestamp")
      sel_lastTouchedStateLatency = unSelector (mkSelector "lastTouchedStateLatency")
      sel_sources = unSelector (mkSelector "sources")
  -- touchedDidChangeHandler
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCTouchedStateInputOverrides
    case _touchedDidChangeHandler rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "touchedDidChangeHandler" "@@:" stub_0

  -- setTouchedDidChangeHandler:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCTouchedStateInputOverrides
    case _setTouchedDidChangeHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setTouchedDidChangeHandler:" "v@:@" stub_1

  -- touched
  stub_2 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCTouchedStateInputOverrides
    case _touched rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "touched" "B@:" stub_2

  -- lastTouchedStateTimestamp
  stub_3 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCTouchedStateInputOverrides
    case _lastTouchedStateTimestamp rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "lastTouchedStateTimestamp" "d@:" stub_3

  -- lastTouchedStateLatency
  stub_4 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCTouchedStateInputOverrides
    case _lastTouchedStateLatency rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "lastTouchedStateLatency" "d@:" stub_4

  -- sources
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCTouchedStateInputOverrides
    case _sources rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "sources" "@@:" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCTouchedStateInputOverrides
    if queriedSel == sel_touchedDidChangeHandler then pure (maybe 0 (const 1) (_touchedDidChangeHandler rec_))
    else if queriedSel == sel_setTouchedDidChangeHandler then pure (maybe 0 (const 1) (_setTouchedDidChangeHandler rec_))
    else if queriedSel == sel_touched then pure (maybe 0 (const 1) (_touched rec_))
    else if queriedSel == sel_lastTouchedStateTimestamp then pure (maybe 0 (const 1) (_lastTouchedStateTimestamp rec_))
    else if queriedSel == sel_lastTouchedStateLatency then pure (maybe 0 (const 1) (_lastTouchedStateLatency rec_))
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
newGCTouchedStateInput :: GCTouchedStateInputOverrides -> IO RawId
newGCTouchedStateInput overrides = do
  inst <- class_createInstance gcTouchedStateInputDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
