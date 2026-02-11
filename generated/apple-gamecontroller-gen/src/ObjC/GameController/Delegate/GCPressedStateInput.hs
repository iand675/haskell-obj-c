{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GCPressedStateInput@.
--
-- Usage:
--
-- @
-- delegate <- newGCPressedStateInput defaultGCPressedStateInputOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameController.Delegate.GCPressedStateInput
  ( GCPressedStateInputOverrides(..)
  , defaultGCPressedStateInputOverrides
  , newGCPressedStateInput
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

-- | Overrides record for @\@protocol GCPressedStateInput@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GCPressedStateInputOverrides = GCPressedStateInputOverrides
  { _pressedDidChangeHandler :: !(Maybe (IO RawId))
  , _setPressedDidChangeHandler :: !(Maybe (RawId -> IO ()))
  , _pressed :: !(Maybe (IO Bool))
  , _lastPressedStateTimestamp :: !(Maybe (IO Double))
  , _lastPressedStateLatency :: !(Maybe (IO Double))
  , _sources :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultGCPressedStateInputOverrides :: GCPressedStateInputOverrides
defaultGCPressedStateInputOverrides = GCPressedStateInputOverrides
  { _pressedDidChangeHandler = Nothing
  , _setPressedDidChangeHandler = Nothing
  , _pressed = Nothing
  , _lastPressedStateTimestamp = Nothing
  , _lastPressedStateLatency = Nothing
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
{-# NOINLINE gcPressedStateInputDelegateClass #-}
gcPressedStateInputDelegateClass :: Class
gcPressedStateInputDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGCPressedStateInput" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_pressedDidChangeHandler = unSelector (mkSelector "pressedDidChangeHandler")
      sel_setPressedDidChangeHandler = unSelector (mkSelector "setPressedDidChangeHandler:")
      sel_pressed = unSelector (mkSelector "pressed")
      sel_lastPressedStateTimestamp = unSelector (mkSelector "lastPressedStateTimestamp")
      sel_lastPressedStateLatency = unSelector (mkSelector "lastPressedStateLatency")
      sel_sources = unSelector (mkSelector "sources")
  -- pressedDidChangeHandler
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCPressedStateInputOverrides
    case _pressedDidChangeHandler rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "pressedDidChangeHandler" "@@:" stub_0

  -- setPressedDidChangeHandler:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCPressedStateInputOverrides
    case _setPressedDidChangeHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setPressedDidChangeHandler:" "v@:@" stub_1

  -- pressed
  stub_2 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCPressedStateInputOverrides
    case _pressed rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "pressed" "B@:" stub_2

  -- lastPressedStateTimestamp
  stub_3 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCPressedStateInputOverrides
    case _lastPressedStateTimestamp rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "lastPressedStateTimestamp" "d@:" stub_3

  -- lastPressedStateLatency
  stub_4 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCPressedStateInputOverrides
    case _lastPressedStateLatency rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "lastPressedStateLatency" "d@:" stub_4

  -- sources
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCPressedStateInputOverrides
    case _sources rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "sources" "@@:" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCPressedStateInputOverrides
    if queriedSel == sel_pressedDidChangeHandler then pure (maybe 0 (const 1) (_pressedDidChangeHandler rec_))
    else if queriedSel == sel_setPressedDidChangeHandler then pure (maybe 0 (const 1) (_setPressedDidChangeHandler rec_))
    else if queriedSel == sel_pressed then pure (maybe 0 (const 1) (_pressed rec_))
    else if queriedSel == sel_lastPressedStateTimestamp then pure (maybe 0 (const 1) (_lastPressedStateTimestamp rec_))
    else if queriedSel == sel_lastPressedStateLatency then pure (maybe 0 (const 1) (_lastPressedStateLatency rec_))
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
newGCPressedStateInput :: GCPressedStateInputOverrides -> IO RawId
newGCPressedStateInput overrides = do
  inst <- class_createInstance gcPressedStateInputDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
