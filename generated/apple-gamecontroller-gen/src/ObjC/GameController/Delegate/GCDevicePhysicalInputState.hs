{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GCDevicePhysicalInputState@.
--
-- Usage:
--
-- @
-- delegate <- newGCDevicePhysicalInputState defaultGCDevicePhysicalInputStateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameController.Delegate.GCDevicePhysicalInputState
  ( GCDevicePhysicalInputStateOverrides(..)
  , defaultGCDevicePhysicalInputStateOverrides
  , newGCDevicePhysicalInputState
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

-- | Overrides record for @\@protocol GCDevicePhysicalInputState@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GCDevicePhysicalInputStateOverrides = GCDevicePhysicalInputStateOverrides
  { _objectForKeyedSubscript :: !(Maybe (RawId -> IO RawId))
  , _device :: !(Maybe (IO RawId))
  , _lastEventTimestamp :: !(Maybe (IO Double))
  , _lastEventLatency :: !(Maybe (IO Double))
  , _elements :: !(Maybe (IO RawId))
  , _buttons :: !(Maybe (IO RawId))
  , _axes :: !(Maybe (IO RawId))
  , _switches :: !(Maybe (IO RawId))
  , _dpads :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultGCDevicePhysicalInputStateOverrides :: GCDevicePhysicalInputStateOverrides
defaultGCDevicePhysicalInputStateOverrides = GCDevicePhysicalInputStateOverrides
  { _objectForKeyedSubscript = Nothing
  , _device = Nothing
  , _lastEventTimestamp = Nothing
  , _lastEventLatency = Nothing
  , _elements = Nothing
  , _buttons = Nothing
  , _axes = Nothing
  , _switches = Nothing
  , _dpads = Nothing
  }

foreign import ccall "wrapper"
  wrap_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE gcDevicePhysicalInputStateDelegateClass #-}
gcDevicePhysicalInputStateDelegateClass :: Class
gcDevicePhysicalInputStateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGCDevicePhysicalInputState" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_objectForKeyedSubscript = unSelector (mkSelector "objectForKeyedSubscript:")
      sel_device = unSelector (mkSelector "device")
      sel_lastEventTimestamp = unSelector (mkSelector "lastEventTimestamp")
      sel_lastEventLatency = unSelector (mkSelector "lastEventLatency")
      sel_elements = unSelector (mkSelector "elements")
      sel_buttons = unSelector (mkSelector "buttons")
      sel_axes = unSelector (mkSelector "axes")
      sel_switches = unSelector (mkSelector "switches")
      sel_dpads = unSelector (mkSelector "dpads")
  -- objectForKeyedSubscript:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCDevicePhysicalInputStateOverrides
    case _objectForKeyedSubscript rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "objectForKeyedSubscript:" "@@:@" stub_0

  -- device
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCDevicePhysicalInputStateOverrides
    case _device rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "device" "@@:" stub_1

  -- lastEventTimestamp
  stub_2 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCDevicePhysicalInputStateOverrides
    case _lastEventTimestamp rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "lastEventTimestamp" "d@:" stub_2

  -- lastEventLatency
  stub_3 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCDevicePhysicalInputStateOverrides
    case _lastEventLatency rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "lastEventLatency" "d@:" stub_3

  -- elements
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCDevicePhysicalInputStateOverrides
    case _elements rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "elements" "@@:" stub_4

  -- buttons
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCDevicePhysicalInputStateOverrides
    case _buttons rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "buttons" "@@:" stub_5

  -- axes
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCDevicePhysicalInputStateOverrides
    case _axes rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "axes" "@@:" stub_6

  -- switches
  stub_7 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCDevicePhysicalInputStateOverrides
    case _switches rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "switches" "@@:" stub_7

  -- dpads
  stub_8 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCDevicePhysicalInputStateOverrides
    case _dpads rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "dpads" "@@:" stub_8

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCDevicePhysicalInputStateOverrides
    if queriedSel == sel_objectForKeyedSubscript then pure (maybe 0 (const 1) (_objectForKeyedSubscript rec_))
    else if queriedSel == sel_device then pure (maybe 0 (const 1) (_device rec_))
    else if queriedSel == sel_lastEventTimestamp then pure (maybe 0 (const 1) (_lastEventTimestamp rec_))
    else if queriedSel == sel_lastEventLatency then pure (maybe 0 (const 1) (_lastEventLatency rec_))
    else if queriedSel == sel_elements then pure (maybe 0 (const 1) (_elements rec_))
    else if queriedSel == sel_buttons then pure (maybe 0 (const 1) (_buttons rec_))
    else if queriedSel == sel_axes then pure (maybe 0 (const 1) (_axes rec_))
    else if queriedSel == sel_switches then pure (maybe 0 (const 1) (_switches rec_))
    else if queriedSel == sel_dpads then pure (maybe 0 (const 1) (_dpads rec_))
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
newGCDevicePhysicalInputState :: GCDevicePhysicalInputStateOverrides -> IO RawId
newGCDevicePhysicalInputState overrides = do
  inst <- class_createInstance gcDevicePhysicalInputStateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
