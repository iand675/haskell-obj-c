{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GCAxisInput@.
--
-- Usage:
--
-- @
-- delegate <- newGCAxisInput defaultGCAxisInputOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameController.Delegate.GCAxisInput
  ( GCAxisInputOverrides(..)
  , defaultGCAxisInputOverrides
  , newGCAxisInput
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

-- | Overrides record for @\@protocol GCAxisInput@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GCAxisInputOverrides = GCAxisInputOverrides
  { _valueDidChangeHandler :: !(Maybe (IO RawId))
  , _setValueDidChangeHandler :: !(Maybe (RawId -> IO ()))
  , _value :: !(Maybe (IO Float))
  , _analog :: !(Maybe (IO Bool))
  , _canWrap :: !(Maybe (IO Bool))
  , _lastValueTimestamp :: !(Maybe (IO Double))
  , _lastValueLatency :: !(Maybe (IO Double))
  , _sources :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultGCAxisInputOverrides :: GCAxisInputOverrides
defaultGCAxisInputOverrides = GCAxisInputOverrides
  { _valueDidChangeHandler = Nothing
  , _setValueDidChangeHandler = Nothing
  , _value = Nothing
  , _analog = Nothing
  , _canWrap = Nothing
  , _lastValueTimestamp = Nothing
  , _lastValueLatency = Nothing
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
{-# NOINLINE gcAxisInputDelegateClass #-}
gcAxisInputDelegateClass :: Class
gcAxisInputDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGCAxisInput" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_valueDidChangeHandler = unSelector (mkSelector "valueDidChangeHandler")
      sel_setValueDidChangeHandler = unSelector (mkSelector "setValueDidChangeHandler:")
      sel_value = unSelector (mkSelector "value")
      sel_analog = unSelector (mkSelector "analog")
      sel_canWrap = unSelector (mkSelector "canWrap")
      sel_lastValueTimestamp = unSelector (mkSelector "lastValueTimestamp")
      sel_lastValueLatency = unSelector (mkSelector "lastValueLatency")
      sel_sources = unSelector (mkSelector "sources")
  -- valueDidChangeHandler
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCAxisInputOverrides
    case _valueDidChangeHandler rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "valueDidChangeHandler" "@@:" stub_0

  -- setValueDidChangeHandler:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCAxisInputOverrides
    case _setValueDidChangeHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setValueDidChangeHandler:" "v@:@" stub_1

  -- value
  stub_2 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCAxisInputOverrides
    case _value rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "value" "f@:" stub_2

  -- analog
  stub_3 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCAxisInputOverrides
    case _analog rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "analog" "B@:" stub_3

  -- canWrap
  stub_4 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCAxisInputOverrides
    case _canWrap rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "canWrap" "B@:" stub_4

  -- lastValueTimestamp
  stub_5 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCAxisInputOverrides
    case _lastValueTimestamp rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "lastValueTimestamp" "d@:" stub_5

  -- lastValueLatency
  stub_6 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCAxisInputOverrides
    case _lastValueLatency rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "lastValueLatency" "d@:" stub_6

  -- sources
  stub_7 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCAxisInputOverrides
    case _sources rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "sources" "@@:" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCAxisInputOverrides
    if queriedSel == sel_valueDidChangeHandler then pure (maybe 0 (const 1) (_valueDidChangeHandler rec_))
    else if queriedSel == sel_setValueDidChangeHandler then pure (maybe 0 (const 1) (_setValueDidChangeHandler rec_))
    else if queriedSel == sel_value then pure (maybe 0 (const 1) (_value rec_))
    else if queriedSel == sel_analog then pure (maybe 0 (const 1) (_analog rec_))
    else if queriedSel == sel_canWrap then pure (maybe 0 (const 1) (_canWrap rec_))
    else if queriedSel == sel_lastValueTimestamp then pure (maybe 0 (const 1) (_lastValueTimestamp rec_))
    else if queriedSel == sel_lastValueLatency then pure (maybe 0 (const 1) (_lastValueLatency rec_))
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
newGCAxisInput :: GCAxisInputOverrides -> IO RawId
newGCAxisInput overrides = do
  inst <- class_createInstance gcAxisInputDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
