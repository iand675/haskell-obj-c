{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GCSwitchPositionInput@.
--
-- Usage:
--
-- @
-- delegate <- newGCSwitchPositionInput defaultGCSwitchPositionInputOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameController.Delegate.GCSwitchPositionInput
  ( GCSwitchPositionInputOverrides(..)
  , defaultGCSwitchPositionInputOverrides
  , newGCSwitchPositionInput
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

-- | Overrides record for @\@protocol GCSwitchPositionInput@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GCSwitchPositionInputOverrides = GCSwitchPositionInputOverrides
  { _positionDidChangeHandler :: !(Maybe (IO RawId))
  , _setPositionDidChangeHandler :: !(Maybe (RawId -> IO ()))
  , _position :: !(Maybe (IO Int))
  , _sequential :: !(Maybe (IO Bool))
  , _canWrap :: !(Maybe (IO Bool))
  , _lastPositionTimestamp :: !(Maybe (IO Double))
  , _lastPositionLatency :: !(Maybe (IO Double))
  , _sources :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultGCSwitchPositionInputOverrides :: GCSwitchPositionInputOverrides
defaultGCSwitchPositionInputOverrides = GCSwitchPositionInputOverrides
  { _positionDidChangeHandler = Nothing
  , _setPositionDidChangeHandler = Nothing
  , _position = Nothing
  , _sequential = Nothing
  , _canWrap = Nothing
  , _lastPositionTimestamp = Nothing
  , _lastPositionLatency = Nothing
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
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))

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
{-# NOINLINE gcSwitchPositionInputDelegateClass #-}
gcSwitchPositionInputDelegateClass :: Class
gcSwitchPositionInputDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGCSwitchPositionInput" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_positionDidChangeHandler = unSelector (mkSelector "positionDidChangeHandler")
      sel_setPositionDidChangeHandler = unSelector (mkSelector "setPositionDidChangeHandler:")
      sel_position = unSelector (mkSelector "position")
      sel_sequential = unSelector (mkSelector "sequential")
      sel_canWrap = unSelector (mkSelector "canWrap")
      sel_lastPositionTimestamp = unSelector (mkSelector "lastPositionTimestamp")
      sel_lastPositionLatency = unSelector (mkSelector "lastPositionLatency")
      sel_sources = unSelector (mkSelector "sources")
  -- positionDidChangeHandler
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCSwitchPositionInputOverrides
    case _positionDidChangeHandler rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "positionDidChangeHandler" "@@:" stub_0

  -- setPositionDidChangeHandler:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCSwitchPositionInputOverrides
    case _setPositionDidChangeHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setPositionDidChangeHandler:" "v@:@" stub_1

  -- position
  stub_2 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCSwitchPositionInputOverrides
    case _position rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "position" "q@:" stub_2

  -- sequential
  stub_3 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCSwitchPositionInputOverrides
    case _sequential rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "sequential" "B@:" stub_3

  -- canWrap
  stub_4 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCSwitchPositionInputOverrides
    case _canWrap rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "canWrap" "B@:" stub_4

  -- lastPositionTimestamp
  stub_5 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCSwitchPositionInputOverrides
    case _lastPositionTimestamp rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "lastPositionTimestamp" "d@:" stub_5

  -- lastPositionLatency
  stub_6 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCSwitchPositionInputOverrides
    case _lastPositionLatency rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "lastPositionLatency" "d@:" stub_6

  -- sources
  stub_7 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCSwitchPositionInputOverrides
    case _sources rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "sources" "@@:" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCSwitchPositionInputOverrides
    if queriedSel == sel_positionDidChangeHandler then pure (maybe 0 (const 1) (_positionDidChangeHandler rec_))
    else if queriedSel == sel_setPositionDidChangeHandler then pure (maybe 0 (const 1) (_setPositionDidChangeHandler rec_))
    else if queriedSel == sel_position then pure (maybe 0 (const 1) (_position rec_))
    else if queriedSel == sel_sequential then pure (maybe 0 (const 1) (_sequential rec_))
    else if queriedSel == sel_canWrap then pure (maybe 0 (const 1) (_canWrap rec_))
    else if queriedSel == sel_lastPositionTimestamp then pure (maybe 0 (const 1) (_lastPositionTimestamp rec_))
    else if queriedSel == sel_lastPositionLatency then pure (maybe 0 (const 1) (_lastPositionLatency rec_))
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
newGCSwitchPositionInput :: GCSwitchPositionInputOverrides -> IO RawId
newGCSwitchPositionInput overrides = do
  inst <- class_createInstance gcSwitchPositionInputDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
