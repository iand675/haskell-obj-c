{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol IMKStateSetting@.
--
-- Usage:
--
-- @
-- delegate <- newIMKStateSetting defaultIMKStateSettingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.InputMethodKit.Delegate.IMKStateSetting
  ( IMKStateSettingOverrides(..)
  , defaultIMKStateSettingOverrides
  , newIMKStateSetting
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

-- | Overrides record for @\@protocol IMKStateSetting@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data IMKStateSettingOverrides = IMKStateSettingOverrides
  { _activateServer :: !(Maybe (RawId -> IO ()))
  , _deactivateServer :: !(Maybe (RawId -> IO ()))
  , _valueForTag_client :: !(Maybe (Int -> RawId -> IO RawId))
  , _setValue_forTag_client :: !(Maybe (RawId -> Int -> RawId -> IO ()))
  , _modes :: !(Maybe (RawId -> IO RawId))
  , _recognizedEvents :: !(Maybe (RawId -> IO Int))
  , _showPreferences :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultIMKStateSettingOverrides :: IMKStateSettingOverrides
defaultIMKStateSettingOverrides = IMKStateSettingOverrides
  { _activateServer = Nothing
  , _deactivateServer = Nothing
  , _valueForTag_client = Nothing
  , _setValue_forTag_client = Nothing
  , _modes = Nothing
  , _recognizedEvents = Nothing
  , _showPreferences = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_q_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_q_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE imkStateSettingDelegateClass #-}
imkStateSettingDelegateClass :: Class
imkStateSettingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsIMKStateSetting" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_activateServer = unSelector (mkSelector "activateServer:")
      sel_deactivateServer = unSelector (mkSelector "deactivateServer:")
      sel_valueForTag_client = unSelector (mkSelector "valueForTag:client:")
      sel_setValue_forTag_client = unSelector (mkSelector "setValue:forTag:client:")
      sel_modes = unSelector (mkSelector "modes:")
      sel_recognizedEvents = unSelector (mkSelector "recognizedEvents:")
      sel_showPreferences = unSelector (mkSelector "showPreferences:")
  -- activateServer:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKStateSettingOverrides
    case _activateServer rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "activateServer:" "v@:@" stub_0

  -- deactivateServer:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKStateSettingOverrides
    case _deactivateServer rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "deactivateServer:" "v@:@" stub_1

  -- valueForTag:client:
  stub_2 <- wrap_q_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKStateSettingOverrides
    case _valueForTag_client rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (fromIntegral arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "valueForTag:client:" "@@:q@" stub_2

  -- setValue:forTag:client:
  stub_3 <- wrap_at_q_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKStateSettingOverrides
    case _setValue_forTag_client rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (RawId arg2)
  addObjCMethod cls "setValue:forTag:client:" "v@:@q@" stub_3

  -- modes:
  stub_4 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKStateSettingOverrides
    case _modes rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "modes:" "@@:@" stub_4

  -- recognizedEvents:
  stub_5 <- wrap_at_Q $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKStateSettingOverrides
    case _recognizedEvents rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (fromIntegral r)
  addObjCMethod cls "recognizedEvents:" "Q@:@" stub_5

  -- showPreferences:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKStateSettingOverrides
    case _showPreferences rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "showPreferences:" "v@:@" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKStateSettingOverrides
    if queriedSel == sel_activateServer then pure (maybe 0 (const 1) (_activateServer rec_))
    else if queriedSel == sel_deactivateServer then pure (maybe 0 (const 1) (_deactivateServer rec_))
    else if queriedSel == sel_valueForTag_client then pure (maybe 0 (const 1) (_valueForTag_client rec_))
    else if queriedSel == sel_setValue_forTag_client then pure (maybe 0 (const 1) (_setValue_forTag_client rec_))
    else if queriedSel == sel_modes then pure (maybe 0 (const 1) (_modes rec_))
    else if queriedSel == sel_recognizedEvents then pure (maybe 0 (const 1) (_recognizedEvents rec_))
    else if queriedSel == sel_showPreferences then pure (maybe 0 (const 1) (_showPreferences rec_))
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
newIMKStateSetting :: IMKStateSettingOverrides -> IO RawId
newIMKStateSetting overrides = do
  inst <- class_createInstance imkStateSettingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
