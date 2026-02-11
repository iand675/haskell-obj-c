{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GKMatchDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newGKMatchDelegate defaultGKMatchDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameKit.Delegate.GKMatchDelegate
  ( GKMatchDelegateOverrides(..)
  , defaultGKMatchDelegateOverrides
  , newGKMatchDelegate
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

-- | Overrides record for @\@protocol GKMatchDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GKMatchDelegateOverrides = GKMatchDelegateOverrides
  { _match_didReceiveData_fromRemotePlayer :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _match_didReceiveData_forRecipient_fromRemotePlayer :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _match_didFailWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _match_shouldReinviteDisconnectedPlayer :: !(Maybe (RawId -> RawId -> IO Bool))
  , _match_didReceiveData_fromPlayer :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _match_shouldReinvitePlayer :: !(Maybe (RawId -> RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultGKMatchDelegateOverrides :: GKMatchDelegateOverrides
defaultGKMatchDelegateOverrides = GKMatchDelegateOverrides
  { _match_didReceiveData_fromRemotePlayer = Nothing
  , _match_didReceiveData_forRecipient_fromRemotePlayer = Nothing
  , _match_didFailWithError = Nothing
  , _match_shouldReinviteDisconnectedPlayer = Nothing
  , _match_didReceiveData_fromPlayer = Nothing
  , _match_shouldReinvitePlayer = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE gkMatchDelegateDelegateClass #-}
gkMatchDelegateDelegateClass :: Class
gkMatchDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGKMatchDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_match_didReceiveData_fromRemotePlayer = unSelector (mkSelector "match:didReceiveData:fromRemotePlayer:")
      sel_match_didReceiveData_forRecipient_fromRemotePlayer = unSelector (mkSelector "match:didReceiveData:forRecipient:fromRemotePlayer:")
      sel_match_didFailWithError = unSelector (mkSelector "match:didFailWithError:")
      sel_match_shouldReinviteDisconnectedPlayer = unSelector (mkSelector "match:shouldReinviteDisconnectedPlayer:")
      sel_match_didReceiveData_fromPlayer = unSelector (mkSelector "match:didReceiveData:fromPlayer:")
      sel_match_shouldReinvitePlayer = unSelector (mkSelector "match:shouldReinvitePlayer:")
  -- match:didReceiveData:fromRemotePlayer:
  stub_0 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKMatchDelegateOverrides
    case _match_didReceiveData_fromRemotePlayer rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "match:didReceiveData:fromRemotePlayer:" "v@:@@@" stub_0

  -- match:didReceiveData:forRecipient:fromRemotePlayer:
  stub_1 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKMatchDelegateOverrides
    case _match_didReceiveData_forRecipient_fromRemotePlayer rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "match:didReceiveData:forRecipient:fromRemotePlayer:" "v@:@@@@" stub_1

  -- match:didFailWithError:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKMatchDelegateOverrides
    case _match_didFailWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "match:didFailWithError:" "v@:@@" stub_2

  -- match:shouldReinviteDisconnectedPlayer:
  stub_3 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKMatchDelegateOverrides
    case _match_shouldReinviteDisconnectedPlayer rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "match:shouldReinviteDisconnectedPlayer:" "B@:@@" stub_3

  -- match:didReceiveData:fromPlayer:
  stub_4 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKMatchDelegateOverrides
    case _match_didReceiveData_fromPlayer rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "match:didReceiveData:fromPlayer:" "v@:@@@" stub_4

  -- match:shouldReinvitePlayer:
  stub_5 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKMatchDelegateOverrides
    case _match_shouldReinvitePlayer rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "match:shouldReinvitePlayer:" "B@:@@" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKMatchDelegateOverrides
    if queriedSel == sel_match_didReceiveData_fromRemotePlayer then pure (maybe 0 (const 1) (_match_didReceiveData_fromRemotePlayer rec_))
    else if queriedSel == sel_match_didReceiveData_forRecipient_fromRemotePlayer then pure (maybe 0 (const 1) (_match_didReceiveData_forRecipient_fromRemotePlayer rec_))
    else if queriedSel == sel_match_didFailWithError then pure (maybe 0 (const 1) (_match_didFailWithError rec_))
    else if queriedSel == sel_match_shouldReinviteDisconnectedPlayer then pure (maybe 0 (const 1) (_match_shouldReinviteDisconnectedPlayer rec_))
    else if queriedSel == sel_match_didReceiveData_fromPlayer then pure (maybe 0 (const 1) (_match_didReceiveData_fromPlayer rec_))
    else if queriedSel == sel_match_shouldReinvitePlayer then pure (maybe 0 (const 1) (_match_shouldReinvitePlayer rec_))
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
newGKMatchDelegate :: GKMatchDelegateOverrides -> IO RawId
newGKMatchDelegate overrides = do
  inst <- class_createInstance gkMatchDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
