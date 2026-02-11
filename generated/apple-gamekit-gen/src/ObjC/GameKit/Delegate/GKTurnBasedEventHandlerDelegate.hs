{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GKTurnBasedEventHandlerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newGKTurnBasedEventHandlerDelegate defaultGKTurnBasedEventHandlerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameKit.Delegate.GKTurnBasedEventHandlerDelegate
  ( GKTurnBasedEventHandlerDelegateOverrides(..)
  , defaultGKTurnBasedEventHandlerDelegateOverrides
  , newGKTurnBasedEventHandlerDelegate
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

-- | Overrides record for @\@protocol GKTurnBasedEventHandlerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GKTurnBasedEventHandlerDelegateOverrides = GKTurnBasedEventHandlerDelegateOverrides
  { _handleInviteFromGameCenter :: !(Maybe (RawId -> IO ()))
  , _handleTurnEventForMatch_didBecomeActive :: !(Maybe (RawId -> Bool -> IO ()))
  , _handleTurnEventForMatch :: !(Maybe (RawId -> IO ()))
  , _handleMatchEnded :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultGKTurnBasedEventHandlerDelegateOverrides :: GKTurnBasedEventHandlerDelegateOverrides
defaultGKTurnBasedEventHandlerDelegateOverrides = GKTurnBasedEventHandlerDelegateOverrides
  { _handleInviteFromGameCenter = Nothing
  , _handleTurnEventForMatch_didBecomeActive = Nothing
  , _handleTurnEventForMatch = Nothing
  , _handleMatchEnded = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE gkTurnBasedEventHandlerDelegateDelegateClass #-}
gkTurnBasedEventHandlerDelegateDelegateClass :: Class
gkTurnBasedEventHandlerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGKTurnBasedEventHandlerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_handleInviteFromGameCenter = unSelector (mkSelector "handleInviteFromGameCenter:")
      sel_handleTurnEventForMatch_didBecomeActive = unSelector (mkSelector "handleTurnEventForMatch:didBecomeActive:")
      sel_handleTurnEventForMatch = unSelector (mkSelector "handleTurnEventForMatch:")
      sel_handleMatchEnded = unSelector (mkSelector "handleMatchEnded:")
  -- handleInviteFromGameCenter:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedEventHandlerDelegateOverrides
    case _handleInviteFromGameCenter rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "handleInviteFromGameCenter:" "v@:@" stub_0

  -- handleTurnEventForMatch:didBecomeActive:
  stub_1 <- wrap_at_B_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedEventHandlerDelegateOverrides
    case _handleTurnEventForMatch_didBecomeActive rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (arg1 /= 0)
  addObjCMethod cls "handleTurnEventForMatch:didBecomeActive:" "v@:@B" stub_1

  -- handleTurnEventForMatch:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedEventHandlerDelegateOverrides
    case _handleTurnEventForMatch rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "handleTurnEventForMatch:" "v@:@" stub_2

  -- handleMatchEnded:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedEventHandlerDelegateOverrides
    case _handleMatchEnded rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "handleMatchEnded:" "v@:@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKTurnBasedEventHandlerDelegateOverrides
    if queriedSel == sel_handleInviteFromGameCenter then pure (maybe 0 (const 1) (_handleInviteFromGameCenter rec_))
    else if queriedSel == sel_handleTurnEventForMatch_didBecomeActive then pure (maybe 0 (const 1) (_handleTurnEventForMatch_didBecomeActive rec_))
    else if queriedSel == sel_handleTurnEventForMatch then pure (maybe 0 (const 1) (_handleTurnEventForMatch rec_))
    else if queriedSel == sel_handleMatchEnded then pure (maybe 0 (const 1) (_handleMatchEnded rec_))
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
newGKTurnBasedEventHandlerDelegate :: GKTurnBasedEventHandlerDelegateOverrides -> IO RawId
newGKTurnBasedEventHandlerDelegate overrides = do
  inst <- class_createInstance gkTurnBasedEventHandlerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
