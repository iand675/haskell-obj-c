{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SWCollaborationViewDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newSWCollaborationViewDelegate defaultSWCollaborationViewDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SharedWithYou.Delegate.SWCollaborationViewDelegate
  ( SWCollaborationViewDelegateOverrides(..)
  , defaultSWCollaborationViewDelegateOverrides
  , newSWCollaborationViewDelegate
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

-- | Overrides record for @\@protocol SWCollaborationViewDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SWCollaborationViewDelegateOverrides = SWCollaborationViewDelegateOverrides
  { _collaborationViewShouldPresentPopover :: !(Maybe (RawId -> IO Bool))
  , _collaborationViewWillPresentPopover :: !(Maybe (RawId -> IO ()))
  , _collaborationViewDidDismissPopover :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSWCollaborationViewDelegateOverrides :: SWCollaborationViewDelegateOverrides
defaultSWCollaborationViewDelegateOverrides = SWCollaborationViewDelegateOverrides
  { _collaborationViewShouldPresentPopover = Nothing
  , _collaborationViewWillPresentPopover = Nothing
  , _collaborationViewDidDismissPopover = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE swCollaborationViewDelegateDelegateClass #-}
swCollaborationViewDelegateDelegateClass :: Class
swCollaborationViewDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSWCollaborationViewDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_collaborationViewShouldPresentPopover = unSelector (mkSelector "collaborationViewShouldPresentPopover:")
      sel_collaborationViewWillPresentPopover = unSelector (mkSelector "collaborationViewWillPresentPopover:")
      sel_collaborationViewDidDismissPopover = unSelector (mkSelector "collaborationViewDidDismissPopover:")
  -- collaborationViewShouldPresentPopover:
  stub_0 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SWCollaborationViewDelegateOverrides
    case _collaborationViewShouldPresentPopover rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "collaborationViewShouldPresentPopover:" "B@:@" stub_0

  -- collaborationViewWillPresentPopover:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SWCollaborationViewDelegateOverrides
    case _collaborationViewWillPresentPopover rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "collaborationViewWillPresentPopover:" "v@:@" stub_1

  -- collaborationViewDidDismissPopover:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SWCollaborationViewDelegateOverrides
    case _collaborationViewDidDismissPopover rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "collaborationViewDidDismissPopover:" "v@:@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SWCollaborationViewDelegateOverrides
    if queriedSel == sel_collaborationViewShouldPresentPopover then pure (maybe 0 (const 1) (_collaborationViewShouldPresentPopover rec_))
    else if queriedSel == sel_collaborationViewWillPresentPopover then pure (maybe 0 (const 1) (_collaborationViewWillPresentPopover rec_))
    else if queriedSel == sel_collaborationViewDidDismissPopover then pure (maybe 0 (const 1) (_collaborationViewDidDismissPopover rec_))
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
newSWCollaborationViewDelegate :: SWCollaborationViewDelegateOverrides -> IO RawId
newSWCollaborationViewDelegate overrides = do
  inst <- class_createInstance swCollaborationViewDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
