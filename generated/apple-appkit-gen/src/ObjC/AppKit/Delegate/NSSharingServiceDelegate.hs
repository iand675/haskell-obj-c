{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSSharingServiceDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSSharingServiceDelegate defaultNSSharingServiceDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSSharingServiceDelegate
  ( NSSharingServiceDelegateOverrides(..)
  , defaultNSSharingServiceDelegateOverrides
  , newNSSharingServiceDelegate
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

-- | Overrides record for @\@protocol NSSharingServiceDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSSharingServiceDelegateOverrides = NSSharingServiceDelegateOverrides
  { _sharingService_willShareItems :: !(Maybe (RawId -> RawId -> IO ()))
  , _sharingService_didFailToShareItems_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _sharingService_didShareItems :: !(Maybe (RawId -> RawId -> IO ()))
  , _sharingService_transitionImageForShareItem_contentRect :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _sharingService_sourceWindowForShareItems_sharingContentScope :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _anchoringViewForSharingService_showRelativeToRect_preferredEdge :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSSharingServiceDelegateOverrides :: NSSharingServiceDelegateOverrides
defaultNSSharingServiceDelegateOverrides = NSSharingServiceDelegateOverrides
  { _sharingService_willShareItems = Nothing
  , _sharingService_didFailToShareItems_error = Nothing
  , _sharingService_didShareItems = Nothing
  , _sharingService_transitionImageForShareItem_contentRect = Nothing
  , _sharingService_sourceWindowForShareItems_sharingContentScope = Nothing
  , _anchoringViewForSharingService_showRelativeToRect_preferredEdge = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsSharingServiceDelegateDelegateClass #-}
nsSharingServiceDelegateDelegateClass :: Class
nsSharingServiceDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSSharingServiceDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_sharingService_willShareItems = unSelector (mkSelector "sharingService:willShareItems:")
      sel_sharingService_didFailToShareItems_error = unSelector (mkSelector "sharingService:didFailToShareItems:error:")
      sel_sharingService_didShareItems = unSelector (mkSelector "sharingService:didShareItems:")
      sel_sharingService_transitionImageForShareItem_contentRect = unSelector (mkSelector "sharingService:transitionImageForShareItem:contentRect:")
      sel_sharingService_sourceWindowForShareItems_sharingContentScope = unSelector (mkSelector "sharingService:sourceWindowForShareItems:sharingContentScope:")
      sel_anchoringViewForSharingService_showRelativeToRect_preferredEdge = unSelector (mkSelector "anchoringViewForSharingService:showRelativeToRect:preferredEdge:")
  -- sharingService:willShareItems:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSharingServiceDelegateOverrides
    case _sharingService_willShareItems rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "sharingService:willShareItems:" "v@:@@" stub_0

  -- sharingService:didFailToShareItems:error:
  stub_1 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSharingServiceDelegateOverrides
    case _sharingService_didFailToShareItems_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "sharingService:didFailToShareItems:error:" "v@:@@@" stub_1

  -- sharingService:didShareItems:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSharingServiceDelegateOverrides
    case _sharingService_didShareItems rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "sharingService:didShareItems:" "v@:@@" stub_2

  -- sharingService:transitionImageForShareItem:contentRect:
  stub_3 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSharingServiceDelegateOverrides
    case _sharingService_transitionImageForShareItem_contentRect rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "sharingService:transitionImageForShareItem:contentRect:" "@@:@@@" stub_3

  -- sharingService:sourceWindowForShareItems:sharingContentScope:
  stub_4 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSharingServiceDelegateOverrides
    case _sharingService_sourceWindowForShareItems_sharingContentScope rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "sharingService:sourceWindowForShareItems:sharingContentScope:" "@@:@@@" stub_4

  -- anchoringViewForSharingService:showRelativeToRect:preferredEdge:
  stub_5 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSharingServiceDelegateOverrides
    case _anchoringViewForSharingService_showRelativeToRect_preferredEdge rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "anchoringViewForSharingService:showRelativeToRect:preferredEdge:" "@@:@@@" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSharingServiceDelegateOverrides
    if queriedSel == sel_sharingService_willShareItems then pure (maybe 0 (const 1) (_sharingService_willShareItems rec_))
    else if queriedSel == sel_sharingService_didFailToShareItems_error then pure (maybe 0 (const 1) (_sharingService_didFailToShareItems_error rec_))
    else if queriedSel == sel_sharingService_didShareItems then pure (maybe 0 (const 1) (_sharingService_didShareItems rec_))
    else if queriedSel == sel_sharingService_transitionImageForShareItem_contentRect then pure (maybe 0 (const 1) (_sharingService_transitionImageForShareItem_contentRect rec_))
    else if queriedSel == sel_sharingService_sourceWindowForShareItems_sharingContentScope then pure (maybe 0 (const 1) (_sharingService_sourceWindowForShareItems_sharingContentScope rec_))
    else if queriedSel == sel_anchoringViewForSharingService_showRelativeToRect_preferredEdge then pure (maybe 0 (const 1) (_anchoringViewForSharingService_showRelativeToRect_preferredEdge rec_))
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
newNSSharingServiceDelegate :: NSSharingServiceDelegateOverrides -> IO RawId
newNSSharingServiceDelegate overrides = do
  inst <- class_createInstance nsSharingServiceDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
