{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol QLPreviewPanelDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newQLPreviewPanelDelegate defaultQLPreviewPanelDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.QuickLookUI.Delegate.QLPreviewPanelDelegate
  ( QLPreviewPanelDelegateOverrides(..)
  , defaultQLPreviewPanelDelegateOverrides
  , newQLPreviewPanelDelegate
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

-- | Overrides record for @\@protocol QLPreviewPanelDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data QLPreviewPanelDelegateOverrides = QLPreviewPanelDelegateOverrides
  { _previewPanel_handleEvent :: !(Maybe (RawId -> RawId -> IO Bool))
  , _previewPanel_transitionImageForPreviewItem_contentRect :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultQLPreviewPanelDelegateOverrides :: QLPreviewPanelDelegateOverrides
defaultQLPreviewPanelDelegateOverrides = QLPreviewPanelDelegateOverrides
  { _previewPanel_handleEvent = Nothing
  , _previewPanel_transitionImageForPreviewItem_contentRect = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE qlPreviewPanelDelegateDelegateClass #-}
qlPreviewPanelDelegateDelegateClass :: Class
qlPreviewPanelDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsQLPreviewPanelDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_previewPanel_handleEvent = unSelector (mkSelector "previewPanel:handleEvent:")
      sel_previewPanel_transitionImageForPreviewItem_contentRect = unSelector (mkSelector "previewPanel:transitionImageForPreviewItem:contentRect:")
  -- previewPanel:handleEvent:
  stub_0 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QLPreviewPanelDelegateOverrides
    case _previewPanel_handleEvent rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "previewPanel:handleEvent:" "B@:@@" stub_0

  -- previewPanel:transitionImageForPreviewItem:contentRect:
  stub_1 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QLPreviewPanelDelegateOverrides
    case _previewPanel_transitionImageForPreviewItem_contentRect rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "previewPanel:transitionImageForPreviewItem:contentRect:" "@@:@@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QLPreviewPanelDelegateOverrides
    if queriedSel == sel_previewPanel_handleEvent then pure (maybe 0 (const 1) (_previewPanel_handleEvent rec_))
    else if queriedSel == sel_previewPanel_transitionImageForPreviewItem_contentRect then pure (maybe 0 (const 1) (_previewPanel_transitionImageForPreviewItem_contentRect rec_))
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
newQLPreviewPanelDelegate :: QLPreviewPanelDelegateOverrides -> IO RawId
newQLPreviewPanelDelegate overrides = do
  inst <- class_createInstance qlPreviewPanelDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
