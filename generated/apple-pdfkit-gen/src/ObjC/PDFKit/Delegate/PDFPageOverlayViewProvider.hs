{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol PDFPageOverlayViewProvider@.
--
-- Usage:
--
-- @
-- delegate <- newPDFPageOverlayViewProvider defaultPDFPageOverlayViewProviderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.PDFKit.Delegate.PDFPageOverlayViewProvider
  ( PDFPageOverlayViewProviderOverrides(..)
  , defaultPDFPageOverlayViewProviderOverrides
  , newPDFPageOverlayViewProvider
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

-- | Overrides record for @\@protocol PDFPageOverlayViewProvider@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data PDFPageOverlayViewProviderOverrides = PDFPageOverlayViewProviderOverrides
  { _pdfView_overlayViewForPage :: !(Maybe (RawId -> RawId -> IO RawId))
  , _pdfView_willDisplayOverlayView_forPage :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _pdfView_willEndDisplayingOverlayView_forPage :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultPDFPageOverlayViewProviderOverrides :: PDFPageOverlayViewProviderOverrides
defaultPDFPageOverlayViewProviderOverrides = PDFPageOverlayViewProviderOverrides
  { _pdfView_overlayViewForPage = Nothing
  , _pdfView_willDisplayOverlayView_forPage = Nothing
  , _pdfView_willEndDisplayingOverlayView_forPage = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE pdfPageOverlayViewProviderDelegateClass #-}
pdfPageOverlayViewProviderDelegateClass :: Class
pdfPageOverlayViewProviderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsPDFPageOverlayViewProvider" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_pdfView_overlayViewForPage = unSelector (mkSelector "pdfView:overlayViewForPage:")
      sel_pdfView_willDisplayOverlayView_forPage = unSelector (mkSelector "pdfView:willDisplayOverlayView:forPage:")
      sel_pdfView_willEndDisplayingOverlayView_forPage = unSelector (mkSelector "pdfView:willEndDisplayingOverlayView:forPage:")
  -- pdfView:overlayViewForPage:
  stub_0 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFPageOverlayViewProviderOverrides
    case _pdfView_overlayViewForPage rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "pdfView:overlayViewForPage:" "@@:@@" stub_0

  -- pdfView:willDisplayOverlayView:forPage:
  stub_1 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFPageOverlayViewProviderOverrides
    case _pdfView_willDisplayOverlayView_forPage rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "pdfView:willDisplayOverlayView:forPage:" "v@:@@@" stub_1

  -- pdfView:willEndDisplayingOverlayView:forPage:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFPageOverlayViewProviderOverrides
    case _pdfView_willEndDisplayingOverlayView_forPage rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "pdfView:willEndDisplayingOverlayView:forPage:" "v@:@@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFPageOverlayViewProviderOverrides
    if queriedSel == sel_pdfView_overlayViewForPage then pure (maybe 0 (const 1) (_pdfView_overlayViewForPage rec_))
    else if queriedSel == sel_pdfView_willDisplayOverlayView_forPage then pure (maybe 0 (const 1) (_pdfView_willDisplayOverlayView_forPage rec_))
    else if queriedSel == sel_pdfView_willEndDisplayingOverlayView_forPage then pure (maybe 0 (const 1) (_pdfView_willEndDisplayingOverlayView_forPage rec_))
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
newPDFPageOverlayViewProvider :: PDFPageOverlayViewProviderOverrides -> IO RawId
newPDFPageOverlayViewProvider overrides = do
  inst <- class_createInstance pdfPageOverlayViewProviderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
