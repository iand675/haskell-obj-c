{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol PDFViewDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newPDFViewDelegate defaultPDFViewDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.PDFKit.Delegate.PDFViewDelegate
  ( PDFViewDelegateOverrides(..)
  , defaultPDFViewDelegateOverrides
  , newPDFViewDelegate
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

-- | Overrides record for @\@protocol PDFViewDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data PDFViewDelegateOverrides = PDFViewDelegateOverrides
  { _pdfViewWillClickOnLink_withURL :: !(Maybe (RawId -> RawId -> IO ()))
  , _pdfViewWillChangeScaleFactor_toScale :: !(Maybe (RawId -> Double -> IO Double))
  , _pdfViewPrintJobTitle :: !(Maybe (RawId -> IO RawId))
  , _pdfViewPerformPrint :: !(Maybe (RawId -> IO ()))
  , _pdfViewPerformFind :: !(Maybe (RawId -> IO ()))
  , _pdfViewPerformGoToPage :: !(Maybe (RawId -> IO ()))
  , _pdfViewOpenPDF_forRemoteGoToAction :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultPDFViewDelegateOverrides :: PDFViewDelegateOverrides
defaultPDFViewDelegateOverrides = PDFViewDelegateOverrides
  { _pdfViewWillClickOnLink_withURL = Nothing
  , _pdfViewWillChangeScaleFactor_toScale = Nothing
  , _pdfViewPrintJobTitle = Nothing
  , _pdfViewPerformPrint = Nothing
  , _pdfViewPerformFind = Nothing
  , _pdfViewPerformGoToPage = Nothing
  , _pdfViewOpenPDF_forRemoteGoToAction = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_d_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CDouble -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CDouble -> IO CDouble))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE pdfViewDelegateDelegateClass #-}
pdfViewDelegateDelegateClass :: Class
pdfViewDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsPDFViewDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_pdfViewWillClickOnLink_withURL = unSelector (mkSelector "PDFViewWillClickOnLink:withURL:")
      sel_pdfViewWillChangeScaleFactor_toScale = unSelector (mkSelector "PDFViewWillChangeScaleFactor:toScale:")
      sel_pdfViewPrintJobTitle = unSelector (mkSelector "PDFViewPrintJobTitle:")
      sel_pdfViewPerformPrint = unSelector (mkSelector "PDFViewPerformPrint:")
      sel_pdfViewPerformFind = unSelector (mkSelector "PDFViewPerformFind:")
      sel_pdfViewPerformGoToPage = unSelector (mkSelector "PDFViewPerformGoToPage:")
      sel_pdfViewOpenPDF_forRemoteGoToAction = unSelector (mkSelector "PDFViewOpenPDF:forRemoteGoToAction:")
  -- PDFViewWillClickOnLink:withURL:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFViewDelegateOverrides
    case _pdfViewWillClickOnLink_withURL rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "PDFViewWillClickOnLink:withURL:" "v@:@@" stub_0

  -- PDFViewWillChangeScaleFactor:toScale:
  stub_1 <- wrap_at_d_d $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFViewDelegateOverrides
    case _pdfViewWillChangeScaleFactor_toScale rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f (RawId arg0) (realToFrac arg1)
        pure (realToFrac r)
  addObjCMethod cls "PDFViewWillChangeScaleFactor:toScale:" "d@:@d" stub_1

  -- PDFViewPrintJobTitle:
  stub_2 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFViewDelegateOverrides
    case _pdfViewPrintJobTitle rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "PDFViewPrintJobTitle:" "@@:@" stub_2

  -- PDFViewPerformPrint:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFViewDelegateOverrides
    case _pdfViewPerformPrint rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "PDFViewPerformPrint:" "v@:@" stub_3

  -- PDFViewPerformFind:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFViewDelegateOverrides
    case _pdfViewPerformFind rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "PDFViewPerformFind:" "v@:@" stub_4

  -- PDFViewPerformGoToPage:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFViewDelegateOverrides
    case _pdfViewPerformGoToPage rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "PDFViewPerformGoToPage:" "v@:@" stub_5

  -- PDFViewOpenPDF:forRemoteGoToAction:
  stub_6 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFViewDelegateOverrides
    case _pdfViewOpenPDF_forRemoteGoToAction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "PDFViewOpenPDF:forRemoteGoToAction:" "v@:@@" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFViewDelegateOverrides
    if queriedSel == sel_pdfViewWillClickOnLink_withURL then pure (maybe 0 (const 1) (_pdfViewWillClickOnLink_withURL rec_))
    else if queriedSel == sel_pdfViewWillChangeScaleFactor_toScale then pure (maybe 0 (const 1) (_pdfViewWillChangeScaleFactor_toScale rec_))
    else if queriedSel == sel_pdfViewPrintJobTitle then pure (maybe 0 (const 1) (_pdfViewPrintJobTitle rec_))
    else if queriedSel == sel_pdfViewPerformPrint then pure (maybe 0 (const 1) (_pdfViewPerformPrint rec_))
    else if queriedSel == sel_pdfViewPerformFind then pure (maybe 0 (const 1) (_pdfViewPerformFind rec_))
    else if queriedSel == sel_pdfViewPerformGoToPage then pure (maybe 0 (const 1) (_pdfViewPerformGoToPage rec_))
    else if queriedSel == sel_pdfViewOpenPDF_forRemoteGoToAction then pure (maybe 0 (const 1) (_pdfViewOpenPDF_forRemoteGoToAction rec_))
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
newPDFViewDelegate :: PDFViewDelegateOverrides -> IO RawId
newPDFViewDelegate overrides = do
  inst <- class_createInstance pdfViewDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
