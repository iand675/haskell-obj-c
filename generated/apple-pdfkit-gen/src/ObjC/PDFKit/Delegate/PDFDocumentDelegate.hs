{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol PDFDocumentDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newPDFDocumentDelegate defaultPDFDocumentDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.PDFKit.Delegate.PDFDocumentDelegate
  ( PDFDocumentDelegateOverrides(..)
  , defaultPDFDocumentDelegateOverrides
  , newPDFDocumentDelegate
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

-- | Overrides record for @\@protocol PDFDocumentDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data PDFDocumentDelegateOverrides = PDFDocumentDelegateOverrides
  { _documentDidUnlock :: !(Maybe (RawId -> IO ()))
  , _documentDidBeginDocumentFind :: !(Maybe (RawId -> IO ()))
  , _documentDidEndDocumentFind :: !(Maybe (RawId -> IO ()))
  , _documentDidBeginPageFind :: !(Maybe (RawId -> IO ()))
  , _documentDidEndPageFind :: !(Maybe (RawId -> IO ()))
  , _documentDidFindMatch :: !(Maybe (RawId -> IO ()))
  , _didMatchString :: !(Maybe (RawId -> IO ()))
  , _classForPage :: !(Maybe (IO Class))
  , _classForAnnotationType :: !(Maybe (RawId -> IO Class))
  , _classForAnnotationClass :: !(Maybe (Class -> IO Class))
  }

-- | Default overrides with all methods unimplemented.
defaultPDFDocumentDelegateOverrides :: PDFDocumentDelegateOverrides
defaultPDFDocumentDelegateOverrides = PDFDocumentDelegateOverrides
  { _documentDidUnlock = Nothing
  , _documentDidBeginDocumentFind = Nothing
  , _documentDidEndDocumentFind = Nothing
  , _documentDidBeginPageFind = Nothing
  , _documentDidEndPageFind = Nothing
  , _documentDidFindMatch = Nothing
  , _didMatchString = Nothing
  , _classForPage = Nothing
  , _classForAnnotationType = Nothing
  , _classForAnnotationClass = Nothing
  }

foreign import ccall "wrapper"
  wrap_cls_cls
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_cls
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_cls
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE pdfDocumentDelegateDelegateClass #-}
pdfDocumentDelegateDelegateClass :: Class
pdfDocumentDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsPDFDocumentDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_documentDidUnlock = unSelector (mkSelector "documentDidUnlock:")
      sel_documentDidBeginDocumentFind = unSelector (mkSelector "documentDidBeginDocumentFind:")
      sel_documentDidEndDocumentFind = unSelector (mkSelector "documentDidEndDocumentFind:")
      sel_documentDidBeginPageFind = unSelector (mkSelector "documentDidBeginPageFind:")
      sel_documentDidEndPageFind = unSelector (mkSelector "documentDidEndPageFind:")
      sel_documentDidFindMatch = unSelector (mkSelector "documentDidFindMatch:")
      sel_didMatchString = unSelector (mkSelector "didMatchString:")
      sel_classForPage = unSelector (mkSelector "classForPage")
      sel_classForAnnotationType = unSelector (mkSelector "classForAnnotationType:")
      sel_classForAnnotationClass = unSelector (mkSelector "classForAnnotationClass:")
  -- documentDidUnlock:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFDocumentDelegateOverrides
    case _documentDidUnlock rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "documentDidUnlock:" "v@:@" stub_0

  -- documentDidBeginDocumentFind:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFDocumentDelegateOverrides
    case _documentDidBeginDocumentFind rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "documentDidBeginDocumentFind:" "v@:@" stub_1

  -- documentDidEndDocumentFind:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFDocumentDelegateOverrides
    case _documentDidEndDocumentFind rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "documentDidEndDocumentFind:" "v@:@" stub_2

  -- documentDidBeginPageFind:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFDocumentDelegateOverrides
    case _documentDidBeginPageFind rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "documentDidBeginPageFind:" "v@:@" stub_3

  -- documentDidEndPageFind:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFDocumentDelegateOverrides
    case _documentDidEndPageFind rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "documentDidEndPageFind:" "v@:@" stub_4

  -- documentDidFindMatch:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFDocumentDelegateOverrides
    case _documentDidFindMatch rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "documentDidFindMatch:" "v@:@" stub_5

  -- didMatchString:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFDocumentDelegateOverrides
    case _didMatchString rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "didMatchString:" "v@:@" stub_6

  -- classForPage
  stub_7 <- wrap_cls $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFDocumentDelegateOverrides
    case _classForPage rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unClass r) :: Ptr ObjCObject)
  addObjCMethod cls "classForPage" "#@:" stub_7

  -- classForAnnotationType:
  stub_8 <- wrap_at_cls $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFDocumentDelegateOverrides
    case _classForAnnotationType rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unClass r) :: Ptr ObjCObject)
  addObjCMethod cls "classForAnnotationType:" "#@:@" stub_8

  -- classForAnnotationClass:
  stub_9 <- wrap_cls_cls $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFDocumentDelegateOverrides
    case _classForAnnotationClass rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (Class (castPtr arg0))
        pure (castPtr (unClass r) :: Ptr ObjCObject)
  addObjCMethod cls "classForAnnotationClass:" "#@:#" stub_9

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDFDocumentDelegateOverrides
    if queriedSel == sel_documentDidUnlock then pure (maybe 0 (const 1) (_documentDidUnlock rec_))
    else if queriedSel == sel_documentDidBeginDocumentFind then pure (maybe 0 (const 1) (_documentDidBeginDocumentFind rec_))
    else if queriedSel == sel_documentDidEndDocumentFind then pure (maybe 0 (const 1) (_documentDidEndDocumentFind rec_))
    else if queriedSel == sel_documentDidBeginPageFind then pure (maybe 0 (const 1) (_documentDidBeginPageFind rec_))
    else if queriedSel == sel_documentDidEndPageFind then pure (maybe 0 (const 1) (_documentDidEndPageFind rec_))
    else if queriedSel == sel_documentDidFindMatch then pure (maybe 0 (const 1) (_documentDidFindMatch rec_))
    else if queriedSel == sel_didMatchString then pure (maybe 0 (const 1) (_didMatchString rec_))
    else if queriedSel == sel_classForPage then pure (maybe 0 (const 1) (_classForPage rec_))
    else if queriedSel == sel_classForAnnotationType then pure (maybe 0 (const 1) (_classForAnnotationType rec_))
    else if queriedSel == sel_classForAnnotationClass then pure (maybe 0 (const 1) (_classForAnnotationClass rec_))
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
newPDFDocumentDelegate :: PDFDocumentDelegateOverrides -> IO RawId
newPDFDocumentDelegate overrides = do
  inst <- class_createInstance pdfDocumentDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
