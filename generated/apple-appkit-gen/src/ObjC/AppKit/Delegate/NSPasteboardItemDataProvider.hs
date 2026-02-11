{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSPasteboardItemDataProvider@.
--
-- Usage:
--
-- @
-- delegate <- newNSPasteboardItemDataProvider defaultNSPasteboardItemDataProviderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSPasteboardItemDataProvider
  ( NSPasteboardItemDataProviderOverrides(..)
  , defaultNSPasteboardItemDataProviderOverrides
  , newNSPasteboardItemDataProvider
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

-- | Overrides record for @\@protocol NSPasteboardItemDataProvider@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSPasteboardItemDataProviderOverrides = NSPasteboardItemDataProviderOverrides
  { _pasteboard_item_provideDataForType :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _pasteboardFinishedWithDataProvider :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSPasteboardItemDataProviderOverrides :: NSPasteboardItemDataProviderOverrides
defaultNSPasteboardItemDataProviderOverrides = NSPasteboardItemDataProviderOverrides
  { _pasteboard_item_provideDataForType = Nothing
  , _pasteboardFinishedWithDataProvider = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsPasteboardItemDataProviderDelegateClass #-}
nsPasteboardItemDataProviderDelegateClass :: Class
nsPasteboardItemDataProviderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSPasteboardItemDataProvider" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_pasteboard_item_provideDataForType = unSelector (mkSelector "pasteboard:item:provideDataForType:")
      sel_pasteboardFinishedWithDataProvider = unSelector (mkSelector "pasteboardFinishedWithDataProvider:")
  -- pasteboard:item:provideDataForType:
  stub_0 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPasteboardItemDataProviderOverrides
    case _pasteboard_item_provideDataForType rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "pasteboard:item:provideDataForType:" "v@:@@@" stub_0

  -- pasteboardFinishedWithDataProvider:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPasteboardItemDataProviderOverrides
    case _pasteboardFinishedWithDataProvider rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "pasteboardFinishedWithDataProvider:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPasteboardItemDataProviderOverrides
    if queriedSel == sel_pasteboard_item_provideDataForType then pure (maybe 0 (const 1) (_pasteboard_item_provideDataForType rec_))
    else if queriedSel == sel_pasteboardFinishedWithDataProvider then pure (maybe 0 (const 1) (_pasteboardFinishedWithDataProvider rec_))
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
newNSPasteboardItemDataProvider :: NSPasteboardItemDataProviderOverrides -> IO RawId
newNSPasteboardItemDataProvider overrides = do
  inst <- class_createInstance nsPasteboardItemDataProviderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
