{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSPrintPanelAccessorizing@.
--
-- Usage:
--
-- @
-- delegate <- newNSPrintPanelAccessorizing defaultNSPrintPanelAccessorizingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSPrintPanelAccessorizing
  ( NSPrintPanelAccessorizingOverrides(..)
  , defaultNSPrintPanelAccessorizingOverrides
  , newNSPrintPanelAccessorizing
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

-- | Overrides record for @\@protocol NSPrintPanelAccessorizing@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSPrintPanelAccessorizingOverrides = NSPrintPanelAccessorizingOverrides
  { _localizedSummaryItems :: !(Maybe (IO RawId))
  , _keyPathsForValuesAffectingPreview :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSPrintPanelAccessorizingOverrides :: NSPrintPanelAccessorizingOverrides
defaultNSPrintPanelAccessorizingOverrides = NSPrintPanelAccessorizingOverrides
  { _localizedSummaryItems = Nothing
  , _keyPathsForValuesAffectingPreview = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsPrintPanelAccessorizingDelegateClass #-}
nsPrintPanelAccessorizingDelegateClass :: Class
nsPrintPanelAccessorizingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSPrintPanelAccessorizing" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_localizedSummaryItems = unSelector (mkSelector "localizedSummaryItems")
      sel_keyPathsForValuesAffectingPreview = unSelector (mkSelector "keyPathsForValuesAffectingPreview")
  -- localizedSummaryItems
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPrintPanelAccessorizingOverrides
    case _localizedSummaryItems rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "localizedSummaryItems" "@@:" stub_0

  -- keyPathsForValuesAffectingPreview
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPrintPanelAccessorizingOverrides
    case _keyPathsForValuesAffectingPreview rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "keyPathsForValuesAffectingPreview" "@@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSPrintPanelAccessorizingOverrides
    if queriedSel == sel_localizedSummaryItems then pure (maybe 0 (const 1) (_localizedSummaryItems rec_))
    else if queriedSel == sel_keyPathsForValuesAffectingPreview then pure (maybe 0 (const 1) (_keyPathsForValuesAffectingPreview rec_))
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
newNSPrintPanelAccessorizing :: NSPrintPanelAccessorizingOverrides -> IO RawId
newNSPrintPanelAccessorizing overrides = do
  inst <- class_createInstance nsPrintPanelAccessorizingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
