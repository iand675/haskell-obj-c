{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSServicesMenuRequestor@.
--
-- Usage:
--
-- @
-- delegate <- newNSServicesMenuRequestor defaultNSServicesMenuRequestorOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSServicesMenuRequestor
  ( NSServicesMenuRequestorOverrides(..)
  , defaultNSServicesMenuRequestorOverrides
  , newNSServicesMenuRequestor
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

-- | Overrides record for @\@protocol NSServicesMenuRequestor@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSServicesMenuRequestorOverrides = NSServicesMenuRequestorOverrides
  { _writeSelectionToPasteboard_types :: !(Maybe (RawId -> RawId -> IO Bool))
  , _readSelectionFromPasteboard :: !(Maybe (RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNSServicesMenuRequestorOverrides :: NSServicesMenuRequestorOverrides
defaultNSServicesMenuRequestorOverrides = NSServicesMenuRequestorOverrides
  { _writeSelectionToPasteboard_types = Nothing
  , _readSelectionFromPasteboard = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsServicesMenuRequestorDelegateClass #-}
nsServicesMenuRequestorDelegateClass :: Class
nsServicesMenuRequestorDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSServicesMenuRequestor" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_writeSelectionToPasteboard_types = unSelector (mkSelector "writeSelectionToPasteboard:types:")
      sel_readSelectionFromPasteboard = unSelector (mkSelector "readSelectionFromPasteboard:")
  -- writeSelectionToPasteboard:types:
  stub_0 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSServicesMenuRequestorOverrides
    case _writeSelectionToPasteboard_types rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "writeSelectionToPasteboard:types:" "B@:@@" stub_0

  -- readSelectionFromPasteboard:
  stub_1 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSServicesMenuRequestorOverrides
    case _readSelectionFromPasteboard rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "readSelectionFromPasteboard:" "B@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSServicesMenuRequestorOverrides
    if queriedSel == sel_writeSelectionToPasteboard_types then pure (maybe 0 (const 1) (_writeSelectionToPasteboard_types rec_))
    else if queriedSel == sel_readSelectionFromPasteboard then pure (maybe 0 (const 1) (_readSelectionFromPasteboard rec_))
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
newNSServicesMenuRequestor :: NSServicesMenuRequestorOverrides -> IO RawId
newNSServicesMenuRequestor overrides = do
  inst <- class_createInstance nsServicesMenuRequestorDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
