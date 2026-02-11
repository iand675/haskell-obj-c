{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSFileProviderTestingLookup@.
--
-- Usage:
--
-- @
-- delegate <- newNSFileProviderTestingLookup defaultNSFileProviderTestingLookupOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.FileProvider.Delegate.NSFileProviderTestingLookup
  ( NSFileProviderTestingLookupOverrides(..)
  , defaultNSFileProviderTestingLookupOverrides
  , newNSFileProviderTestingLookup
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

-- | Overrides record for @\@protocol NSFileProviderTestingLookup@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSFileProviderTestingLookupOverrides = NSFileProviderTestingLookupOverrides
  { _itemIdentifier :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSFileProviderTestingLookupOverrides :: NSFileProviderTestingLookupOverrides
defaultNSFileProviderTestingLookupOverrides = NSFileProviderTestingLookupOverrides
  { _itemIdentifier = Nothing
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
{-# NOINLINE nsFileProviderTestingLookupDelegateClass #-}
nsFileProviderTestingLookupDelegateClass :: Class
nsFileProviderTestingLookupDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSFileProviderTestingLookup" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_itemIdentifier = unSelector (mkSelector "itemIdentifier")
  -- itemIdentifier
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderTestingLookupOverrides
    case _itemIdentifier rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "itemIdentifier" "@@:" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderTestingLookupOverrides
    if queriedSel == sel_itemIdentifier then pure (maybe 0 (const 1) (_itemIdentifier rec_))
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
newNSFileProviderTestingLookup :: NSFileProviderTestingLookupOverrides -> IO RawId
newNSFileProviderTestingLookup overrides = do
  inst <- class_createInstance nsFileProviderTestingLookupDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
