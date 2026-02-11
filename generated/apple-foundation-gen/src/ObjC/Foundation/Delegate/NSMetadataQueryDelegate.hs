{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSMetadataQueryDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSMetadataQueryDelegate defaultNSMetadataQueryDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSMetadataQueryDelegate
  ( NSMetadataQueryDelegateOverrides(..)
  , defaultNSMetadataQueryDelegateOverrides
  , newNSMetadataQueryDelegate
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

-- | Overrides record for @\@protocol NSMetadataQueryDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSMetadataQueryDelegateOverrides = NSMetadataQueryDelegateOverrides
  { _metadataQuery_replacementObjectForResultObject :: !(Maybe (RawId -> RawId -> IO RawId))
  , _metadataQuery_replacementValueForAttribute_value :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSMetadataQueryDelegateOverrides :: NSMetadataQueryDelegateOverrides
defaultNSMetadataQueryDelegateOverrides = NSMetadataQueryDelegateOverrides
  { _metadataQuery_replacementObjectForResultObject = Nothing
  , _metadataQuery_replacementValueForAttribute_value = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsMetadataQueryDelegateDelegateClass #-}
nsMetadataQueryDelegateDelegateClass :: Class
nsMetadataQueryDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSMetadataQueryDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_metadataQuery_replacementObjectForResultObject = unSelector (mkSelector "metadataQuery:replacementObjectForResultObject:")
      sel_metadataQuery_replacementValueForAttribute_value = unSelector (mkSelector "metadataQuery:replacementValueForAttribute:value:")
  -- metadataQuery:replacementObjectForResultObject:
  stub_0 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSMetadataQueryDelegateOverrides
    case _metadataQuery_replacementObjectForResultObject rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "metadataQuery:replacementObjectForResultObject:" "@@:@@" stub_0

  -- metadataQuery:replacementValueForAttribute:value:
  stub_1 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSMetadataQueryDelegateOverrides
    case _metadataQuery_replacementValueForAttribute_value rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "metadataQuery:replacementValueForAttribute:value:" "@@:@@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSMetadataQueryDelegateOverrides
    if queriedSel == sel_metadataQuery_replacementObjectForResultObject then pure (maybe 0 (const 1) (_metadataQuery_replacementObjectForResultObject rec_))
    else if queriedSel == sel_metadataQuery_replacementValueForAttribute_value then pure (maybe 0 (const 1) (_metadataQuery_replacementValueForAttribute_value rec_))
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
newNSMetadataQueryDelegate :: NSMetadataQueryDelegateOverrides -> IO RawId
newNSMetadataQueryDelegate overrides = do
  inst <- class_createInstance nsMetadataQueryDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
