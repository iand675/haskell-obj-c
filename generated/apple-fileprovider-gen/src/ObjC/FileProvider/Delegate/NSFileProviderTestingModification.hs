{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSFileProviderTestingModification@.
--
-- Usage:
--
-- @
-- delegate <- newNSFileProviderTestingModification defaultNSFileProviderTestingModificationOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.FileProvider.Delegate.NSFileProviderTestingModification
  ( NSFileProviderTestingModificationOverrides(..)
  , defaultNSFileProviderTestingModificationOverrides
  , newNSFileProviderTestingModification
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

-- | Overrides record for @\@protocol NSFileProviderTestingModification@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSFileProviderTestingModificationOverrides = NSFileProviderTestingModificationOverrides
  { _targetItemIdentifier :: !(Maybe (IO RawId))
  , _targetItemBaseVersion :: !(Maybe (IO RawId))
  , _domainVersion :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSFileProviderTestingModificationOverrides :: NSFileProviderTestingModificationOverrides
defaultNSFileProviderTestingModificationOverrides = NSFileProviderTestingModificationOverrides
  { _targetItemIdentifier = Nothing
  , _targetItemBaseVersion = Nothing
  , _domainVersion = Nothing
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
{-# NOINLINE nsFileProviderTestingModificationDelegateClass #-}
nsFileProviderTestingModificationDelegateClass :: Class
nsFileProviderTestingModificationDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSFileProviderTestingModification" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_targetItemIdentifier = unSelector (mkSelector "targetItemIdentifier")
      sel_targetItemBaseVersion = unSelector (mkSelector "targetItemBaseVersion")
      sel_domainVersion = unSelector (mkSelector "domainVersion")
  -- targetItemIdentifier
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderTestingModificationOverrides
    case _targetItemIdentifier rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "targetItemIdentifier" "@@:" stub_0

  -- targetItemBaseVersion
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderTestingModificationOverrides
    case _targetItemBaseVersion rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "targetItemBaseVersion" "@@:" stub_1

  -- domainVersion
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderTestingModificationOverrides
    case _domainVersion rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "domainVersion" "@@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderTestingModificationOverrides
    if queriedSel == sel_targetItemIdentifier then pure (maybe 0 (const 1) (_targetItemIdentifier rec_))
    else if queriedSel == sel_targetItemBaseVersion then pure (maybe 0 (const 1) (_targetItemBaseVersion rec_))
    else if queriedSel == sel_domainVersion then pure (maybe 0 (const 1) (_domainVersion rec_))
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
newNSFileProviderTestingModification :: NSFileProviderTestingModificationOverrides -> IO RawId
newNSFileProviderTestingModification overrides = do
  inst <- class_createInstance nsFileProviderTestingModificationDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
