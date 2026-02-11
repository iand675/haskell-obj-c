{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSFileProviderTestingOperation@.
--
-- Usage:
--
-- @
-- delegate <- newNSFileProviderTestingOperation defaultNSFileProviderTestingOperationOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.FileProvider.Delegate.NSFileProviderTestingOperation
  ( NSFileProviderTestingOperationOverrides(..)
  , defaultNSFileProviderTestingOperationOverrides
  , newNSFileProviderTestingOperation
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

-- | Overrides record for @\@protocol NSFileProviderTestingOperation@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSFileProviderTestingOperationOverrides = NSFileProviderTestingOperationOverrides
  { _asIngestion :: !(Maybe (IO RawId))
  , _asLookup :: !(Maybe (IO RawId))
  , _asCreation :: !(Maybe (IO RawId))
  , _asModification :: !(Maybe (IO RawId))
  , _asDeletion :: !(Maybe (IO RawId))
  , _asContentFetch :: !(Maybe (IO RawId))
  , _asChildrenEnumeration :: !(Maybe (IO RawId))
  , _asCollisionResolution :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSFileProviderTestingOperationOverrides :: NSFileProviderTestingOperationOverrides
defaultNSFileProviderTestingOperationOverrides = NSFileProviderTestingOperationOverrides
  { _asIngestion = Nothing
  , _asLookup = Nothing
  , _asCreation = Nothing
  , _asModification = Nothing
  , _asDeletion = Nothing
  , _asContentFetch = Nothing
  , _asChildrenEnumeration = Nothing
  , _asCollisionResolution = Nothing
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
{-# NOINLINE nsFileProviderTestingOperationDelegateClass #-}
nsFileProviderTestingOperationDelegateClass :: Class
nsFileProviderTestingOperationDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSFileProviderTestingOperation" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_asIngestion = unSelector (mkSelector "asIngestion")
      sel_asLookup = unSelector (mkSelector "asLookup")
      sel_asCreation = unSelector (mkSelector "asCreation")
      sel_asModification = unSelector (mkSelector "asModification")
      sel_asDeletion = unSelector (mkSelector "asDeletion")
      sel_asContentFetch = unSelector (mkSelector "asContentFetch")
      sel_asChildrenEnumeration = unSelector (mkSelector "asChildrenEnumeration")
      sel_asCollisionResolution = unSelector (mkSelector "asCollisionResolution")
  -- asIngestion
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderTestingOperationOverrides
    case _asIngestion rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "asIngestion" "@@:" stub_0

  -- asLookup
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderTestingOperationOverrides
    case _asLookup rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "asLookup" "@@:" stub_1

  -- asCreation
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderTestingOperationOverrides
    case _asCreation rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "asCreation" "@@:" stub_2

  -- asModification
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderTestingOperationOverrides
    case _asModification rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "asModification" "@@:" stub_3

  -- asDeletion
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderTestingOperationOverrides
    case _asDeletion rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "asDeletion" "@@:" stub_4

  -- asContentFetch
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderTestingOperationOverrides
    case _asContentFetch rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "asContentFetch" "@@:" stub_5

  -- asChildrenEnumeration
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderTestingOperationOverrides
    case _asChildrenEnumeration rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "asChildrenEnumeration" "@@:" stub_6

  -- asCollisionResolution
  stub_7 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderTestingOperationOverrides
    case _asCollisionResolution rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "asCollisionResolution" "@@:" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderTestingOperationOverrides
    if queriedSel == sel_asIngestion then pure (maybe 0 (const 1) (_asIngestion rec_))
    else if queriedSel == sel_asLookup then pure (maybe 0 (const 1) (_asLookup rec_))
    else if queriedSel == sel_asCreation then pure (maybe 0 (const 1) (_asCreation rec_))
    else if queriedSel == sel_asModification then pure (maybe 0 (const 1) (_asModification rec_))
    else if queriedSel == sel_asDeletion then pure (maybe 0 (const 1) (_asDeletion rec_))
    else if queriedSel == sel_asContentFetch then pure (maybe 0 (const 1) (_asContentFetch rec_))
    else if queriedSel == sel_asChildrenEnumeration then pure (maybe 0 (const 1) (_asChildrenEnumeration rec_))
    else if queriedSel == sel_asCollisionResolution then pure (maybe 0 (const 1) (_asCollisionResolution rec_))
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
newNSFileProviderTestingOperation :: NSFileProviderTestingOperationOverrides -> IO RawId
newNSFileProviderTestingOperation overrides = do
  inst <- class_createInstance nsFileProviderTestingOperationDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
