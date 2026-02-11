{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSFileProviderSearchResult@.
--
-- Usage:
--
-- @
-- delegate <- newNSFileProviderSearchResult defaultNSFileProviderSearchResultOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.FileProvider.Delegate.NSFileProviderSearchResult
  ( NSFileProviderSearchResultOverrides(..)
  , defaultNSFileProviderSearchResultOverrides
  , newNSFileProviderSearchResult
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

-- | Overrides record for @\@protocol NSFileProviderSearchResult@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSFileProviderSearchResultOverrides = NSFileProviderSearchResultOverrides
  { _itemIdentifier :: !(Maybe (IO RawId))
  , _filename :: !(Maybe (IO RawId))
  , _creationDate :: !(Maybe (IO RawId))
  , _contentModificationDate :: !(Maybe (IO RawId))
  , _lastUsedDate :: !(Maybe (IO RawId))
  , _contentType :: !(Maybe (IO RawId))
  , _documentSize :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSFileProviderSearchResultOverrides :: NSFileProviderSearchResultOverrides
defaultNSFileProviderSearchResultOverrides = NSFileProviderSearchResultOverrides
  { _itemIdentifier = Nothing
  , _filename = Nothing
  , _creationDate = Nothing
  , _contentModificationDate = Nothing
  , _lastUsedDate = Nothing
  , _contentType = Nothing
  , _documentSize = Nothing
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
{-# NOINLINE nsFileProviderSearchResultDelegateClass #-}
nsFileProviderSearchResultDelegateClass :: Class
nsFileProviderSearchResultDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSFileProviderSearchResult" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_itemIdentifier = unSelector (mkSelector "itemIdentifier")
      sel_filename = unSelector (mkSelector "filename")
      sel_creationDate = unSelector (mkSelector "creationDate")
      sel_contentModificationDate = unSelector (mkSelector "contentModificationDate")
      sel_lastUsedDate = unSelector (mkSelector "lastUsedDate")
      sel_contentType = unSelector (mkSelector "contentType")
      sel_documentSize = unSelector (mkSelector "documentSize")
  -- itemIdentifier
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderSearchResultOverrides
    case _itemIdentifier rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "itemIdentifier" "@@:" stub_0

  -- filename
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderSearchResultOverrides
    case _filename rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "filename" "@@:" stub_1

  -- creationDate
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderSearchResultOverrides
    case _creationDate rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "creationDate" "@@:" stub_2

  -- contentModificationDate
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderSearchResultOverrides
    case _contentModificationDate rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "contentModificationDate" "@@:" stub_3

  -- lastUsedDate
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderSearchResultOverrides
    case _lastUsedDate rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "lastUsedDate" "@@:" stub_4

  -- contentType
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderSearchResultOverrides
    case _contentType rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "contentType" "@@:" stub_5

  -- documentSize
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderSearchResultOverrides
    case _documentSize rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "documentSize" "@@:" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderSearchResultOverrides
    if queriedSel == sel_itemIdentifier then pure (maybe 0 (const 1) (_itemIdentifier rec_))
    else if queriedSel == sel_filename then pure (maybe 0 (const 1) (_filename rec_))
    else if queriedSel == sel_creationDate then pure (maybe 0 (const 1) (_creationDate rec_))
    else if queriedSel == sel_contentModificationDate then pure (maybe 0 (const 1) (_contentModificationDate rec_))
    else if queriedSel == sel_lastUsedDate then pure (maybe 0 (const 1) (_lastUsedDate rec_))
    else if queriedSel == sel_contentType then pure (maybe 0 (const 1) (_contentType rec_))
    else if queriedSel == sel_documentSize then pure (maybe 0 (const 1) (_documentSize rec_))
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
newNSFileProviderSearchResult :: NSFileProviderSearchResultOverrides -> IO RawId
newNSFileProviderSearchResult overrides = do
  inst <- class_createInstance nsFileProviderSearchResultDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
