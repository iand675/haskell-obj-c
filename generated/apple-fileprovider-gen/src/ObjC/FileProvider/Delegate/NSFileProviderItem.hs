{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSFileProviderItem@.
--
-- Usage:
--
-- @
-- delegate <- newNSFileProviderItem defaultNSFileProviderItemOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.FileProvider.Delegate.NSFileProviderItem
  ( NSFileProviderItemOverrides(..)
  , defaultNSFileProviderItemOverrides
  , newNSFileProviderItem
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

-- | Overrides record for @\@protocol NSFileProviderItem@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSFileProviderItemOverrides = NSFileProviderItemOverrides
  { _itemIdentifier :: !(Maybe (IO RawId))
  , _parentItemIdentifier :: !(Maybe (IO RawId))
  , _filename :: !(Maybe (IO RawId))
  , _contentType :: !(Maybe (IO RawId))
  , _typeIdentifier :: !(Maybe (IO RawId))
  , _documentSize :: !(Maybe (IO RawId))
  , _childItemCount :: !(Maybe (IO RawId))
  , _creationDate :: !(Maybe (IO RawId))
  , _contentModificationDate :: !(Maybe (IO RawId))
  , _extendedAttributes :: !(Maybe (IO RawId))
  , _lastUsedDate :: !(Maybe (IO RawId))
  , _tagData :: !(Maybe (IO RawId))
  , _favoriteRank :: !(Maybe (IO RawId))
  , _trashed :: !(Maybe (IO Bool))
  , _uploaded :: !(Maybe (IO Bool))
  , _uploading :: !(Maybe (IO Bool))
  , _uploadingError :: !(Maybe (IO RawId))
  , _downloaded :: !(Maybe (IO Bool))
  , _downloading :: !(Maybe (IO Bool))
  , _downloadingError :: !(Maybe (IO RawId))
  , _mostRecentVersionDownloaded :: !(Maybe (IO Bool))
  , _shared :: !(Maybe (IO Bool))
  , _sharedByCurrentUser :: !(Maybe (IO Bool))
  , _ownerNameComponents :: !(Maybe (IO RawId))
  , _mostRecentEditorNameComponents :: !(Maybe (IO RawId))
  , _versionIdentifier :: !(Maybe (IO RawId))
  , _itemVersion :: !(Maybe (IO RawId))
  , _symlinkTargetPath :: !(Maybe (IO RawId))
  , _userInfo :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSFileProviderItemOverrides :: NSFileProviderItemOverrides
defaultNSFileProviderItemOverrides = NSFileProviderItemOverrides
  { _itemIdentifier = Nothing
  , _parentItemIdentifier = Nothing
  , _filename = Nothing
  , _contentType = Nothing
  , _typeIdentifier = Nothing
  , _documentSize = Nothing
  , _childItemCount = Nothing
  , _creationDate = Nothing
  , _contentModificationDate = Nothing
  , _extendedAttributes = Nothing
  , _lastUsedDate = Nothing
  , _tagData = Nothing
  , _favoriteRank = Nothing
  , _trashed = Nothing
  , _uploaded = Nothing
  , _uploading = Nothing
  , _uploadingError = Nothing
  , _downloaded = Nothing
  , _downloading = Nothing
  , _downloadingError = Nothing
  , _mostRecentVersionDownloaded = Nothing
  , _shared = Nothing
  , _sharedByCurrentUser = Nothing
  , _ownerNameComponents = Nothing
  , _mostRecentEditorNameComponents = Nothing
  , _versionIdentifier = Nothing
  , _itemVersion = Nothing
  , _symlinkTargetPath = Nothing
  , _userInfo = Nothing
  }

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsFileProviderItemDelegateClass #-}
nsFileProviderItemDelegateClass :: Class
nsFileProviderItemDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSFileProviderItem" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_itemIdentifier = unSelector (mkSelector "itemIdentifier")
      sel_parentItemIdentifier = unSelector (mkSelector "parentItemIdentifier")
      sel_filename = unSelector (mkSelector "filename")
      sel_contentType = unSelector (mkSelector "contentType")
      sel_typeIdentifier = unSelector (mkSelector "typeIdentifier")
      sel_documentSize = unSelector (mkSelector "documentSize")
      sel_childItemCount = unSelector (mkSelector "childItemCount")
      sel_creationDate = unSelector (mkSelector "creationDate")
      sel_contentModificationDate = unSelector (mkSelector "contentModificationDate")
      sel_extendedAttributes = unSelector (mkSelector "extendedAttributes")
      sel_lastUsedDate = unSelector (mkSelector "lastUsedDate")
      sel_tagData = unSelector (mkSelector "tagData")
      sel_favoriteRank = unSelector (mkSelector "favoriteRank")
      sel_trashed = unSelector (mkSelector "trashed")
      sel_uploaded = unSelector (mkSelector "uploaded")
      sel_uploading = unSelector (mkSelector "uploading")
      sel_uploadingError = unSelector (mkSelector "uploadingError")
      sel_downloaded = unSelector (mkSelector "downloaded")
      sel_downloading = unSelector (mkSelector "downloading")
      sel_downloadingError = unSelector (mkSelector "downloadingError")
      sel_mostRecentVersionDownloaded = unSelector (mkSelector "mostRecentVersionDownloaded")
      sel_shared = unSelector (mkSelector "shared")
      sel_sharedByCurrentUser = unSelector (mkSelector "sharedByCurrentUser")
      sel_ownerNameComponents = unSelector (mkSelector "ownerNameComponents")
      sel_mostRecentEditorNameComponents = unSelector (mkSelector "mostRecentEditorNameComponents")
      sel_versionIdentifier = unSelector (mkSelector "versionIdentifier")
      sel_itemVersion = unSelector (mkSelector "itemVersion")
      sel_symlinkTargetPath = unSelector (mkSelector "symlinkTargetPath")
      sel_userInfo = unSelector (mkSelector "userInfo")
  -- itemIdentifier
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _itemIdentifier rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "itemIdentifier" "@@:" stub_0

  -- parentItemIdentifier
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _parentItemIdentifier rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "parentItemIdentifier" "@@:" stub_1

  -- filename
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _filename rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "filename" "@@:" stub_2

  -- contentType
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _contentType rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "contentType" "@@:" stub_3

  -- typeIdentifier
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _typeIdentifier rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "typeIdentifier" "@@:" stub_4

  -- documentSize
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _documentSize rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "documentSize" "@@:" stub_5

  -- childItemCount
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _childItemCount rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "childItemCount" "@@:" stub_6

  -- creationDate
  stub_7 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _creationDate rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "creationDate" "@@:" stub_7

  -- contentModificationDate
  stub_8 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _contentModificationDate rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "contentModificationDate" "@@:" stub_8

  -- extendedAttributes
  stub_9 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _extendedAttributes rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "extendedAttributes" "@@:" stub_9

  -- lastUsedDate
  stub_10 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _lastUsedDate rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "lastUsedDate" "@@:" stub_10

  -- tagData
  stub_11 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _tagData rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tagData" "@@:" stub_11

  -- favoriteRank
  stub_12 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _favoriteRank rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "favoriteRank" "@@:" stub_12

  -- trashed
  stub_13 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _trashed rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "trashed" "B@:" stub_13

  -- uploaded
  stub_14 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _uploaded rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "uploaded" "B@:" stub_14

  -- uploading
  stub_15 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _uploading rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "uploading" "B@:" stub_15

  -- uploadingError
  stub_16 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _uploadingError rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "uploadingError" "@@:" stub_16

  -- downloaded
  stub_17 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _downloaded rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "downloaded" "B@:" stub_17

  -- downloading
  stub_18 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _downloading rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "downloading" "B@:" stub_18

  -- downloadingError
  stub_19 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _downloadingError rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "downloadingError" "@@:" stub_19

  -- mostRecentVersionDownloaded
  stub_20 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _mostRecentVersionDownloaded rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "mostRecentVersionDownloaded" "B@:" stub_20

  -- shared
  stub_21 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _shared rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "shared" "B@:" stub_21

  -- sharedByCurrentUser
  stub_22 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _sharedByCurrentUser rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "sharedByCurrentUser" "B@:" stub_22

  -- ownerNameComponents
  stub_23 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _ownerNameComponents rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "ownerNameComponents" "@@:" stub_23

  -- mostRecentEditorNameComponents
  stub_24 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _mostRecentEditorNameComponents rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "mostRecentEditorNameComponents" "@@:" stub_24

  -- versionIdentifier
  stub_25 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _versionIdentifier rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "versionIdentifier" "@@:" stub_25

  -- itemVersion
  stub_26 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _itemVersion rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "itemVersion" "@@:" stub_26

  -- symlinkTargetPath
  stub_27 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _symlinkTargetPath rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "symlinkTargetPath" "@@:" stub_27

  -- userInfo
  stub_28 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    case _userInfo rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "userInfo" "@@:" stub_28

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFileProviderItemOverrides
    if queriedSel == sel_itemIdentifier then pure (maybe 0 (const 1) (_itemIdentifier rec_))
    else if queriedSel == sel_parentItemIdentifier then pure (maybe 0 (const 1) (_parentItemIdentifier rec_))
    else if queriedSel == sel_filename then pure (maybe 0 (const 1) (_filename rec_))
    else if queriedSel == sel_contentType then pure (maybe 0 (const 1) (_contentType rec_))
    else if queriedSel == sel_typeIdentifier then pure (maybe 0 (const 1) (_typeIdentifier rec_))
    else if queriedSel == sel_documentSize then pure (maybe 0 (const 1) (_documentSize rec_))
    else if queriedSel == sel_childItemCount then pure (maybe 0 (const 1) (_childItemCount rec_))
    else if queriedSel == sel_creationDate then pure (maybe 0 (const 1) (_creationDate rec_))
    else if queriedSel == sel_contentModificationDate then pure (maybe 0 (const 1) (_contentModificationDate rec_))
    else if queriedSel == sel_extendedAttributes then pure (maybe 0 (const 1) (_extendedAttributes rec_))
    else if queriedSel == sel_lastUsedDate then pure (maybe 0 (const 1) (_lastUsedDate rec_))
    else if queriedSel == sel_tagData then pure (maybe 0 (const 1) (_tagData rec_))
    else if queriedSel == sel_favoriteRank then pure (maybe 0 (const 1) (_favoriteRank rec_))
    else if queriedSel == sel_trashed then pure (maybe 0 (const 1) (_trashed rec_))
    else if queriedSel == sel_uploaded then pure (maybe 0 (const 1) (_uploaded rec_))
    else if queriedSel == sel_uploading then pure (maybe 0 (const 1) (_uploading rec_))
    else if queriedSel == sel_uploadingError then pure (maybe 0 (const 1) (_uploadingError rec_))
    else if queriedSel == sel_downloaded then pure (maybe 0 (const 1) (_downloaded rec_))
    else if queriedSel == sel_downloading then pure (maybe 0 (const 1) (_downloading rec_))
    else if queriedSel == sel_downloadingError then pure (maybe 0 (const 1) (_downloadingError rec_))
    else if queriedSel == sel_mostRecentVersionDownloaded then pure (maybe 0 (const 1) (_mostRecentVersionDownloaded rec_))
    else if queriedSel == sel_shared then pure (maybe 0 (const 1) (_shared rec_))
    else if queriedSel == sel_sharedByCurrentUser then pure (maybe 0 (const 1) (_sharedByCurrentUser rec_))
    else if queriedSel == sel_ownerNameComponents then pure (maybe 0 (const 1) (_ownerNameComponents rec_))
    else if queriedSel == sel_mostRecentEditorNameComponents then pure (maybe 0 (const 1) (_mostRecentEditorNameComponents rec_))
    else if queriedSel == sel_versionIdentifier then pure (maybe 0 (const 1) (_versionIdentifier rec_))
    else if queriedSel == sel_itemVersion then pure (maybe 0 (const 1) (_itemVersion rec_))
    else if queriedSel == sel_symlinkTargetPath then pure (maybe 0 (const 1) (_symlinkTargetPath rec_))
    else if queriedSel == sel_userInfo then pure (maybe 0 (const 1) (_userInfo rec_))
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
newNSFileProviderItem :: NSFileProviderItemOverrides -> IO RawId
newNSFileProviderItem overrides = do
  inst <- class_createInstance nsFileProviderItemDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
