{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CSSearchableIndexDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newCSSearchableIndexDelegate defaultCSSearchableIndexDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreSpotlight.Delegate.CSSearchableIndexDelegate
  ( CSSearchableIndexDelegateOverrides(..)
  , defaultCSSearchableIndexDelegateOverrides
  , newCSSearchableIndexDelegate
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

-- | Overrides record for @\@protocol CSSearchableIndexDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CSSearchableIndexDelegateOverrides = CSSearchableIndexDelegateOverrides
  { _searchableIndexDidThrottle :: !(Maybe (RawId -> IO ()))
  , _searchableIndexDidFinishThrottle :: !(Maybe (RawId -> IO ()))
  , _dataForSearchableIndex_itemIdentifier_typeIdentifier_error :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO RawId))
  , _fileURLForSearchableIndex_itemIdentifier_typeIdentifier_inPlace_error :: !(Maybe (RawId -> RawId -> RawId -> Bool -> RawId -> IO RawId))
  , _searchableItemsForIdentifiers_searchableItemsHandler :: !(Maybe (RawId -> RawId -> IO ()))
  , _searchableItemsDidUpdate :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultCSSearchableIndexDelegateOverrides :: CSSearchableIndexDelegateOverrides
defaultCSSearchableIndexDelegateOverrides = CSSearchableIndexDelegateOverrides
  { _searchableIndexDidThrottle = Nothing
  , _searchableIndexDidFinishThrottle = Nothing
  , _dataForSearchableIndex_itemIdentifier_typeIdentifier_error = Nothing
  , _fileURLForSearchableIndex_itemIdentifier_typeIdentifier_inPlace_error = Nothing
  , _searchableItemsForIdentifiers_searchableItemsHandler = Nothing
  , _searchableItemsDidUpdate = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_B_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE csSearchableIndexDelegateDelegateClass #-}
csSearchableIndexDelegateDelegateClass :: Class
csSearchableIndexDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCSSearchableIndexDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_searchableIndexDidThrottle = unSelector (mkSelector "searchableIndexDidThrottle:")
      sel_searchableIndexDidFinishThrottle = unSelector (mkSelector "searchableIndexDidFinishThrottle:")
      sel_dataForSearchableIndex_itemIdentifier_typeIdentifier_error = unSelector (mkSelector "dataForSearchableIndex:itemIdentifier:typeIdentifier:error:")
      sel_fileURLForSearchableIndex_itemIdentifier_typeIdentifier_inPlace_error = unSelector (mkSelector "fileURLForSearchableIndex:itemIdentifier:typeIdentifier:inPlace:error:")
      sel_searchableItemsForIdentifiers_searchableItemsHandler = unSelector (mkSelector "searchableItemsForIdentifiers:searchableItemsHandler:")
      sel_searchableItemsDidUpdate = unSelector (mkSelector "searchableItemsDidUpdate:")
  -- searchableIndexDidThrottle:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CSSearchableIndexDelegateOverrides
    case _searchableIndexDidThrottle rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "searchableIndexDidThrottle:" "v@:@" stub_0

  -- searchableIndexDidFinishThrottle:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CSSearchableIndexDelegateOverrides
    case _searchableIndexDidFinishThrottle rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "searchableIndexDidFinishThrottle:" "v@:@" stub_1

  -- dataForSearchableIndex:itemIdentifier:typeIdentifier:error:
  stub_2 <- wrap_at_at_at_at_at $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CSSearchableIndexDelegateOverrides
    case _dataForSearchableIndex_itemIdentifier_typeIdentifier_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "dataForSearchableIndex:itemIdentifier:typeIdentifier:error:" "@@:@@@@" stub_2

  -- fileURLForSearchableIndex:itemIdentifier:typeIdentifier:inPlace:error:
  stub_3 <- wrap_at_at_at_B_at_at $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CSSearchableIndexDelegateOverrides
    case _fileURLForSearchableIndex_itemIdentifier_typeIdentifier_inPlace_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (arg3 /= 0) (RawId arg4)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "fileURLForSearchableIndex:itemIdentifier:typeIdentifier:inPlace:error:" "@@:@@@B@" stub_3

  -- searchableItemsForIdentifiers:searchableItemsHandler:
  stub_4 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CSSearchableIndexDelegateOverrides
    case _searchableItemsForIdentifiers_searchableItemsHandler rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "searchableItemsForIdentifiers:searchableItemsHandler:" "v@:@@" stub_4

  -- searchableItemsDidUpdate:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CSSearchableIndexDelegateOverrides
    case _searchableItemsDidUpdate rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "searchableItemsDidUpdate:" "v@:@" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CSSearchableIndexDelegateOverrides
    if queriedSel == sel_searchableIndexDidThrottle then pure (maybe 0 (const 1) (_searchableIndexDidThrottle rec_))
    else if queriedSel == sel_searchableIndexDidFinishThrottle then pure (maybe 0 (const 1) (_searchableIndexDidFinishThrottle rec_))
    else if queriedSel == sel_dataForSearchableIndex_itemIdentifier_typeIdentifier_error then pure (maybe 0 (const 1) (_dataForSearchableIndex_itemIdentifier_typeIdentifier_error rec_))
    else if queriedSel == sel_fileURLForSearchableIndex_itemIdentifier_typeIdentifier_inPlace_error then pure (maybe 0 (const 1) (_fileURLForSearchableIndex_itemIdentifier_typeIdentifier_inPlace_error rec_))
    else if queriedSel == sel_searchableItemsForIdentifiers_searchableItemsHandler then pure (maybe 0 (const 1) (_searchableItemsForIdentifiers_searchableItemsHandler rec_))
    else if queriedSel == sel_searchableItemsDidUpdate then pure (maybe 0 (const 1) (_searchableItemsDidUpdate rec_))
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
newCSSearchableIndexDelegate :: CSSearchableIndexDelegateOverrides -> IO RawId
newCSSearchableIndexDelegate overrides = do
  inst <- class_createInstance csSearchableIndexDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
