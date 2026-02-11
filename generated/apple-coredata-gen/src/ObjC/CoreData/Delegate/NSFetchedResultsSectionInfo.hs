{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSFetchedResultsSectionInfo@.
--
-- Usage:
--
-- @
-- delegate <- newNSFetchedResultsSectionInfo defaultNSFetchedResultsSectionInfoOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreData.Delegate.NSFetchedResultsSectionInfo
  ( NSFetchedResultsSectionInfoOverrides(..)
  , defaultNSFetchedResultsSectionInfoOverrides
  , newNSFetchedResultsSectionInfo
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

-- | Overrides record for @\@protocol NSFetchedResultsSectionInfo@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSFetchedResultsSectionInfoOverrides = NSFetchedResultsSectionInfoOverrides
  { _name :: !(Maybe (IO RawId))
  , _indexTitle :: !(Maybe (IO RawId))
  , _numberOfObjects :: !(Maybe (IO Int))
  , _objects :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSFetchedResultsSectionInfoOverrides :: NSFetchedResultsSectionInfoOverrides
defaultNSFetchedResultsSectionInfoOverrides = NSFetchedResultsSectionInfoOverrides
  { _name = Nothing
  , _indexTitle = Nothing
  , _numberOfObjects = Nothing
  , _objects = Nothing
  }

foreign import ccall "wrapper"
  wrap_Q
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
{-# NOINLINE nsFetchedResultsSectionInfoDelegateClass #-}
nsFetchedResultsSectionInfoDelegateClass :: Class
nsFetchedResultsSectionInfoDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSFetchedResultsSectionInfo" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_name = unSelector (mkSelector "name")
      sel_indexTitle = unSelector (mkSelector "indexTitle")
      sel_numberOfObjects = unSelector (mkSelector "numberOfObjects")
      sel_objects = unSelector (mkSelector "objects")
  -- name
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFetchedResultsSectionInfoOverrides
    case _name rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "name" "@@:" stub_0

  -- indexTitle
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFetchedResultsSectionInfoOverrides
    case _indexTitle rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "indexTitle" "@@:" stub_1

  -- numberOfObjects
  stub_2 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFetchedResultsSectionInfoOverrides
    case _numberOfObjects rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "numberOfObjects" "Q@:" stub_2

  -- objects
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFetchedResultsSectionInfoOverrides
    case _objects rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "objects" "@@:" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFetchedResultsSectionInfoOverrides
    if queriedSel == sel_name then pure (maybe 0 (const 1) (_name rec_))
    else if queriedSel == sel_indexTitle then pure (maybe 0 (const 1) (_indexTitle rec_))
    else if queriedSel == sel_numberOfObjects then pure (maybe 0 (const 1) (_numberOfObjects rec_))
    else if queriedSel == sel_objects then pure (maybe 0 (const 1) (_objects rec_))
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
newNSFetchedResultsSectionInfo :: NSFetchedResultsSectionInfoOverrides -> IO RawId
newNSFetchedResultsSectionInfo overrides = do
  inst <- class_createInstance nsFetchedResultsSectionInfoDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
