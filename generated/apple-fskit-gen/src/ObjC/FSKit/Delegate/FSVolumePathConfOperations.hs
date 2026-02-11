{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol FSVolumePathConfOperations@.
--
-- Usage:
--
-- @
-- delegate <- newFSVolumePathConfOperations defaultFSVolumePathConfOperationsOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.FSKit.Delegate.FSVolumePathConfOperations
  ( FSVolumePathConfOperationsOverrides(..)
  , defaultFSVolumePathConfOperationsOverrides
  , newFSVolumePathConfOperations
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

-- | Overrides record for @\@protocol FSVolumePathConfOperations@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data FSVolumePathConfOperationsOverrides = FSVolumePathConfOperationsOverrides
  { _maximumLinkCount :: !(Maybe (IO Int))
  , _maximumNameLength :: !(Maybe (IO Int))
  , _restrictsOwnershipChanges :: !(Maybe (IO Bool))
  , _truncatesLongNames :: !(Maybe (IO Bool))
  , _maximumXattrSize :: !(Maybe (IO Int))
  , _maximumXattrSizeInBits :: !(Maybe (IO Int))
  , _maximumFileSize :: !(Maybe (IO Int))
  , _maximumFileSizeInBits :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultFSVolumePathConfOperationsOverrides :: FSVolumePathConfOperationsOverrides
defaultFSVolumePathConfOperationsOverrides = FSVolumePathConfOperationsOverrides
  { _maximumLinkCount = Nothing
  , _maximumNameLength = Nothing
  , _restrictsOwnershipChanges = Nothing
  , _truncatesLongNames = Nothing
  , _maximumXattrSize = Nothing
  , _maximumXattrSizeInBits = Nothing
  , _maximumFileSize = Nothing
  , _maximumFileSizeInBits = Nothing
  }

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE fsVolumePathConfOperationsDelegateClass #-}
fsVolumePathConfOperationsDelegateClass :: Class
fsVolumePathConfOperationsDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsFSVolumePathConfOperations" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_maximumLinkCount = unSelector (mkSelector "maximumLinkCount")
      sel_maximumNameLength = unSelector (mkSelector "maximumNameLength")
      sel_restrictsOwnershipChanges = unSelector (mkSelector "restrictsOwnershipChanges")
      sel_truncatesLongNames = unSelector (mkSelector "truncatesLongNames")
      sel_maximumXattrSize = unSelector (mkSelector "maximumXattrSize")
      sel_maximumXattrSizeInBits = unSelector (mkSelector "maximumXattrSizeInBits")
      sel_maximumFileSize = unSelector (mkSelector "maximumFileSize")
      sel_maximumFileSizeInBits = unSelector (mkSelector "maximumFileSizeInBits")
  -- maximumLinkCount
  stub_0 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumePathConfOperationsOverrides
    case _maximumLinkCount rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "maximumLinkCount" "q@:" stub_0

  -- maximumNameLength
  stub_1 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumePathConfOperationsOverrides
    case _maximumNameLength rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "maximumNameLength" "q@:" stub_1

  -- restrictsOwnershipChanges
  stub_2 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumePathConfOperationsOverrides
    case _restrictsOwnershipChanges rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "restrictsOwnershipChanges" "B@:" stub_2

  -- truncatesLongNames
  stub_3 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumePathConfOperationsOverrides
    case _truncatesLongNames rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "truncatesLongNames" "B@:" stub_3

  -- maximumXattrSize
  stub_4 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumePathConfOperationsOverrides
    case _maximumXattrSize rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "maximumXattrSize" "q@:" stub_4

  -- maximumXattrSizeInBits
  stub_5 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumePathConfOperationsOverrides
    case _maximumXattrSizeInBits rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "maximumXattrSizeInBits" "q@:" stub_5

  -- maximumFileSize
  stub_6 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumePathConfOperationsOverrides
    case _maximumFileSize rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "maximumFileSize" "Q@:" stub_6

  -- maximumFileSizeInBits
  stub_7 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumePathConfOperationsOverrides
    case _maximumFileSizeInBits rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "maximumFileSizeInBits" "q@:" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO FSVolumePathConfOperationsOverrides
    if queriedSel == sel_maximumLinkCount then pure (maybe 0 (const 1) (_maximumLinkCount rec_))
    else if queriedSel == sel_maximumNameLength then pure (maybe 0 (const 1) (_maximumNameLength rec_))
    else if queriedSel == sel_restrictsOwnershipChanges then pure (maybe 0 (const 1) (_restrictsOwnershipChanges rec_))
    else if queriedSel == sel_truncatesLongNames then pure (maybe 0 (const 1) (_truncatesLongNames rec_))
    else if queriedSel == sel_maximumXattrSize then pure (maybe 0 (const 1) (_maximumXattrSize rec_))
    else if queriedSel == sel_maximumXattrSizeInBits then pure (maybe 0 (const 1) (_maximumXattrSizeInBits rec_))
    else if queriedSel == sel_maximumFileSize then pure (maybe 0 (const 1) (_maximumFileSize rec_))
    else if queriedSel == sel_maximumFileSizeInBits then pure (maybe 0 (const 1) (_maximumFileSizeInBits rec_))
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
newFSVolumePathConfOperations :: FSVolumePathConfOperationsOverrides -> IO RawId
newFSVolumePathConfOperations overrides = do
  inst <- class_createInstance fsVolumePathConfOperationsDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
