{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol IKImageEditPanelDataSource@.
--
-- Usage:
--
-- @
-- delegate <- newIKImageEditPanelDataSource defaultIKImageEditPanelDataSourceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Quartz.Delegate.IKImageEditPanelDataSource
  ( IKImageEditPanelDataSourceOverrides(..)
  , defaultIKImageEditPanelDataSourceOverrides
  , newIKImageEditPanelDataSource
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

-- | Overrides record for @\@protocol IKImageEditPanelDataSource@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data IKImageEditPanelDataSourceOverrides = IKImageEditPanelDataSourceOverrides
  { _imageProperties :: !(Maybe (IO RawId))
  , _hasAdjustMode :: !(Maybe (IO Bool))
  , _hasEffectsMode :: !(Maybe (IO Bool))
  , _hasDetailsMode :: !(Maybe (IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultIKImageEditPanelDataSourceOverrides :: IKImageEditPanelDataSourceOverrides
defaultIKImageEditPanelDataSourceOverrides = IKImageEditPanelDataSourceOverrides
  { _imageProperties = Nothing
  , _hasAdjustMode = Nothing
  , _hasEffectsMode = Nothing
  , _hasDetailsMode = Nothing
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
{-# NOINLINE ikImageEditPanelDataSourceDelegateClass #-}
ikImageEditPanelDataSourceDelegateClass :: Class
ikImageEditPanelDataSourceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsIKImageEditPanelDataSource" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_imageProperties = unSelector (mkSelector "imageProperties")
      sel_hasAdjustMode = unSelector (mkSelector "hasAdjustMode")
      sel_hasEffectsMode = unSelector (mkSelector "hasEffectsMode")
      sel_hasDetailsMode = unSelector (mkSelector "hasDetailsMode")
  -- imageProperties
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKImageEditPanelDataSourceOverrides
    case _imageProperties rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "imageProperties" "@@:" stub_0

  -- hasAdjustMode
  stub_1 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKImageEditPanelDataSourceOverrides
    case _hasAdjustMode rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "hasAdjustMode" "B@:" stub_1

  -- hasEffectsMode
  stub_2 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKImageEditPanelDataSourceOverrides
    case _hasEffectsMode rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "hasEffectsMode" "B@:" stub_2

  -- hasDetailsMode
  stub_3 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKImageEditPanelDataSourceOverrides
    case _hasDetailsMode rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "hasDetailsMode" "B@:" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKImageEditPanelDataSourceOverrides
    if queriedSel == sel_imageProperties then pure (maybe 0 (const 1) (_imageProperties rec_))
    else if queriedSel == sel_hasAdjustMode then pure (maybe 0 (const 1) (_hasAdjustMode rec_))
    else if queriedSel == sel_hasEffectsMode then pure (maybe 0 (const 1) (_hasEffectsMode rec_))
    else if queriedSel == sel_hasDetailsMode then pure (maybe 0 (const 1) (_hasDetailsMode rec_))
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
newIKImageEditPanelDataSource :: IKImageEditPanelDataSourceOverrides -> IO RawId
newIKImageEditPanelDataSource overrides = do
  inst <- class_createInstance ikImageEditPanelDataSourceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
