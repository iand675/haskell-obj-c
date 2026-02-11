{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSColorPickingDefault@.
--
-- Usage:
--
-- @
-- delegate <- newNSColorPickingDefault defaultNSColorPickingDefaultOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSColorPickingDefault
  ( NSColorPickingDefaultOverrides(..)
  , defaultNSColorPickingDefaultOverrides
  , newNSColorPickingDefault
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

-- | Overrides record for @\@protocol NSColorPickingDefault@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSColorPickingDefaultOverrides = NSColorPickingDefaultOverrides
  { _initWithPickerMask_colorPanel :: !(Maybe (Int -> RawId -> IO RawId))
  , _provideNewButtonImage :: !(Maybe (IO RawId))
  , _insertNewButtonImage_in :: !(Maybe (RawId -> RawId -> IO ()))
  , _viewSizeChanged :: !(Maybe (RawId -> IO ()))
  , _alphaControlAddedOrRemoved :: !(Maybe (RawId -> IO ()))
  , _attachColorList :: !(Maybe (RawId -> IO ()))
  , _detachColorList :: !(Maybe (RawId -> IO ()))
  , _buttonToolTip :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSColorPickingDefaultOverrides :: NSColorPickingDefaultOverrides
defaultNSColorPickingDefaultOverrides = NSColorPickingDefaultOverrides
  { _initWithPickerMask_colorPanel = Nothing
  , _provideNewButtonImage = Nothing
  , _insertNewButtonImage_in = Nothing
  , _viewSizeChanged = Nothing
  , _alphaControlAddedOrRemoved = Nothing
  , _attachColorList = Nothing
  , _detachColorList = Nothing
  , _buttonToolTip = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_Q_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsColorPickingDefaultDelegateClass #-}
nsColorPickingDefaultDelegateClass :: Class
nsColorPickingDefaultDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSColorPickingDefault" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_initWithPickerMask_colorPanel = unSelector (mkSelector "initWithPickerMask:colorPanel:")
      sel_provideNewButtonImage = unSelector (mkSelector "provideNewButtonImage")
      sel_insertNewButtonImage_in = unSelector (mkSelector "insertNewButtonImage:in:")
      sel_viewSizeChanged = unSelector (mkSelector "viewSizeChanged:")
      sel_alphaControlAddedOrRemoved = unSelector (mkSelector "alphaControlAddedOrRemoved:")
      sel_attachColorList = unSelector (mkSelector "attachColorList:")
      sel_detachColorList = unSelector (mkSelector "detachColorList:")
      sel_buttonToolTip = unSelector (mkSelector "buttonToolTip")
  -- initWithPickerMask:colorPanel:
  stub_0 <- wrap_Q_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSColorPickingDefaultOverrides
    case _initWithPickerMask_colorPanel rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (fromIntegral arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "initWithPickerMask:colorPanel:" "@@:Q@" stub_0

  -- provideNewButtonImage
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSColorPickingDefaultOverrides
    case _provideNewButtonImage rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "provideNewButtonImage" "@@:" stub_1

  -- insertNewButtonImage:in:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSColorPickingDefaultOverrides
    case _insertNewButtonImage_in rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "insertNewButtonImage:in:" "v@:@@" stub_2

  -- viewSizeChanged:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSColorPickingDefaultOverrides
    case _viewSizeChanged rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "viewSizeChanged:" "v@:@" stub_3

  -- alphaControlAddedOrRemoved:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSColorPickingDefaultOverrides
    case _alphaControlAddedOrRemoved rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "alphaControlAddedOrRemoved:" "v@:@" stub_4

  -- attachColorList:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSColorPickingDefaultOverrides
    case _attachColorList rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "attachColorList:" "v@:@" stub_5

  -- detachColorList:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSColorPickingDefaultOverrides
    case _detachColorList rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "detachColorList:" "v@:@" stub_6

  -- buttonToolTip
  stub_7 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSColorPickingDefaultOverrides
    case _buttonToolTip rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "buttonToolTip" "@@:" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSColorPickingDefaultOverrides
    if queriedSel == sel_initWithPickerMask_colorPanel then pure (maybe 0 (const 1) (_initWithPickerMask_colorPanel rec_))
    else if queriedSel == sel_provideNewButtonImage then pure (maybe 0 (const 1) (_provideNewButtonImage rec_))
    else if queriedSel == sel_insertNewButtonImage_in then pure (maybe 0 (const 1) (_insertNewButtonImage_in rec_))
    else if queriedSel == sel_viewSizeChanged then pure (maybe 0 (const 1) (_viewSizeChanged rec_))
    else if queriedSel == sel_alphaControlAddedOrRemoved then pure (maybe 0 (const 1) (_alphaControlAddedOrRemoved rec_))
    else if queriedSel == sel_attachColorList then pure (maybe 0 (const 1) (_attachColorList rec_))
    else if queriedSel == sel_detachColorList then pure (maybe 0 (const 1) (_detachColorList rec_))
    else if queriedSel == sel_buttonToolTip then pure (maybe 0 (const 1) (_buttonToolTip rec_))
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
newNSColorPickingDefault :: NSColorPickingDefaultOverrides -> IO RawId
newNSColorPickingDefault overrides = do
  inst <- class_createInstance nsColorPickingDefaultDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
