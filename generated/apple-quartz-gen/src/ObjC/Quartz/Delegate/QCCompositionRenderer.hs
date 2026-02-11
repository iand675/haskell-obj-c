{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol QCCompositionRenderer@.
--
-- Usage:
--
-- @
-- delegate <- newQCCompositionRenderer defaultQCCompositionRendererOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Quartz.Delegate.QCCompositionRenderer
  ( QCCompositionRendererOverrides(..)
  , defaultQCCompositionRendererOverrides
  , newQCCompositionRenderer
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

-- | Overrides record for @\@protocol QCCompositionRenderer@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data QCCompositionRendererOverrides = QCCompositionRendererOverrides
  { _attributes :: !(Maybe (IO RawId))
  , _inputKeys :: !(Maybe (IO RawId))
  , _outputKeys :: !(Maybe (IO RawId))
  , _setValue_forInputKey :: !(Maybe (RawId -> RawId -> IO Bool))
  , _valueForInputKey :: !(Maybe (RawId -> IO RawId))
  , _valueForOutputKey :: !(Maybe (RawId -> IO RawId))
  , _valueForOutputKey_ofType :: !(Maybe (RawId -> RawId -> IO RawId))
  , _propertyListFromInputValues :: !(Maybe (IO RawId))
  , _setInputValuesWithPropertyList :: !(Maybe (RawId -> IO ()))
  , _userInfo :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultQCCompositionRendererOverrides :: QCCompositionRendererOverrides
defaultQCCompositionRendererOverrides = QCCompositionRendererOverrides
  { _attributes = Nothing
  , _inputKeys = Nothing
  , _outputKeys = Nothing
  , _setValue_forInputKey = Nothing
  , _valueForInputKey = Nothing
  , _valueForOutputKey = Nothing
  , _valueForOutputKey_ofType = Nothing
  , _propertyListFromInputValues = Nothing
  , _setInputValuesWithPropertyList = Nothing
  , _userInfo = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE qcCompositionRendererDelegateClass #-}
qcCompositionRendererDelegateClass :: Class
qcCompositionRendererDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsQCCompositionRenderer" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_attributes = unSelector (mkSelector "attributes")
      sel_inputKeys = unSelector (mkSelector "inputKeys")
      sel_outputKeys = unSelector (mkSelector "outputKeys")
      sel_setValue_forInputKey = unSelector (mkSelector "setValue:forInputKey:")
      sel_valueForInputKey = unSelector (mkSelector "valueForInputKey:")
      sel_valueForOutputKey = unSelector (mkSelector "valueForOutputKey:")
      sel_valueForOutputKey_ofType = unSelector (mkSelector "valueForOutputKey:ofType:")
      sel_propertyListFromInputValues = unSelector (mkSelector "propertyListFromInputValues")
      sel_setInputValuesWithPropertyList = unSelector (mkSelector "setInputValuesWithPropertyList:")
      sel_userInfo = unSelector (mkSelector "userInfo")
  -- attributes
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCCompositionRendererOverrides
    case _attributes rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "attributes" "@@:" stub_0

  -- inputKeys
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCCompositionRendererOverrides
    case _inputKeys rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "inputKeys" "@@:" stub_1

  -- outputKeys
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCCompositionRendererOverrides
    case _outputKeys rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outputKeys" "@@:" stub_2

  -- setValue:forInputKey:
  stub_3 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCCompositionRendererOverrides
    case _setValue_forInputKey rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "setValue:forInputKey:" "B@:@@" stub_3

  -- valueForInputKey:
  stub_4 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCCompositionRendererOverrides
    case _valueForInputKey rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "valueForInputKey:" "@@:@" stub_4

  -- valueForOutputKey:
  stub_5 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCCompositionRendererOverrides
    case _valueForOutputKey rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "valueForOutputKey:" "@@:@" stub_5

  -- valueForOutputKey:ofType:
  stub_6 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCCompositionRendererOverrides
    case _valueForOutputKey_ofType rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "valueForOutputKey:ofType:" "@@:@@" stub_6

  -- propertyListFromInputValues
  stub_7 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCCompositionRendererOverrides
    case _propertyListFromInputValues rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "propertyListFromInputValues" "@@:" stub_7

  -- setInputValuesWithPropertyList:
  stub_8 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCCompositionRendererOverrides
    case _setInputValuesWithPropertyList rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setInputValuesWithPropertyList:" "v@:@" stub_8

  -- userInfo
  stub_9 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCCompositionRendererOverrides
    case _userInfo rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "userInfo" "@@:" stub_9

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO QCCompositionRendererOverrides
    if queriedSel == sel_attributes then pure (maybe 0 (const 1) (_attributes rec_))
    else if queriedSel == sel_inputKeys then pure (maybe 0 (const 1) (_inputKeys rec_))
    else if queriedSel == sel_outputKeys then pure (maybe 0 (const 1) (_outputKeys rec_))
    else if queriedSel == sel_setValue_forInputKey then pure (maybe 0 (const 1) (_setValue_forInputKey rec_))
    else if queriedSel == sel_valueForInputKey then pure (maybe 0 (const 1) (_valueForInputKey rec_))
    else if queriedSel == sel_valueForOutputKey then pure (maybe 0 (const 1) (_valueForOutputKey rec_))
    else if queriedSel == sel_valueForOutputKey_ofType then pure (maybe 0 (const 1) (_valueForOutputKey_ofType rec_))
    else if queriedSel == sel_propertyListFromInputValues then pure (maybe 0 (const 1) (_propertyListFromInputValues rec_))
    else if queriedSel == sel_setInputValuesWithPropertyList then pure (maybe 0 (const 1) (_setInputValuesWithPropertyList rec_))
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
newQCCompositionRenderer :: QCCompositionRendererOverrides -> IO RawId
newQCCompositionRenderer overrides = do
  inst <- class_createInstance qcCompositionRendererDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
