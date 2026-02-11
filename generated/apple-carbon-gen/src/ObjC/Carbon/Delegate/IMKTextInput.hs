{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol IMKTextInput@.
--
-- Usage:
--
-- @
-- delegate <- newIMKTextInput defaultIMKTextInputOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Carbon.Delegate.IMKTextInput
  ( IMKTextInputOverrides(..)
  , defaultIMKTextInputOverrides
  , newIMKTextInput
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

-- | Overrides record for @\@protocol IMKTextInput@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data IMKTextInputOverrides = IMKTextInputOverrides
  { _length :: !(Maybe (IO Int))
  , _attributesForCharacterIndex_lineHeightRectangle :: !(Maybe (Int -> RawId -> IO RawId))
  , _validAttributesForMarkedText :: !(Maybe (IO RawId))
  , _overrideKeyboardWithKeyboardNamed :: !(Maybe (RawId -> IO ()))
  , _selectInputMode :: !(Maybe (RawId -> IO ()))
  , _supportsUnicode :: !(Maybe (IO Bool))
  , _bundleIdentifier :: !(Maybe (IO RawId))
  , _windowLevel :: !(Maybe (IO Int))
  , _supportsProperty :: !(Maybe (Int -> IO Bool))
  , _uniqueClientIdentifierString :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultIMKTextInputOverrides :: IMKTextInputOverrides
defaultIMKTextInputOverrides = IMKTextInputOverrides
  { _length = Nothing
  , _attributesForCharacterIndex_lineHeightRectangle = Nothing
  , _validAttributesForMarkedText = Nothing
  , _overrideKeyboardWithKeyboardNamed = Nothing
  , _selectInputMode = Nothing
  , _supportsUnicode = Nothing
  , _bundleIdentifier = Nothing
  , _windowLevel = Nothing
  , _supportsProperty = Nothing
  , _uniqueClientIdentifierString = Nothing
  }

foreign import ccall "wrapper"
  wrap_I_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CUInt -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CUInt -> IO CULong))

foreign import ccall "wrapper"
  wrap_i
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CInt)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CInt))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_Q_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE imkTextInputDelegateClass #-}
imkTextInputDelegateClass :: Class
imkTextInputDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsIMKTextInput" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_length = unSelector (mkSelector "length")
      sel_attributesForCharacterIndex_lineHeightRectangle = unSelector (mkSelector "attributesForCharacterIndex:lineHeightRectangle:")
      sel_validAttributesForMarkedText = unSelector (mkSelector "validAttributesForMarkedText")
      sel_overrideKeyboardWithKeyboardNamed = unSelector (mkSelector "overrideKeyboardWithKeyboardNamed:")
      sel_selectInputMode = unSelector (mkSelector "selectInputMode:")
      sel_supportsUnicode = unSelector (mkSelector "supportsUnicode")
      sel_bundleIdentifier = unSelector (mkSelector "bundleIdentifier")
      sel_windowLevel = unSelector (mkSelector "windowLevel")
      sel_supportsProperty = unSelector (mkSelector "supportsProperty:")
      sel_uniqueClientIdentifierString = unSelector (mkSelector "uniqueClientIdentifierString")
  -- length
  stub_0 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKTextInputOverrides
    case _length rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "length" "q@:" stub_0

  -- attributesForCharacterIndex:lineHeightRectangle:
  stub_1 <- wrap_Q_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKTextInputOverrides
    case _attributesForCharacterIndex_lineHeightRectangle rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (fromIntegral arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "attributesForCharacterIndex:lineHeightRectangle:" "@@:Q@" stub_1

  -- validAttributesForMarkedText
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKTextInputOverrides
    case _validAttributesForMarkedText rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "validAttributesForMarkedText" "@@:" stub_2

  -- overrideKeyboardWithKeyboardNamed:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKTextInputOverrides
    case _overrideKeyboardWithKeyboardNamed rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "overrideKeyboardWithKeyboardNamed:" "v@:@" stub_3

  -- selectInputMode:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKTextInputOverrides
    case _selectInputMode rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "selectInputMode:" "v@:@" stub_4

  -- supportsUnicode
  stub_5 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKTextInputOverrides
    case _supportsUnicode rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "supportsUnicode" "B@:" stub_5

  -- bundleIdentifier
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKTextInputOverrides
    case _bundleIdentifier rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "bundleIdentifier" "@@:" stub_6

  -- windowLevel
  stub_7 <- wrap_i $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKTextInputOverrides
    case _windowLevel rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "windowLevel" "i@:" stub_7

  -- supportsProperty:
  stub_8 <- wrap_I_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKTextInputOverrides
    case _supportsProperty rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (fromIntegral arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "supportsProperty:" "B@:I" stub_8

  -- uniqueClientIdentifierString
  stub_9 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKTextInputOverrides
    case _uniqueClientIdentifierString rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "uniqueClientIdentifierString" "@@:" stub_9

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IMKTextInputOverrides
    if queriedSel == sel_length then pure (maybe 0 (const 1) (_length rec_))
    else if queriedSel == sel_attributesForCharacterIndex_lineHeightRectangle then pure (maybe 0 (const 1) (_attributesForCharacterIndex_lineHeightRectangle rec_))
    else if queriedSel == sel_validAttributesForMarkedText then pure (maybe 0 (const 1) (_validAttributesForMarkedText rec_))
    else if queriedSel == sel_overrideKeyboardWithKeyboardNamed then pure (maybe 0 (const 1) (_overrideKeyboardWithKeyboardNamed rec_))
    else if queriedSel == sel_selectInputMode then pure (maybe 0 (const 1) (_selectInputMode rec_))
    else if queriedSel == sel_supportsUnicode then pure (maybe 0 (const 1) (_supportsUnicode rec_))
    else if queriedSel == sel_bundleIdentifier then pure (maybe 0 (const 1) (_bundleIdentifier rec_))
    else if queriedSel == sel_windowLevel then pure (maybe 0 (const 1) (_windowLevel rec_))
    else if queriedSel == sel_supportsProperty then pure (maybe 0 (const 1) (_supportsProperty rec_))
    else if queriedSel == sel_uniqueClientIdentifierString then pure (maybe 0 (const 1) (_uniqueClientIdentifierString rec_))
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
newIMKTextInput :: IMKTextInputOverrides -> IO RawId
newIMKTextInput overrides = do
  inst <- class_createInstance imkTextInputDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
