{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSTextInputClient@.
--
-- Usage:
--
-- @
-- delegate <- newNSTextInputClient defaultNSTextInputClientOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSTextInputClient
  ( NSTextInputClientOverrides(..)
  , defaultNSTextInputClientOverrides
  , newNSTextInputClient
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

-- | Overrides record for @\@protocol NSTextInputClient@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSTextInputClientOverrides = NSTextInputClientOverrides
  { _doCommandBySelector :: !(Maybe (Selector -> IO ()))
  , _unmarkText :: !(Maybe (IO ()))
  , _hasMarkedText :: !(Maybe (IO Bool))
  , _validAttributesForMarkedText :: !(Maybe (IO RawId))
  , _attributedString :: !(Maybe (IO RawId))
  , _baselineDeltaForCharacterAtIndex :: !(Maybe (Int -> IO Double))
  , _windowLevel :: !(Maybe (IO Int))
  , _drawsVerticallyForCharacterAtIndex :: !(Maybe (Int -> IO Bool))
  , _supportsAdaptiveImageGlyph :: !(Maybe (IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNSTextInputClientOverrides :: NSTextInputClientOverrides
defaultNSTextInputClientOverrides = NSTextInputClientOverrides
  { _doCommandBySelector = Nothing
  , _unmarkText = Nothing
  , _hasMarkedText = Nothing
  , _validAttributesForMarkedText = Nothing
  , _attributedString = Nothing
  , _baselineDeltaForCharacterAtIndex = Nothing
  , _windowLevel = Nothing
  , _drawsVerticallyForCharacterAtIndex = Nothing
  , _supportsAdaptiveImageGlyph = Nothing
  }

foreign import ccall "wrapper"
  wrap_Q_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO CULong))

foreign import ccall "wrapper"
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))

foreign import ccall "wrapper"
  wrap_Q_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO CDouble))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_sel_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsTextInputClientDelegateClass #-}
nsTextInputClientDelegateClass :: Class
nsTextInputClientDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSTextInputClient" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_doCommandBySelector = unSelector (mkSelector "doCommandBySelector:")
      sel_unmarkText = unSelector (mkSelector "unmarkText")
      sel_hasMarkedText = unSelector (mkSelector "hasMarkedText")
      sel_validAttributesForMarkedText = unSelector (mkSelector "validAttributesForMarkedText")
      sel_attributedString = unSelector (mkSelector "attributedString")
      sel_baselineDeltaForCharacterAtIndex = unSelector (mkSelector "baselineDeltaForCharacterAtIndex:")
      sel_windowLevel = unSelector (mkSelector "windowLevel")
      sel_drawsVerticallyForCharacterAtIndex = unSelector (mkSelector "drawsVerticallyForCharacterAtIndex:")
      sel_supportsAdaptiveImageGlyph = unSelector (mkSelector "supportsAdaptiveImageGlyph")
  -- doCommandBySelector:
  stub_0 <- wrap_sel_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextInputClientOverrides
    case _doCommandBySelector rec_ of
      Nothing -> pure ()
      Just f -> f (Selector arg0)
  addObjCMethod cls "doCommandBySelector:" "v@::" stub_0

  -- unmarkText
  stub_1 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextInputClientOverrides
    case _unmarkText rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "unmarkText" "v@:" stub_1

  -- hasMarkedText
  stub_2 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextInputClientOverrides
    case _hasMarkedText rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "hasMarkedText" "B@:" stub_2

  -- validAttributesForMarkedText
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextInputClientOverrides
    case _validAttributesForMarkedText rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "validAttributesForMarkedText" "@@:" stub_3

  -- attributedString
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextInputClientOverrides
    case _attributedString rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "attributedString" "@@:" stub_4

  -- baselineDeltaForCharacterAtIndex:
  stub_5 <- wrap_Q_d $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextInputClientOverrides
    case _baselineDeltaForCharacterAtIndex rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f (fromIntegral arg0)
        pure (realToFrac r)
  addObjCMethod cls "baselineDeltaForCharacterAtIndex:" "d@:Q" stub_5

  -- windowLevel
  stub_6 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextInputClientOverrides
    case _windowLevel rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "windowLevel" "q@:" stub_6

  -- drawsVerticallyForCharacterAtIndex:
  stub_7 <- wrap_Q_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextInputClientOverrides
    case _drawsVerticallyForCharacterAtIndex rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (fromIntegral arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "drawsVerticallyForCharacterAtIndex:" "B@:Q" stub_7

  -- supportsAdaptiveImageGlyph
  stub_8 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextInputClientOverrides
    case _supportsAdaptiveImageGlyph rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "supportsAdaptiveImageGlyph" "B@:" stub_8

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextInputClientOverrides
    if queriedSel == sel_doCommandBySelector then pure (maybe 0 (const 1) (_doCommandBySelector rec_))
    else if queriedSel == sel_unmarkText then pure (maybe 0 (const 1) (_unmarkText rec_))
    else if queriedSel == sel_hasMarkedText then pure (maybe 0 (const 1) (_hasMarkedText rec_))
    else if queriedSel == sel_validAttributesForMarkedText then pure (maybe 0 (const 1) (_validAttributesForMarkedText rec_))
    else if queriedSel == sel_attributedString then pure (maybe 0 (const 1) (_attributedString rec_))
    else if queriedSel == sel_baselineDeltaForCharacterAtIndex then pure (maybe 0 (const 1) (_baselineDeltaForCharacterAtIndex rec_))
    else if queriedSel == sel_windowLevel then pure (maybe 0 (const 1) (_windowLevel rec_))
    else if queriedSel == sel_drawsVerticallyForCharacterAtIndex then pure (maybe 0 (const 1) (_drawsVerticallyForCharacterAtIndex rec_))
    else if queriedSel == sel_supportsAdaptiveImageGlyph then pure (maybe 0 (const 1) (_supportsAdaptiveImageGlyph rec_))
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
newNSTextInputClient :: NSTextInputClientOverrides -> IO RawId
newNSTextInputClient overrides = do
  inst <- class_createInstance nsTextInputClientDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
