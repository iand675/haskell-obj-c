{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSTextInput@.
--
-- Usage:
--
-- @
-- delegate <- newNSTextInput defaultNSTextInputOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSTextInput
  ( NSTextInputOverrides(..)
  , defaultNSTextInputOverrides
  , newNSTextInput
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

-- | Overrides record for @\@protocol NSTextInput@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSTextInputOverrides = NSTextInputOverrides
  { _insertText :: !(Maybe (RawId -> IO ()))
  , _doCommandBySelector :: !(Maybe (Sel -> IO ()))
  , _unmarkText :: !(Maybe (IO ()))
  , _hasMarkedText :: !(Maybe (IO Bool))
  , _conversationIdentifier :: !(Maybe (IO Int))
  , _validAttributesForMarkedText :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSTextInputOverrides :: NSTextInputOverrides
defaultNSTextInputOverrides = NSTextInputOverrides
  { _insertText = Nothing
  , _doCommandBySelector = Nothing
  , _unmarkText = Nothing
  , _hasMarkedText = Nothing
  , _conversationIdentifier = Nothing
  , _validAttributesForMarkedText = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))

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
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsTextInputDelegateClass #-}
nsTextInputDelegateClass :: Class
nsTextInputDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSTextInput" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_insertText = unSelector (mkSelector "insertText:")
      sel_doCommandBySelector = unSelector (mkSelector "doCommandBySelector:")
      sel_unmarkText = unSelector (mkSelector "unmarkText")
      sel_hasMarkedText = unSelector (mkSelector "hasMarkedText")
      sel_conversationIdentifier = unSelector (mkSelector "conversationIdentifier")
      sel_validAttributesForMarkedText = unSelector (mkSelector "validAttributesForMarkedText")
  -- insertText:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextInputOverrides
    case _insertText rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "insertText:" "v@:@" stub_0

  -- doCommandBySelector:
  stub_1 <- wrap_sel_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextInputOverrides
    case _doCommandBySelector rec_ of
      Nothing -> pure ()
      Just f -> f (Selector arg0)
  addObjCMethod cls "doCommandBySelector:" "v@::" stub_1

  -- unmarkText
  stub_2 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextInputOverrides
    case _unmarkText rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "unmarkText" "v@:" stub_2

  -- hasMarkedText
  stub_3 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextInputOverrides
    case _hasMarkedText rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "hasMarkedText" "B@:" stub_3

  -- conversationIdentifier
  stub_4 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextInputOverrides
    case _conversationIdentifier rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "conversationIdentifier" "q@:" stub_4

  -- validAttributesForMarkedText
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextInputOverrides
    case _validAttributesForMarkedText rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "validAttributesForMarkedText" "@@:" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextInputOverrides
    if queriedSel == sel_insertText then pure (maybe 0 (const 1) (_insertText rec_))
    else if queriedSel == sel_doCommandBySelector then pure (maybe 0 (const 1) (_doCommandBySelector rec_))
    else if queriedSel == sel_unmarkText then pure (maybe 0 (const 1) (_unmarkText rec_))
    else if queriedSel == sel_hasMarkedText then pure (maybe 0 (const 1) (_hasMarkedText rec_))
    else if queriedSel == sel_conversationIdentifier then pure (maybe 0 (const 1) (_conversationIdentifier rec_))
    else if queriedSel == sel_validAttributesForMarkedText then pure (maybe 0 (const 1) (_validAttributesForMarkedText rec_))
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
newNSTextInput :: NSTextInputOverrides -> IO RawId
newNSTextInput overrides = do
  inst <- class_createInstance nsTextInputDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
