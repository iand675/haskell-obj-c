{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSInputServiceProvider@.
--
-- Usage:
--
-- @
-- delegate <- newNSInputServiceProvider defaultNSInputServiceProviderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSInputServiceProvider
  ( NSInputServiceProviderOverrides(..)
  , defaultNSInputServiceProviderOverrides
  , newNSInputServiceProvider
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

-- | Overrides record for @\@protocol NSInputServiceProvider@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSInputServiceProviderOverrides = NSInputServiceProviderOverrides
  { _insertText_client :: !(Maybe (RawId -> RawId -> IO ()))
  , _doCommandBySelector_client :: !(Maybe (Sel -> RawId -> IO ()))
  , _markedTextAbandoned :: !(Maybe (RawId -> IO ()))
  , _terminate :: !(Maybe (RawId -> IO ()))
  , _canBeDisabled :: !(Maybe (IO Bool))
  , _wantsToInterpretAllKeystrokes :: !(Maybe (IO Bool))
  , _wantsToHandleMouseEvents :: !(Maybe (IO Bool))
  , _wantsToDelayTextChangeNotifications :: !(Maybe (IO Bool))
  , _inputClientBecomeActive :: !(Maybe (RawId -> IO ()))
  , _inputClientResignActive :: !(Maybe (RawId -> IO ()))
  , _inputClientEnabled :: !(Maybe (RawId -> IO ()))
  , _inputClientDisabled :: !(Maybe (RawId -> IO ()))
  , _activeConversationWillChange_fromOldConversation :: !(Maybe (RawId -> Int -> IO ()))
  , _activeConversationChanged_toNewConversation :: !(Maybe (RawId -> Int -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSInputServiceProviderOverrides :: NSInputServiceProviderOverrides
defaultNSInputServiceProviderOverrides = NSInputServiceProviderOverrides
  { _insertText_client = Nothing
  , _doCommandBySelector_client = Nothing
  , _markedTextAbandoned = Nothing
  , _terminate = Nothing
  , _canBeDisabled = Nothing
  , _wantsToInterpretAllKeystrokes = Nothing
  , _wantsToHandleMouseEvents = Nothing
  , _wantsToDelayTextChangeNotifications = Nothing
  , _inputClientBecomeActive = Nothing
  , _inputClientResignActive = Nothing
  , _inputClientEnabled = Nothing
  , _inputClientDisabled = Nothing
  , _activeConversationWillChange_fromOldConversation = Nothing
  , _activeConversationChanged_toNewConversation = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_sel_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsInputServiceProviderDelegateClass #-}
nsInputServiceProviderDelegateClass :: Class
nsInputServiceProviderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSInputServiceProvider" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_insertText_client = unSelector (mkSelector "insertText:client:")
      sel_doCommandBySelector_client = unSelector (mkSelector "doCommandBySelector:client:")
      sel_markedTextAbandoned = unSelector (mkSelector "markedTextAbandoned:")
      sel_terminate = unSelector (mkSelector "terminate:")
      sel_canBeDisabled = unSelector (mkSelector "canBeDisabled")
      sel_wantsToInterpretAllKeystrokes = unSelector (mkSelector "wantsToInterpretAllKeystrokes")
      sel_wantsToHandleMouseEvents = unSelector (mkSelector "wantsToHandleMouseEvents")
      sel_wantsToDelayTextChangeNotifications = unSelector (mkSelector "wantsToDelayTextChangeNotifications")
      sel_inputClientBecomeActive = unSelector (mkSelector "inputClientBecomeActive:")
      sel_inputClientResignActive = unSelector (mkSelector "inputClientResignActive:")
      sel_inputClientEnabled = unSelector (mkSelector "inputClientEnabled:")
      sel_inputClientDisabled = unSelector (mkSelector "inputClientDisabled:")
      sel_activeConversationWillChange_fromOldConversation = unSelector (mkSelector "activeConversationWillChange:fromOldConversation:")
      sel_activeConversationChanged_toNewConversation = unSelector (mkSelector "activeConversationChanged:toNewConversation:")
  -- insertText:client:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSInputServiceProviderOverrides
    case _insertText_client rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "insertText:client:" "v@:@@" stub_0

  -- doCommandBySelector:client:
  stub_1 <- wrap_sel_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSInputServiceProviderOverrides
    case _doCommandBySelector_client rec_ of
      Nothing -> pure ()
      Just f -> f (Selector arg0) (RawId arg1)
  addObjCMethod cls "doCommandBySelector:client:" "v@::@" stub_1

  -- markedTextAbandoned:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSInputServiceProviderOverrides
    case _markedTextAbandoned rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "markedTextAbandoned:" "v@:@" stub_2

  -- terminate:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSInputServiceProviderOverrides
    case _terminate rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "terminate:" "v@:@" stub_3

  -- canBeDisabled
  stub_4 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSInputServiceProviderOverrides
    case _canBeDisabled rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "canBeDisabled" "B@:" stub_4

  -- wantsToInterpretAllKeystrokes
  stub_5 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSInputServiceProviderOverrides
    case _wantsToInterpretAllKeystrokes rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "wantsToInterpretAllKeystrokes" "B@:" stub_5

  -- wantsToHandleMouseEvents
  stub_6 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSInputServiceProviderOverrides
    case _wantsToHandleMouseEvents rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "wantsToHandleMouseEvents" "B@:" stub_6

  -- wantsToDelayTextChangeNotifications
  stub_7 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSInputServiceProviderOverrides
    case _wantsToDelayTextChangeNotifications rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "wantsToDelayTextChangeNotifications" "B@:" stub_7

  -- inputClientBecomeActive:
  stub_8 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSInputServiceProviderOverrides
    case _inputClientBecomeActive rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "inputClientBecomeActive:" "v@:@" stub_8

  -- inputClientResignActive:
  stub_9 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSInputServiceProviderOverrides
    case _inputClientResignActive rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "inputClientResignActive:" "v@:@" stub_9

  -- inputClientEnabled:
  stub_10 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSInputServiceProviderOverrides
    case _inputClientEnabled rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "inputClientEnabled:" "v@:@" stub_10

  -- inputClientDisabled:
  stub_11 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSInputServiceProviderOverrides
    case _inputClientDisabled rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "inputClientDisabled:" "v@:@" stub_11

  -- activeConversationWillChange:fromOldConversation:
  stub_12 <- wrap_at_q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSInputServiceProviderOverrides
    case _activeConversationWillChange_fromOldConversation rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "activeConversationWillChange:fromOldConversation:" "v@:@q" stub_12

  -- activeConversationChanged:toNewConversation:
  stub_13 <- wrap_at_q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSInputServiceProviderOverrides
    case _activeConversationChanged_toNewConversation rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "activeConversationChanged:toNewConversation:" "v@:@q" stub_13

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSInputServiceProviderOverrides
    if queriedSel == sel_insertText_client then pure (maybe 0 (const 1) (_insertText_client rec_))
    else if queriedSel == sel_doCommandBySelector_client then pure (maybe 0 (const 1) (_doCommandBySelector_client rec_))
    else if queriedSel == sel_markedTextAbandoned then pure (maybe 0 (const 1) (_markedTextAbandoned rec_))
    else if queriedSel == sel_terminate then pure (maybe 0 (const 1) (_terminate rec_))
    else if queriedSel == sel_canBeDisabled then pure (maybe 0 (const 1) (_canBeDisabled rec_))
    else if queriedSel == sel_wantsToInterpretAllKeystrokes then pure (maybe 0 (const 1) (_wantsToInterpretAllKeystrokes rec_))
    else if queriedSel == sel_wantsToHandleMouseEvents then pure (maybe 0 (const 1) (_wantsToHandleMouseEvents rec_))
    else if queriedSel == sel_wantsToDelayTextChangeNotifications then pure (maybe 0 (const 1) (_wantsToDelayTextChangeNotifications rec_))
    else if queriedSel == sel_inputClientBecomeActive then pure (maybe 0 (const 1) (_inputClientBecomeActive rec_))
    else if queriedSel == sel_inputClientResignActive then pure (maybe 0 (const 1) (_inputClientResignActive rec_))
    else if queriedSel == sel_inputClientEnabled then pure (maybe 0 (const 1) (_inputClientEnabled rec_))
    else if queriedSel == sel_inputClientDisabled then pure (maybe 0 (const 1) (_inputClientDisabled rec_))
    else if queriedSel == sel_activeConversationWillChange_fromOldConversation then pure (maybe 0 (const 1) (_activeConversationWillChange_fromOldConversation rec_))
    else if queriedSel == sel_activeConversationChanged_toNewConversation then pure (maybe 0 (const 1) (_activeConversationChanged_toNewConversation rec_))
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
newNSInputServiceProvider :: NSInputServiceProviderOverrides -> IO RawId
newNSInputServiceProvider overrides = do
  inst <- class_createInstance nsInputServiceProviderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
