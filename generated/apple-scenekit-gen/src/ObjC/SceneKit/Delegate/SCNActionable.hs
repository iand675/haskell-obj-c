{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SCNActionable@.
--
-- Usage:
--
-- @
-- delegate <- newSCNActionable defaultSCNActionableOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SceneKit.Delegate.SCNActionable
  ( SCNActionableOverrides(..)
  , defaultSCNActionableOverrides
  , newSCNActionable
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

-- | Overrides record for @\@protocol SCNActionable@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SCNActionableOverrides = SCNActionableOverrides
  { _runAction :: !(Maybe (RawId -> IO ()))
  , _runAction_forKey :: !(Maybe (RawId -> RawId -> IO ()))
  , _actionForKey :: !(Maybe (RawId -> IO RawId))
  , _removeActionForKey :: !(Maybe (RawId -> IO ()))
  , _removeAllActions :: !(Maybe (IO ()))
  , _hasActions :: !(Maybe (IO Bool))
  , _actionKeys :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultSCNActionableOverrides :: SCNActionableOverrides
defaultSCNActionableOverrides = SCNActionableOverrides
  { _runAction = Nothing
  , _runAction_forKey = Nothing
  , _actionForKey = Nothing
  , _removeActionForKey = Nothing
  , _removeAllActions = Nothing
  , _hasActions = Nothing
  , _actionKeys = Nothing
  }

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
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE scnActionableDelegateClass #-}
scnActionableDelegateClass :: Class
scnActionableDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSCNActionable" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_runAction = unSelector (mkSelector "runAction:")
      sel_runAction_forKey = unSelector (mkSelector "runAction:forKey:")
      sel_actionForKey = unSelector (mkSelector "actionForKey:")
      sel_removeActionForKey = unSelector (mkSelector "removeActionForKey:")
      sel_removeAllActions = unSelector (mkSelector "removeAllActions")
      sel_hasActions = unSelector (mkSelector "hasActions")
      sel_actionKeys = unSelector (mkSelector "actionKeys")
  -- runAction:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNActionableOverrides
    case _runAction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "runAction:" "v@:@" stub_0

  -- runAction:forKey:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNActionableOverrides
    case _runAction_forKey rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "runAction:forKey:" "v@:@@" stub_1

  -- actionForKey:
  stub_2 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNActionableOverrides
    case _actionForKey rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "actionForKey:" "@@:@" stub_2

  -- removeActionForKey:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNActionableOverrides
    case _removeActionForKey rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "removeActionForKey:" "v@:@" stub_3

  -- removeAllActions
  stub_4 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNActionableOverrides
    case _removeAllActions rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "removeAllActions" "v@:" stub_4

  -- hasActions
  stub_5 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNActionableOverrides
    case _hasActions rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "hasActions" "B@:" stub_5

  -- actionKeys
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNActionableOverrides
    case _actionKeys rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "actionKeys" "@@:" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNActionableOverrides
    if queriedSel == sel_runAction then pure (maybe 0 (const 1) (_runAction rec_))
    else if queriedSel == sel_runAction_forKey then pure (maybe 0 (const 1) (_runAction_forKey rec_))
    else if queriedSel == sel_actionForKey then pure (maybe 0 (const 1) (_actionForKey rec_))
    else if queriedSel == sel_removeActionForKey then pure (maybe 0 (const 1) (_removeActionForKey rec_))
    else if queriedSel == sel_removeAllActions then pure (maybe 0 (const 1) (_removeAllActions rec_))
    else if queriedSel == sel_hasActions then pure (maybe 0 (const 1) (_hasActions rec_))
    else if queriedSel == sel_actionKeys then pure (maybe 0 (const 1) (_actionKeys rec_))
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
newSCNActionable :: SCNActionableOverrides -> IO RawId
newSCNActionable overrides = do
  inst <- class_createInstance scnActionableDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
