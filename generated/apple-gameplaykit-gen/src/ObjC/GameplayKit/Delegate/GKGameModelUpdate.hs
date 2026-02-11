{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GKGameModelUpdate@.
--
-- Usage:
--
-- @
-- delegate <- newGKGameModelUpdate defaultGKGameModelUpdateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameplayKit.Delegate.GKGameModelUpdate
  ( GKGameModelUpdateOverrides(..)
  , defaultGKGameModelUpdateOverrides
  , newGKGameModelUpdate
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

-- | Overrides record for @\@protocol GKGameModelUpdate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GKGameModelUpdateOverrides = GKGameModelUpdateOverrides
  { _value :: !(Maybe (IO Int))
  , _setValue :: !(Maybe (Int -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultGKGameModelUpdateOverrides :: GKGameModelUpdateOverrides
defaultGKGameModelUpdateOverrides = GKGameModelUpdateOverrides
  { _value = Nothing
  , _setValue = Nothing
  }

foreign import ccall "wrapper"
  wrap_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CLong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE gkGameModelUpdateDelegateClass #-}
gkGameModelUpdateDelegateClass :: Class
gkGameModelUpdateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGKGameModelUpdate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_value = unSelector (mkSelector "value")
      sel_setValue = unSelector (mkSelector "setValue:")
  -- value
  stub_0 <- wrap_q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameModelUpdateOverrides
    case _value rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "value" "q@:" stub_0

  -- setValue:
  stub_1 <- wrap_q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameModelUpdateOverrides
    case _setValue rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "setValue:" "v@:q" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GKGameModelUpdateOverrides
    if queriedSel == sel_value then pure (maybe 0 (const 1) (_value rec_))
    else if queriedSel == sel_setValue then pure (maybe 0 (const 1) (_setValue rec_))
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
newGKGameModelUpdate :: GKGameModelUpdateOverrides -> IO RawId
newGKGameModelUpdate overrides = do
  inst <- class_createInstance gkGameModelUpdateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
