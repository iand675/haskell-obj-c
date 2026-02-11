{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol TKTokenDriverDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newTKTokenDriverDelegate defaultTKTokenDriverDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CryptoTokenKit.Delegate.TKTokenDriverDelegate
  ( TKTokenDriverDelegateOverrides(..)
  , defaultTKTokenDriverDelegateOverrides
  , newTKTokenDriverDelegate
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

-- | Overrides record for @\@protocol TKTokenDriverDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data TKTokenDriverDelegateOverrides = TKTokenDriverDelegateOverrides
  { _tokenDriver_tokenForConfiguration_error :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _tokenDriver_terminateToken :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultTKTokenDriverDelegateOverrides :: TKTokenDriverDelegateOverrides
defaultTKTokenDriverDelegateOverrides = TKTokenDriverDelegateOverrides
  { _tokenDriver_tokenForConfiguration_error = Nothing
  , _tokenDriver_terminateToken = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE tkTokenDriverDelegateDelegateClass #-}
tkTokenDriverDelegateDelegateClass :: Class
tkTokenDriverDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsTKTokenDriverDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_tokenDriver_tokenForConfiguration_error = unSelector (mkSelector "tokenDriver:tokenForConfiguration:error:")
      sel_tokenDriver_terminateToken = unSelector (mkSelector "tokenDriver:terminateToken:")
  -- tokenDriver:tokenForConfiguration:error:
  stub_0 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO TKTokenDriverDelegateOverrides
    case _tokenDriver_tokenForConfiguration_error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tokenDriver:tokenForConfiguration:error:" "@@:@@@" stub_0

  -- tokenDriver:terminateToken:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO TKTokenDriverDelegateOverrides
    case _tokenDriver_terminateToken rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "tokenDriver:terminateToken:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO TKTokenDriverDelegateOverrides
    if queriedSel == sel_tokenDriver_tokenForConfiguration_error then pure (maybe 0 (const 1) (_tokenDriver_tokenForConfiguration_error rec_))
    else if queriedSel == sel_tokenDriver_terminateToken then pure (maybe 0 (const 1) (_tokenDriver_terminateToken rec_))
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
newTKTokenDriverDelegate :: TKTokenDriverDelegateOverrides -> IO RawId
newTKTokenDriverDelegate overrides = do
  inst <- class_createInstance tkTokenDriverDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
