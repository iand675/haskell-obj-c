{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLFunctionLog@.
--
-- Usage:
--
-- @
-- delegate <- newMTLFunctionLog defaultMTLFunctionLogOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLFunctionLog
  ( MTLFunctionLogOverrides(..)
  , defaultMTLFunctionLogOverrides
  , newMTLFunctionLog
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

-- | Overrides record for @\@protocol MTLFunctionLog@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLFunctionLogOverrides = MTLFunctionLogOverrides
  { _encoderLabel :: !(Maybe (IO RawId))
  , _function :: !(Maybe (IO RawId))
  , _debugLocation :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLFunctionLogOverrides :: MTLFunctionLogOverrides
defaultMTLFunctionLogOverrides = MTLFunctionLogOverrides
  { _encoderLabel = Nothing
  , _function = Nothing
  , _debugLocation = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlFunctionLogDelegateClass #-}
mtlFunctionLogDelegateClass :: Class
mtlFunctionLogDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLFunctionLog" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_encoderLabel = unSelector (mkSelector "encoderLabel")
      sel_function = unSelector (mkSelector "function")
      sel_debugLocation = unSelector (mkSelector "debugLocation")
  -- encoderLabel
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFunctionLogOverrides
    case _encoderLabel rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "encoderLabel" "@@:" stub_0

  -- function
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFunctionLogOverrides
    case _function rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "function" "@@:" stub_1

  -- debugLocation
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFunctionLogOverrides
    case _debugLocation rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "debugLocation" "@@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLFunctionLogOverrides
    if queriedSel == sel_encoderLabel then pure (maybe 0 (const 1) (_encoderLabel rec_))
    else if queriedSel == sel_function then pure (maybe 0 (const 1) (_function rec_))
    else if queriedSel == sel_debugLocation then pure (maybe 0 (const 1) (_debugLocation rec_))
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
newMTLFunctionLog :: MTLFunctionLogOverrides -> IO RawId
newMTLFunctionLog overrides = do
  inst <- class_createInstance mtlFunctionLogDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
