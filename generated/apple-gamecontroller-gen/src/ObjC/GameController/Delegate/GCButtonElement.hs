{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GCButtonElement@.
--
-- Usage:
--
-- @
-- delegate <- newGCButtonElement defaultGCButtonElementOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameController.Delegate.GCButtonElement
  ( GCButtonElementOverrides(..)
  , defaultGCButtonElementOverrides
  , newGCButtonElement
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

-- | Overrides record for @\@protocol GCButtonElement@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GCButtonElementOverrides = GCButtonElementOverrides
  { _pressedInput :: !(Maybe (IO RawId))
  , _touchedInput :: !(Maybe (IO RawId))
  , _forceInput :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultGCButtonElementOverrides :: GCButtonElementOverrides
defaultGCButtonElementOverrides = GCButtonElementOverrides
  { _pressedInput = Nothing
  , _touchedInput = Nothing
  , _forceInput = Nothing
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
{-# NOINLINE gcButtonElementDelegateClass #-}
gcButtonElementDelegateClass :: Class
gcButtonElementDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGCButtonElement" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_pressedInput = unSelector (mkSelector "pressedInput")
      sel_touchedInput = unSelector (mkSelector "touchedInput")
      sel_forceInput = unSelector (mkSelector "forceInput")
  -- pressedInput
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCButtonElementOverrides
    case _pressedInput rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "pressedInput" "@@:" stub_0

  -- touchedInput
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCButtonElementOverrides
    case _touchedInput rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "touchedInput" "@@:" stub_1

  -- forceInput
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCButtonElementOverrides
    case _forceInput rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "forceInput" "@@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCButtonElementOverrides
    if queriedSel == sel_pressedInput then pure (maybe 0 (const 1) (_pressedInput rec_))
    else if queriedSel == sel_touchedInput then pure (maybe 0 (const 1) (_touchedInput rec_))
    else if queriedSel == sel_forceInput then pure (maybe 0 (const 1) (_forceInput rec_))
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
newGCButtonElement :: GCButtonElementOverrides -> IO RawId
newGCButtonElement overrides = do
  inst <- class_createInstance gcButtonElementDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
