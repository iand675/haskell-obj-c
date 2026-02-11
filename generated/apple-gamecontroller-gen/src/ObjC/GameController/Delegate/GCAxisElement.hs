{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GCAxisElement@.
--
-- Usage:
--
-- @
-- delegate <- newGCAxisElement defaultGCAxisElementOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameController.Delegate.GCAxisElement
  ( GCAxisElementOverrides(..)
  , defaultGCAxisElementOverrides
  , newGCAxisElement
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

-- | Overrides record for @\@protocol GCAxisElement@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GCAxisElementOverrides = GCAxisElementOverrides
  { _absoluteInput :: !(Maybe (IO RawId))
  , _relativeInput :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultGCAxisElementOverrides :: GCAxisElementOverrides
defaultGCAxisElementOverrides = GCAxisElementOverrides
  { _absoluteInput = Nothing
  , _relativeInput = Nothing
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
{-# NOINLINE gcAxisElementDelegateClass #-}
gcAxisElementDelegateClass :: Class
gcAxisElementDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGCAxisElement" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_absoluteInput = unSelector (mkSelector "absoluteInput")
      sel_relativeInput = unSelector (mkSelector "relativeInput")
  -- absoluteInput
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCAxisElementOverrides
    case _absoluteInput rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "absoluteInput" "@@:" stub_0

  -- relativeInput
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCAxisElementOverrides
    case _relativeInput rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "relativeInput" "@@:" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCAxisElementOverrides
    if queriedSel == sel_absoluteInput then pure (maybe 0 (const 1) (_absoluteInput rec_))
    else if queriedSel == sel_relativeInput then pure (maybe 0 (const 1) (_relativeInput rec_))
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
newGCAxisElement :: GCAxisElementOverrides -> IO RawId
newGCAxisElement overrides = do
  inst <- class_createInstance gcAxisElementDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
