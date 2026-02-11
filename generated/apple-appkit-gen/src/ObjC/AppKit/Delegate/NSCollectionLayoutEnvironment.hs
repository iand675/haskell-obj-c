{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSCollectionLayoutEnvironment@.
--
-- Usage:
--
-- @
-- delegate <- newNSCollectionLayoutEnvironment defaultNSCollectionLayoutEnvironmentOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSCollectionLayoutEnvironment
  ( NSCollectionLayoutEnvironmentOverrides(..)
  , defaultNSCollectionLayoutEnvironmentOverrides
  , newNSCollectionLayoutEnvironment
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

-- | Overrides record for @\@protocol NSCollectionLayoutEnvironment@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSCollectionLayoutEnvironmentOverrides = NSCollectionLayoutEnvironmentOverrides
  { _container :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSCollectionLayoutEnvironmentOverrides :: NSCollectionLayoutEnvironmentOverrides
defaultNSCollectionLayoutEnvironmentOverrides = NSCollectionLayoutEnvironmentOverrides
  { _container = Nothing
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
{-# NOINLINE nsCollectionLayoutEnvironmentDelegateClass #-}
nsCollectionLayoutEnvironmentDelegateClass :: Class
nsCollectionLayoutEnvironmentDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSCollectionLayoutEnvironment" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_container = unSelector (mkSelector "container")
  -- container
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionLayoutEnvironmentOverrides
    case _container rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "container" "@@:" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionLayoutEnvironmentOverrides
    if queriedSel == sel_container then pure (maybe 0 (const 1) (_container rec_))
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
newNSCollectionLayoutEnvironment :: NSCollectionLayoutEnvironmentOverrides -> IO RawId
newNSCollectionLayoutEnvironment overrides = do
  inst <- class_createInstance nsCollectionLayoutEnvironmentDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
