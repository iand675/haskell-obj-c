{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GCPhysicalInputSource@.
--
-- Usage:
--
-- @
-- delegate <- newGCPhysicalInputSource defaultGCPhysicalInputSourceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameController.Delegate.GCPhysicalInputSource
  ( GCPhysicalInputSourceOverrides(..)
  , defaultGCPhysicalInputSourceOverrides
  , newGCPhysicalInputSource
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

-- | Overrides record for @\@protocol GCPhysicalInputSource@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GCPhysicalInputSourceOverrides = GCPhysicalInputSourceOverrides
  { _elementAliases :: !(Maybe (IO RawId))
  , _elementLocalizedName :: !(Maybe (IO RawId))
  , _sfSymbolsName :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultGCPhysicalInputSourceOverrides :: GCPhysicalInputSourceOverrides
defaultGCPhysicalInputSourceOverrides = GCPhysicalInputSourceOverrides
  { _elementAliases = Nothing
  , _elementLocalizedName = Nothing
  , _sfSymbolsName = Nothing
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
{-# NOINLINE gcPhysicalInputSourceDelegateClass #-}
gcPhysicalInputSourceDelegateClass :: Class
gcPhysicalInputSourceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGCPhysicalInputSource" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_elementAliases = unSelector (mkSelector "elementAliases")
      sel_elementLocalizedName = unSelector (mkSelector "elementLocalizedName")
      sel_sfSymbolsName = unSelector (mkSelector "sfSymbolsName")
  -- elementAliases
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCPhysicalInputSourceOverrides
    case _elementAliases rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "elementAliases" "@@:" stub_0

  -- elementLocalizedName
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCPhysicalInputSourceOverrides
    case _elementLocalizedName rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "elementLocalizedName" "@@:" stub_1

  -- sfSymbolsName
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCPhysicalInputSourceOverrides
    case _sfSymbolsName rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "sfSymbolsName" "@@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCPhysicalInputSourceOverrides
    if queriedSel == sel_elementAliases then pure (maybe 0 (const 1) (_elementAliases rec_))
    else if queriedSel == sel_elementLocalizedName then pure (maybe 0 (const 1) (_elementLocalizedName rec_))
    else if queriedSel == sel_sfSymbolsName then pure (maybe 0 (const 1) (_sfSymbolsName rec_))
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
newGCPhysicalInputSource :: GCPhysicalInputSourceOverrides -> IO RawId
newGCPhysicalInputSource overrides = do
  inst <- class_createInstance gcPhysicalInputSourceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
