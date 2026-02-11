{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol GCPhysicalInputExtents@.
--
-- Usage:
--
-- @
-- delegate <- newGCPhysicalInputExtents defaultGCPhysicalInputExtentsOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.GameController.Delegate.GCPhysicalInputExtents
  ( GCPhysicalInputExtentsOverrides(..)
  , defaultGCPhysicalInputExtentsOverrides
  , newGCPhysicalInputExtents
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

-- | Overrides record for @\@protocol GCPhysicalInputExtents@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data GCPhysicalInputExtentsOverrides = GCPhysicalInputExtentsOverrides
  { _scaledValue :: !(Maybe (IO Double))
  , _minimumValue :: !(Maybe (IO Double))
  , _maximumValue :: !(Maybe (IO Double))
  }

-- | Default overrides with all methods unimplemented.
defaultGCPhysicalInputExtentsOverrides :: GCPhysicalInputExtentsOverrides
defaultGCPhysicalInputExtentsOverrides = GCPhysicalInputExtentsOverrides
  { _scaledValue = Nothing
  , _minimumValue = Nothing
  , _maximumValue = Nothing
  }

foreign import ccall "wrapper"
  wrap_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE gcPhysicalInputExtentsDelegateClass #-}
gcPhysicalInputExtentsDelegateClass :: Class
gcPhysicalInputExtentsDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsGCPhysicalInputExtents" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_scaledValue = unSelector (mkSelector "scaledValue")
      sel_minimumValue = unSelector (mkSelector "minimumValue")
      sel_maximumValue = unSelector (mkSelector "maximumValue")
  -- scaledValue
  stub_0 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCPhysicalInputExtentsOverrides
    case _scaledValue rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "scaledValue" "d@:" stub_0

  -- minimumValue
  stub_1 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCPhysicalInputExtentsOverrides
    case _minimumValue rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "minimumValue" "d@:" stub_1

  -- maximumValue
  stub_2 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCPhysicalInputExtentsOverrides
    case _maximumValue rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "maximumValue" "d@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO GCPhysicalInputExtentsOverrides
    if queriedSel == sel_scaledValue then pure (maybe 0 (const 1) (_scaledValue rec_))
    else if queriedSel == sel_minimumValue then pure (maybe 0 (const 1) (_minimumValue rec_))
    else if queriedSel == sel_maximumValue then pure (maybe 0 (const 1) (_maximumValue rec_))
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
newGCPhysicalInputExtents :: GCPhysicalInputExtentsOverrides -> IO RawId
newGCPhysicalInputExtents overrides = do
  inst <- class_createInstance gcPhysicalInputExtentsDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
