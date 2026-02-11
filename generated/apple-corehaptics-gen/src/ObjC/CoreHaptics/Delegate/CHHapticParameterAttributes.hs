{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CHHapticParameterAttributes@.
--
-- Usage:
--
-- @
-- delegate <- newCHHapticParameterAttributes defaultCHHapticParameterAttributesOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CoreHaptics.Delegate.CHHapticParameterAttributes
  ( CHHapticParameterAttributesOverrides(..)
  , defaultCHHapticParameterAttributesOverrides
  , newCHHapticParameterAttributes
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

-- | Overrides record for @\@protocol CHHapticParameterAttributes@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CHHapticParameterAttributesOverrides = CHHapticParameterAttributesOverrides
  { _minValue :: !(Maybe (IO Float))
  , _maxValue :: !(Maybe (IO Float))
  , _defaultValue :: !(Maybe (IO Float))
  }

-- | Default overrides with all methods unimplemented.
defaultCHHapticParameterAttributesOverrides :: CHHapticParameterAttributesOverrides
defaultCHHapticParameterAttributesOverrides = CHHapticParameterAttributesOverrides
  { _minValue = Nothing
  , _maxValue = Nothing
  , _defaultValue = Nothing
  }

foreign import ccall "wrapper"
  wrap_f
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CFloat)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CFloat))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE chHapticParameterAttributesDelegateClass #-}
chHapticParameterAttributesDelegateClass :: Class
chHapticParameterAttributesDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCHHapticParameterAttributes" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_minValue = unSelector (mkSelector "minValue")
      sel_maxValue = unSelector (mkSelector "maxValue")
      sel_defaultValue = unSelector (mkSelector "defaultValue")
  -- minValue
  stub_0 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticParameterAttributesOverrides
    case _minValue rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "minValue" "f@:" stub_0

  -- maxValue
  stub_1 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticParameterAttributesOverrides
    case _maxValue rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "maxValue" "f@:" stub_1

  -- defaultValue
  stub_2 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticParameterAttributesOverrides
    case _defaultValue rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "defaultValue" "f@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CHHapticParameterAttributesOverrides
    if queriedSel == sel_minValue then pure (maybe 0 (const 1) (_minValue rec_))
    else if queriedSel == sel_maxValue then pure (maybe 0 (const 1) (_maxValue rec_))
    else if queriedSel == sel_defaultValue then pure (maybe 0 (const 1) (_defaultValue rec_))
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
newCHHapticParameterAttributes :: CHHapticParameterAttributesOverrides -> IO RawId
newCHHapticParameterAttributes overrides = do
  inst <- class_createInstance chHapticParameterAttributesDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
