{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MKLocalSearchCompleterDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newMKLocalSearchCompleterDelegate defaultMKLocalSearchCompleterDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MapKit.Delegate.MKLocalSearchCompleterDelegate
  ( MKLocalSearchCompleterDelegateOverrides(..)
  , defaultMKLocalSearchCompleterDelegateOverrides
  , newMKLocalSearchCompleterDelegate
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

-- | Overrides record for @\@protocol MKLocalSearchCompleterDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MKLocalSearchCompleterDelegateOverrides = MKLocalSearchCompleterDelegateOverrides
  { _completerDidUpdateResults :: !(Maybe (RawId -> IO ()))
  , _completer_didFailWithError :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMKLocalSearchCompleterDelegateOverrides :: MKLocalSearchCompleterDelegateOverrides
defaultMKLocalSearchCompleterDelegateOverrides = MKLocalSearchCompleterDelegateOverrides
  { _completerDidUpdateResults = Nothing
  , _completer_didFailWithError = Nothing
  }

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
{-# NOINLINE mkLocalSearchCompleterDelegateDelegateClass #-}
mkLocalSearchCompleterDelegateDelegateClass :: Class
mkLocalSearchCompleterDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMKLocalSearchCompleterDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_completerDidUpdateResults = unSelector (mkSelector "completerDidUpdateResults:")
      sel_completer_didFailWithError = unSelector (mkSelector "completer:didFailWithError:")
  -- completerDidUpdateResults:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKLocalSearchCompleterDelegateOverrides
    case _completerDidUpdateResults rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "completerDidUpdateResults:" "v@:@" stub_0

  -- completer:didFailWithError:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKLocalSearchCompleterDelegateOverrides
    case _completer_didFailWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "completer:didFailWithError:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKLocalSearchCompleterDelegateOverrides
    if queriedSel == sel_completerDidUpdateResults then pure (maybe 0 (const 1) (_completerDidUpdateResults rec_))
    else if queriedSel == sel_completer_didFailWithError then pure (maybe 0 (const 1) (_completer_didFailWithError rec_))
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
newMKLocalSearchCompleterDelegate :: MKLocalSearchCompleterDelegateOverrides -> IO RawId
newMKLocalSearchCompleterDelegate overrides = do
  inst <- class_createInstance mkLocalSearchCompleterDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
