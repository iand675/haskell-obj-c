{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MKMapItemDetailViewControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newMKMapItemDetailViewControllerDelegate defaultMKMapItemDetailViewControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MapKit.Delegate.MKMapItemDetailViewControllerDelegate
  ( MKMapItemDetailViewControllerDelegateOverrides(..)
  , defaultMKMapItemDetailViewControllerDelegateOverrides
  , newMKMapItemDetailViewControllerDelegate
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

-- | Overrides record for @\@protocol MKMapItemDetailViewControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MKMapItemDetailViewControllerDelegateOverrides = MKMapItemDetailViewControllerDelegateOverrides
  { _mapItemDetailViewControllerDidFinish :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMKMapItemDetailViewControllerDelegateOverrides :: MKMapItemDetailViewControllerDelegateOverrides
defaultMKMapItemDetailViewControllerDelegateOverrides = MKMapItemDetailViewControllerDelegateOverrides
  { _mapItemDetailViewControllerDidFinish = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mkMapItemDetailViewControllerDelegateDelegateClass #-}
mkMapItemDetailViewControllerDelegateDelegateClass :: Class
mkMapItemDetailViewControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMKMapItemDetailViewControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_mapItemDetailViewControllerDidFinish = unSelector (mkSelector "mapItemDetailViewControllerDidFinish:")
  -- mapItemDetailViewControllerDidFinish:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapItemDetailViewControllerDelegateOverrides
    case _mapItemDetailViewControllerDidFinish rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "mapItemDetailViewControllerDidFinish:" "v@:@" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKMapItemDetailViewControllerDelegateOverrides
    if queriedSel == sel_mapItemDetailViewControllerDidFinish then pure (maybe 0 (const 1) (_mapItemDetailViewControllerDidFinish rec_))
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
newMKMapItemDetailViewControllerDelegate :: MKMapItemDetailViewControllerDelegateOverrides -> IO RawId
newMKMapItemDetailViewControllerDelegate overrides = do
  inst <- class_createInstance mkMapItemDetailViewControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
