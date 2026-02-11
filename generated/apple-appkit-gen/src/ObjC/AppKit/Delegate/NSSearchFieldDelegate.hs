{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSSearchFieldDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSSearchFieldDelegate defaultNSSearchFieldDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSSearchFieldDelegate
  ( NSSearchFieldDelegateOverrides(..)
  , defaultNSSearchFieldDelegateOverrides
  , newNSSearchFieldDelegate
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

-- | Overrides record for @\@protocol NSSearchFieldDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSSearchFieldDelegateOverrides = NSSearchFieldDelegateOverrides
  { _searchFieldDidStartSearching :: !(Maybe (RawId -> IO ()))
  , _searchFieldDidEndSearching :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSSearchFieldDelegateOverrides :: NSSearchFieldDelegateOverrides
defaultNSSearchFieldDelegateOverrides = NSSearchFieldDelegateOverrides
  { _searchFieldDidStartSearching = Nothing
  , _searchFieldDidEndSearching = Nothing
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
{-# NOINLINE nsSearchFieldDelegateDelegateClass #-}
nsSearchFieldDelegateDelegateClass :: Class
nsSearchFieldDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSSearchFieldDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_searchFieldDidStartSearching = unSelector (mkSelector "searchFieldDidStartSearching:")
      sel_searchFieldDidEndSearching = unSelector (mkSelector "searchFieldDidEndSearching:")
  -- searchFieldDidStartSearching:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSearchFieldDelegateOverrides
    case _searchFieldDidStartSearching rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "searchFieldDidStartSearching:" "v@:@" stub_0

  -- searchFieldDidEndSearching:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSearchFieldDelegateOverrides
    case _searchFieldDidEndSearching rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "searchFieldDidEndSearching:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSearchFieldDelegateOverrides
    if queriedSel == sel_searchFieldDidStartSearching then pure (maybe 0 (const 1) (_searchFieldDidStartSearching rec_))
    else if queriedSel == sel_searchFieldDidEndSearching then pure (maybe 0 (const 1) (_searchFieldDidEndSearching rec_))
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
newNSSearchFieldDelegate :: NSSearchFieldDelegateOverrides -> IO RawId
newNSSearchFieldDelegate overrides = do
  inst <- class_createInstance nsSearchFieldDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
