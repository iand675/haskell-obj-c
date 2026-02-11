{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol WebDocumentSearching@.
--
-- Usage:
--
-- @
-- delegate <- newWebDocumentSearching defaultWebDocumentSearchingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.WebKit.Delegate.WebDocumentSearching
  ( WebDocumentSearchingOverrides(..)
  , defaultWebDocumentSearchingOverrides
  , newWebDocumentSearching
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

-- | Overrides record for @\@protocol WebDocumentSearching@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data WebDocumentSearchingOverrides = WebDocumentSearchingOverrides
  { _searchFor_direction_caseSensitive_wrap :: !(Maybe (RawId -> Bool -> Bool -> Bool -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultWebDocumentSearchingOverrides :: WebDocumentSearchingOverrides
defaultWebDocumentSearchingOverrides = WebDocumentSearchingOverrides
  { _searchFor_direction_caseSensitive_wrap = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_B_B_B_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> CULong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> CULong -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE webDocumentSearchingDelegateClass #-}
webDocumentSearchingDelegateClass :: Class
webDocumentSearchingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsWebDocumentSearching" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_searchFor_direction_caseSensitive_wrap = unSelector (mkSelector "searchFor:direction:caseSensitive:wrap:")
  -- searchFor:direction:caseSensitive:wrap:
  stub_0 <- wrap_at_B_B_B_B $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebDocumentSearchingOverrides
    case _searchFor_direction_caseSensitive_wrap rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (arg1 /= 0) (arg2 /= 0) (arg3 /= 0)
        pure (if r then 1 else 0)
  addObjCMethod cls "searchFor:direction:caseSensitive:wrap:" "B@:@BBB" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebDocumentSearchingOverrides
    if queriedSel == sel_searchFor_direction_caseSensitive_wrap then pure (maybe 0 (const 1) (_searchFor_direction_caseSensitive_wrap rec_))
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
newWebDocumentSearching :: WebDocumentSearchingOverrides -> IO RawId
newWebDocumentSearching overrides = do
  inst <- class_createInstance webDocumentSearchingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
