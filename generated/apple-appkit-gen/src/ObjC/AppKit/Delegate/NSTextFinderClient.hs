{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSTextFinderClient@.
--
-- Usage:
--
-- @
-- delegate <- newNSTextFinderClient defaultNSTextFinderClientOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSTextFinderClient
  ( NSTextFinderClientOverrides(..)
  , defaultNSTextFinderClientOverrides
  , newNSTextFinderClient
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

-- | Overrides record for @\@protocol NSTextFinderClient@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSTextFinderClientOverrides = NSTextFinderClientOverrides
  { _stringAtIndex_effectiveRange_endsWithSearchBoundary :: !(Maybe (Int -> RawId -> RawId -> IO RawId))
  , _stringLength :: !(Maybe (IO Int))
  , _shouldReplaceCharactersInRanges_withStrings :: !(Maybe (RawId -> RawId -> IO Bool))
  , _didReplaceCharacters :: !(Maybe (IO ()))
  , _contentViewAtIndex_effectiveCharacterRange :: !(Maybe (Int -> RawId -> IO RawId))
  , _selectable :: !(Maybe (IO Bool))
  , _allowsMultipleSelection :: !(Maybe (IO Bool))
  , _editable :: !(Maybe (IO Bool))
  , _string :: !(Maybe (IO RawId))
  , _selectedRanges :: !(Maybe (IO RawId))
  , _setSelectedRanges :: !(Maybe (RawId -> IO ()))
  , _visibleCharacterRanges :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSTextFinderClientOverrides :: NSTextFinderClientOverrides
defaultNSTextFinderClientOverrides = NSTextFinderClientOverrides
  { _stringAtIndex_effectiveRange_endsWithSearchBoundary = Nothing
  , _stringLength = Nothing
  , _shouldReplaceCharactersInRanges_withStrings = Nothing
  , _didReplaceCharacters = Nothing
  , _contentViewAtIndex_effectiveCharacterRange = Nothing
  , _selectable = Nothing
  , _allowsMultipleSelection = Nothing
  , _editable = Nothing
  , _string = Nothing
  , _selectedRanges = Nothing
  , _setSelectedRanges = Nothing
  , _visibleCharacterRanges = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_Q_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_Q_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsTextFinderClientDelegateClass #-}
nsTextFinderClientDelegateClass :: Class
nsTextFinderClientDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSTextFinderClient" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_stringAtIndex_effectiveRange_endsWithSearchBoundary = unSelector (mkSelector "stringAtIndex:effectiveRange:endsWithSearchBoundary:")
      sel_stringLength = unSelector (mkSelector "stringLength")
      sel_shouldReplaceCharactersInRanges_withStrings = unSelector (mkSelector "shouldReplaceCharactersInRanges:withStrings:")
      sel_didReplaceCharacters = unSelector (mkSelector "didReplaceCharacters")
      sel_contentViewAtIndex_effectiveCharacterRange = unSelector (mkSelector "contentViewAtIndex:effectiveCharacterRange:")
      sel_selectable = unSelector (mkSelector "selectable")
      sel_allowsMultipleSelection = unSelector (mkSelector "allowsMultipleSelection")
      sel_editable = unSelector (mkSelector "editable")
      sel_string = unSelector (mkSelector "string")
      sel_selectedRanges = unSelector (mkSelector "selectedRanges")
      sel_setSelectedRanges = unSelector (mkSelector "setSelectedRanges:")
      sel_visibleCharacterRanges = unSelector (mkSelector "visibleCharacterRanges")
  -- stringAtIndex:effectiveRange:endsWithSearchBoundary:
  stub_0 <- wrap_Q_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderClientOverrides
    case _stringAtIndex_effectiveRange_endsWithSearchBoundary rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (fromIntegral arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "stringAtIndex:effectiveRange:endsWithSearchBoundary:" "@@:Q@@" stub_0

  -- stringLength
  stub_1 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderClientOverrides
    case _stringLength rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "stringLength" "Q@:" stub_1

  -- shouldReplaceCharactersInRanges:withStrings:
  stub_2 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderClientOverrides
    case _shouldReplaceCharactersInRanges_withStrings rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldReplaceCharactersInRanges:withStrings:" "B@:@@" stub_2

  -- didReplaceCharacters
  stub_3 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderClientOverrides
    case _didReplaceCharacters rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "didReplaceCharacters" "v@:" stub_3

  -- contentViewAtIndex:effectiveCharacterRange:
  stub_4 <- wrap_Q_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderClientOverrides
    case _contentViewAtIndex_effectiveCharacterRange rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (fromIntegral arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "contentViewAtIndex:effectiveCharacterRange:" "@@:Q@" stub_4

  -- selectable
  stub_5 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderClientOverrides
    case _selectable rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "selectable" "B@:" stub_5

  -- allowsMultipleSelection
  stub_6 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderClientOverrides
    case _allowsMultipleSelection rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "allowsMultipleSelection" "B@:" stub_6

  -- editable
  stub_7 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderClientOverrides
    case _editable rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "editable" "B@:" stub_7

  -- string
  stub_8 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderClientOverrides
    case _string rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "string" "@@:" stub_8

  -- selectedRanges
  stub_9 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderClientOverrides
    case _selectedRanges rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "selectedRanges" "@@:" stub_9

  -- setSelectedRanges:
  stub_10 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderClientOverrides
    case _setSelectedRanges rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setSelectedRanges:" "v@:@" stub_10

  -- visibleCharacterRanges
  stub_11 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderClientOverrides
    case _visibleCharacterRanges rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "visibleCharacterRanges" "@@:" stub_11

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderClientOverrides
    if queriedSel == sel_stringAtIndex_effectiveRange_endsWithSearchBoundary then pure (maybe 0 (const 1) (_stringAtIndex_effectiveRange_endsWithSearchBoundary rec_))
    else if queriedSel == sel_stringLength then pure (maybe 0 (const 1) (_stringLength rec_))
    else if queriedSel == sel_shouldReplaceCharactersInRanges_withStrings then pure (maybe 0 (const 1) (_shouldReplaceCharactersInRanges_withStrings rec_))
    else if queriedSel == sel_didReplaceCharacters then pure (maybe 0 (const 1) (_didReplaceCharacters rec_))
    else if queriedSel == sel_contentViewAtIndex_effectiveCharacterRange then pure (maybe 0 (const 1) (_contentViewAtIndex_effectiveCharacterRange rec_))
    else if queriedSel == sel_selectable then pure (maybe 0 (const 1) (_selectable rec_))
    else if queriedSel == sel_allowsMultipleSelection then pure (maybe 0 (const 1) (_allowsMultipleSelection rec_))
    else if queriedSel == sel_editable then pure (maybe 0 (const 1) (_editable rec_))
    else if queriedSel == sel_string then pure (maybe 0 (const 1) (_string rec_))
    else if queriedSel == sel_selectedRanges then pure (maybe 0 (const 1) (_selectedRanges rec_))
    else if queriedSel == sel_setSelectedRanges then pure (maybe 0 (const 1) (_setSelectedRanges rec_))
    else if queriedSel == sel_visibleCharacterRanges then pure (maybe 0 (const 1) (_visibleCharacterRanges rec_))
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
newNSTextFinderClient :: NSTextFinderClientOverrides -> IO RawId
newNSTextFinderClient overrides = do
  inst <- class_createInstance nsTextFinderClientDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
