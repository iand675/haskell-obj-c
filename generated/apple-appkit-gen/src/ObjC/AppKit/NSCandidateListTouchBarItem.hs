{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCandidateListTouchBarItem@.
module ObjC.AppKit.NSCandidateListTouchBarItem
  ( NSCandidateListTouchBarItem
  , IsNSCandidateListTouchBarItem(..)
  , updateWithInsertionPointVisibility
  , setCandidates_forSelectedRange_inString
  , client
  , setClient
  , delegate
  , setDelegate
  , collapsed
  , setCollapsed
  , allowsCollapsing
  , setAllowsCollapsing
  , candidateListVisible
  , allowsTextInputContextCandidates
  , setAllowsTextInputContextCandidates
  , attributedStringForCandidate
  , setAttributedStringForCandidate
  , candidates
  , customizationLabel
  , setCustomizationLabel
  , updateWithInsertionPointVisibilitySelector
  , setCandidates_forSelectedRange_inStringSelector
  , clientSelector
  , setClientSelector
  , delegateSelector
  , setDelegateSelector
  , collapsedSelector
  , setCollapsedSelector
  , allowsCollapsingSelector
  , setAllowsCollapsingSelector
  , candidateListVisibleSelector
  , allowsTextInputContextCandidatesSelector
  , setAllowsTextInputContextCandidatesSelector
  , attributedStringForCandidateSelector
  , setAttributedStringForCandidateSelector
  , candidatesSelector
  , customizationLabelSelector
  , setCustomizationLabelSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- updateWithInsertionPointVisibility:@
updateWithInsertionPointVisibility :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> Bool -> IO ()
updateWithInsertionPointVisibility nsCandidateListTouchBarItem  isVisible =
    sendMsg nsCandidateListTouchBarItem (mkSelector "updateWithInsertionPointVisibility:") retVoid [argCULong (if isVisible then 1 else 0)]

-- | @- setCandidates:forSelectedRange:inString:@
setCandidates_forSelectedRange_inString :: (IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem, IsNSArray candidates, IsNSString originalString) => nsCandidateListTouchBarItem -> candidates -> NSRange -> originalString -> IO ()
setCandidates_forSelectedRange_inString nsCandidateListTouchBarItem  candidates selectedRange originalString =
  withObjCPtr candidates $ \raw_candidates ->
    withObjCPtr originalString $ \raw_originalString ->
        sendMsg nsCandidateListTouchBarItem (mkSelector "setCandidates:forSelectedRange:inString:") retVoid [argPtr (castPtr raw_candidates :: Ptr ()), argNSRange selectedRange, argPtr (castPtr raw_originalString :: Ptr ())]

-- | @- client@
client :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> IO (Id NSView)
client nsCandidateListTouchBarItem  =
    sendMsg nsCandidateListTouchBarItem (mkSelector "client") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setClient:@
setClient :: (IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem, IsNSView value) => nsCandidateListTouchBarItem -> value -> IO ()
setClient nsCandidateListTouchBarItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsCandidateListTouchBarItem (mkSelector "setClient:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- delegate@
delegate :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> IO RawId
delegate nsCandidateListTouchBarItem  =
    fmap (RawId . castPtr) $ sendMsg nsCandidateListTouchBarItem (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> RawId -> IO ()
setDelegate nsCandidateListTouchBarItem  value =
    sendMsg nsCandidateListTouchBarItem (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- collapsed@
collapsed :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> IO Bool
collapsed nsCandidateListTouchBarItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCandidateListTouchBarItem (mkSelector "collapsed") retCULong []

-- | @- setCollapsed:@
setCollapsed :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> Bool -> IO ()
setCollapsed nsCandidateListTouchBarItem  value =
    sendMsg nsCandidateListTouchBarItem (mkSelector "setCollapsed:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsCollapsing@
allowsCollapsing :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> IO Bool
allowsCollapsing nsCandidateListTouchBarItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCandidateListTouchBarItem (mkSelector "allowsCollapsing") retCULong []

-- | @- setAllowsCollapsing:@
setAllowsCollapsing :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> Bool -> IO ()
setAllowsCollapsing nsCandidateListTouchBarItem  value =
    sendMsg nsCandidateListTouchBarItem (mkSelector "setAllowsCollapsing:") retVoid [argCULong (if value then 1 else 0)]

-- | @- candidateListVisible@
candidateListVisible :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> IO Bool
candidateListVisible nsCandidateListTouchBarItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCandidateListTouchBarItem (mkSelector "candidateListVisible") retCULong []

-- | @- allowsTextInputContextCandidates@
allowsTextInputContextCandidates :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> IO Bool
allowsTextInputContextCandidates nsCandidateListTouchBarItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCandidateListTouchBarItem (mkSelector "allowsTextInputContextCandidates") retCULong []

-- | @- setAllowsTextInputContextCandidates:@
setAllowsTextInputContextCandidates :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> Bool -> IO ()
setAllowsTextInputContextCandidates nsCandidateListTouchBarItem  value =
    sendMsg nsCandidateListTouchBarItem (mkSelector "setAllowsTextInputContextCandidates:") retVoid [argCULong (if value then 1 else 0)]

-- | @- attributedStringForCandidate@
attributedStringForCandidate :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> IO (Ptr ())
attributedStringForCandidate nsCandidateListTouchBarItem  =
    fmap castPtr $ sendMsg nsCandidateListTouchBarItem (mkSelector "attributedStringForCandidate") (retPtr retVoid) []

-- | @- setAttributedStringForCandidate:@
setAttributedStringForCandidate :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> Ptr () -> IO ()
setAttributedStringForCandidate nsCandidateListTouchBarItem  value =
    sendMsg nsCandidateListTouchBarItem (mkSelector "setAttributedStringForCandidate:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- candidates@
candidates :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> IO (Id NSArray)
candidates nsCandidateListTouchBarItem  =
    sendMsg nsCandidateListTouchBarItem (mkSelector "candidates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- customizationLabel@
customizationLabel :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> IO (Id NSString)
customizationLabel nsCandidateListTouchBarItem  =
    sendMsg nsCandidateListTouchBarItem (mkSelector "customizationLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCustomizationLabel:@
setCustomizationLabel :: (IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem, IsNSString value) => nsCandidateListTouchBarItem -> value -> IO ()
setCustomizationLabel nsCandidateListTouchBarItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsCandidateListTouchBarItem (mkSelector "setCustomizationLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateWithInsertionPointVisibility:@
updateWithInsertionPointVisibilitySelector :: Selector
updateWithInsertionPointVisibilitySelector = mkSelector "updateWithInsertionPointVisibility:"

-- | @Selector@ for @setCandidates:forSelectedRange:inString:@
setCandidates_forSelectedRange_inStringSelector :: Selector
setCandidates_forSelectedRange_inStringSelector = mkSelector "setCandidates:forSelectedRange:inString:"

-- | @Selector@ for @client@
clientSelector :: Selector
clientSelector = mkSelector "client"

-- | @Selector@ for @setClient:@
setClientSelector :: Selector
setClientSelector = mkSelector "setClient:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @collapsed@
collapsedSelector :: Selector
collapsedSelector = mkSelector "collapsed"

-- | @Selector@ for @setCollapsed:@
setCollapsedSelector :: Selector
setCollapsedSelector = mkSelector "setCollapsed:"

-- | @Selector@ for @allowsCollapsing@
allowsCollapsingSelector :: Selector
allowsCollapsingSelector = mkSelector "allowsCollapsing"

-- | @Selector@ for @setAllowsCollapsing:@
setAllowsCollapsingSelector :: Selector
setAllowsCollapsingSelector = mkSelector "setAllowsCollapsing:"

-- | @Selector@ for @candidateListVisible@
candidateListVisibleSelector :: Selector
candidateListVisibleSelector = mkSelector "candidateListVisible"

-- | @Selector@ for @allowsTextInputContextCandidates@
allowsTextInputContextCandidatesSelector :: Selector
allowsTextInputContextCandidatesSelector = mkSelector "allowsTextInputContextCandidates"

-- | @Selector@ for @setAllowsTextInputContextCandidates:@
setAllowsTextInputContextCandidatesSelector :: Selector
setAllowsTextInputContextCandidatesSelector = mkSelector "setAllowsTextInputContextCandidates:"

-- | @Selector@ for @attributedStringForCandidate@
attributedStringForCandidateSelector :: Selector
attributedStringForCandidateSelector = mkSelector "attributedStringForCandidate"

-- | @Selector@ for @setAttributedStringForCandidate:@
setAttributedStringForCandidateSelector :: Selector
setAttributedStringForCandidateSelector = mkSelector "setAttributedStringForCandidate:"

-- | @Selector@ for @candidates@
candidatesSelector :: Selector
candidatesSelector = mkSelector "candidates"

-- | @Selector@ for @customizationLabel@
customizationLabelSelector :: Selector
customizationLabelSelector = mkSelector "customizationLabel"

-- | @Selector@ for @setCustomizationLabel:@
setCustomizationLabelSelector :: Selector
setCustomizationLabelSelector = mkSelector "setCustomizationLabel:"

