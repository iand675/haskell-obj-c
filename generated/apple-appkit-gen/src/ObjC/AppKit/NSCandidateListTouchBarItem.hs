{-# LANGUAGE DataKinds #-}
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
  , allowsCollapsingSelector
  , allowsTextInputContextCandidatesSelector
  , attributedStringForCandidateSelector
  , candidateListVisibleSelector
  , candidatesSelector
  , clientSelector
  , collapsedSelector
  , customizationLabelSelector
  , delegateSelector
  , setAllowsCollapsingSelector
  , setAllowsTextInputContextCandidatesSelector
  , setAttributedStringForCandidateSelector
  , setCandidates_forSelectedRange_inStringSelector
  , setClientSelector
  , setCollapsedSelector
  , setCustomizationLabelSelector
  , setDelegateSelector
  , updateWithInsertionPointVisibilitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- updateWithInsertionPointVisibility:@
updateWithInsertionPointVisibility :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> Bool -> IO ()
updateWithInsertionPointVisibility nsCandidateListTouchBarItem isVisible =
  sendMessage nsCandidateListTouchBarItem updateWithInsertionPointVisibilitySelector isVisible

-- | @- setCandidates:forSelectedRange:inString:@
setCandidates_forSelectedRange_inString :: (IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem, IsNSArray candidates, IsNSString originalString) => nsCandidateListTouchBarItem -> candidates -> NSRange -> originalString -> IO ()
setCandidates_forSelectedRange_inString nsCandidateListTouchBarItem candidates selectedRange originalString =
  sendMessage nsCandidateListTouchBarItem setCandidates_forSelectedRange_inStringSelector (toNSArray candidates) selectedRange (toNSString originalString)

-- | @- client@
client :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> IO (Id NSView)
client nsCandidateListTouchBarItem =
  sendMessage nsCandidateListTouchBarItem clientSelector

-- | @- setClient:@
setClient :: (IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem, IsNSView value) => nsCandidateListTouchBarItem -> value -> IO ()
setClient nsCandidateListTouchBarItem value =
  sendMessage nsCandidateListTouchBarItem setClientSelector (toNSView value)

-- | @- delegate@
delegate :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> IO RawId
delegate nsCandidateListTouchBarItem =
  sendMessage nsCandidateListTouchBarItem delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> RawId -> IO ()
setDelegate nsCandidateListTouchBarItem value =
  sendMessage nsCandidateListTouchBarItem setDelegateSelector value

-- | @- collapsed@
collapsed :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> IO Bool
collapsed nsCandidateListTouchBarItem =
  sendMessage nsCandidateListTouchBarItem collapsedSelector

-- | @- setCollapsed:@
setCollapsed :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> Bool -> IO ()
setCollapsed nsCandidateListTouchBarItem value =
  sendMessage nsCandidateListTouchBarItem setCollapsedSelector value

-- | @- allowsCollapsing@
allowsCollapsing :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> IO Bool
allowsCollapsing nsCandidateListTouchBarItem =
  sendMessage nsCandidateListTouchBarItem allowsCollapsingSelector

-- | @- setAllowsCollapsing:@
setAllowsCollapsing :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> Bool -> IO ()
setAllowsCollapsing nsCandidateListTouchBarItem value =
  sendMessage nsCandidateListTouchBarItem setAllowsCollapsingSelector value

-- | @- candidateListVisible@
candidateListVisible :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> IO Bool
candidateListVisible nsCandidateListTouchBarItem =
  sendMessage nsCandidateListTouchBarItem candidateListVisibleSelector

-- | @- allowsTextInputContextCandidates@
allowsTextInputContextCandidates :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> IO Bool
allowsTextInputContextCandidates nsCandidateListTouchBarItem =
  sendMessage nsCandidateListTouchBarItem allowsTextInputContextCandidatesSelector

-- | @- setAllowsTextInputContextCandidates:@
setAllowsTextInputContextCandidates :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> Bool -> IO ()
setAllowsTextInputContextCandidates nsCandidateListTouchBarItem value =
  sendMessage nsCandidateListTouchBarItem setAllowsTextInputContextCandidatesSelector value

-- | @- attributedStringForCandidate@
attributedStringForCandidate :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> IO (Ptr ())
attributedStringForCandidate nsCandidateListTouchBarItem =
  sendMessage nsCandidateListTouchBarItem attributedStringForCandidateSelector

-- | @- setAttributedStringForCandidate:@
setAttributedStringForCandidate :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> Ptr () -> IO ()
setAttributedStringForCandidate nsCandidateListTouchBarItem value =
  sendMessage nsCandidateListTouchBarItem setAttributedStringForCandidateSelector value

-- | @- candidates@
candidates :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> IO (Id NSArray)
candidates nsCandidateListTouchBarItem =
  sendMessage nsCandidateListTouchBarItem candidatesSelector

-- | @- customizationLabel@
customizationLabel :: IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem => nsCandidateListTouchBarItem -> IO (Id NSString)
customizationLabel nsCandidateListTouchBarItem =
  sendMessage nsCandidateListTouchBarItem customizationLabelSelector

-- | @- setCustomizationLabel:@
setCustomizationLabel :: (IsNSCandidateListTouchBarItem nsCandidateListTouchBarItem, IsNSString value) => nsCandidateListTouchBarItem -> value -> IO ()
setCustomizationLabel nsCandidateListTouchBarItem value =
  sendMessage nsCandidateListTouchBarItem setCustomizationLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updateWithInsertionPointVisibility:@
updateWithInsertionPointVisibilitySelector :: Selector '[Bool] ()
updateWithInsertionPointVisibilitySelector = mkSelector "updateWithInsertionPointVisibility:"

-- | @Selector@ for @setCandidates:forSelectedRange:inString:@
setCandidates_forSelectedRange_inStringSelector :: Selector '[Id NSArray, NSRange, Id NSString] ()
setCandidates_forSelectedRange_inStringSelector = mkSelector "setCandidates:forSelectedRange:inString:"

-- | @Selector@ for @client@
clientSelector :: Selector '[] (Id NSView)
clientSelector = mkSelector "client"

-- | @Selector@ for @setClient:@
setClientSelector :: Selector '[Id NSView] ()
setClientSelector = mkSelector "setClient:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @collapsed@
collapsedSelector :: Selector '[] Bool
collapsedSelector = mkSelector "collapsed"

-- | @Selector@ for @setCollapsed:@
setCollapsedSelector :: Selector '[Bool] ()
setCollapsedSelector = mkSelector "setCollapsed:"

-- | @Selector@ for @allowsCollapsing@
allowsCollapsingSelector :: Selector '[] Bool
allowsCollapsingSelector = mkSelector "allowsCollapsing"

-- | @Selector@ for @setAllowsCollapsing:@
setAllowsCollapsingSelector :: Selector '[Bool] ()
setAllowsCollapsingSelector = mkSelector "setAllowsCollapsing:"

-- | @Selector@ for @candidateListVisible@
candidateListVisibleSelector :: Selector '[] Bool
candidateListVisibleSelector = mkSelector "candidateListVisible"

-- | @Selector@ for @allowsTextInputContextCandidates@
allowsTextInputContextCandidatesSelector :: Selector '[] Bool
allowsTextInputContextCandidatesSelector = mkSelector "allowsTextInputContextCandidates"

-- | @Selector@ for @setAllowsTextInputContextCandidates:@
setAllowsTextInputContextCandidatesSelector :: Selector '[Bool] ()
setAllowsTextInputContextCandidatesSelector = mkSelector "setAllowsTextInputContextCandidates:"

-- | @Selector@ for @attributedStringForCandidate@
attributedStringForCandidateSelector :: Selector '[] (Ptr ())
attributedStringForCandidateSelector = mkSelector "attributedStringForCandidate"

-- | @Selector@ for @setAttributedStringForCandidate:@
setAttributedStringForCandidateSelector :: Selector '[Ptr ()] ()
setAttributedStringForCandidateSelector = mkSelector "setAttributedStringForCandidate:"

-- | @Selector@ for @candidates@
candidatesSelector :: Selector '[] (Id NSArray)
candidatesSelector = mkSelector "candidates"

-- | @Selector@ for @customizationLabel@
customizationLabelSelector :: Selector '[] (Id NSString)
customizationLabelSelector = mkSelector "customizationLabel"

-- | @Selector@ for @setCustomizationLabel:@
setCustomizationLabelSelector :: Selector '[Id NSString] ()
setCustomizationLabelSelector = mkSelector "setCustomizationLabel:"

