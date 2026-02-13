{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextStorage@.
module ObjC.AppKit.NSTextStorage
  ( NSTextStorage
  , IsNSTextStorage(..)
  , addLayoutManager
  , removeLayoutManager
  , edited_range_changeInLength
  , processEditing
  , invalidateAttributesInRange
  , ensureAttributesAreFixedInRange
  , layoutManagers
  , editedMask
  , editedRange
  , changeInLength
  , delegate
  , setDelegate
  , fixesAttributesLazily
  , textStorageObserver
  , setTextStorageObserver
  , attributeRuns
  , setAttributeRuns
  , paragraphs
  , setParagraphs
  , words_
  , setWords
  , characters
  , setCharacters
  , font
  , setFont
  , foregroundColor
  , setForegroundColor
  , addLayoutManagerSelector
  , attributeRunsSelector
  , changeInLengthSelector
  , charactersSelector
  , delegateSelector
  , editedMaskSelector
  , editedRangeSelector
  , edited_range_changeInLengthSelector
  , ensureAttributesAreFixedInRangeSelector
  , fixesAttributesLazilySelector
  , fontSelector
  , foregroundColorSelector
  , invalidateAttributesInRangeSelector
  , layoutManagersSelector
  , paragraphsSelector
  , processEditingSelector
  , removeLayoutManagerSelector
  , setAttributeRunsSelector
  , setCharactersSelector
  , setDelegateSelector
  , setFontSelector
  , setForegroundColorSelector
  , setParagraphsSelector
  , setTextStorageObserverSelector
  , setWordsSelector
  , textStorageObserverSelector
  , wordsSelector

  -- * Enum types
  , NSTextStorageEditActions(NSTextStorageEditActions)
  , pattern NSTextStorageEditedAttributes
  , pattern NSTextStorageEditedCharacters

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- addLayoutManager:@
addLayoutManager :: (IsNSTextStorage nsTextStorage, IsNSLayoutManager aLayoutManager) => nsTextStorage -> aLayoutManager -> IO ()
addLayoutManager nsTextStorage aLayoutManager =
  sendMessage nsTextStorage addLayoutManagerSelector (toNSLayoutManager aLayoutManager)

-- | @- removeLayoutManager:@
removeLayoutManager :: (IsNSTextStorage nsTextStorage, IsNSLayoutManager aLayoutManager) => nsTextStorage -> aLayoutManager -> IO ()
removeLayoutManager nsTextStorage aLayoutManager =
  sendMessage nsTextStorage removeLayoutManagerSelector (toNSLayoutManager aLayoutManager)

-- | ************************** Edit management ***************************
--
-- ObjC selector: @- edited:range:changeInLength:@
edited_range_changeInLength :: IsNSTextStorage nsTextStorage => nsTextStorage -> NSTextStorageEditActions -> NSRange -> CLong -> IO ()
edited_range_changeInLength nsTextStorage editedMask editedRange delta =
  sendMessage nsTextStorage edited_range_changeInLengthSelector editedMask editedRange delta

-- | @- processEditing@
processEditing :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO ()
processEditing nsTextStorage =
  sendMessage nsTextStorage processEditingSelector

-- | @- invalidateAttributesInRange:@
invalidateAttributesInRange :: IsNSTextStorage nsTextStorage => nsTextStorage -> NSRange -> IO ()
invalidateAttributesInRange nsTextStorage range =
  sendMessage nsTextStorage invalidateAttributesInRangeSelector range

-- | @- ensureAttributesAreFixedInRange:@
ensureAttributesAreFixedInRange :: IsNSTextStorage nsTextStorage => nsTextStorage -> NSRange -> IO ()
ensureAttributesAreFixedInRange nsTextStorage range =
  sendMessage nsTextStorage ensureAttributesAreFixedInRangeSelector range

-- | ************************** Layout manager ***************************
--
-- ObjC selector: @- layoutManagers@
layoutManagers :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO (Id NSArray)
layoutManagers nsTextStorage =
  sendMessage nsTextStorage layoutManagersSelector

-- | ************************** Pending edit info ***************************
--
-- ObjC selector: @- editedMask@
editedMask :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO NSTextStorageEditActions
editedMask nsTextStorage =
  sendMessage nsTextStorage editedMaskSelector

-- | @- editedRange@
editedRange :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO NSRange
editedRange nsTextStorage =
  sendMessage nsTextStorage editedRangeSelector

-- | @- changeInLength@
changeInLength :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO CLong
changeInLength nsTextStorage =
  sendMessage nsTextStorage changeInLengthSelector

-- | ************************** Delegate ***************************
--
-- ObjC selector: @- delegate@
delegate :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO RawId
delegate nsTextStorage =
  sendMessage nsTextStorage delegateSelector

-- | ************************** Delegate ***************************
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsNSTextStorage nsTextStorage => nsTextStorage -> RawId -> IO ()
setDelegate nsTextStorage value =
  sendMessage nsTextStorage setDelegateSelector value

-- | ************************** Attribute fixing ***************************
--
-- ObjC selector: @- fixesAttributesLazily@
fixesAttributesLazily :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO Bool
fixesAttributesLazily nsTextStorage =
  sendMessage nsTextStorage fixesAttributesLazilySelector

-- | ************************** NSTextStorageObserving ***************************
--
-- ObjC selector: @- textStorageObserver@
textStorageObserver :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO RawId
textStorageObserver nsTextStorage =
  sendMessage nsTextStorage textStorageObserverSelector

-- | ************************** NSTextStorageObserving ***************************
--
-- ObjC selector: @- setTextStorageObserver:@
setTextStorageObserver :: IsNSTextStorage nsTextStorage => nsTextStorage -> RawId -> IO ()
setTextStorageObserver nsTextStorage value =
  sendMessage nsTextStorage setTextStorageObserverSelector value

-- | @- attributeRuns@
attributeRuns :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO (Id NSArray)
attributeRuns nsTextStorage =
  sendMessage nsTextStorage attributeRunsSelector

-- | @- setAttributeRuns:@
setAttributeRuns :: (IsNSTextStorage nsTextStorage, IsNSArray value) => nsTextStorage -> value -> IO ()
setAttributeRuns nsTextStorage value =
  sendMessage nsTextStorage setAttributeRunsSelector (toNSArray value)

-- | @- paragraphs@
paragraphs :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO (Id NSArray)
paragraphs nsTextStorage =
  sendMessage nsTextStorage paragraphsSelector

-- | @- setParagraphs:@
setParagraphs :: (IsNSTextStorage nsTextStorage, IsNSArray value) => nsTextStorage -> value -> IO ()
setParagraphs nsTextStorage value =
  sendMessage nsTextStorage setParagraphsSelector (toNSArray value)

-- | @- words@
words_ :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO (Id NSArray)
words_ nsTextStorage =
  sendMessage nsTextStorage wordsSelector

-- | @- setWords:@
setWords :: (IsNSTextStorage nsTextStorage, IsNSArray value) => nsTextStorage -> value -> IO ()
setWords nsTextStorage value =
  sendMessage nsTextStorage setWordsSelector (toNSArray value)

-- | @- characters@
characters :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO (Id NSArray)
characters nsTextStorage =
  sendMessage nsTextStorage charactersSelector

-- | @- setCharacters:@
setCharacters :: (IsNSTextStorage nsTextStorage, IsNSArray value) => nsTextStorage -> value -> IO ()
setCharacters nsTextStorage value =
  sendMessage nsTextStorage setCharactersSelector (toNSArray value)

-- | @- font@
font :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO (Id NSFont)
font nsTextStorage =
  sendMessage nsTextStorage fontSelector

-- | @- setFont:@
setFont :: (IsNSTextStorage nsTextStorage, IsNSFont value) => nsTextStorage -> value -> IO ()
setFont nsTextStorage value =
  sendMessage nsTextStorage setFontSelector (toNSFont value)

-- | @- foregroundColor@
foregroundColor :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO (Id NSColor)
foregroundColor nsTextStorage =
  sendMessage nsTextStorage foregroundColorSelector

-- | @- setForegroundColor:@
setForegroundColor :: (IsNSTextStorage nsTextStorage, IsNSColor value) => nsTextStorage -> value -> IO ()
setForegroundColor nsTextStorage value =
  sendMessage nsTextStorage setForegroundColorSelector (toNSColor value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addLayoutManager:@
addLayoutManagerSelector :: Selector '[Id NSLayoutManager] ()
addLayoutManagerSelector = mkSelector "addLayoutManager:"

-- | @Selector@ for @removeLayoutManager:@
removeLayoutManagerSelector :: Selector '[Id NSLayoutManager] ()
removeLayoutManagerSelector = mkSelector "removeLayoutManager:"

-- | @Selector@ for @edited:range:changeInLength:@
edited_range_changeInLengthSelector :: Selector '[NSTextStorageEditActions, NSRange, CLong] ()
edited_range_changeInLengthSelector = mkSelector "edited:range:changeInLength:"

-- | @Selector@ for @processEditing@
processEditingSelector :: Selector '[] ()
processEditingSelector = mkSelector "processEditing"

-- | @Selector@ for @invalidateAttributesInRange:@
invalidateAttributesInRangeSelector :: Selector '[NSRange] ()
invalidateAttributesInRangeSelector = mkSelector "invalidateAttributesInRange:"

-- | @Selector@ for @ensureAttributesAreFixedInRange:@
ensureAttributesAreFixedInRangeSelector :: Selector '[NSRange] ()
ensureAttributesAreFixedInRangeSelector = mkSelector "ensureAttributesAreFixedInRange:"

-- | @Selector@ for @layoutManagers@
layoutManagersSelector :: Selector '[] (Id NSArray)
layoutManagersSelector = mkSelector "layoutManagers"

-- | @Selector@ for @editedMask@
editedMaskSelector :: Selector '[] NSTextStorageEditActions
editedMaskSelector = mkSelector "editedMask"

-- | @Selector@ for @editedRange@
editedRangeSelector :: Selector '[] NSRange
editedRangeSelector = mkSelector "editedRange"

-- | @Selector@ for @changeInLength@
changeInLengthSelector :: Selector '[] CLong
changeInLengthSelector = mkSelector "changeInLength"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @fixesAttributesLazily@
fixesAttributesLazilySelector :: Selector '[] Bool
fixesAttributesLazilySelector = mkSelector "fixesAttributesLazily"

-- | @Selector@ for @textStorageObserver@
textStorageObserverSelector :: Selector '[] RawId
textStorageObserverSelector = mkSelector "textStorageObserver"

-- | @Selector@ for @setTextStorageObserver:@
setTextStorageObserverSelector :: Selector '[RawId] ()
setTextStorageObserverSelector = mkSelector "setTextStorageObserver:"

-- | @Selector@ for @attributeRuns@
attributeRunsSelector :: Selector '[] (Id NSArray)
attributeRunsSelector = mkSelector "attributeRuns"

-- | @Selector@ for @setAttributeRuns:@
setAttributeRunsSelector :: Selector '[Id NSArray] ()
setAttributeRunsSelector = mkSelector "setAttributeRuns:"

-- | @Selector@ for @paragraphs@
paragraphsSelector :: Selector '[] (Id NSArray)
paragraphsSelector = mkSelector "paragraphs"

-- | @Selector@ for @setParagraphs:@
setParagraphsSelector :: Selector '[Id NSArray] ()
setParagraphsSelector = mkSelector "setParagraphs:"

-- | @Selector@ for @words@
wordsSelector :: Selector '[] (Id NSArray)
wordsSelector = mkSelector "words"

-- | @Selector@ for @setWords:@
setWordsSelector :: Selector '[Id NSArray] ()
setWordsSelector = mkSelector "setWords:"

-- | @Selector@ for @characters@
charactersSelector :: Selector '[] (Id NSArray)
charactersSelector = mkSelector "characters"

-- | @Selector@ for @setCharacters:@
setCharactersSelector :: Selector '[Id NSArray] ()
setCharactersSelector = mkSelector "setCharacters:"

-- | @Selector@ for @font@
fontSelector :: Selector '[] (Id NSFont)
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector '[Id NSFont] ()
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @foregroundColor@
foregroundColorSelector :: Selector '[] (Id NSColor)
foregroundColorSelector = mkSelector "foregroundColor"

-- | @Selector@ for @setForegroundColor:@
setForegroundColorSelector :: Selector '[Id NSColor] ()
setForegroundColorSelector = mkSelector "setForegroundColor:"

