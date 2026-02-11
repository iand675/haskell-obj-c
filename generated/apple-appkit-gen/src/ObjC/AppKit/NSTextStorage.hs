{-# LANGUAGE PatternSynonyms #-}
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
  , removeLayoutManagerSelector
  , edited_range_changeInLengthSelector
  , processEditingSelector
  , invalidateAttributesInRangeSelector
  , ensureAttributesAreFixedInRangeSelector
  , layoutManagersSelector
  , editedMaskSelector
  , editedRangeSelector
  , changeInLengthSelector
  , delegateSelector
  , setDelegateSelector
  , fixesAttributesLazilySelector
  , textStorageObserverSelector
  , setTextStorageObserverSelector
  , attributeRunsSelector
  , setAttributeRunsSelector
  , paragraphsSelector
  , setParagraphsSelector
  , wordsSelector
  , setWordsSelector
  , charactersSelector
  , setCharactersSelector
  , fontSelector
  , setFontSelector
  , foregroundColorSelector
  , setForegroundColorSelector

  -- * Enum types
  , NSTextStorageEditActions(NSTextStorageEditActions)
  , pattern NSTextStorageEditedAttributes
  , pattern NSTextStorageEditedCharacters

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- addLayoutManager:@
addLayoutManager :: (IsNSTextStorage nsTextStorage, IsNSLayoutManager aLayoutManager) => nsTextStorage -> aLayoutManager -> IO ()
addLayoutManager nsTextStorage  aLayoutManager =
  withObjCPtr aLayoutManager $ \raw_aLayoutManager ->
      sendMsg nsTextStorage (mkSelector "addLayoutManager:") retVoid [argPtr (castPtr raw_aLayoutManager :: Ptr ())]

-- | @- removeLayoutManager:@
removeLayoutManager :: (IsNSTextStorage nsTextStorage, IsNSLayoutManager aLayoutManager) => nsTextStorage -> aLayoutManager -> IO ()
removeLayoutManager nsTextStorage  aLayoutManager =
  withObjCPtr aLayoutManager $ \raw_aLayoutManager ->
      sendMsg nsTextStorage (mkSelector "removeLayoutManager:") retVoid [argPtr (castPtr raw_aLayoutManager :: Ptr ())]

-- | ************************** Edit management ***************************
--
-- ObjC selector: @- edited:range:changeInLength:@
edited_range_changeInLength :: IsNSTextStorage nsTextStorage => nsTextStorage -> NSTextStorageEditActions -> NSRange -> CLong -> IO ()
edited_range_changeInLength nsTextStorage  editedMask editedRange delta =
    sendMsg nsTextStorage (mkSelector "edited:range:changeInLength:") retVoid [argCULong (coerce editedMask), argNSRange editedRange, argCLong delta]

-- | @- processEditing@
processEditing :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO ()
processEditing nsTextStorage  =
    sendMsg nsTextStorage (mkSelector "processEditing") retVoid []

-- | @- invalidateAttributesInRange:@
invalidateAttributesInRange :: IsNSTextStorage nsTextStorage => nsTextStorage -> NSRange -> IO ()
invalidateAttributesInRange nsTextStorage  range =
    sendMsg nsTextStorage (mkSelector "invalidateAttributesInRange:") retVoid [argNSRange range]

-- | @- ensureAttributesAreFixedInRange:@
ensureAttributesAreFixedInRange :: IsNSTextStorage nsTextStorage => nsTextStorage -> NSRange -> IO ()
ensureAttributesAreFixedInRange nsTextStorage  range =
    sendMsg nsTextStorage (mkSelector "ensureAttributesAreFixedInRange:") retVoid [argNSRange range]

-- | ************************** Layout manager ***************************
--
-- ObjC selector: @- layoutManagers@
layoutManagers :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO (Id NSArray)
layoutManagers nsTextStorage  =
    sendMsg nsTextStorage (mkSelector "layoutManagers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ************************** Pending edit info ***************************
--
-- ObjC selector: @- editedMask@
editedMask :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO NSTextStorageEditActions
editedMask nsTextStorage  =
    fmap (coerce :: CULong -> NSTextStorageEditActions) $ sendMsg nsTextStorage (mkSelector "editedMask") retCULong []

-- | @- editedRange@
editedRange :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO NSRange
editedRange nsTextStorage  =
    sendMsgStret nsTextStorage (mkSelector "editedRange") retNSRange []

-- | @- changeInLength@
changeInLength :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO CLong
changeInLength nsTextStorage  =
    sendMsg nsTextStorage (mkSelector "changeInLength") retCLong []

-- | ************************** Delegate ***************************
--
-- ObjC selector: @- delegate@
delegate :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO RawId
delegate nsTextStorage  =
    fmap (RawId . castPtr) $ sendMsg nsTextStorage (mkSelector "delegate") (retPtr retVoid) []

-- | ************************** Delegate ***************************
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsNSTextStorage nsTextStorage => nsTextStorage -> RawId -> IO ()
setDelegate nsTextStorage  value =
    sendMsg nsTextStorage (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | ************************** Attribute fixing ***************************
--
-- ObjC selector: @- fixesAttributesLazily@
fixesAttributesLazily :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO Bool
fixesAttributesLazily nsTextStorage  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextStorage (mkSelector "fixesAttributesLazily") retCULong []

-- | ************************** NSTextStorageObserving ***************************
--
-- ObjC selector: @- textStorageObserver@
textStorageObserver :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO RawId
textStorageObserver nsTextStorage  =
    fmap (RawId . castPtr) $ sendMsg nsTextStorage (mkSelector "textStorageObserver") (retPtr retVoid) []

-- | ************************** NSTextStorageObserving ***************************
--
-- ObjC selector: @- setTextStorageObserver:@
setTextStorageObserver :: IsNSTextStorage nsTextStorage => nsTextStorage -> RawId -> IO ()
setTextStorageObserver nsTextStorage  value =
    sendMsg nsTextStorage (mkSelector "setTextStorageObserver:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- attributeRuns@
attributeRuns :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO (Id NSArray)
attributeRuns nsTextStorage  =
    sendMsg nsTextStorage (mkSelector "attributeRuns") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributeRuns:@
setAttributeRuns :: (IsNSTextStorage nsTextStorage, IsNSArray value) => nsTextStorage -> value -> IO ()
setAttributeRuns nsTextStorage  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextStorage (mkSelector "setAttributeRuns:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- paragraphs@
paragraphs :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO (Id NSArray)
paragraphs nsTextStorage  =
    sendMsg nsTextStorage (mkSelector "paragraphs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setParagraphs:@
setParagraphs :: (IsNSTextStorage nsTextStorage, IsNSArray value) => nsTextStorage -> value -> IO ()
setParagraphs nsTextStorage  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextStorage (mkSelector "setParagraphs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- words@
words_ :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO (Id NSArray)
words_ nsTextStorage  =
    sendMsg nsTextStorage (mkSelector "words") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWords:@
setWords :: (IsNSTextStorage nsTextStorage, IsNSArray value) => nsTextStorage -> value -> IO ()
setWords nsTextStorage  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextStorage (mkSelector "setWords:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- characters@
characters :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO (Id NSArray)
characters nsTextStorage  =
    sendMsg nsTextStorage (mkSelector "characters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCharacters:@
setCharacters :: (IsNSTextStorage nsTextStorage, IsNSArray value) => nsTextStorage -> value -> IO ()
setCharacters nsTextStorage  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextStorage (mkSelector "setCharacters:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- font@
font :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO (Id NSFont)
font nsTextStorage  =
    sendMsg nsTextStorage (mkSelector "font") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFont:@
setFont :: (IsNSTextStorage nsTextStorage, IsNSFont value) => nsTextStorage -> value -> IO ()
setFont nsTextStorage  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextStorage (mkSelector "setFont:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- foregroundColor@
foregroundColor :: IsNSTextStorage nsTextStorage => nsTextStorage -> IO (Id NSColor)
foregroundColor nsTextStorage  =
    sendMsg nsTextStorage (mkSelector "foregroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setForegroundColor:@
setForegroundColor :: (IsNSTextStorage nsTextStorage, IsNSColor value) => nsTextStorage -> value -> IO ()
setForegroundColor nsTextStorage  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextStorage (mkSelector "setForegroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addLayoutManager:@
addLayoutManagerSelector :: Selector
addLayoutManagerSelector = mkSelector "addLayoutManager:"

-- | @Selector@ for @removeLayoutManager:@
removeLayoutManagerSelector :: Selector
removeLayoutManagerSelector = mkSelector "removeLayoutManager:"

-- | @Selector@ for @edited:range:changeInLength:@
edited_range_changeInLengthSelector :: Selector
edited_range_changeInLengthSelector = mkSelector "edited:range:changeInLength:"

-- | @Selector@ for @processEditing@
processEditingSelector :: Selector
processEditingSelector = mkSelector "processEditing"

-- | @Selector@ for @invalidateAttributesInRange:@
invalidateAttributesInRangeSelector :: Selector
invalidateAttributesInRangeSelector = mkSelector "invalidateAttributesInRange:"

-- | @Selector@ for @ensureAttributesAreFixedInRange:@
ensureAttributesAreFixedInRangeSelector :: Selector
ensureAttributesAreFixedInRangeSelector = mkSelector "ensureAttributesAreFixedInRange:"

-- | @Selector@ for @layoutManagers@
layoutManagersSelector :: Selector
layoutManagersSelector = mkSelector "layoutManagers"

-- | @Selector@ for @editedMask@
editedMaskSelector :: Selector
editedMaskSelector = mkSelector "editedMask"

-- | @Selector@ for @editedRange@
editedRangeSelector :: Selector
editedRangeSelector = mkSelector "editedRange"

-- | @Selector@ for @changeInLength@
changeInLengthSelector :: Selector
changeInLengthSelector = mkSelector "changeInLength"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @fixesAttributesLazily@
fixesAttributesLazilySelector :: Selector
fixesAttributesLazilySelector = mkSelector "fixesAttributesLazily"

-- | @Selector@ for @textStorageObserver@
textStorageObserverSelector :: Selector
textStorageObserverSelector = mkSelector "textStorageObserver"

-- | @Selector@ for @setTextStorageObserver:@
setTextStorageObserverSelector :: Selector
setTextStorageObserverSelector = mkSelector "setTextStorageObserver:"

-- | @Selector@ for @attributeRuns@
attributeRunsSelector :: Selector
attributeRunsSelector = mkSelector "attributeRuns"

-- | @Selector@ for @setAttributeRuns:@
setAttributeRunsSelector :: Selector
setAttributeRunsSelector = mkSelector "setAttributeRuns:"

-- | @Selector@ for @paragraphs@
paragraphsSelector :: Selector
paragraphsSelector = mkSelector "paragraphs"

-- | @Selector@ for @setParagraphs:@
setParagraphsSelector :: Selector
setParagraphsSelector = mkSelector "setParagraphs:"

-- | @Selector@ for @words@
wordsSelector :: Selector
wordsSelector = mkSelector "words"

-- | @Selector@ for @setWords:@
setWordsSelector :: Selector
setWordsSelector = mkSelector "setWords:"

-- | @Selector@ for @characters@
charactersSelector :: Selector
charactersSelector = mkSelector "characters"

-- | @Selector@ for @setCharacters:@
setCharactersSelector :: Selector
setCharactersSelector = mkSelector "setCharacters:"

-- | @Selector@ for @font@
fontSelector :: Selector
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @foregroundColor@
foregroundColorSelector :: Selector
foregroundColorSelector = mkSelector "foregroundColor"

-- | @Selector@ for @setForegroundColor:@
setForegroundColorSelector :: Selector
setForegroundColorSelector = mkSelector "setForegroundColor:"

