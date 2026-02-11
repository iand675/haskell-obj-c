{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that represents a contiguous range between two locations inside document contents.
--
-- An @NSTextRange@ consists of the starting and terminating locations. There the two basic properties: ``location`` and ``endLocation``, respectively. The terminating ``location``, ``endLocation``, is directly following the last location in the range. For example, a location contains a range if `(range.location <= location) && (location < range.endLocation)@ is @true`.
--
-- Generated bindings for @NSTextRange@.
module ObjC.AppKit.NSTextRange
  ( NSTextRange
  , IsNSTextRange(..)
  , initWithLocation_endLocation
  , initWithLocation
  , init_
  , new
  , isEqualToTextRange
  , containsLocation
  , containsRange
  , intersectsWithTextRange
  , textRangeByIntersectingWithTextRange
  , textRangeByFormingUnionWithTextRange
  , empty
  , initWithLocation_endLocationSelector
  , initWithLocationSelector
  , initSelector
  , newSelector
  , isEqualToTextRangeSelector
  , containsLocationSelector
  , containsRangeSelector
  , intersectsWithTextRangeSelector
  , textRangeByIntersectingWithTextRangeSelector
  , textRangeByFormingUnionWithTextRangeSelector
  , emptySelector


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
import ObjC.Foundation.Internal.Classes

-- | Creates a new text range with the starting and ending locations you specify.
--
-- - Parameters:   - location: The starting location.   - endLocation: The ending location.
--
-- ObjC selector: @- initWithLocation:endLocation:@
initWithLocation_endLocation :: IsNSTextRange nsTextRange => nsTextRange -> RawId -> RawId -> IO (Id NSTextRange)
initWithLocation_endLocation nsTextRange  location endLocation =
  sendMsg nsTextRange (mkSelector "initWithLocation:endLocation:") (retPtr retVoid) [argPtr (castPtr (unRawId location) :: Ptr ()), argPtr (castPtr (unRawId endLocation) :: Ptr ())] >>= ownedObject . castPtr

-- | Creates a new text range at the location you specify.
--
-- - Parameters:   - location: An ``NSTextLocation``.
--
-- ObjC selector: @- initWithLocation:@
initWithLocation :: IsNSTextRange nsTextRange => nsTextRange -> RawId -> IO (Id NSTextRange)
initWithLocation nsTextRange  location =
  sendMsg nsTextRange (mkSelector "initWithLocation:") (retPtr retVoid) [argPtr (castPtr (unRawId location) :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSTextRange nsTextRange => nsTextRange -> IO (Id NSTextRange)
init_ nsTextRange  =
  sendMsg nsTextRange (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSTextRange)
new  =
  do
    cls' <- getRequiredClass "NSTextRange"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Compares two text ranges.
--
-- - Parameters:   - textRange: The range used to compare against the current range to evaluate for differences.
--
-- - Returns: Returns @true@ if the ranges are equal.
--
-- ObjC selector: @- isEqualToTextRange:@
isEqualToTextRange :: (IsNSTextRange nsTextRange, IsNSTextRange textRange) => nsTextRange -> textRange -> IO Bool
isEqualToTextRange nsTextRange  textRange =
withObjCPtr textRange $ \raw_textRange ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextRange (mkSelector "isEqualToTextRange:") retCULong [argPtr (castPtr raw_textRange :: Ptr ())]

-- | Determines if the text location you specify is in the current text range.
--
-- - Parameters:   - location: An ``NSTextLocation``.
--
-- - Returns: Returns @true@ if the location is in the range otherwise @false@ .
--
-- ObjC selector: @- containsLocation:@
containsLocation :: IsNSTextRange nsTextRange => nsTextRange -> RawId -> IO Bool
containsLocation nsTextRange  location =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextRange (mkSelector "containsLocation:") retCULong [argPtr (castPtr (unRawId location) :: Ptr ())]

-- | Determines if the text range you specify is in the current text range.
--
-- - Parameters:   - textRange: An ``NSTextRange``.
--
-- - Returns: Returns @true@ if the range you provide is in the current range; otherwise @false@.
--
-- ObjC selector: @- containsRange:@
containsRange :: (IsNSTextRange nsTextRange, IsNSTextRange textRange) => nsTextRange -> textRange -> IO Bool
containsRange nsTextRange  textRange =
withObjCPtr textRange $ \raw_textRange ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextRange (mkSelector "containsRange:") retCULong [argPtr (castPtr raw_textRange :: Ptr ())]

-- | Determines if two ranges intersect.
--
-- - Parameters:   - textRange: The range used to compare against the current range to evaluate for differences.
--
-- - Returns: Returns @true@ if the ranges intersect.
--
-- ObjC selector: @- intersectsWithTextRange:@
intersectsWithTextRange :: (IsNSTextRange nsTextRange, IsNSTextRange textRange) => nsTextRange -> textRange -> IO Bool
intersectsWithTextRange nsTextRange  textRange =
withObjCPtr textRange $ \raw_textRange ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextRange (mkSelector "intersectsWithTextRange:") retCULong [argPtr (castPtr raw_textRange :: Ptr ())]

-- | Returns the range, if any, where two text ranges intersect.
--
-- - Parameters:   - textRange: The range used to compare against the current range to evaluate for differences.
--
-- - Returns: An   <doc://com.apple.documentation/documentation/foundation/nsrange> that   represents the intersection of the ranges, or @nil@ if they don't   intersect.
--
-- ObjC selector: @- textRangeByIntersectingWithTextRange:@
textRangeByIntersectingWithTextRange :: (IsNSTextRange nsTextRange, IsNSTextRange textRange) => nsTextRange -> textRange -> IO (Id NSTextRange)
textRangeByIntersectingWithTextRange nsTextRange  textRange =
withObjCPtr textRange $ \raw_textRange ->
    sendMsg nsTextRange (mkSelector "textRangeByIntersectingWithTextRange:") (retPtr retVoid) [argPtr (castPtr raw_textRange :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a new text range by forming the union with the text range you provide.
--
-- - Parameters:   - textRange: The range to use to create the union.
--
-- - Returns: An ``NSTextRange`` that represent the union of the two ranges.
--
-- ObjC selector: @- textRangeByFormingUnionWithTextRange:@
textRangeByFormingUnionWithTextRange :: (IsNSTextRange nsTextRange, IsNSTextRange textRange) => nsTextRange -> textRange -> IO (Id NSTextRange)
textRangeByFormingUnionWithTextRange nsTextRange  textRange =
withObjCPtr textRange $ \raw_textRange ->
    sendMsg nsTextRange (mkSelector "textRangeByFormingUnionWithTextRange:") (retPtr retVoid) [argPtr (castPtr raw_textRange :: Ptr ())] >>= retainedObject . castPtr

-- | Returns whether the text range is empty.
--
-- ObjC selector: @- empty@
empty :: IsNSTextRange nsTextRange => nsTextRange -> IO Bool
empty nsTextRange  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextRange (mkSelector "empty") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLocation:endLocation:@
initWithLocation_endLocationSelector :: Selector
initWithLocation_endLocationSelector = mkSelector "initWithLocation:endLocation:"

-- | @Selector@ for @initWithLocation:@
initWithLocationSelector :: Selector
initWithLocationSelector = mkSelector "initWithLocation:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @isEqualToTextRange:@
isEqualToTextRangeSelector :: Selector
isEqualToTextRangeSelector = mkSelector "isEqualToTextRange:"

-- | @Selector@ for @containsLocation:@
containsLocationSelector :: Selector
containsLocationSelector = mkSelector "containsLocation:"

-- | @Selector@ for @containsRange:@
containsRangeSelector :: Selector
containsRangeSelector = mkSelector "containsRange:"

-- | @Selector@ for @intersectsWithTextRange:@
intersectsWithTextRangeSelector :: Selector
intersectsWithTextRangeSelector = mkSelector "intersectsWithTextRange:"

-- | @Selector@ for @textRangeByIntersectingWithTextRange:@
textRangeByIntersectingWithTextRangeSelector :: Selector
textRangeByIntersectingWithTextRangeSelector = mkSelector "textRangeByIntersectingWithTextRange:"

-- | @Selector@ for @textRangeByFormingUnionWithTextRange:@
textRangeByFormingUnionWithTextRangeSelector :: Selector
textRangeByFormingUnionWithTextRangeSelector = mkSelector "textRangeByFormingUnionWithTextRange:"

-- | @Selector@ for @empty@
emptySelector :: Selector
emptySelector = mkSelector "empty"

