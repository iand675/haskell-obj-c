{-# LANGUAGE DataKinds #-}
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
  , location
  , endLocation
  , containsLocationSelector
  , containsRangeSelector
  , emptySelector
  , endLocationSelector
  , initSelector
  , initWithLocationSelector
  , initWithLocation_endLocationSelector
  , intersectsWithTextRangeSelector
  , isEqualToTextRangeSelector
  , locationSelector
  , newSelector
  , textRangeByFormingUnionWithTextRangeSelector
  , textRangeByIntersectingWithTextRangeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithLocation_endLocation nsTextRange location endLocation =
  sendOwnedMessage nsTextRange initWithLocation_endLocationSelector location endLocation

-- | Creates a new text range at the location you specify.
--
-- - Parameters:   - location: An ``NSTextLocation``.
--
-- ObjC selector: @- initWithLocation:@
initWithLocation :: IsNSTextRange nsTextRange => nsTextRange -> RawId -> IO (Id NSTextRange)
initWithLocation nsTextRange location =
  sendOwnedMessage nsTextRange initWithLocationSelector location

-- | @- init@
init_ :: IsNSTextRange nsTextRange => nsTextRange -> IO (Id NSTextRange)
init_ nsTextRange =
  sendOwnedMessage nsTextRange initSelector

-- | @+ new@
new :: IO (Id NSTextRange)
new  =
  do
    cls' <- getRequiredClass "NSTextRange"
    sendOwnedClassMessage cls' newSelector

-- | Compares two text ranges.
--
-- - Parameters:   - textRange: The range used to compare against the current range to evaluate for differences.
--
-- - Returns: Returns @true@ if the ranges are equal.
--
-- ObjC selector: @- isEqualToTextRange:@
isEqualToTextRange :: (IsNSTextRange nsTextRange, IsNSTextRange textRange) => nsTextRange -> textRange -> IO Bool
isEqualToTextRange nsTextRange textRange =
  sendMessage nsTextRange isEqualToTextRangeSelector (toNSTextRange textRange)

-- | Determines if the text location you specify is in the current text range.
--
-- - Parameters:   - location: An ``NSTextLocation``.
--
-- - Returns: Returns @true@ if the location is in the range otherwise @false@ .
--
-- ObjC selector: @- containsLocation:@
containsLocation :: IsNSTextRange nsTextRange => nsTextRange -> RawId -> IO Bool
containsLocation nsTextRange location =
  sendMessage nsTextRange containsLocationSelector location

-- | Determines if the text range you specify is in the current text range.
--
-- - Parameters:   - textRange: An ``NSTextRange``.
--
-- - Returns: Returns @true@ if the range you provide is in the current range; otherwise @false@.
--
-- ObjC selector: @- containsRange:@
containsRange :: (IsNSTextRange nsTextRange, IsNSTextRange textRange) => nsTextRange -> textRange -> IO Bool
containsRange nsTextRange textRange =
  sendMessage nsTextRange containsRangeSelector (toNSTextRange textRange)

-- | Determines if two ranges intersect.
--
-- - Parameters:   - textRange: The range used to compare against the current range to evaluate for differences.
--
-- - Returns: Returns @true@ if the ranges intersect.
--
-- ObjC selector: @- intersectsWithTextRange:@
intersectsWithTextRange :: (IsNSTextRange nsTextRange, IsNSTextRange textRange) => nsTextRange -> textRange -> IO Bool
intersectsWithTextRange nsTextRange textRange =
  sendMessage nsTextRange intersectsWithTextRangeSelector (toNSTextRange textRange)

-- | Returns the range, if any, where two text ranges intersect.
--
-- - Parameters:   - textRange: The range used to compare against the current range to evaluate for differences.
--
-- - Returns: An   <doc://com.apple.documentation/documentation/foundation/nsrange> that   represents the intersection of the ranges, or @nil@ if they don't   intersect.
--
-- ObjC selector: @- textRangeByIntersectingWithTextRange:@
textRangeByIntersectingWithTextRange :: (IsNSTextRange nsTextRange, IsNSTextRange textRange) => nsTextRange -> textRange -> IO (Id NSTextRange)
textRangeByIntersectingWithTextRange nsTextRange textRange =
  sendMessage nsTextRange textRangeByIntersectingWithTextRangeSelector (toNSTextRange textRange)

-- | Returns a new text range by forming the union with the text range you provide.
--
-- - Parameters:   - textRange: The range to use to create the union.
--
-- - Returns: An ``NSTextRange`` that represent the union of the two ranges.
--
-- ObjC selector: @- textRangeByFormingUnionWithTextRange:@
textRangeByFormingUnionWithTextRange :: (IsNSTextRange nsTextRange, IsNSTextRange textRange) => nsTextRange -> textRange -> IO (Id NSTextRange)
textRangeByFormingUnionWithTextRange nsTextRange textRange =
  sendMessage nsTextRange textRangeByFormingUnionWithTextRangeSelector (toNSTextRange textRange)

-- | Returns whether the text range is empty.
--
-- ObjC selector: @- empty@
empty :: IsNSTextRange nsTextRange => nsTextRange -> IO Bool
empty nsTextRange =
  sendMessage nsTextRange emptySelector

-- | The starting location of the text range.
--
-- ObjC selector: @- location@
location :: IsNSTextRange nsTextRange => nsTextRange -> IO RawId
location nsTextRange =
  sendMessage nsTextRange locationSelector

-- | The ending location of the text range.
--
-- ObjC selector: @- endLocation@
endLocation :: IsNSTextRange nsTextRange => nsTextRange -> IO RawId
endLocation nsTextRange =
  sendMessage nsTextRange endLocationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLocation:endLocation:@
initWithLocation_endLocationSelector :: Selector '[RawId, RawId] (Id NSTextRange)
initWithLocation_endLocationSelector = mkSelector "initWithLocation:endLocation:"

-- | @Selector@ for @initWithLocation:@
initWithLocationSelector :: Selector '[RawId] (Id NSTextRange)
initWithLocationSelector = mkSelector "initWithLocation:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTextRange)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSTextRange)
newSelector = mkSelector "new"

-- | @Selector@ for @isEqualToTextRange:@
isEqualToTextRangeSelector :: Selector '[Id NSTextRange] Bool
isEqualToTextRangeSelector = mkSelector "isEqualToTextRange:"

-- | @Selector@ for @containsLocation:@
containsLocationSelector :: Selector '[RawId] Bool
containsLocationSelector = mkSelector "containsLocation:"

-- | @Selector@ for @containsRange:@
containsRangeSelector :: Selector '[Id NSTextRange] Bool
containsRangeSelector = mkSelector "containsRange:"

-- | @Selector@ for @intersectsWithTextRange:@
intersectsWithTextRangeSelector :: Selector '[Id NSTextRange] Bool
intersectsWithTextRangeSelector = mkSelector "intersectsWithTextRange:"

-- | @Selector@ for @textRangeByIntersectingWithTextRange:@
textRangeByIntersectingWithTextRangeSelector :: Selector '[Id NSTextRange] (Id NSTextRange)
textRangeByIntersectingWithTextRangeSelector = mkSelector "textRangeByIntersectingWithTextRange:"

-- | @Selector@ for @textRangeByFormingUnionWithTextRange:@
textRangeByFormingUnionWithTextRangeSelector :: Selector '[Id NSTextRange] (Id NSTextRange)
textRangeByFormingUnionWithTextRangeSelector = mkSelector "textRangeByFormingUnionWithTextRange:"

-- | @Selector@ for @empty@
emptySelector :: Selector '[] Bool
emptySelector = mkSelector "empty"

-- | @Selector@ for @location@
locationSelector :: Selector '[] RawId
locationSelector = mkSelector "location"

-- | @Selector@ for @endLocation@
endLocationSelector :: Selector '[] RawId
endLocationSelector = mkSelector "endLocation"

