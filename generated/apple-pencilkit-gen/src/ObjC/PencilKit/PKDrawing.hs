{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The data model object for storing drawing data created from PKCanvasView.
--
-- Generated bindings for @PKDrawing@.
module ObjC.PencilKit.PKDrawing
  ( PKDrawing
  , IsPKDrawing(..)
  , init_
  , initWithStrokes
  , initWithData_error
  , dataRepresentation
  , drawingByAppendingDrawing
  , drawingByAppendingStrokes
  , strokes
  , requiredContentVersion
  , dataRepresentationSelector
  , drawingByAppendingDrawingSelector
  , drawingByAppendingStrokesSelector
  , initSelector
  , initWithData_errorSelector
  , initWithStrokesSelector
  , requiredContentVersionSelector
  , strokesSelector

  -- * Enum types
  , PKContentVersion(PKContentVersion)
  , pattern PKContentVersion1
  , pattern PKContentVersion2
  , pattern PKContentVersion3
  , pattern PKContentVersion4
  , pattern PKContentVersionLatest

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PencilKit.Internal.Classes
import ObjC.PencilKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes and returns a blank drawing.
--
-- ObjC selector: @- init@
init_ :: IsPKDrawing pkDrawing => pkDrawing -> IO (Id PKDrawing)
init_ pkDrawing =
  sendOwnedMessage pkDrawing initSelector

-- | Initializes a drawing with an array of strokes.
--
-- ObjC selector: @- initWithStrokes:@
initWithStrokes :: (IsPKDrawing pkDrawing, IsNSArray strokes) => pkDrawing -> strokes -> IO (Id PKDrawing)
initWithStrokes pkDrawing strokes =
  sendOwnedMessage pkDrawing initWithStrokesSelector (toNSArray strokes)

-- | Initializes and returns the drawing with the specified data.
--
-- @data@ — The data containing the drawing data.
--
-- @error@ — If an error occurs, upon return the NSError object describes the error.   Set to NULL to ignore errors.
--
-- Returns: On success, an initialized PKDrawing object. If nil, the outError parameter   contains an NSError instance describing the problem.
--
-- ObjC selector: @- initWithData:error:@
initWithData_error :: (IsPKDrawing pkDrawing, IsNSData data_, IsNSError error_) => pkDrawing -> data_ -> error_ -> IO (Id PKDrawing)
initWithData_error pkDrawing data_ error_ =
  sendOwnedMessage pkDrawing initWithData_errorSelector (toNSData data_) (toNSError error_)

-- | Generate a data representation of the drawing.
--
-- Returns: A NSData object containing a representation of the drawing.
--
-- ObjC selector: @- dataRepresentation@
dataRepresentation :: IsPKDrawing pkDrawing => pkDrawing -> IO (Id NSData)
dataRepresentation pkDrawing =
  sendMessage pkDrawing dataRepresentationSelector

-- | Returns a new drawing by appending the contents of @drawing@ on top of the receiver’s contents.
--
-- @drawing@ — The drawing to append.
--
-- Returns: A new copy of this drawing with @drawing@ appended onto it.
--
-- ObjC selector: @- drawingByAppendingDrawing:@
drawingByAppendingDrawing :: (IsPKDrawing pkDrawing, IsPKDrawing drawing) => pkDrawing -> drawing -> IO (Id PKDrawing)
drawingByAppendingDrawing pkDrawing drawing =
  sendMessage pkDrawing drawingByAppendingDrawingSelector (toPKDrawing drawing)

-- | Create a new drawing by appending an array of strokes to this drawing. This is a convenience method, to quickly add strokes to a drawing.
--
-- @strokes@ — The strokes to append.
--
-- Returns: A new copy of this drawing with @strokes@ appended onto it.
--
-- ObjC selector: @- drawingByAppendingStrokes:@
drawingByAppendingStrokes :: (IsPKDrawing pkDrawing, IsNSArray strokes) => pkDrawing -> strokes -> IO (Id PKDrawing)
drawingByAppendingStrokes pkDrawing strokes =
  sendMessage pkDrawing drawingByAppendingStrokesSelector (toNSArray strokes)

-- | The strokes that this drawing contains.
--
-- ObjC selector: @- strokes@
strokes :: IsPKDrawing pkDrawing => pkDrawing -> IO (Id NSArray)
strokes pkDrawing =
  sendMessage pkDrawing strokesSelector

-- | The PencilKit version required to use this drawing.
--
-- ObjC selector: @- requiredContentVersion@
requiredContentVersion :: IsPKDrawing pkDrawing => pkDrawing -> IO PKContentVersion
requiredContentVersion pkDrawing =
  sendMessage pkDrawing requiredContentVersionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKDrawing)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithStrokes:@
initWithStrokesSelector :: Selector '[Id NSArray] (Id PKDrawing)
initWithStrokesSelector = mkSelector "initWithStrokes:"

-- | @Selector@ for @initWithData:error:@
initWithData_errorSelector :: Selector '[Id NSData, Id NSError] (Id PKDrawing)
initWithData_errorSelector = mkSelector "initWithData:error:"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector '[] (Id NSData)
dataRepresentationSelector = mkSelector "dataRepresentation"

-- | @Selector@ for @drawingByAppendingDrawing:@
drawingByAppendingDrawingSelector :: Selector '[Id PKDrawing] (Id PKDrawing)
drawingByAppendingDrawingSelector = mkSelector "drawingByAppendingDrawing:"

-- | @Selector@ for @drawingByAppendingStrokes:@
drawingByAppendingStrokesSelector :: Selector '[Id NSArray] (Id PKDrawing)
drawingByAppendingStrokesSelector = mkSelector "drawingByAppendingStrokes:"

-- | @Selector@ for @strokes@
strokesSelector :: Selector '[] (Id NSArray)
strokesSelector = mkSelector "strokes"

-- | @Selector@ for @requiredContentVersion@
requiredContentVersionSelector :: Selector '[] PKContentVersion
requiredContentVersionSelector = mkSelector "requiredContentVersion"

