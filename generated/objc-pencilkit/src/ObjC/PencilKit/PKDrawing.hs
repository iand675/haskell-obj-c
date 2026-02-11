{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithStrokesSelector
  , initWithData_errorSelector
  , dataRepresentationSelector
  , drawingByAppendingDrawingSelector
  , drawingByAppendingStrokesSelector
  , strokesSelector
  , requiredContentVersionSelector

  -- * Enum types
  , PKContentVersion(PKContentVersion)
  , pattern PKContentVersion1
  , pattern PKContentVersion2
  , pattern PKContentVersion3
  , pattern PKContentVersion4
  , pattern PKContentVersionLatest

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

import ObjC.PencilKit.Internal.Classes
import ObjC.PencilKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes and returns a blank drawing.
--
-- ObjC selector: @- init@
init_ :: IsPKDrawing pkDrawing => pkDrawing -> IO (Id PKDrawing)
init_ pkDrawing  =
  sendMsg pkDrawing (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initializes a drawing with an array of strokes.
--
-- ObjC selector: @- initWithStrokes:@
initWithStrokes :: (IsPKDrawing pkDrawing, IsNSArray strokes) => pkDrawing -> strokes -> IO (Id PKDrawing)
initWithStrokes pkDrawing  strokes =
withObjCPtr strokes $ \raw_strokes ->
    sendMsg pkDrawing (mkSelector "initWithStrokes:") (retPtr retVoid) [argPtr (castPtr raw_strokes :: Ptr ())] >>= ownedObject . castPtr

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
initWithData_error pkDrawing  data_ error_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg pkDrawing (mkSelector "initWithData:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Generate a data representation of the drawing.
--
-- Returns: A NSData object containing a representation of the drawing.
--
-- ObjC selector: @- dataRepresentation@
dataRepresentation :: IsPKDrawing pkDrawing => pkDrawing -> IO (Id NSData)
dataRepresentation pkDrawing  =
  sendMsg pkDrawing (mkSelector "dataRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a new drawing by appending the contents of @drawing@ on top of the receiver’s contents.
--
-- @drawing@ — The drawing to append.
--
-- Returns: A new copy of this drawing with @drawing@ appended onto it.
--
-- ObjC selector: @- drawingByAppendingDrawing:@
drawingByAppendingDrawing :: (IsPKDrawing pkDrawing, IsPKDrawing drawing) => pkDrawing -> drawing -> IO (Id PKDrawing)
drawingByAppendingDrawing pkDrawing  drawing =
withObjCPtr drawing $ \raw_drawing ->
    sendMsg pkDrawing (mkSelector "drawingByAppendingDrawing:") (retPtr retVoid) [argPtr (castPtr raw_drawing :: Ptr ())] >>= retainedObject . castPtr

-- | Create a new drawing by appending an array of strokes to this drawing. This is a convenience method, to quickly add strokes to a drawing.
--
-- @strokes@ — The strokes to append.
--
-- Returns: A new copy of this drawing with @strokes@ appended onto it.
--
-- ObjC selector: @- drawingByAppendingStrokes:@
drawingByAppendingStrokes :: (IsPKDrawing pkDrawing, IsNSArray strokes) => pkDrawing -> strokes -> IO (Id PKDrawing)
drawingByAppendingStrokes pkDrawing  strokes =
withObjCPtr strokes $ \raw_strokes ->
    sendMsg pkDrawing (mkSelector "drawingByAppendingStrokes:") (retPtr retVoid) [argPtr (castPtr raw_strokes :: Ptr ())] >>= retainedObject . castPtr

-- | The strokes that this drawing contains.
--
-- ObjC selector: @- strokes@
strokes :: IsPKDrawing pkDrawing => pkDrawing -> IO (Id NSArray)
strokes pkDrawing  =
  sendMsg pkDrawing (mkSelector "strokes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The PencilKit version required to use this drawing.
--
-- ObjC selector: @- requiredContentVersion@
requiredContentVersion :: IsPKDrawing pkDrawing => pkDrawing -> IO PKContentVersion
requiredContentVersion pkDrawing  =
  fmap (coerce :: CLong -> PKContentVersion) $ sendMsg pkDrawing (mkSelector "requiredContentVersion") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithStrokes:@
initWithStrokesSelector :: Selector
initWithStrokesSelector = mkSelector "initWithStrokes:"

-- | @Selector@ for @initWithData:error:@
initWithData_errorSelector :: Selector
initWithData_errorSelector = mkSelector "initWithData:error:"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector
dataRepresentationSelector = mkSelector "dataRepresentation"

-- | @Selector@ for @drawingByAppendingDrawing:@
drawingByAppendingDrawingSelector :: Selector
drawingByAppendingDrawingSelector = mkSelector "drawingByAppendingDrawing:"

-- | @Selector@ for @drawingByAppendingStrokes:@
drawingByAppendingStrokesSelector :: Selector
drawingByAppendingStrokesSelector = mkSelector "drawingByAppendingStrokes:"

-- | @Selector@ for @strokes@
strokesSelector :: Selector
strokesSelector = mkSelector "strokes"

-- | @Selector@ for @requiredContentVersion@
requiredContentVersionSelector :: Selector
requiredContentVersionSelector = mkSelector "requiredContentVersion"

