{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.PencilKit.Internal.Classes (
    module ObjC.PencilKit.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- PKDrawing ----------

-- | The data model object for storing drawing data created from PKCanvasView.
-- 
-- Phantom type for @PKDrawing@.
data PKDrawing

instance IsObjCObject (Id PKDrawing) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKDrawing"

class IsNSObject a => IsPKDrawing a where
  toPKDrawing :: a -> Id PKDrawing

instance IsPKDrawing (Id PKDrawing) where
  toPKDrawing = unsafeCastId

instance IsNSObject (Id PKDrawing) where
  toNSObject = unsafeCastId

-- ---------- PKFloatRange ----------

-- | Phantom type for @PKFloatRange@.
data PKFloatRange

instance IsObjCObject (Id PKFloatRange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKFloatRange"

class IsNSObject a => IsPKFloatRange a where
  toPKFloatRange :: a -> Id PKFloatRange

instance IsPKFloatRange (Id PKFloatRange) where
  toPKFloatRange = unsafeCastId

instance IsNSObject (Id PKFloatRange) where
  toNSObject = unsafeCastId

-- ---------- PKInk ----------

-- | PKInk provides a description of how marks on a PKCanvas render and are created.
-- 
-- Phantom type for @PKInk@.
data PKInk

instance IsObjCObject (Id PKInk) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKInk"

class IsNSObject a => IsPKInk a where
  toPKInk :: a -> Id PKInk

instance IsPKInk (Id PKInk) where
  toPKInk = unsafeCastId

instance IsNSObject (Id PKInk) where
  toNSObject = unsafeCastId

-- ---------- PKStroke ----------

-- | The data model value representing a stroke in a @PKDrawing@.
-- 
-- Phantom type for @PKStroke@.
data PKStroke

instance IsObjCObject (Id PKStroke) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKStroke"

class IsNSObject a => IsPKStroke a where
  toPKStroke :: a -> Id PKStroke

instance IsPKStroke (Id PKStroke) where
  toPKStroke = unsafeCastId

instance IsNSObject (Id PKStroke) where
  toNSObject = unsafeCastId

-- ---------- PKStrokePath ----------

-- | A uniform cubic B-spline representing the point data of a @PKStroke@.
-- 
-- Phantom type for @PKStrokePath@.
data PKStrokePath

instance IsObjCObject (Id PKStrokePath) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKStrokePath"

class IsNSObject a => IsPKStrokePath a where
  toPKStrokePath :: a -> Id PKStrokePath

instance IsPKStrokePath (Id PKStrokePath) where
  toPKStrokePath = unsafeCastId

instance IsNSObject (Id PKStrokePath) where
  toNSObject = unsafeCastId

-- ---------- PKStrokePoint ----------

-- | A point value stores all the attributes of a PKStroke at a specific point. @PKStrokePoint@ stores its properties compressed, the value read for a property may not exactly equal the value set for a property.
-- 
-- Phantom type for @PKStrokePoint@.
data PKStrokePoint

instance IsObjCObject (Id PKStrokePoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKStrokePoint"

class IsNSObject a => IsPKStrokePoint a where
  toPKStrokePoint :: a -> Id PKStrokePoint

instance IsPKStrokePoint (Id PKStrokePoint) where
  toPKStrokePoint = unsafeCastId

instance IsNSObject (Id PKStrokePoint) where
  toNSObject = unsafeCastId

-- ---------- PKTool ----------

-- | An interaction behavior for a PKCanvasView. Should not be subclassed outside of the PencilKit framework.
-- 
-- Phantom type for @PKTool@.
data PKTool

instance IsObjCObject (Id PKTool) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKTool"

class IsNSObject a => IsPKTool a where
  toPKTool :: a -> Id PKTool

instance IsPKTool (Id PKTool) where
  toPKTool = unsafeCastId

instance IsNSObject (Id PKTool) where
  toNSObject = unsafeCastId

-- ---------- PKToolPickerItem ----------

-- | A user interface for a tool item in PKToolPicker.
-- 
-- Phantom type for @PKToolPickerItem@.
data PKToolPickerItem

instance IsObjCObject (Id PKToolPickerItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKToolPickerItem"

class IsNSObject a => IsPKToolPickerItem a where
  toPKToolPickerItem :: a -> Id PKToolPickerItem

instance IsPKToolPickerItem (Id PKToolPickerItem) where
  toPKToolPickerItem = unsafeCastId

instance IsNSObject (Id PKToolPickerItem) where
  toNSObject = unsafeCastId

-- ---------- PKEraserTool ----------

-- | An eraser tool for erasing parts of a drawing.
-- 
-- Phantom type for @PKEraserTool@.
data PKEraserTool

instance IsObjCObject (Id PKEraserTool) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKEraserTool"

class IsPKTool a => IsPKEraserTool a where
  toPKEraserTool :: a -> Id PKEraserTool

instance IsPKEraserTool (Id PKEraserTool) where
  toPKEraserTool = unsafeCastId

instance IsNSObject (Id PKEraserTool) where
  toNSObject = unsafeCastId

instance IsPKTool (Id PKEraserTool) where
  toPKTool = unsafeCastId

-- ---------- PKInkingTool ----------

-- | A tool for drawing on a PKCanvasView.
-- 
-- Phantom type for @PKInkingTool@.
data PKInkingTool

instance IsObjCObject (Id PKInkingTool) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKInkingTool"

class IsPKTool a => IsPKInkingTool a where
  toPKInkingTool :: a -> Id PKInkingTool

instance IsPKInkingTool (Id PKInkingTool) where
  toPKInkingTool = unsafeCastId

instance IsNSObject (Id PKInkingTool) where
  toNSObject = unsafeCastId

instance IsPKTool (Id PKInkingTool) where
  toPKTool = unsafeCastId

-- ---------- PKLassoTool ----------

-- | A lasso tool for selecting parts of a drawing.
-- 
-- Phantom type for @PKLassoTool@.
data PKLassoTool

instance IsObjCObject (Id PKLassoTool) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKLassoTool"

class IsPKTool a => IsPKLassoTool a where
  toPKLassoTool :: a -> Id PKLassoTool

instance IsPKLassoTool (Id PKLassoTool) where
  toPKLassoTool = unsafeCastId

instance IsNSObject (Id PKLassoTool) where
  toNSObject = unsafeCastId

instance IsPKTool (Id PKLassoTool) where
  toPKTool = unsafeCastId

-- ---------- PKToolPickerEraserItem ----------

-- | A user interface for an eraser tool item in PKToolPicker.
-- 
-- Phantom type for @PKToolPickerEraserItem@.
data PKToolPickerEraserItem

instance IsObjCObject (Id PKToolPickerEraserItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKToolPickerEraserItem"

class IsPKToolPickerItem a => IsPKToolPickerEraserItem a where
  toPKToolPickerEraserItem :: a -> Id PKToolPickerEraserItem

instance IsPKToolPickerEraserItem (Id PKToolPickerEraserItem) where
  toPKToolPickerEraserItem = unsafeCastId

instance IsNSObject (Id PKToolPickerEraserItem) where
  toNSObject = unsafeCastId

instance IsPKToolPickerItem (Id PKToolPickerEraserItem) where
  toPKToolPickerItem = unsafeCastId

-- ---------- PKToolPickerInkingItem ----------

-- | A user interface for an inking tool item in PKToolPicker.
-- 
-- Phantom type for @PKToolPickerInkingItem@.
data PKToolPickerInkingItem

instance IsObjCObject (Id PKToolPickerInkingItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "PKToolPickerInkingItem"

class IsPKToolPickerItem a => IsPKToolPickerInkingItem a where
  toPKToolPickerInkingItem :: a -> Id PKToolPickerInkingItem

instance IsPKToolPickerInkingItem (Id PKToolPickerInkingItem) where
  toPKToolPickerInkingItem = unsafeCastId

instance IsNSObject (Id PKToolPickerInkingItem) where
  toNSObject = unsafeCastId

instance IsPKToolPickerItem (Id PKToolPickerInkingItem) where
  toPKToolPickerItem = unsafeCastId
