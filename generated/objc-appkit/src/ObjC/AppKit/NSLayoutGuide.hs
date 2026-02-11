{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSLayoutGuide@.
module ObjC.AppKit.NSLayoutGuide
  ( NSLayoutGuide
  , IsNSLayoutGuide(..)
  , constraintsAffectingLayoutForOrientation
  , frame
  , owningView
  , setOwningView
  , identifier
  , setIdentifier
  , leadingAnchor
  , trailingAnchor
  , leftAnchor
  , rightAnchor
  , topAnchor
  , bottomAnchor
  , widthAnchor
  , heightAnchor
  , centerXAnchor
  , centerYAnchor
  , hasAmbiguousLayout
  , constraintsAffectingLayoutForOrientationSelector
  , frameSelector
  , owningViewSelector
  , setOwningViewSelector
  , identifierSelector
  , setIdentifierSelector
  , leadingAnchorSelector
  , trailingAnchorSelector
  , leftAnchorSelector
  , rightAnchorSelector
  , topAnchorSelector
  , bottomAnchorSelector
  , widthAnchorSelector
  , heightAnchorSelector
  , centerXAnchorSelector
  , centerYAnchorSelector
  , hasAmbiguousLayoutSelector

  -- * Enum types
  , NSLayoutConstraintOrientation(NSLayoutConstraintOrientation)
  , pattern NSLayoutConstraintOrientationHorizontal
  , pattern NSLayoutConstraintOrientationVertical

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

-- | @- constraintsAffectingLayoutForOrientation:@
constraintsAffectingLayoutForOrientation :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> NSLayoutConstraintOrientation -> IO (Id NSArray)
constraintsAffectingLayoutForOrientation nsLayoutGuide  orientation =
  sendMsg nsLayoutGuide (mkSelector "constraintsAffectingLayoutForOrientation:") (retPtr retVoid) [argCLong (coerce orientation)] >>= retainedObject . castPtr

-- | @- frame@
frame :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO NSRect
frame nsLayoutGuide  =
  sendMsgStret nsLayoutGuide (mkSelector "frame") retNSRect []

-- | @- owningView@
owningView :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSView)
owningView nsLayoutGuide  =
  sendMsg nsLayoutGuide (mkSelector "owningView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOwningView:@
setOwningView :: (IsNSLayoutGuide nsLayoutGuide, IsNSView value) => nsLayoutGuide -> value -> IO ()
setOwningView nsLayoutGuide  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsLayoutGuide (mkSelector "setOwningView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- identifier@
identifier :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSString)
identifier nsLayoutGuide  =
  sendMsg nsLayoutGuide (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIdentifier:@
setIdentifier :: (IsNSLayoutGuide nsLayoutGuide, IsNSString value) => nsLayoutGuide -> value -> IO ()
setIdentifier nsLayoutGuide  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsLayoutGuide (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- leadingAnchor@
leadingAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutXAxisAnchor)
leadingAnchor nsLayoutGuide  =
  sendMsg nsLayoutGuide (mkSelector "leadingAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- trailingAnchor@
trailingAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutXAxisAnchor)
trailingAnchor nsLayoutGuide  =
  sendMsg nsLayoutGuide (mkSelector "trailingAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- leftAnchor@
leftAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutXAxisAnchor)
leftAnchor nsLayoutGuide  =
  sendMsg nsLayoutGuide (mkSelector "leftAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rightAnchor@
rightAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutXAxisAnchor)
rightAnchor nsLayoutGuide  =
  sendMsg nsLayoutGuide (mkSelector "rightAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- topAnchor@
topAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutYAxisAnchor)
topAnchor nsLayoutGuide  =
  sendMsg nsLayoutGuide (mkSelector "topAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bottomAnchor@
bottomAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutYAxisAnchor)
bottomAnchor nsLayoutGuide  =
  sendMsg nsLayoutGuide (mkSelector "bottomAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- widthAnchor@
widthAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutDimension)
widthAnchor nsLayoutGuide  =
  sendMsg nsLayoutGuide (mkSelector "widthAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- heightAnchor@
heightAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutDimension)
heightAnchor nsLayoutGuide  =
  sendMsg nsLayoutGuide (mkSelector "heightAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- centerXAnchor@
centerXAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutXAxisAnchor)
centerXAnchor nsLayoutGuide  =
  sendMsg nsLayoutGuide (mkSelector "centerXAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- centerYAnchor@
centerYAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutYAxisAnchor)
centerYAnchor nsLayoutGuide  =
  sendMsg nsLayoutGuide (mkSelector "centerYAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hasAmbiguousLayout@
hasAmbiguousLayout :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO Bool
hasAmbiguousLayout nsLayoutGuide  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLayoutGuide (mkSelector "hasAmbiguousLayout") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @constraintsAffectingLayoutForOrientation:@
constraintsAffectingLayoutForOrientationSelector :: Selector
constraintsAffectingLayoutForOrientationSelector = mkSelector "constraintsAffectingLayoutForOrientation:"

-- | @Selector@ for @frame@
frameSelector :: Selector
frameSelector = mkSelector "frame"

-- | @Selector@ for @owningView@
owningViewSelector :: Selector
owningViewSelector = mkSelector "owningView"

-- | @Selector@ for @setOwningView:@
setOwningViewSelector :: Selector
setOwningViewSelector = mkSelector "setOwningView:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @leadingAnchor@
leadingAnchorSelector :: Selector
leadingAnchorSelector = mkSelector "leadingAnchor"

-- | @Selector@ for @trailingAnchor@
trailingAnchorSelector :: Selector
trailingAnchorSelector = mkSelector "trailingAnchor"

-- | @Selector@ for @leftAnchor@
leftAnchorSelector :: Selector
leftAnchorSelector = mkSelector "leftAnchor"

-- | @Selector@ for @rightAnchor@
rightAnchorSelector :: Selector
rightAnchorSelector = mkSelector "rightAnchor"

-- | @Selector@ for @topAnchor@
topAnchorSelector :: Selector
topAnchorSelector = mkSelector "topAnchor"

-- | @Selector@ for @bottomAnchor@
bottomAnchorSelector :: Selector
bottomAnchorSelector = mkSelector "bottomAnchor"

-- | @Selector@ for @widthAnchor@
widthAnchorSelector :: Selector
widthAnchorSelector = mkSelector "widthAnchor"

-- | @Selector@ for @heightAnchor@
heightAnchorSelector :: Selector
heightAnchorSelector = mkSelector "heightAnchor"

-- | @Selector@ for @centerXAnchor@
centerXAnchorSelector :: Selector
centerXAnchorSelector = mkSelector "centerXAnchor"

-- | @Selector@ for @centerYAnchor@
centerYAnchorSelector :: Selector
centerYAnchorSelector = mkSelector "centerYAnchor"

-- | @Selector@ for @hasAmbiguousLayout@
hasAmbiguousLayoutSelector :: Selector
hasAmbiguousLayoutSelector = mkSelector "hasAmbiguousLayout"

