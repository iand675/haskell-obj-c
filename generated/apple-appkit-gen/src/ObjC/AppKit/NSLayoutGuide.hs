{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , bottomAnchorSelector
  , centerXAnchorSelector
  , centerYAnchorSelector
  , constraintsAffectingLayoutForOrientationSelector
  , frameSelector
  , hasAmbiguousLayoutSelector
  , heightAnchorSelector
  , identifierSelector
  , leadingAnchorSelector
  , leftAnchorSelector
  , owningViewSelector
  , rightAnchorSelector
  , setIdentifierSelector
  , setOwningViewSelector
  , topAnchorSelector
  , trailingAnchorSelector
  , widthAnchorSelector

  -- * Enum types
  , NSLayoutConstraintOrientation(NSLayoutConstraintOrientation)
  , pattern NSLayoutConstraintOrientationHorizontal
  , pattern NSLayoutConstraintOrientationVertical

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

-- | @- constraintsAffectingLayoutForOrientation:@
constraintsAffectingLayoutForOrientation :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> NSLayoutConstraintOrientation -> IO (Id NSArray)
constraintsAffectingLayoutForOrientation nsLayoutGuide orientation =
  sendMessage nsLayoutGuide constraintsAffectingLayoutForOrientationSelector orientation

-- | @- frame@
frame :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO NSRect
frame nsLayoutGuide =
  sendMessage nsLayoutGuide frameSelector

-- | @- owningView@
owningView :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSView)
owningView nsLayoutGuide =
  sendMessage nsLayoutGuide owningViewSelector

-- | @- setOwningView:@
setOwningView :: (IsNSLayoutGuide nsLayoutGuide, IsNSView value) => nsLayoutGuide -> value -> IO ()
setOwningView nsLayoutGuide value =
  sendMessage nsLayoutGuide setOwningViewSelector (toNSView value)

-- | @- identifier@
identifier :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSString)
identifier nsLayoutGuide =
  sendMessage nsLayoutGuide identifierSelector

-- | @- setIdentifier:@
setIdentifier :: (IsNSLayoutGuide nsLayoutGuide, IsNSString value) => nsLayoutGuide -> value -> IO ()
setIdentifier nsLayoutGuide value =
  sendMessage nsLayoutGuide setIdentifierSelector (toNSString value)

-- | @- leadingAnchor@
leadingAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutXAxisAnchor)
leadingAnchor nsLayoutGuide =
  sendMessage nsLayoutGuide leadingAnchorSelector

-- | @- trailingAnchor@
trailingAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutXAxisAnchor)
trailingAnchor nsLayoutGuide =
  sendMessage nsLayoutGuide trailingAnchorSelector

-- | @- leftAnchor@
leftAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutXAxisAnchor)
leftAnchor nsLayoutGuide =
  sendMessage nsLayoutGuide leftAnchorSelector

-- | @- rightAnchor@
rightAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutXAxisAnchor)
rightAnchor nsLayoutGuide =
  sendMessage nsLayoutGuide rightAnchorSelector

-- | @- topAnchor@
topAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutYAxisAnchor)
topAnchor nsLayoutGuide =
  sendMessage nsLayoutGuide topAnchorSelector

-- | @- bottomAnchor@
bottomAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutYAxisAnchor)
bottomAnchor nsLayoutGuide =
  sendMessage nsLayoutGuide bottomAnchorSelector

-- | @- widthAnchor@
widthAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutDimension)
widthAnchor nsLayoutGuide =
  sendMessage nsLayoutGuide widthAnchorSelector

-- | @- heightAnchor@
heightAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutDimension)
heightAnchor nsLayoutGuide =
  sendMessage nsLayoutGuide heightAnchorSelector

-- | @- centerXAnchor@
centerXAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutXAxisAnchor)
centerXAnchor nsLayoutGuide =
  sendMessage nsLayoutGuide centerXAnchorSelector

-- | @- centerYAnchor@
centerYAnchor :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO (Id NSLayoutYAxisAnchor)
centerYAnchor nsLayoutGuide =
  sendMessage nsLayoutGuide centerYAnchorSelector

-- | @- hasAmbiguousLayout@
hasAmbiguousLayout :: IsNSLayoutGuide nsLayoutGuide => nsLayoutGuide -> IO Bool
hasAmbiguousLayout nsLayoutGuide =
  sendMessage nsLayoutGuide hasAmbiguousLayoutSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @constraintsAffectingLayoutForOrientation:@
constraintsAffectingLayoutForOrientationSelector :: Selector '[NSLayoutConstraintOrientation] (Id NSArray)
constraintsAffectingLayoutForOrientationSelector = mkSelector "constraintsAffectingLayoutForOrientation:"

-- | @Selector@ for @frame@
frameSelector :: Selector '[] NSRect
frameSelector = mkSelector "frame"

-- | @Selector@ for @owningView@
owningViewSelector :: Selector '[] (Id NSView)
owningViewSelector = mkSelector "owningView"

-- | @Selector@ for @setOwningView:@
setOwningViewSelector :: Selector '[Id NSView] ()
setOwningViewSelector = mkSelector "setOwningView:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @leadingAnchor@
leadingAnchorSelector :: Selector '[] (Id NSLayoutXAxisAnchor)
leadingAnchorSelector = mkSelector "leadingAnchor"

-- | @Selector@ for @trailingAnchor@
trailingAnchorSelector :: Selector '[] (Id NSLayoutXAxisAnchor)
trailingAnchorSelector = mkSelector "trailingAnchor"

-- | @Selector@ for @leftAnchor@
leftAnchorSelector :: Selector '[] (Id NSLayoutXAxisAnchor)
leftAnchorSelector = mkSelector "leftAnchor"

-- | @Selector@ for @rightAnchor@
rightAnchorSelector :: Selector '[] (Id NSLayoutXAxisAnchor)
rightAnchorSelector = mkSelector "rightAnchor"

-- | @Selector@ for @topAnchor@
topAnchorSelector :: Selector '[] (Id NSLayoutYAxisAnchor)
topAnchorSelector = mkSelector "topAnchor"

-- | @Selector@ for @bottomAnchor@
bottomAnchorSelector :: Selector '[] (Id NSLayoutYAxisAnchor)
bottomAnchorSelector = mkSelector "bottomAnchor"

-- | @Selector@ for @widthAnchor@
widthAnchorSelector :: Selector '[] (Id NSLayoutDimension)
widthAnchorSelector = mkSelector "widthAnchor"

-- | @Selector@ for @heightAnchor@
heightAnchorSelector :: Selector '[] (Id NSLayoutDimension)
heightAnchorSelector = mkSelector "heightAnchor"

-- | @Selector@ for @centerXAnchor@
centerXAnchorSelector :: Selector '[] (Id NSLayoutXAxisAnchor)
centerXAnchorSelector = mkSelector "centerXAnchor"

-- | @Selector@ for @centerYAnchor@
centerYAnchorSelector :: Selector '[] (Id NSLayoutYAxisAnchor)
centerYAnchorSelector = mkSelector "centerYAnchor"

-- | @Selector@ for @hasAmbiguousLayout@
hasAmbiguousLayoutSelector :: Selector '[] Bool
hasAmbiguousLayoutSelector = mkSelector "hasAmbiguousLayout"

