{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The abstract superclass for objects representing notable features detected in an image.
--
-- > Note: In macOS 10.13, iOS 11, and tvOS 11 or later, the Vision framework replaces these classes  for identifying and analyzing image features. See <doc://com.apple.documentation/documentation/vision/vnobservation>)
--
-- A @CIFeature@ object represents a portion of an image that a detector believes matches its criteria.  Subclasses of CIFeature holds additional information specific to the detector that discovered the feature.
--
-- Generated bindings for @CIFeature@.
module ObjC.CoreImage.CIFeature
  ( CIFeature
  , IsCIFeature(..)
  , type_
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The type of feature that was discovered.
--
-- The type can be one of: * ``CIFeatureTypeFace`` * ``CIFeatureTypeRectangle`` * ``CIFeatureTypeQRCode`` * ``CIFeatureTypeText``
--
-- ObjC selector: @- type@
type_ :: IsCIFeature ciFeature => ciFeature -> IO (Id NSString)
type_ ciFeature =
  sendMessage ciFeature typeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

