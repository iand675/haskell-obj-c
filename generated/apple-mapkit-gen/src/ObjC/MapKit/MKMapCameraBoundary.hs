{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapCameraBoundary@.
module ObjC.MapKit.MKMapCameraBoundary
  ( MKMapCameraBoundary
  , IsMKMapCameraBoundary(..)
  , initWithCoder
  , initWithCoderSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCoder:@
initWithCoder :: (IsMKMapCameraBoundary mkMapCameraBoundary, IsNSCoder coder) => mkMapCameraBoundary -> coder -> IO (Id MKMapCameraBoundary)
initWithCoder mkMapCameraBoundary coder =
  sendOwnedMessage mkMapCameraBoundary initWithCoderSelector (toNSCoder coder)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id MKMapCameraBoundary)
initWithCoderSelector = mkSelector "initWithCoder:"

