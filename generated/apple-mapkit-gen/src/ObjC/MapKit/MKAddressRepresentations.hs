{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKAddressRepresentations@.
module ObjC.MapKit.MKAddressRepresentations
  ( MKAddressRepresentations
  , IsMKAddressRepresentations(..)
  , init_
  , new
  , fullAddressIncludingRegion_singleLine
  , cityWithContextUsingStyle
  , cityName
  , cityWithContext
  , regionName
  , regionCode
  , cityNameSelector
  , cityWithContextSelector
  , cityWithContextUsingStyleSelector
  , fullAddressIncludingRegion_singleLineSelector
  , initSelector
  , newSelector
  , regionCodeSelector
  , regionNameSelector

  -- * Enum types
  , MKAddressRepresentationsContextStyle(MKAddressRepresentationsContextStyle)
  , pattern MKAddressRepresentationsContextStyleAutomatic
  , pattern MKAddressRepresentationsContextStyleShort
  , pattern MKAddressRepresentationsContextStyleFull

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMKAddressRepresentations mkAddressRepresentations => mkAddressRepresentations -> IO (Id MKAddressRepresentations)
init_ mkAddressRepresentations =
  sendOwnedMessage mkAddressRepresentations initSelector

-- | @+ new@
new :: IO (Id MKAddressRepresentations)
new  =
  do
    cls' <- getRequiredClass "MKAddressRepresentations"
    sendOwnedClassMessage cls' newSelector

-- | @- fullAddressIncludingRegion:singleLine:@
fullAddressIncludingRegion_singleLine :: IsMKAddressRepresentations mkAddressRepresentations => mkAddressRepresentations -> Bool -> Bool -> IO (Id NSString)
fullAddressIncludingRegion_singleLine mkAddressRepresentations includingRegion singleLine =
  sendMessage mkAddressRepresentations fullAddressIncludingRegion_singleLineSelector includingRegion singleLine

-- | @- cityWithContextUsingStyle:@
cityWithContextUsingStyle :: IsMKAddressRepresentations mkAddressRepresentations => mkAddressRepresentations -> MKAddressRepresentationsContextStyle -> IO (Id NSString)
cityWithContextUsingStyle mkAddressRepresentations style =
  sendMessage mkAddressRepresentations cityWithContextUsingStyleSelector style

-- | @- cityName@
cityName :: IsMKAddressRepresentations mkAddressRepresentations => mkAddressRepresentations -> IO (Id NSString)
cityName mkAddressRepresentations =
  sendMessage mkAddressRepresentations cityNameSelector

-- | @- cityWithContext@
cityWithContext :: IsMKAddressRepresentations mkAddressRepresentations => mkAddressRepresentations -> IO (Id NSString)
cityWithContext mkAddressRepresentations =
  sendMessage mkAddressRepresentations cityWithContextSelector

-- | @- regionName@
regionName :: IsMKAddressRepresentations mkAddressRepresentations => mkAddressRepresentations -> IO (Id NSString)
regionName mkAddressRepresentations =
  sendMessage mkAddressRepresentations regionNameSelector

-- | @- regionCode@
regionCode :: IsMKAddressRepresentations mkAddressRepresentations => mkAddressRepresentations -> IO (Id NSString)
regionCode mkAddressRepresentations =
  sendMessage mkAddressRepresentations regionCodeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MKAddressRepresentations)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MKAddressRepresentations)
newSelector = mkSelector "new"

-- | @Selector@ for @fullAddressIncludingRegion:singleLine:@
fullAddressIncludingRegion_singleLineSelector :: Selector '[Bool, Bool] (Id NSString)
fullAddressIncludingRegion_singleLineSelector = mkSelector "fullAddressIncludingRegion:singleLine:"

-- | @Selector@ for @cityWithContextUsingStyle:@
cityWithContextUsingStyleSelector :: Selector '[MKAddressRepresentationsContextStyle] (Id NSString)
cityWithContextUsingStyleSelector = mkSelector "cityWithContextUsingStyle:"

-- | @Selector@ for @cityName@
cityNameSelector :: Selector '[] (Id NSString)
cityNameSelector = mkSelector "cityName"

-- | @Selector@ for @cityWithContext@
cityWithContextSelector :: Selector '[] (Id NSString)
cityWithContextSelector = mkSelector "cityWithContext"

-- | @Selector@ for @regionName@
regionNameSelector :: Selector '[] (Id NSString)
regionNameSelector = mkSelector "regionName"

-- | @Selector@ for @regionCode@
regionCodeSelector :: Selector '[] (Id NSString)
regionCodeSelector = mkSelector "regionCode"

